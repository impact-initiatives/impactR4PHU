source("src/utils/utils_analysis.R")
source("src/utils/misc_utils.R")
source("src/utils/utils_descriptive_analysis.R")
source("src/utils/kobo_utils.R")
source("src/utils/check_kobo.R")
source("src/utils/tabular_analysis_utils.R")

library(dplyr)
options(scipen = 999)
options(dplyr.summarise.inform = FALSE)

# ------------------------------------------------------------------------------
label_colname <- tcltk::tk_select.list(names(readxl::read_excel(strings['path.tool'],"survey"))[grepl("label",names(readxl::read_excel(strings['path.tool'],"survey")))], title = "Label column to choose", multiple = F)

##  LOAD TOOL  -----------------------------------------------------------------

cat("\n> Loading Kobo tool from", strings['path.tool'], "...\n")

tool.survey <- load.tool.survey(strings['path.tool'])
tool.choices <- load.tool.choices(strings['path.tool'])

# ensure tool.choices has unique labels:
labels_groups <- tool.choices %>% select(-name) %>% distinct() %>%
  transmute(uname = paste0(list_name, "/", !!sym(label_colname))) %>%
  group_by(uname) %>% summarise(n = n()) %>% filter(n>1)
if(nrow(labels_groups) > 0){
  warning(paste0("You have some duplicated labels in your tool.choices!\nThey are: ",
                 paste(labels_groups$uname, collapse = ", ")),"\nGood luck working with that...")
}

rm(labels_groups)
cat("\n> ...Done.\n")
# remove _cat categories
tool.survey <-  tool.survey %>%
  filter(!name %in% c("hhs_cat","hhs_score","rcsi_cat","lcsi_cat","fcs_cat"))

##  LOAD DATA -------------------------------------------------------------------

cat("\n> Loading data for analysis from", strings['filename.data'], "...\n")
sheet_names <- readxl::excel_sheets(strings['filename.data'])
sheet_names[1] <- paste(sheet_names[1], "(main)")
cat("> Found the following datasheets:", paste(sheet_names, collapse = ", "), "\n")

# the first sheet is always named "main"!!!
sheet_names[1] <- "main"
data.list <- list("main" = readxl::read_excel(strings['filename.data'], sheet=1, col_types = "text"))

for(sheet in sheet_names[-1])
  data.list[[sheet]] <- readxl::read_excel(strings['filename.data'], sheet=sheet, col_types = "text")

# check for mismatch between datasheet names in tool.survey and in data:
tool_datasheets <- tool.survey %>% distinct(datasheet) %>% filter(!is.na(.)) %>% pull
if(length(sheet_names) != length(tool_datasheets)){
  warning(paste0("Mismatch in the number of datasheets found in tool.survey (", length(tool_datasheets), ")\tand in the data (", length(sheet_names), ")"))
  tool_datasheets <- tool_datasheets[1:length(sheet_names)]
}

if(any(sort(tool_datasheets) != sort(sheet_names))){
  if(params["fix_sheet_names_to_match"] %in% c("data", "data.list", "datalist")){
    # the 'datasheet' columns in tool.survey will be fixed to match the data sheet names
    cat(paste0("Mismatch in datasheet names will be fixed by changing tool.survey to match the sheet names from the data.\n"))
    for (sheet in sheet_names[-1]) {
      # take the first column from this sheet and find it in tool.survey
      i <- 1
      data_cnames <- data.list[[sheet]] %>% select(-contains("/"), -starts_with("_")) %>% names
      first_col <- data_cnames[i]
      while (!first_col %in% tool.survey$name) {
        i <- i + 1
        first_col <- data_cnames[i]
      }
      old_sheetname <- tool.survey %>% filter(name == first_col) %>% pull(datasheet)
      # change all occurences of `old_sheetname` to `sheet`
      tool.survey <- tool.survey %>% mutate(datasheet = ifelse(datasheet %==na% old_sheetname, sheet, datasheet))
    }
    rm(i, first_col, old_sheetname, data_cnames)
  }else if(params["fix_sheet_names_to_match"] %in% c("tool", "tool.survey", "toolsurvey")){
    # the names for data.list will be fixed to match tool.survey
    cat(paste0("Mismatch in the names of datasheets will be fixed to match the `datasheet` column from tool.survey.\n"))
    names(data.list) <- tool_datasheets[1:length(sheet_names)]
  } else {
    # names will not be fixed. Outputs a warning instead. The analysis will most likely break.
    warning(paste0("Mismatch in datasheet names found in tool.survey (", paste(tool_datasheets, collapse = ", "),
                   ") and in the data (", paste(sheet_names, collapse = ", "), ")\n\tThe analysis will most likely break!\n"))
  }
}

# check whether main actually has the most columns:
col_counts <- tibble(sheet = names(data.list)) %>% mutate(num_cols = 0)
for (s in sheet_names)
  col_counts[col_counts$sheet == s,]$num_cols <- ncol(data.list[[s]])
if(max(col_counts$num_cols) != col_counts[col_counts$sheet == "main",]$num_cols)
  warning("Unexpectedly, the 'main' sheet is not the one with the most columns! Check if the data looks good, please!")

rm(sheet_names, tool_datasheets, col_counts)

cat("\n> ...Done.\n")


##  LOAD and check DAF  --------------------------------------------------------

cat("\n> Creating a DAF...\n")

iycf_daf_variable <- tcltk::tk_select.list(tool.survey %>%
                                            filter(q.type %in% c("decimal","integer",
                                                                 "select_one","select_multiple")) %>%
                                            pull(name),
                                          title = "IYCF variables",
                                          multiple = T)

daf <- data.frame(variable = iycf_daf_variable) %>%
  dplyr::mutate(section = "IYCF_Part1",
                label = get.label(variable))


cat("\n> Checking your DAF...\n")

# ensure daf has the right columns:
necessary.dap.cols <- c(
  "section",  #       <- optional (take sections from begin_groups, or set everything to one section, maybe "Analysis")
  "variable"
  # "label", #          <- optional (read from tool.survey)
  # "func",  #          <- optional! (use var_type)
  # "admin" #          <- optional (strata if strata in names, otherwise overall)
  # "disaggregate.variable" #   <- obsolete (renamed to disaggregations)
  # "data", #         <- obsolete
  # "xlsx_name", #     <- obsolete
  # "calculation", #    <- optional
  # "join", #           <- obsolete
  # "comments" #        <- optional
)

if(!all(necessary.dap.cols %in% colnames(daf)))
  stop(paste("Your DAF is missing some necessary columns:",
             paste0(necessary.dap.cols[!necessary.dap.cols %in% colnames(daf)], collapse = ", ")))


## check daf using code from `convert.cols.check.dap`

for(daf_col in necessary.dap.cols){
  are_na <- is.na(daf[[daf_col]])
  if(any(are_na)) stop(paste("Missing parameter", daf_col, "in row(s):", paste(which(are_na), collapse = ", ")))
}

valid.func.params <- c(
  "mean", "median", "integer", "numeric", # ...?
  "select_one", "select_multiple",
  "count"
)

if("func" %in% names(daf)){
  # check if all func's are valid
  if(any(!daf$func %in% valid.func.params)) stop("There are unexpected func parameters in your DAF: ", paste(daf$func[!daf$func %in% valid.func.params], collapse = ", "))
  type_mismatches <- daf %>% filter(variable %in% tool.survey$name) %>%
    left_join(tool.survey %>% select(name, q.type), by = c("variable" = "name")) %>%
    filter(func != "count" & (q.type != func & !(q.type %in% c("decimal", "integer", "calculate") & func %in% c("mean", "median", "integer", "numeric"))))
  if(nrow(type_mismatches) > 0) stop("Invalid func parameter for variable(s): ", paste0(type_mismatches$variable, " (",type_mismatches$func, ")", collapse = ", "))
  rm(type_mismatches)
}

rm(valid.func.params, necessary.dap.cols, are_na, daf_col)


## upgrade DAF with additional columns
## -----------------------------------------------------------------------------

list_name <- tool.survey %>%
  select(name,list_name)

variables_not_in_tool <- daf %>% filter(!variable %in% tool.survey$name) %>% pull(variable) %>% unique
list_variables_not_in_tool <- list()
for (i in variables_not_in_tool){
  list_variables_not_in_tool[[i]] <- "select_one"
}
daf <- suppressWarnings(daf %>% mutate(
  var_type = ifelse(!variable %in% tool.survey$name,list_variables_not_in_tool[variable],get.type(variable))) %>%
    left_join(list_name, by = c("variable"="name")))

## adding missing legacy/optional columns and filling them with default values (NA or something else)
additional.daf.cols <- c(
  "disaggregations",
  "calculation", "comments",
  "section"
)
for(column in additional.daf.cols){
  if(!column %in% names(daf)){
    daf[[column]] <- NA
    cat(paste0("\n> Your DAF is missing the '",column,"' column - by default it was filled with NA"))
  }
}

## func - use the type of variable from tool.survey (mean is default for numeric questions)
if(!"func" %in% names(daf)) {
  cat("\n> Your DAF is missing the 'func' column - default functions will be taken by looking up q.type from tool.survey\n")
  daf <- daf %>% mutate(func = ifelse(var_type %in% c("integer", "numeric"), "numeric",
                                      ifelse(var_type == "text", "count", var_type)))
}else{
  daf <- daf %>% mutate(func = ifelse(func %in% c("integer", "numeric","mean","median"), "numeric", func))
}

## admin - strata if strata in names, otherwise overall
admin_default <- ifelse("strata" %in% names(data.list$main), "strata", "overall")
if(!"admin" %in% names(daf)){
  cat(paste0("\n> Your DAF is missing the 'admin' column - by default it was set to '", admin_default,"'\n"))
  daf$admin <- admin_default
}

## label - copy the question label from tool.survey
if(!"label" %in% names(daf)) {
  cat("\n> Your DAF is missing the 'label' column - by default labels will be taken from tool.survey\n")
  daf <- daf %>% mutate(label = get.label(variable))
}

## xlsx_name - calculate using variable+admin
daf_var_unames <- daf %>% mutate(uname = paste0(variable, "_", admin,
                                                ifelse(isna(disaggregations), "", "_D"))) %>%
  group_by(variable, uname) %>% pull(uname)
# if(any(daf_var_unames %>% duplicated) | length(daf_var_unames) != nrow(daf)) daf_var_unames <- paste0(daf_var_unames, "_" ,1:nrow(daf))

daf$xlsx_name <- daf_var_unames

##Detect empty variables
is_empty_new <- function(x) {
  obj <- get(x)
  length(obj) == 0 || is.null(obj) ||  (is.character(obj) && all(obj == ""))
}

cat("\n> ...Done.")

cat("\n> Finished init.\n")
