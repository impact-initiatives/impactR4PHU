source("./../src/utils/misc_utils.R")
source("./../src/utils/check_kobo.R")
source("./../src/utils/kobo_utils.R")
source("./../src/utils/regional_detect_data_falsification.R")
source("./../src/utils/utils_audit.R")
source("./../src/utils/utils_cleaning.R")
source("./../src/utils/utils_cleaning_loops.R")
source("./../src/utils/utils_translate.R")
source("./../src/utils/tabular_analysis_utils.R")
source("./../src/functions_phu.R")

library(dplyr)


options(scipen = 999)
options(dplyr.summarise.inform = FALSE)

dir.audits <- "./../data/inputs/audits/reach/"
dir.requests <- "./../output/checking/requests/"
dir.responses <- "./../output/checking/responses/"


# ------------------------------------------------------------------------------
enum_colname <- "enumerator"

# ------------------------------------------------------------------------------
# small utility function 
make.short.name <- function(name, no_date = F) return(gsub("__","_", paste0(dataset.name.short,"_", name, ifelse(no_date, "", paste0("_", strings['out_date'])))))
make.filename.xlsx <- function(dir = ".", name, no_date = F) return(gsub("//","/", paste0(dir, "/", make.short.name(name, no_date), ".xlsx")))


##  LOAD TOOL  -----------------------------------------------------------------

cat("\n> Loading Kobo tool from", strings['filename.tool'], "...\n")

label_colname <- load.label_colname(strings['filename.tool'])

tool.survey <- load.tool.survey(strings['filename.tool'])
tool.choices <- load.tool.choices(strings['filename.tool'])

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

##  LOAD DATA -------------------------------------------------------------------

cat("\n> Loading data for analysis from", strings['filename.data'], "...\n")
sheet_names <- readxl::excel_sheets(strings['filename.data'])
sheet_names[1] <- paste(sheet_names[1], "(main)")
cat("> Found the following datasheets:", paste(sheet_names, collapse = ", "), "\n")

# the first sheet is always named "main"!!!
sheet_names[1] <- "main"
data.list <- list("main" = readxl::read_excel(strings['filename.data'], sheet=1, col_types = "text"))
uuid_name <- names(data.list[["main"]])[grepl("uuid",names(data.list[["main"]]))]
index_name <- names(data.list[["main"]])[grepl("_index",names(data.list[["main"]]))]

data.list[["main"]] <- data.list[["main"]] %>% 
  rename(uuid = uuid_name,
         index = index_name) 

for(sheet in sheet_names[-1]){
  data.list[[sheet]] <- readxl::read_excel(strings['filename.data'], sheet=sheet, col_types = "text")
  uuid_sheet_name <- names(data.list[[sheet]])[grepl("uuid",names(data.list[[sheet]]))]
  data.list[[sheet]] <- data.list[[sheet]] %>% 
    rename(uuid = uuid_sheet_name) %>% 
    mutate(loop_index = paste0("loop_",sheet,"_",`_index`))
}
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

cat("\n> Finished init.\n")
