###############################################################################
# UTILITY FUNCTIONS RELATED TO KOBO TOOLS
###############################################################################

# ------------------------------------------------------------------------------
# LOADING THE KOBO TOOL
# ------------------------------------------------------------------------------
load.label_colname <- function(filename_tool, language = "English"){
  tool_colnames <- readxl::read_xlsx(filename_tool, sheet = "survey", col_types = "text") %>% names
  return(tool_colnames[agrep(paste0("label::",language), tool_colnames)])
}

load.tool.survey <- function(filename_tool, keep_cols = F){
  #' Load the 'survey' tab from a Kobo tool.
  #' 
  #' The variable `label_colname` must be set before running this function!
  #' 
  #' @param filename_tool This is the path to the file that contains the tool (probably in your 'resources' folder)
  #' @param keep_cols Whether all columns from the original tool should be kept. Defaults to False, meaning that only the relevant labels, hints, etc are kept.
  #' @returns A dataframe: tool.survey, it's almost the same as the 'survey' tab from the tool, with new columns added: `datasheet`, `q.type`, `list_name`
  
  tool.survey <- readxl::read_xlsx(filename_tool, sheet = "survey", col_types = "text") %>% 
    filter(!is.na(type)) %>%
    mutate(q.type=as.character(lapply(type, function(x) stringr::str_split(x, " ")[[1]][1])),
           list_name=as.character(lapply(type, function(x) stringr::str_split(x, " ")[[1]][2])),
           list_name=ifelse(stringr::str_starts(type, "select_"), list_name, NA))
  
  if(!keep_cols){
    # select only the relevant (English) labels, hints etc.
    lang_code <- stringr::str_split(label_colname, "::", 2, T)[2]
    lang_code <- stringr::str_replace(stringr::str_replace(lang_code, "\\(", "\\\\("), "\\)", "\\\\)")
    cols <- colnames(tool.survey)
    cols_to_keep <- cols[stringr::str_detect(cols, paste0("((label)|(hint)|(constraint_message))::",lang_code)) | 
                           !stringr::str_detect(cols, "((label)|(hint)|(constraint_message))::")]
  
    tool.survey <- select(tool.survey, all_of(cols_to_keep))
  }
  # Find which data sheet question belongs to:
  tool.survey <- tool.survey %>% mutate(datasheet = NA)
  sheet_name <- "main"
  for(i in 1:nrow(tool.survey)){
    toolrow <- tool.survey %>% slice(i)
    if(stringr::str_detect(toolrow$type, "begin[ _]repeat")) sheet_name <- toolrow$name
    else if(stringr::str_detect(toolrow$type, "end[ _]repeat")) sheet_name <- "main"   # watch out for nested repeats (Why would you even want to do that?)
    else if(stringr::str_detect(toolrow$type, "((end)|(begin))[ _]group", T)) tool.survey[i, "datasheet"] <- sheet_name
  }
  
  tool.survey <- tool.survey %>% mutate(count_repeat = NA)
  repeat_c <- NA
  for(i in 1:nrow(tool.survey)){
    toolrow <- tool.survey %>% slice(i)
    if(stringr::str_detect(toolrow$type, "begin[ _]repeat")) repeat_c <- toolrow$repeat_count
    else if(stringr::str_detect(toolrow$type, "end[ _]repeat")) repeat_c <- NA   # watch out for nested repeats (Why would you even want to do that?)
    else if(stringr::str_detect(toolrow$type, "((end)|(begin))[ _]group", T)) tool.survey[i, "count_repeat"] <- repeat_c
  }
  return(tool.survey)
  
}

load.tool.choices <- function(filename_tool){
  #' Load the 'choices' tab from a Kobo tool.
  #' 
  #' The variable `label_colname` must be set before running this function!
  #' 
  #' @param filename_tool This is the path to the file that contains the tool (probably in your 'resources' folder)
  #' @returns A dataframe: tool.choices, it's the same as the 'choices' tab from the tool, filtered to include only distinct rows.
  
  readxl::read_xlsx(filename_tool, sheet = "choices", col_types = "text") %>% 
    filter(!is.na(list_name)) %>% 
    select(all_of(c("list_name", "name")), !!sym(label_colname)) %>% distinct()
}

# ------------------------------------------------------------------------------

get.type <- function(variable){
  #' Find the type of variables.
  #'
  #' Looks up the "name" and "q.type" columns in tool.survey. Operates on single values and vectors.
  #'
  #' @param variable This is the name of the header from raw data.

    res <- data.frame(name = variable) %>%
        left_join(select(tool.survey, name, q.type), by = "name", na_matches = "never") %>%
        mutate(q.type = ifelse(is.na(q.type) & stringr::str_detect(name, "/"), "select_multiple", q.type))
    if(any(is.na(res$q.type))){
        warning(paste("Variables not found in tool.survey:", paste0(filter(res, is.na(q.type)) %>% pull(name),collapse = ", ")))
    }
    return(pull(res, q.type))
}

get.label <- function(variable){
  #' Find the label of a variable
  #'
  #' Looks up the "name" and `label_colname` in tool.survey. Operates on single values and vectors.
  #' For column named after select_multiple choices (i.e containing a slash in the name),
  #' the returned label will be the one for the base question itself (e.g. if variable == "pytanie/choice", the result is the label of "pytanie")
  #'
  #' @param variable This is the name of the header from raw data.
  
  not_in_tool <- variable[!variable %in% tool.survey$name]
  if(length(variable) > 0){
    warning(paste("Variables not found in tool.survey:", paste0(not_in_tool, collapse = ", ")))
  }
  if (any(stringr::str_detect(variable, "/"))) variable <- stringr::str_split(variable, "/", 2, T)[,1]
  res <- data.frame(name = variable) %>% 
    left_join(select(tool.survey, name, !!sym(label_colname)), by = "name", na_matches = "never")
  
  return(pull(res, label_colname))
}

get.choice.label <- function(choice, list, simplify = FALSE){
  #' Find the label of choices in a list
  #'
  #' Looks up the "name" and `label_colname` in tool.choices. Operates on single values and vectors.
  #'
  #' @param choice the name of the choice
  #' @param list the name of the list containing choice
  #' @param simplify If TRUE, output labels will be modified and simplified...
  
  if(!list %in% tool.choices$list_name) stop(paste("list",list, "not found in tool.choices!"))
  
  res <- data.frame(name = unlist(choice)) %>%
    left_join(select(tool.choices, name, list_name, all_of(label_colname)) %>% filter(list_name == list),
              by = "name", na_matches = "never")
  if(any(is.na(res[[label_colname]]))){
    culprits <- paste0(filter(res, is.na(!!sym(label_colname))) %>%
                         pull(name), collapse = ", ")
    warning(paste0("Choices not in the list (", list, "):", culprits))
  }
  if(nrow(res) == 0) stop("All choices not in the list!")
  
  res_vec <- pull(res, label_colname)
  if(simplify){
    # if "e.g." or "for example" in label, shorten up to this point
    e.g._pattern <- " *\\(?((e\\.g\\.)|(for exa?m?a?ple))"
    res_vec <- stringr::str_split(res_vec, e.g._pattern, n=2,simplify = T)[,1] %>% stringr::str_squish()
  }
  return(res_vec)
}

# ------------------------------------------------------------------------------

get.choice.list.from.name <- function(variable){
  #' find the choices list name
  #' @param variable This is the name of the header from raw data.
  if (stringr::str_detect(variable, "/")) variable <- stringr::str_split(variable, "/")[[1]][1]
  return(tool.survey %>% filter(name == variable) %>% pull(list_name))
}

get.choice.list.from.type <- function(q_type){
  #' finds the choice list for a question basing on its type
  q_type.1 <- stringr::str_split(q_type, " ")[[1]]
  if (length(q_type.1)==1) return(NA)
  else return(q_type.1[2])
}

get.ref.question <- function(q_relevancy){
  #' Very smart function that finds ref.question basing on relevancy text
  q_relevancy.1 <- stringr::str_split(q_relevancy, "\\{")[[1]][2]
  return(stringr::str_split(q_relevancy.1, "\\}")[[1]][1])
}

# ------------------------------------------------------------------------------------------
split.q.type <- function(x) return(stringr::str_split(x, " ")[[1]][1])

# ------------------------------------------------------------------------------------------
get.other.variables <- function(other_cnames = c()){
  #' finds all 'other' question in tool.survey
  #'
  #' This function is superceded by get.other.db and should be considered deprecated.
  #'
  #' question needs to have 'other' in its relevancy, or be in `other_cnames`
  #' @returns Dataframe containing ref.question, label, name etc.

  ov <- tool.survey %>%
    filter(type=="text" &
             (stringr::str_detect(tolower(relevant), "other'") |
                name %in% other_cnames)) %>%
    select("type", "name", label_colname, "relevant") %>%
    mutate(ref.question=as.character(lapply(relevant, get.ref.question)))
  return(ov)
}

# ------------------------------------------------------------------------------------------
get.var.labels <- function() {
  var.labels <- tool.survey %>%
    select(type, name, `label_colname`) %>%
    rename(label=`label_colname`) %>%
    left_join(get.other.variables() %>% select(name, ref.question), by="name")

  var.labels <- var.labels %>%
    left_join(var.labels %>% select(name, label), by=c("ref.question"="name")) %>%
    mutate(label.x=ifelse(is.na(label.x), name, label.x),
           label.full=ifelse(is.na(label.y), label.x, paste0(label.y, " / ", label.x))) %>%
    select(-c(label.x, label.y))
  return(var.labels)
}

# ------------------------------------------------------------------------------------------
get.select.db <- function(){
  #' finds all 'select' type questions, and their choices

  # list of choices for each list_name (from TOOL_CHOICES)
  list.choices <- tool.choices %>% filter(!is.na(list_name)) %>% group_by(list_name) %>%
    mutate(choices=paste(name, collapse=";\n"),
           choices.label=paste(!!sym(label_colname), collapse=";\n")) %>%
    summarise(choices=choices[1], choices.label=choices.label[1])
  # list of choices for each question
  select.questions <- tool.survey %>%
    rename(q.label=label_colname) %>%
    select(type, name, q.label) %>%
    mutate(q.type=as.character(lapply(type, split.q.type)),
           list_name=as.character(lapply(type, get.choice.list.from.type))) %>%
    filter(list_name!="NA" & list_name!="group" & list_name!="repeat") %>%
    left_join(list.choices, by="list_name") %>%
    filter(!is.na(choices))
  return(select.questions)
}

# ------------------------------------------------------------------------------------------
get.other.db <- function(){
  #' finds all 'other' questions and their ref question and choices
  #' #' @returns Dataframe containing name, ref.name, full.label, choices etc.
  select.questions <- get.select.db()

  # for each "other" question, get ref.question and list of choices
  df1 <- tool.survey %>% filter(stringr::str_ends(name, "_other"), type=="text") %>%
    rename(label=label_colname) %>%
    select("name", "label", "relevant") %>%
    mutate(ref.name=as.character(lapply(relevant, get.ref.question))) %>%
    left_join(select(select.questions, "name", "q.type", "q.label", "list_name", "choices", "choices.label"),
              by=c("ref.name"="name")) %>%
    rename(ref.label=q.label, ref.type=q.type) %>%
    mutate(full.label=paste0(ref.label, " - ", label)) %>%
    select(name, ref.name, full.label, ref.type, choices, choices.label)

  return(df1)
}

# ------------------------------------------------------------------------------------------
get.trans.db <- function(include.all=c()){
  #' finds all questions which should be translated (meaning all 'text'-type question that are not 'other's)
  #' somewhat obsolete because it searches for ref questions too which are unnecesary
  select.questions <- get.select.db()

  df1 <- tool.survey %>% filter(type == "text" & (!(stringr::str_ends(name, "_other")) | name %in% include.all)) %>%
    rename(label=label_colname) %>%
    select("name", "label")
  return(df1)
}
