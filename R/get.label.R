#' Find the label of a variable
#'
#' @param variable This is the name of the header from raw data. By default NULL
#' @param tool.survey This is the tool.survey dataset. By default NULL
#'
#' @return return label will be the one for the base question itself
#'    (e.g. if variable == "water_source", the result is the label of "What are
#'    the water sources in your household")
#'
#' @examples
#' \dontrun{
#' label <- get.label("water_source", tool.survey = tool.survey)
#' }


get.label <- function(variable = NULL, tool.survey = NULL){

  if(is.null(variable)) stop("Variable is missing from function")

  if(is.null(tool.survey)) stop("Tool survey not available")

  if (!is.data.frame(tool.survey)) {
    stop("tool.survey should be a dataset")
  }

  if (nrow(tool.survey) == 0) {
    stop("tool.survey is empty")
  }

  label_colname <- load.label_colname(tool.survey)
  not_in_tool <- variable[!variable %in% tool.survey$name]
  if(length(not_in_tool) > 0){
    warning(paste("Variables not found in tool.survey:", paste0(not_in_tool, collapse = ", ")))
  }
  if (any(stringr::str_detect(variable, "/"))) variable <- stringr::str_split(variable, "/", 2, T)[,1]
  res <- data.frame(name = variable) %>%
    dplyr::left_join(dplyr::select(tool.survey, name, !!rlang::sym(label_colname)), by = "name", na_matches = "never")

  return(dplyr::pull(res, label_colname))
}


