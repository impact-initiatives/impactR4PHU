#' get the label::x colname from the tool.survey
#'
#' @param tool.survey This is the tool.survey dataset. By default NULL
#' @param language This is the language desired to be selected. By default "English"
#'
#' @return return the name of the label column (ex: label::English)
#'
#' @examples
#' \dontrun{
#' label_colname <- load.label_colname(tool.survey, language = "English")
#' }

load.label_colname <- function(tool.survey = NULL, language = "English") {
  if (is.null(tool.survey)) {
    stop("Tool survey not available")
  }

  if (!is.data.frame(tool.survey)) {
    stop("tool.survey should be a dataset")
  }

  if (nrow(tool.survey) == 0) {
    stop("tool.survey is empty")
  }
  tool_colnames <- tool.survey %>% names
  return(tool_colnames[agrep(paste0("label::", language), tool_colnames)])
}
