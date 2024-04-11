#' Create a project folder for fsl quality report
#'
#' @param folder_path Path for the new folder
#' @param ... Extra information collected from the RStudio wizard
#'
#' @return Folder with a copy of the fsl_quality_report
#'
#' @examples
#' \dontrun{
#' create_analysis_review_template("path/to/folder")
#' }
create_fsl_quality_report_template <- function(folder_path, ...) {

  from <- system.file("fsl_quality_report", package = "impactR4PHU")
  fs::dir_copy(from, folder_path, overwrite = FALSE)

}
