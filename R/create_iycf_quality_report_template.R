#' Create a project folder for iycf quality report
#'
#' @param folder_path Path for the new folder
#' @param ... Extra information collected from the RStudio wizard
#'
#' @return Folder with a copy of the iycf_quality_report
#'
#' @examples
#' \dontrun{
#' create_iycf_quality_report_template("path/to/folder")
#' }
create_iycf_quality_report_template <- function(folder_path, ...) {

  from <- system.file("iycf_quality_report", package = "impactR4PHU")
  fs::dir_copy(from, folder_path, overwrite = FALSE)

}
