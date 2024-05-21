#' Create a project folder for iycf analysis
#'
#' @param folder_path Path for the new folder
#' @param ... Extra information collected from the RStudio wizard
#'
#' @return Folder with a copy of the iycf_analysis
#'
#' @examples
#' \dontrun{
#' create_iycf_descriptive_analysis_template("path/to/folder")
#' }
create_iycf_descriptive_analysis_template <- function(folder_path, ...) {

  from <- system.file("iycf_descriptive_analysis", package = "impactR4PHU")
  fs::dir_copy(from, folder_path, overwrite = FALSE)

}
