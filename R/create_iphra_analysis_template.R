#' Create a project folder for iphra analysis
#'
#' @param folder_path Path for the new folder
#' @param ... Extra information collected from the RStudio wizard
#'
#' @return Folder with a copy of the iphra_analysis
#'
#' @examples
#' \dontrun{
#' create_iphra_analysis_template("path/to/folder")
#' }
create_iphra_analysis_template <- function(folder_path, ...) {
  from <- system.file("iphra_analysis", package = "impactR4PHU")
  fs::dir_copy(from, folder_path, overwrite = FALSE)
}
