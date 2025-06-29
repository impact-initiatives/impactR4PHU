#' Create a project folder for ph integrated tables
#'
#' @param folder_path Path for the new folder
#' @param ... Extra information collected from the RStudio wizard
#'
#' @return Folder with a copy of the ph_integrated_tables
#'
#' @examples
#' \dontrun{
#' create_ph_integrated_tables_template("path/to/folder")
#' }
create_ph_integrated_tables_template <- function(folder_path, ...) {
  from <- system.file("ph_integrated_tables", package = "impactR4PHU")
  fs::dir_copy(from, folder_path, overwrite = FALSE)
}
