#' Create a project folder for mort cleaning
#'
#' @param folder_path Path for the new folder
#' @param ... Extra information collected from the RStudio wizard
#'
#' @return Folder with a copy of the mort_cleaning
#'
#' @examples
#' \dontrun{
#' create_mort_cleaning_template("path/to/folder")
#' }
create_mort_cleaning_template <- function(folder_path, ...) {

  from <- system.file("mort_cleaning", package = "impactR4PHU")
  fs::dir_copy(from, folder_path, overwrite = FALSE)

}
