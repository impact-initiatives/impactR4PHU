#' Create a project folder for fsl quality report
#'
#' @param folder_path Path for the new folder
#' @param ... Extra information collected from the RStudio wizard
#'
#' @return Folder with a copy of the fsl_quality_report
#'
#' @export
#'
#' @examples
#' \dontrun{
#' create_fsl_quality_report_template("path/to/folder")
#' }
create_fsl_quality_report_template <- function(folder_path, ...) {
  from <- system.file("fsl_quality_report", package = "impactR4PHU")
  fs::dir_copy(from, folder_path, overwrite = FALSE)
  orig <- file.path(folder_path, "fsl_quality_report", "Rprofile.R")
  dest <- file.path(folder_path, "fsl_quality_report", ".Rprofile")
  if (fs::file_exists(orig)) {
    fs::file_move(orig, dest)
  } else {
    stop("Expected activation file Rprofile.R not found in template")
  }
  invisible(folder_path)
}
