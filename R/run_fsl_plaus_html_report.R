#' run_fsl_plaus_html_report
#'
#' @param .dataset Main Dataset
#' @param uuid_var the name of the variable that indicates the uuid column
#' @param group_var the name of the variable that indicates the grouping column, usually enumerator ID column
#' @param output_file the name of the output file
#' @param output_dir the directory to export the output file
#'
#' @return an HTML rmarkdown file with FSL quality summaries
#' @export
#'
#' @examples

run_fsl_plaus_html_report <- function(
  .dataset = NULL,
  uuid_var = "_uuid",
  group_var = "enum_id",
  output_file = "fsl_quality_report.pdf",
  output_dir = "reports"
) {
  if (is.null(.dataset)) {
    stop("Error: 'dataset_main' must be provided.")
  }

  if (is.null(uuid_var)) {
    stop("Error: 'uuid' variable must be provided.")
  }

  if (is.null(group_var)) {
    stop("Error: 'group' var must be provided.")
  }

  rmarkdown::render(
    input = "fsl_quality_report_markdown_v2.Rmd", # Replace with your actual .Rmd file path
    output_file = output_file, # Name of the output file
    output_dir = output_dir, # Directory to save the report
    params = list(
      mainData = .dataset,
      uuidVar = uuid_var,
      GroupVar = group_var
    ),
    # envir = new.env()                  # Use a clean environment to avoid conflicts
  )
}
