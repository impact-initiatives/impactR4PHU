#' Plot Correlogram
#'
#' @param .dataset output of the create_fsl_flags functions
#' @param numeric_cols a vector of the fsl indicator scores.
#' By default: c("fsl_fcs_score",  "fsl_rcsi_score",  "fsl_hhs_score")
#' @param file_path File path for saving the plot in a directory
#' @param wdth width of the plot. By default: 5
#' @param hght height of the plot. By default: 5
#' @param title_name Title of the plot
#'
#' @return a Correlogram plot
#' @export
#'
#' @examples
#' \dontrun{
#'   plot_correlogram(.dataset)
#' }

plot_correlogram <- function(
  .dataset,
  numeric_cols = c("fsl_fcs_score", "fsl_rcsi_score", "fsl_hhs_score"),
  file_path = NULL,
  wdth = 5,
  hght = 5,
  title_name = NULL
) {
  g <- GGally::ggpairs(data = .dataset, columns = numeric_cols)
  if (!is.null(title_name)) {
    g <- g + ggplot2::ggtitle(title_name)
  }
  if (!is.null(file_path)) {
    ggplot2::ggsave(filename = file_path, width = wdth, height = hght)
  }
  return(g)
}
