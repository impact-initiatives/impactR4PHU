
#' Plot Ridge Distribution
#'
#' @param .dataset output of the create_fsl_flags functions
#' @param numeric_cols a vector of the same fsl indicator score columns
#' By default: NULL.
#' @param name_groups Name of the groups. By default: "Groups"
#' @param name_units Name of the units. By default: "Units"
#' @param grouping Variable name from the output create_fsl_flags for grouping
#' @param file_path File path for saving the plot in a directory
#' @param wdth width of the plot. By default: 5
#' @param hght height of the plot. By default: 5
#' @param title_name Title of the plot
#'
#' @return A Ridge Plot with the distribution of values
#' @export
#'
#' @examples
#' \dontrun{
#'   plot_ridge_distribution(df)
#' }

plot_ridge_distribution <- function (.dataset, numeric_cols = NULL,
                                     name_groups = NULL, name_units = NULL, grouping = NULL,
                                     file_path = NULL, wdth = 5, hght = 5, title_name = NULL)
{
  a <- 0

  if (is.null(grouping)) {
    .dataset <- .dataset %>% dplyr::mutate(group = "All")
    grouping <- "group"
    a <- 1
  }

  .dataset <- .dataset %>% dplyr::select(grouping, numeric_cols) %>% tidyr::gather(key = !!name_groups,
                                                                       value = !!name_units, numeric_cols)
  g <- ggplot2::ggplot(.dataset, ggplot2::aes(x = get(name_units),
                                        y = get(name_groups), fill = get(name_groups))) + ggridges::geom_density_ridges() +
    ggridges::theme_ridges() + ggplot2::xlab(name_units) +
    ggplot2::ylab(name_groups) + ggplot2::theme(legend.position = "none",
                                                legend.title = ggplot2::element_text(name_groups))
  if (a == 0) {
    g <- g + ggplot2::facet_wrap(~get(grouping))
  }
  if (!is.null(title_name)) {
    g <- g + ggplot2::ggtitle(title_name)
  }
  if (!is.null(file_path)) {
    ggplot2::ggsave(filename = file_path, width = wdth,
                    height = hght)
  }
  return(g)
}
