#' Plot Age Pyramid
#'
#' @param .dataset HH_roster Data with Age and Sex of individuals
#' @param sex_column the variable name in the Data indicating the sex of the individual
#' By default: "sex"
#' @param age_years the variable name in the Data indicating the age of the individual
#' By default: "age_years"
#' @param age_grouping If True, user using age_grouping in data with variable name
#' "age_group". If default False, age_years will be grouped as followed.
#' 0-4/5-9/10-14/15-19/20-24/25-29/30-34/35-39/40-44/
#' 45-49/50-54/55-59/60-64/65-69/70-74/75-79/80-84/85+
#' @param file_path File path for saving the plot in a directory
#' @param wdth width of the plot. By default: 5
#' @param hght height of the plot. By default: 5
#' @param title_name Title of the plot
#'
#' @return An Age Pyramid plot
#' @export
#'
#' @examples
#' \dontrun{
#'   plot_age_pyramid(hh_roster)
#' }
plot_age_pyramid <- function (.dataset,
                              sex_column = "sex",
                              age_years = "age_years",
                              age_grouping = FALSE,
                              file_path = NULL,
                              wdth = 5,
                              hght = 5,
                              title_name = NULL)
{
  print("Please note, the sex variable must be coded numerically by 1s (male) and 2s (female).")
  if (age_grouping == FALSE) {
    .dataset <- .dataset %>%
      dplyr::mutate(age_group = cut(as.numeric(!!rlang::sym(age_years)),
                                    breaks = c(-1,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84, Inf),
                                    labels = c("0-4", "5-9", "10-14", "15-19",
                                               "20-24", "25-29", "30-34", "35-39","40-44", "45-49", "50-54", "55-59",
                                               "60-64", "65-69", "70-74", "75-79", "80-84", "85+")))

  }
  .dataset <- .dataset %>%
    dplyr::arrange(!!rlang::sym(sex_column)) %>%
    dplyr::rename(sex = sex_column)

  g <- apyramid::age_pyramid(data = .dataset, age_group = "age_group",
                               proportional = TRUE) +
    ggplot2::ylab(paste0("Proportion of Population ")) +
    ggplot2::scale_fill_manual(name = "Sex", labels = c("Male",
                                                        "Female"),
                               values = c("#08bcc4", "#ff746c"))
  if (!is.null(title_name)) {
    g <- g +
      ggplot2::ggtitle(title_name)
  }
  if (!is.null(file_path)) {
    ggplot2::ggsave(filename = file_path,
                    width = wdth,
                    height = hght)
  }
  return(g)
}
