#' Plot Age Distribution
#'
#' @param .dataset HH_roster Data with Age and Sex of individuals
#' @param by_group Disaggregated variable
#' @param year_or_month Month or Year Plot. "year" for Year, "month" for Month.
#' By deault it will calculate year.
#' @param age_years the variable name in the Data indicating the year age of the individual
#' By default: "age_years"
#' @param age_months the variable name in the Data indicating the month age of the individual
#' By default: "age_months"
#' @param min_age Minimum Age cutoff. By default 0 for year and month.
#' @param max_age Maximum Age cutoff. By default 5 for year and 59 for month.
#' @param breaks Breaks of the Bins. By default 1 for year and 12 for month.
#' @param file_path File path for saving the plot in a directory
#' @param wdth width of the plot. By default: 5
#' @param hght height of the plot. By default: 5
#' @param title_name Title of the plot
#'
#' @return an age per year or month distribution plot
#' @export
#'
#' @examples
#' \dontrun{
#'   plot_age_distribution(hh_roster)
#' }


plot_age_distribution <- function (.dataset,
                                   by_group = NULL,
                                   year_or_month = NULL,
                                   age_years = "age_years",
                                   age_months = "age_months",
                                   min_age = NULL,
                                   max_age = NULL,
                                   breaks = NULL,
                                   file_path = NULL,
                                   wdth = 5,
                                   hght = 5,
                                   title_name = NULL) {
  if(is.null(year_or_month) | year_or_month == "year"){
    if (is.null(min_age)) {
      min_age <- 0
      print("No minimum age specified. Defaulting to 0 years.")
    }
    if (is.null(max_age)) {
      max_age <- 5
      print("No maximum age specified. Defaulting to 5 years.")
    }
    if (is.null(breaks)) {
      breaks <- 1
    }
    .dataset <- .dataset %>% dplyr::filter(!!rlang::sym(age_years) >= min_age &
                                             !!rlang::sym(age_years) <= max_age)
    if (is.null(by_group)) {
      g <- ggplot2::ggplot(data = .dataset, ggplot2::aes(x = age_years)) +
        ggplot2::geom_histogram(binwidth = breaks) + ggplot2::scale_x_continuous(minor_breaks = seq(min_age,
                                                                                                    max_age, by = 1), breaks = seq(min_age, max_age,
                                                                                                                                   by = breaks), limits = c(min_age, max_age))
    }
    else {
      g <- ggplot2::ggplot(data = .dataset, ggplot2::aes(x = age_years)) +
        ggplot2::geom_histogram(binwidth = breaks) + ggplot2::scale_x_continuous(minor_breaks = seq(min_age,
                                                                                                    max_age, by = 1), breaks = seq(min_age, max_age,
                                                                                                                                   by = breaks), limits = c(min_age, max_age)) + ggplot2::facet_wrap(~get(by_group),
                                                                                                                                                                                                     ncol = 1)
    }
  }

  if(year_or_month == "month"){
    if (is.null(min_age)) {
      min_age <- 0
      print("No minimum age specified. Defaulting to 0 years.")
    }
    if (is.null(max_age)) {
      max_age <- 59
      print("No maximum age specified. Defaulting to 5 years.")
    }
    if (is.null(breaks)) {
      breaks <- 12
    }

    .dataset <- .dataset %>%
      dplyr::mutate(!!rlang::sym(age_months) := as.numeric(!!rlang::sym(age_months))) %>%
      dplyr::filter(!!rlang::sym(age_months) >= min_age &
                      !!rlang::sym(age_months) <= max_age)
    if (is.null(by_group)) {
      g <- ggplot2::ggplot(data = .dataset, ggplot2::aes(x = !!rlang::sym(age_month))) +
        ggplot2::geom_histogram(binwidth = breaks) + ggplot2::scale_x_continuous(minor_breaks = seq(min_age,
                                                                                                    max_age, by = 1), breaks = seq(min_age, max_age,
                                                                                                                                   by = breaks), limits = c(min_age, max_age))
    }
    else {
      g <- ggplot2::ggplot(data = .dataset, ggplot2::aes(x = !!rlang::sym(age_month))) +
        ggplot2::geom_histogram(binwidth = breaks) + ggplot2::scale_x_continuous(minor_breaks = seq(min_age,
                                                                                                    max_age, by = 1), breaks = seq(min_age, max_age,
                                                                                                                                   by = breaks), limits = c(min_age, max_age)) + ggplot2::facet_wrap(~get(by_group),
                                                                                                                                                                                                     ncol = 1)
    }
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
