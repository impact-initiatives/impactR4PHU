#' check_mortality_flags
#'
#' @param df_mortality output dataframe long mortality from create_mortality_long_df
#' @param cause_death_f vector list of the cause of death options related to female
#'
#'
#' @return df_mortality with two extra flag columns:
#' -flag_multiple_death
#' -flag_cause_death
#' @export
#'
#' @examples
#' \dontrun{
#'   check_mortality_flags(df_mortality,
#'   cause_death_f = c("post_partum","during_pregnancy","during_delivery"))
#' }

check_mortality_flags <- function(df_mortality,
                                  cause_death = "cause_death",
                                  cause_death_f = c("post_partum","during_pregnancy","during_delivery")){
  options(warn = -1)
  if (!"uuid" %in% names(df_mortality)) stop("uuid argument incorrect, or not available in the df Mortality")

  hh_summary <- df_mortality %>%
    dplyr::group_by(uuid) %>%
    dplyr::summarize(flag_multiple_death = ifelse(sum(!is.na(death))>1, 1, 0))

  df_mortality <- merge(df_mortality, hh_summary, by = "uuid", all.x = TRUE)

  df_mortality <- df_mortality %>%
    dplyr::mutate(flag_cause_death = ifelse(sex == 1 & death_cause %in% cause_death_f,1,0))

  options(warn = 0)
  return(df_mortality)
}
