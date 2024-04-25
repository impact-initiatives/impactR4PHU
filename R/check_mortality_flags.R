#' check_mortality_flags
#'
#' @param df output dataframe long mortality from create_mortality_long_df
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

check_mortality_flags <- function(df,
                                  cause_death_f = c("post_partum","during_pregnancy","during_delivery")){
  options(warn = -1)
  if (!"uuid" %in% names(df)) stop("uuid not available in the df Mortality")
  if (!"enumerator" %in% names(df)) stop("enumerator not available in the df Mortality")

  hh_summary <- df %>%
    dplyr::mutate(flag_cause_death = ifelse(sex == 1 & death_cause %in% cause_death_f, 1, 0),
                  flag_negative_pt = ifelse(as.numeric(person_time) < 0 , 1, 0)) %>%
    dplyr::group_by(uuid, enumerator) %>%
    dplyr::summarize(flag_multiple_death = ifelse(sum(!is.na(death))>1, 1, 0),
                     flag_cause_death = sum(flag_cause_death,na.rm = T),
                     flag_negative_pt =  sum(flag_negative_pt,na.rm = T))

  options(warn = 0)
  return(hh_summary)
}
