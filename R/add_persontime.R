#' Add Person Time
#'
#' @param df_mortality long dataframe containing (roster/left/died dfs)
#' @param smart boolean to check if calculation should follow smart methodology or IMPACT
#'
#' @return Long dataframe with person time added
#' @export
#'
#' @examples
#' \dontrun{
#'   add_persontime(df_mortality, smart = F)
#' }
add_persontime <- function(df_mortality, smart = FALSE) {

  if (smart == TRUE) {
    df_mortality <- df_mortality %>% dplyr::mutate(age_years = as.numeric(age_years),
                                                   join = ifelse(date_recall_date - date_join_date > 0, NA, join),
                                                   left = ifelse(date_left_date -date_dc_date >= 0, NA, left),
                                                   birth = ifelse(date_recall_date -date_birth_date > 0, NA, birth),
                                                   death = ifelse(date_death_date -date_dc_date >= 0, NA, death),
                                                   person_time = case_when(is.na(date_join_date) &
                                                                             is.na(date_left_date) &
                                                                             (is.na(date_birth_date) |
                                                                                !is.na(date_birth_date) &
                                                                                lubridate::year(date_birth_date) < as.numeric(date_dc_year)) &
                                                                             is.na(date_death_date) ~ as.numeric(lubridate::days(date_dc_date - date_recall_date))/86400,
                                                                           (!is.na(date_join_date) & date_join_date < date_recall_date) &
                                                                             is.na(date_left_date) & is.na(date_death_date) ~ as.numeric(lubridate::days(date_dc_date - date_recall_date))/86400,
                                                                           (!is.na(date_join_date) &date_join_date >= date_recall_date) & is.na(date_left_date) & is.na(date_death_date) ~ as.numeric(lubridate::days(date_dc_date - date_join_date))/86400,
                                                                           (!is.na(date_join_date) & date_join_date < date_recall_date) & (!is.na(date_left_date) & date_left_date >= date_dc_date) & is.na(date_death_date) ~ as.numeric(lubridate::days(date_dc_date - date_recall_date))/86400,
                                                                           (!is.na(date_join_date) & date_join_date >= date_recall_date) & (!is.na(date_left_date) & date_left_date >= date_dc_date) & is.na(date_death_date) ~ as.numeric(lubridate::days(date_dc_date - date_join_date))/86400,
                                                                           (!is.na(date_join_date) & date_join_date < date_recall_date) & (!is.na(date_left_date) & date_left_date < date_dc_date) & is.na(date_death_date) ~ as.numeric(lubridate::days(date_left_date - date_recall_date))/86400,
                                                                           (!is.na(date_join_date) & date_join_date >= date_recall_date) & (!is.na(date_left_date) & date_left_date < date_dc_date) & is.na(date_death_date) ~ as.numeric(lubridate::days(date_left_date - date_join_date))/86400,
                                                                           (!is.na(date_join_date) & date_join_date >= date_recall_date) & is.na(date_left_date) & (!is.na(date_death_date) & date_death_date > date_dc_date) ~ as.numeric(lubridate::days(date_dc_date - date_join_date))/86400,
                                                                           (!is.na(date_join_date) & date_join_date < date_recall_date) & is.na(date_left_date) & (!is.na(date_death_date) & date_death_date > date_dc_date) ~ as.numeric(lubridate::days(date_dc_date - date_recall_date))/86400,
                                                                           (!is.na(date_join_date) & date_join_date < date_recall_date) & is.na(date_left_date) & (!is.na(date_death_date) & date_death_date <= date_dc_date) ~ as.numeric(lubridate::days(date_death_date - date_recall_date))/86400,
                                                                           (!is.na(date_join_date) & date_join_date >= date_recall_date) & is.na(date_left_date) & (!is.na(date_death_date) & date_death_date <= date_dc_date) ~ as.numeric(lubridate::days(date_death_date - date_join_date))/86400,
                                                                           is.na(date_join_date) & (!is.na(date_left_date) & date_left_date > date_dc_date) & (is.na(date_birth_date) | lubridate::year(date_birth_date) < as.numeric(date_dc_year)) ~ as.numeric(lubridate::days(date_dc_date - date_recall_date))/86400,
                                                                           is.na(date_join_date) & (!is.na(date_left_date) & date_left_date <= date_dc_date) & (is.na(date_birth_date) | lubridate::year(date_birth_date) < as.numeric(date_dc_year)) ~ as.numeric(lubridate::days(date_left_date - date_recall_date))/86400,
                                                                           is.na(date_join_date) & is.na(date_death_date) & (!is.na(date_left_date) & date_left_date <= date_dc_date) & (!is.na(date_birth_date) & lubridate::year(date_birth_date) == as.numeric(date_dc_year) & date_birth_date < date_recall_date) ~ as.numeric(lubridate::days(date_left_date - date_recall_date))/86400,
                                                                           is.na(date_join_date) & is.na(date_death_date) & (!is.na(date_left_date) & date_left_date <= date_dc_date) & (!is.na(date_birth_date) & lubridate::year(date_birth_date) == as.numeric(date_dc_year) & date_birth_date >= date_recall_date) ~ as.numeric(lubridate::days(date_left_date - date_birth_date))/86400,
                                                                           is.na(date_join_date) & is.na(date_death_date) & (!is.na(date_left_date) & date_left_date > date_dc_date) & (!is.na(date_birth_date) & lubridate::year(date_birth_date) == as.numeric(date_dc_year) & date_birth_date >= date_recall_date) ~ as.numeric(lubridate::days(date_dc_date - date_birth_date))/86400,
                                                                           is.na(date_join_date) & is.na(date_death_date) & (!is.na(date_left_date) & date_left_date > date_dc_date) & (!is.na(date_birth_date) & lubridate::year(date_birth_date) == as.numeric(date_dc_year) & date_birth_date < date_recall_date) ~ as.numeric(lubridate::days(date_dc_date - date_recall_date))/86400,
                                                                           (!is.na(date_birth_date) & lubridate::year(date_birth_date) == as.numeric(date_dc_year) & date_birth_date < date_recall_date) & is.na(date_death_date) ~ as.numeric(lubridate::days(date_dc_date - date_recall_date))/86400,
                                                                           (!is.na(date_birth_date) & lubridate::year(date_birth_date) == as.numeric(date_dc_year) & date_birth_date >= date_recall_date) & is.na(date_death_date) ~ as.numeric(lubridate::days(date_dc_date - date_birth_date))/86400,
                                                                           (!is.na(date_birth_date) & lubridate::year(date_birth_date) == as.numeric(date_dc_year) & date_birth_date < date_recall_date) & (!is.na(date_death_date) & date_death_date > date_dc_date) ~ as.numeric(lubridate::days(date_dc_date - date_recall_date))/86400,
                                                                           (!is.na(date_birth_date) & lubridate::year(date_birth_date) == as.numeric(date_dc_year) & date_birth_date >= date_recall_date) & (!is.na(date_death_date) & date_death_date > date_dc_date) ~ as.numeric(lubridate::days(date_dc_date - date_birth_date))/86400,
                                                                           (!is.na(date_birth_date) & lubridate::year(date_birth_date) == as.numeric(date_dc_year) & date_birth_date >= date_recall_date) & (!is.na(date_death_date) & date_death_date <= date_dc_date) ~ as.numeric(lubridate::days(date_death_date - date_birth_date))/86400,
                                                                           (!is.na(date_birth_date) & lubridate::year(date_birth_date) == as.numeric(date_dc_year) & date_birth_date < date_recall_date) & (!is.na(date_death_date) & date_death_date <= date_dc_date) ~ as.numeric(lubridate::days(date_death_date - date_recall_date))/86400,
                                                                           is.na(date_join_date) & is.na(date_left_date) & (!is.na(date_death_date) & date_death_date > date_dc_date) ~ as.numeric(lubridate::days(date_dc_date - date_recall_date))/86400,
                                                                           (!is.na(date_death_date) & date_death_date <= date_dc_date) ~ as.numeric(lubridate::days(date_death_date - date_recall_date))/86400, TRUE ~ NA))

    df_mortality <- df_mortality %>% dplyr::mutate(under_5 = ifelse(is.na(age_years),
                                                                    NA, ifelse(as.numeric(age_years) < 5, 1, NA)), under_5_pt = ifelse(is.na(under_5),
                                                                                                                                       NA, ifelse(under_5 == 1, person_time, NA)))
  }
  if(smart == FALSE) {
    df_mortality <- df_mortality %>% dplyr::mutate(age_years = as.numeric(age_years),
                                                   person_time = date_dc_date - date_recall_date,
                                                   person_time = as.numeric(person_time),
                                                   person_time_in = ifelse(!is.na(join) | !is.na(birth) , person_time *
                                                                             0.5, 0),
                                                   person_time_out = ifelse(!is.na(death) | !is.na(left), person_time *
                                                                              0.5, 0),
                                                   under_5 = ifelse(is.na(age_years),NA, ifelse(as.numeric(age_years) < 5, 1, NA)),
                                                   under_5_pt = ifelse(is.na(under_5), NA, ifelse(under_5 ==
                                                                                                    1, person_time, NA)))
  }
  df_mortality <- df_mortality %>% dplyr::mutate(join_under5 = ifelse(is.na(under_5),
                                                                      NA, join), left_under5 = ifelse(is.na(under_5), NA,
                                                                                                      left), birth_under5 = ifelse(is.na(under_5), NA, birth),
                                                 death_under5 = ifelse(is.na(under_5), NA, death), age_0to2 = ifelse(is.na(age_years),
                                                                                                                     NA, ifelse(age_years >= 0 & age_years < 2, 1, NA)),
                                                 age_2to5 = ifelse(is.na(age_years), NA, ifelse(age_years >=
                                                                                                  2 & age_years < 5, 1, NA)), age_5to10 = ifelse(is.na(age_years),
                                                                                                                                                 NA, ifelse(age_years >= 5 & age_years < 10, 1, NA)),
                                                 age_0to5 = ifelse(is.na(age_years), NA, ifelse(age_years >=
                                                                                                  0 & age_years < 5, 1, NA)), age_5plus = ifelse(is.na(age_years),
                                                                                                                                                 NA, ifelse(age_years >= 5 & age_years < 200, 1,
                                                                                                                                                            NA)), )
  df_mortality$age_group <- cut(as.numeric(df_mortality$age_years),
                                breaks = c(-1, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49,
                                           54, 59, 64, 69, 74, 79, 84, Inf), labels = c("0-4",
                                                                                        "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
                                                                                        "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
                                                                                        "65-69", "70-74", "75-79", "80-84", "85+"))
  df_mortality <- df_mortality %>% dplyr::group_by(uuid) %>%
    dplyr::mutate(individual_id = dplyr::row_number()) %>%
    dplyr::ungroup() %>% dplyr::mutate(id = paste0(uuid,
                                                   "_", individual_id), individual_id = NULL) %>% dplyr::select(id,
                                                                                                                dplyr::everything())
  return(df_mortality)

}
