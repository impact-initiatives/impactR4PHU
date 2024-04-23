#' create_mortality_plaus
#'
#' @param df_mortality output dataframe long mortality from create_mortality_long_df
#' @param exp_sex_ratio expected sex ratio in the population of male:female
#' By default: 1:1
#' @param exp_ratio_0_4 expected age ratio of <5years to >5years in the population
#' By default: 0.2:0.8
#' @param exp_ratio_2_5 expected age ratio of children 0-<2 years to children 2-<5years in the population
#' By default: 0.4118:0.5882
#' @param exp_ratio_5_10 expected age ratio of children 0-<5 years to children 5-<10years in the population
#' By default: 0.5238:0.4762
#' @param exp_hh_size expected hh size in the population. By default: 5
#' @param grouping the name of the variable that indicates the grouping variable - usually "enumerator"
#' @param uuid uuid variable
#' @param short_report Inputs a boolean value TRUE or FALSE to return just key variables. If FALSE,
#' returns a dataframe of all the variables calculated.
#' @param file_path Inputs an optional character value specifying the file location to save a copy
#' of the results.
#'
#' @return a dataframe with all mortality related plausibility columns
#' @export
#'
#' @examples
#' \dontrun{
#'   create_mortality_plaus(df_mortality)
#' }
create_mortality_plaus <- function(df_mortality,
                                   exp_sex_ratio = NULL,
                                   exp_ratio_0_4 = NULL,
                                   exp_ratio_2_5 = NULL,
                                   exp_ratio_5_10 = NULL,
                                   exp_hh_size = NULL,
                                   grouping = NULL,
                                   uuid = "uuid",
                                   short_report = FALSE,
                                   file_path = NULL) {
  options(warn=-1)

  ## Throw an error if a dataset wasn't provided as a first argument
  if (!is.data.frame(df_mortality)) {
    stop("First argument should be a dataset")
  }

  if (!uuid %in% names(df_mortality)) stop("uuid argument incorrect, or not available in the dataset")

  ## Throw an error if the dataset is empty
  if (nrow(df_mortality) == 0) {
    stop("Dataset is empty")
  }
  if(!all(c("sex", "age_years", "join", "left", "birth", "death", "date_dc", "date_recall") %in% names(df_mortality))) {
    stop("It does not appear that the dataset has been formatted yet by the create_mortality_long_df function,\n as it is missing at least one of the column names of sex, age_years, join, left, birth, death, date_dc, date_recall. Please standardize the data before using this function.")
  }

  if (is.null(grouping)) {
    df_mortality <- df_mortality %>% dplyr::mutate(group = "All")
  } else {
    df_mortality <- df_mortality %>% dplyr::mutate(group = !!rlang::sym(grouping))
  }

  if(!is.null(exp_sex_ratio)) {
    if(!is.numeric(exp_sex_ratio) & length(exp_sex_ratio)==1) {stop("Invalid input for exp_sex_ratio. Please put a single numeric value for the expected sex ratio in the population of male:female...or leave blank to assume a 1:1 male to female ratio (or 1).")}

    tot <- 100*exp_sex_ratio + 100
    left <- (100*exp_sex_ratio) / tot
    right <- (100) / tot

    sx_ratio <- c(left, right)

  } else {
    print("No expected sex ratio given, defaulting to 1:1 (male:female).")
    sx_ratio <- c(1,1)
  }

  if(!is.null(exp_ratio_0_4)) {
    if(!is.numeric(exp_ratio_0_4) & length(exp_ratio_0_4)==1) {stop("Invalid input for exp_ratio_0_4years Please put a single numeric value for the expected age ratio of <5years to >5years in the population...or leave blank to assume a 1:4 <5 years to >5 years ratio (that is, 20% of individuals are <5 years of age).")}

    tot <- 100*exp_ratio_0_4 + 100
    left <- (100*exp_ratio_0_4) / tot
    right <- (100) / tot

    age_under5_ratio <- c(left, right)

  } else {
    print("No expected age ratio given, defaulting to 0.25 (or 20% of individuals are <5 years of age.")
    age_under5_ratio <- c(.2,.8)
  }

  if(!is.null(exp_ratio_2_5)) {
    if(!is.numeric(exp_ratio_2_5) & length(exp_ratio_2_5)==1) {stop("Invalid input for exp_ratio_2_5 Please put a single numeric value for the expected age ratio of children 0-<2 years to children 2-<5years in the population...or leave blank to assume a 0-<2 years to 2-<5years ratio of 0.7 (that is, ~41% of individuals are <2 years of age out of under-5 children.).")}

    tot <- 100*exp_ratio_2_5 + 100
    left <- (100*exp_ratio_2_5) / tot
    right <- (100) / tot

    age_under2to5_ratio <- c(left, right)
  } else {
    print("No expected age ratio given, defaulting to 0.7 (or approx. 41% of individuals are <2 years of age of all children <5 years.")
    age_under2to5_ratio <- c(0.4118, 0.5882)
  }

  if(!is.null(exp_ratio_5_10)) {
    if(!is.numeric(exp_ratio_5_10) & length(exp_ratio_5_10)==1) {stop("Invalid input for exp_ratio_5_10 Please put a single numeric value for the expected age ratio of children 0-<5 years to children 5-<10years in the population...or leave blank to assume a 0-<5 years to 5-<10years ratio of 1.1 (that is, ~52% of individuals are <5 years of age out of under-10 children).")}

    tot <- 100*exp_ratio_5_10 + 100
    left <- (100*exp_ratio_5_10) / tot
    right <- (100) / tot

    age_under5to10_ratio <- c(left, right)
  } else {
    print("No expected age ratio given, defaulting to 1.1 (or approx. 52% of individuals are <5 years of age out all children <10 years.")
    age_under5to10_ratio <- c(0.5238, 0.4762)
  }

  if(!is.null(exp_hh_size)) {
    if(!is.numeric(exp_hh_size) & length(exp_hh_size)==1) {stop("Invalid input for exp_hh_size Please put a single numeric value for the expected hh size in the population...or leave blank to assume a 5 average HH size.")}

    expected_hh_size <- exp_hh_size

  } else {
    print("No expected household size given, defaulting to 5 people per household. Advised to use a value from secondary data instead.")
    expected_hh_size <- 5
  }

  if(c("age_years") %in% names(df_mortality)) {df_mortality <- df_mortality %>% dplyr::mutate(age_years = as.numeric(age_years))}

  # need to add sex and age ratios, poisson p-values for deaths, proportion of HHs with under-5 child, Avg. household size per grouping

  # summarizing individual level indicators

  df2 <- df_mortality %>%
    dplyr::group_by(group) %>%
    dplyr::summarize(total_people = sum(!is.na(person_time), na.rm = TRUE),
                     total_persontime = sum(person_time, na.rm = TRUE),
                     avg.persontime = mean(person_time, na.rm = TRUE),
                     total_under5 = sum(under_5, na.rm = TRUE),
                     total_under5_persontime = sum(under_5_pt, na.rm = TRUE),
                     avg.under5_persontime = mean(under_5_pt, na.rm = TRUE),
                     joins = sum(!is.na(join), na.rm = TRUE),
                     joins_under5 = sum(!is.na(join_under5), na.rm = TRUE),
                     lefts = sum(!is.na(left), na.rm = TRUE),
                     lefts_under5 = sum(!is.na(left_under5), na.rm = TRUE),
                     births = sum(!is.na(birth), na.rm = TRUE),
                     births_under5 = sum(!is.na(birth_under5), na.rm = TRUE),
                     deaths = sum(!is.na(death), na.rm = TRUE),
                     deaths_under5 = sum(!is.na(death_under5), na.rm = TRUE),
                     sex_ratio = round(as.numeric(nipnTK::sexRatioTest(sex, codes = c("1", "2"), pop = sx_ratio)[1]),3),
                     sex_ratio.pvalue = round(as.numeric(nipnTK::sexRatioTest(sex, codes = c("1", "2"), pop = sx_ratio)[5]),2),
                     age_ratio_0_5 = sum(!is.na(age_0to5)) / sum(!is.na(age_5plus)),
                     age_ratio_0_5.pvalue = stats::chisq.test(x = c(sum(!is.na(age_0to5)), sum(!is.na(age_5plus))), p = age_under5_ratio)[3],
                     age_ratio_2_5 = sum(!is.na(age_0to2)) / sum(!is.na(age_2to5)),
                     age_ratio_2_5.pvalue = stats::chisq.test(x = c(sum(!is.na(age_0to2)), sum(!is.na(age_2to5))), p = age_under2to5_ratio)[3],
                     age_ratio_5_10 = sum(!is.na(under_5)) / sum(!is.na(age_5to10)),
                     age_ratio_5_10.pvalue = stats::chisq.test(x = c(sum(!is.na(under_5)), sum(!is.na(age_5to10))), p = age_under5to10_ratio)[3]
    ) %>%
    dplyr::mutate(cdr = deaths / (total_persontime),
                  cdr_se = sqrt((cdr * (1 - cdr)) / total_persontime),
                  cdr_lower_ci = round((cdr - 1.96*cdr_se)*10000,3),
                  cdr_upper_ci = round((cdr + 1.96*cdr_se)*10000,3),
                  u5dr = deaths_under5 / (total_under5_persontime),
                  u5dr_se = sqrt((u5dr * (1 - u5dr)) / total_under5_persontime),
                  u5dr_lower_ci = round((u5dr - 1.96*u5dr_se)*10000,3),
                  u5dr_upper_ci = round((u5dr + 1.96*u5dr_se)*10000,3),
                  cdr = round(cdr,6)*10000,
                  u5dr = round(u5dr*10000,3),
                  cdr_ci = paste0(cdr, " [", cdr_lower_ci, " - ", cdr_upper_ci, "]"),
                  u5dr_ci = paste0(u5dr, " [", u5dr_lower_ci, " - ", u5dr_upper_ci, "]"),
                  prop_join_people = round((joins / total_people),2)*100,
                  prop_left_people = round((lefts / total_people),2)*100) %>%
    dplyr::select(cdr_ci, u5dr_ci, dplyr::everything())

  # summarizing household level indicators
  # average household size, % of households with a child under 5
  # of households

  df3 <- df_mortality %>%
    dplyr::group_by(group, uuid) %>%
    dplyr::summarise(hh_size = sum(!is.na(sex), na.rm = TRUE),
                     total_under5 = sum(!is.na(under_5), na.rm = TRUE),
                     num_deaths = sum(!is.na(death), na.rm = TRUE),
                     total_flag_deaths = sum(flag_multiple_death, na.rm = TRUE),
                     total_flag_cause_deaths = sum(flag_cause_death, na.rm = TRUE)) %>%
    dplyr::mutate(is_hh = ifelse(is.na(uuid), NA, 1),
                  is_hh_under5 = ifelse(is.na(uuid), NA, ifelse(total_under5 > 0, 1, 0)),
                  is_hh_flag_deaths = ifelse(is.na(uuid), NA, ifelse(total_flag_deaths > 0, 1, 0)),
                  is_hh_flag_cause_deaths = ifelse(is.na(uuid), NA, ifelse(total_flag_cause_deaths > 0, 1, 0))) %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(mean_hh_size = mean(hh_size, na.rm = TRUE),
                     mean_hh_size.pvalue = round(as.numeric(stats::t.test(x = hh_size, mu = expected_hh_size, alternative = "two.sided")[3]),2),
                     mean_num_under5 = mean(total_under5, na.rm = TRUE),
                     mean_deaths_per_hh = mean(num_deaths, na.rm = TRUE),
                     n_hh = sum(is_hh, na.rm = TRUE),
                     n_hh_under_5 = sum(is_hh_under5, na.rm = TRUE),
                     n_hh_flag_deaths = sum(is_hh_flag_deaths, na.rm = TRUE),
                     n_hh_flag_cause_deaths = sum(is_hh_flag_deaths, na.rm = TRUE)
    ) %>%
    dplyr::mutate(prop_hh_under5 = round((n_hh_under_5 / n_hh),2),
                  prop_hh_flag_deaths = round((n_hh_flag_deaths / n_hh), 2))

  df4 <- merge(df3, df2, all.x = TRUE)


  df4 <- impactR4PHU::calculate_plausibility(df4)

  df4 <- df4 %>%
    dplyr::select(c(1, cdr, cdr_ci, u5dr, u5dr_ci, deaths, deaths_under5, mean_deaths_per_hh,
                    n_hh_flag_deaths, prop_hh_flag_deaths, total_people, mean_hh_size,
                    mean_hh_size.pvalue, total_persontime, total_under5, mean_num_under5, total_under5_persontime,
                    n_hh, n_hh_under_5, sex_ratio, sex_ratio.pvalue, age_ratio_0_5,
                    age_ratio_0_5.pvalue, age_ratio_2_5, age_ratio_2_5.pvalue, age_ratio_5_10, age_ratio_5_10.pvalue,
                    joins, prop_join_people, lefts, prop_left_people, births,
                    plaus_cdr,  plaus_hh_multiple_death, plaus_sex_ratio,
                    plaus_age0to4_5plus_ratio, plaus_age0to1_2to4_ratio, plaus_age0to4_5to10_ratio,
                    plaus_mort_score, plaus_mort_cat))


  if(short_report == TRUE) {

    df4 <- df4 %>%
      dplyr::select(1,cdr_ci,u5dr_ci,deaths, deaths_under5, prop_hh_flag_deaths,
                    sex_ratio.pvalue, age_ratio_0_5.pvalue,prop_join_people,prop_left_people,
                    plaus_cdr,  plaus_hh_multiple_death, plaus_sex_ratio,
                    plaus_age0to4_5plus_ratio, plaus_age0to1_2to4_ratio, plaus_age0to4_5to10_ratio,
                    plaus_mort_score, plaus_mort_cat)
  }
  # Saving the new dataframe to a xlsx, if specified
  if(!is.null(file_path)) {writexl::write_xlsx(df4, file_path)}
  options(warn=0)

  return(df4)
}
