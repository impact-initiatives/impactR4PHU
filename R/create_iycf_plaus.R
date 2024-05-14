create_iycf_plaus <- function(df_iycf,
                              age_months = NULL,
                              sex = NULL,
                              iycf_8 = NULL,
                              iycf_caregiver = NULL,
                              yes_value_caregiver = NULL,
                              no_value_caregiver = NULL,
                              exp_prevalence_mad = NULL,
                              exp_sex_ratio = NULL,
                              exp_ratio_under6m_6to23m = NULL,
                              grouping = NULL,
                              uuid = "uuid",
                              short_report = FALSE,
                              file_path = NULL){
  options(warn=-1)

  ## Throw an error if a dataset wasn't provided as a first argument
  if (!is.data.frame(df_iycf)) {
    stop("First argument should be a dataset")
  }

  if (!uuid %in% names(df_iycf)) stop("uuid argument incorrect, or not available in the dataset")

  ## Throw an error if the dataset is empty
  if (nrow(df_iycf) == 0) {
    stop("Dataset is empty")
  }

  if (is.null(grouping)) {
    df_iycf <- df_iycf %>% dplyr::mutate(group = "All")
  } else {
    df_iycf <- df_iycf %>% dplyr::mutate(group = !!rlang::sym(grouping))
  }

  if(!is.null(exp_prevalence_mad)) {
    if(!is.numeric(exp_prevalence_mad) & length(exp_prevalence_mad)==1) {stop("Invalid input for expected prevalence for Minimum Acceptable Diet (MAD). Please input exp_prevalence_mad as a decimal between 0 and 1...or leave blank to assume a default of 30%")}
    if(exp_prevalence_mad > 1) {stop("Please input exp_prevalence_mad as a decimal between 0 and 1.")}

    tot <- 100*exp_prevalence_mad + 100
    left <- (exp_prevalence_mad)
    right <- (1 - exp_prevalence_mad)

    mad_ratio <- c(left, right)

  } else {
    mad_ratio <- c(0.3,0.7)
  }

  if(!is.null(exp_sex_ratio)) {
    if(!is.numeric(exp_sex_ratio) & length(exp_sex_ratio)==1) {stop("Invalid input for exp_sex_ratio. Please put a single numeric value for the expected sex ratio in the population of male:female...or leave blank to assume a 1:1 male to female ratio (or 1).")}

    tot <- 100*exp_sex_ratio + 100
    left <- (100*exp_sex_ratio) / tot
    right <- (100) / tot

    sx_ratio <- c(left, right)

  } else {
    sx_ratio <- c(1,1)
  }

  if(!is.null(exp_ratio_under6m_6to23m)) {
    if(!is.numeric(exp_ratio_under6m_6to23m) & length(exp_ratio_under6m_6to23m)==1) {stop("Invalid input for exp_ratio_under6m_6to23m. Please put a single numeric value for the expected age ratio of children <6 month to children 2-<5years in the population...or leave blank to assume a 0-<2 years to 6 - 23 month ratio of 1/4 (that is, 25% of children are <6 months of age out of under-2 children.).")}

    tot <- 100*exp_ratio_under6m_6to23m + 100
    left <- (100*exp_ratio_under6m_6to23m) / tot
    right <- (100) / tot

    age_under6m_23m_ratio <- c(left, right)
  } else {
    age_under6m_23m_ratio <- c(0.25, 0.75)
  }

  if(age_months %in% colnames(df_iycf)) {

    df2 <- df_iycf %>%
      dplyr::mutate(is_under6m = ifelse(is.na(!!rlang::sym(age_months)), NA, ifelse(!!rlang::sym(age_months) < 6, 1, NA)),
                    is_6to23m = ifelse(is.na(!!rlang::sym(age_months)), NA, ifelse(!!rlang::sym(age_months) > 5 & !!rlang::sym(age_months) < 24, 1, NA))) %>%
      dplyr::group_by(group) %>%
      dplyr::summarize(age_ratio_under6m_6to23m = sum(!is.na(is_under6m)) / sum(!is.na(is_6to23m)),
                       age_ratio_under6m_6to23m.pvalue = stats::chisq.test(x = c(sum(!is.na(is_under6m)), sum(!is.na(is_6to23m))), p = age_under6m_23m_ratio)[3])

    if(!exists("results")) {results <- df2} else {results <- merge(results, df2)}

  }

  if(!is.null(mad_ratio)) {

    if(is.null(exp_prevalence_mad)) { exp_prevalence_mad <- (mad_ratio[1] / mad_ratio[2])}

    left <- mad_ratio[1]
    right <- mad_ratio[2]

    df2 <- df_iycf %>%
      dplyr::mutate(is_mad = ifelse(is.na(iycf_mad), NA, ifelse(iycf_mad == 1, 1, NA)),
                    is_not_mad = ifelse(is.na(iycf_mad), NA, ifelse(iycf_mad == 0, 1, NA))) %>%
      dplyr::group_by(group) %>%
      dplyr::summarize(prop_mad_exp = exp_prevalence_mad,
                       prop_mad_obs = sum(!is.na(is_mad)) / sum(!is.na(iycf_mad)),
                       mad_ratio = sum(!is.na(is_mad)) / sum(!is.na(is_not_mad)),
                       sum_is_mad = sum(!is.na(is_mad)),
                       sum_is_not_mad = sum(!is.na(is_not_mad)),
                       mad_ratio.pvalue = stats::chisq.test(x = c(sum_is_mad, sum_is_not_mad), p = c(left, right))[3])

    if(!exists("results")) {results <- df2} else {results <- merge(results, df2)}

  }

  if(sex %in% colnames(df_iycf)) {

    df2 <- df_iycf %>%
      dplyr::group_by(group) %>%
      dplyr::summarize(sex_ratio = round(as.numeric(nipnTK::sexRatioTest(sex, codes = c("1", "2"), pop = sx_ratio)[1]),3),
                       sex_ratio.pvalue = round(as.numeric(nipnTK::sexRatioTest(sex, codes = c("1", "2"), pop = sx_ratio)[5]),2))

    if(!exists("results")) {results <- df2} else {results <- merge(results, df2)}


  }

  if(c("iycf_mdd_score") %in% colnames(df_iycf)) {

    df2 <- df_iycf %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(mean_mdd = mean(iycf_mdd_score, na.rm = TRUE),
                       sd_mdd = stats::sd(iycf_mdd_score, na.rm = TRUE))

    if(!exists("results")) {results <- df2} else {results <- merge(results, df2)}

  }

  if(iycf_8 %in% colnames(df_iycf)) {

    df2 <- df_iycf %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(mean_mmf = mean(iycf_8, na.rm = TRUE),
                       sd_mmf = stats::sd(iycf_8, na.rm = TRUE))

    if(!exists("results")) {results <- df2} else {results <- merge(results, df2)}

  }

  if(c("flag_no_foods") %in% colnames(df_iycf)) {

    df2 <- df_iycf %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(prop_flag_no_foods = sum(flag_no_foods, na.rm = TRUE) / sum(!is.na(flag_no_foods)))

    if(!exists("results")) {results <- df2} else {results <- merge(results, df2)}

  }

  if(c("flag_yes_foods") %in% colnames(df_iycf)) {

    df2 <- df_iycf %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(prop_flag_yes_foods = sum(flag_yes_foods, na.rm = TRUE) / sum(!is.na(flag_yes_foods)))

    if(!exists("results")) {results <- df2} else {results <- merge(results, df2)}


  }

  if(c("flag_all_foods_no_meal") %in% colnames(df_iycf)) {

    df2 <- df_iycf %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(prop_flag_all_foods_no_meal = sum(flag_all_foods_no_meal, na.rm = TRUE) / sum(!is.na(flag_all_foods_no_meal)))

    if(!exists("results")) {results <- df2} else {results <- merge(results, df2)}


  }

  if(c("flag_some_foods_no_meal") %in% colnames(df_iycf)) {

    df2 <- df_iycf %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(prop_flag_some_foods_no_meal = sum(flag_some_foods_no_meal, na.rm = TRUE) / sum(!is.na(flag_some_foods_no_meal)))

    if(!exists("results")) {results <- df2} else {results <- merge(results, df2)}

  }

  if(c("flag_yes_liquids") %in% colnames(df_iycf)) {

    df2 <- df_iycf %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(prop_flag_yes_liquids = sum(flag_yes_liquids , na.rm = TRUE) / sum(!is.na(flag_yes_liquids )))

    if(!exists("results")) {results <- df2} else {results <- merge(results, df2)}
  }

  if(c("flag_no_anything") %in% colnames(df_iycf)) {

    df2 <- df_iycf %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(prop_flag_no_anything = sum(flag_no_anything, na.rm = TRUE) / sum(!is.na(flag_no_anything)))

    if(!exists("results")) {results <- df2} else {results <- merge(results, df2)}
  }

  if(iycf_caregiver %in% colnames(df_iycf)) {

    df2 <- df_iycf %>%
      dplyr::mutate(total_iycf_caregiver = ifelse(!is.na(!!rlang::sym(iycf_caregiver)), 1, 0),
                    !!rlang::sym(iycf_caregiver) := dplyr::if_else(is.na(!!rlang::sym(iycf_caregiver)), NA,
                                                                    dplyr::if_else(!!rlang::sym(iycf_caregiver) == yes_value_caregiver, 1,
                                                                                   dplyr::if_else(!!rlang::sym(iycf_caregiver) == no_value_caregiver, 0,NA)))) %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(prop_iycf_caregiver = sum(!!rlang::sym(iycf_caregiver), na.rm = TRUE) / sum(total_iycf_caregiver))

    if(!exists("results")) {results <- df2} else {results <- merge(results, df2)}

  } else {

    df2 <- df_iycf %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(prop_iycf_caregiver = 0)

    if(!exists("results")) {results <- df2} else {results <- merge(results, df2)}

  }

  results <- impactR4PHU::calculate_plausibility(results)

  a <- c("n", "prop_mad_obs", "mad_ratio.pvalue", "age_ratio_under6m_6to23m",
         "sex_ratio", "sex_ratio.pvalue",
         "age_ratio_under6m_6to23m.pvalue", "prop_iycf_caregiver", "prop_flag_high_mdd_low_mmf",
         "iycf_plaus_score", "iycf_plaus_cat")

  b <- intersect(a, colnames(results))

  if(short_report == TRUE & length(setdiff(b, colnames(results)))==0) {

    results <- results %>%
      dplyr::select(1, b)

  }

  # Saving the new dataframe to a xlsx, if specified
  if(!is.null(file_path)) {writexl::write_xlsx(results, file_path)}
  options(warn=0)
  return(results)
}
