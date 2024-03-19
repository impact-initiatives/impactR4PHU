#' check_fsl_flags
#'
#' @param .dataset the raw dataset with all add_x indicators functions called in
#' @param fcs_cereal  the name of the variable that indicates the number of days cereals were consumed
#' @param fcs_legumes  the name of the variable that indicates the number of days legumes were consumed
#' @param fcs_dairy  the name of the variable that indicates the number of days dairy were consumed
#' @param fcs_meat  the name of the variable that indicates the number of days meat were consumed
#' @param fcs_veg  the name of the variable that indicates the number of days vegetables were consumed
#' @param fcs_fruit  the name of the variable that indicates the number of days fruit were consumed
#' @param fcs_oil  the name of the variable that indicates the number of days oil was consumed
#' @param fcs_sugar  the name of the variable that indicates the number of days sugar was consumed
#' @param rcsi_lessquality Column representing question- During the last 7 days, were there days (and, if so, how many) when your household had to rely on less preferred and less expensive food to cope with a lack of food or money to buy it?
#' @param rcsi_borrow Column representing question- During the last 7 days, were there days (and, if so, how many) when your household had to borrow food or rely on help from a relative or friend to cope with a lack of food or money to buy it?
#' @param rcsi_mealsize Column representing question- During the last 7 days, were there days (and, if so, how many) when your household had to limit portion size of meals at meal times to cope with a lack of food or money to buy it?
#' @param rcsi_mealadult Column representing question- During the last 7 days, were there days (and, if so, how many) when your household had to restrict consumption by adults in order for small children to eat to cope with a lack of food or money to buy it?
#' @param rcsi_mealnb Column representing question - During the last 7 days, were there days (and, if so, how many) when your household had to reduce number of meals eaten in a day to cope with a lack of food or money to buy it?
#' @param hhs_nofoodhh The name of the column "In the past 4 weeks (30 days), was there ever no food to eat
#' of any kind in your house because of lack of resources to get food?". It has to be a string.
#' @param hhs_nofoodhh_freq The name of the column "How often did this happen in the past (4 weeks/30 days)?".
#' It has to be a string.
#' @param hhs_sleephungry The name of the column "In the past 4 weeks (30 days), did you or any household
#' member go to sleep at night hungry because there was not enough food?". It has to be a string.
#' @param hhs_sleephungry_freq The name of the column "How often did this happen in the past (4 weeks/30 days)?".
#' It has to be a string.
#' @param hhs_alldaynight The name of the column "In the past 4 weeks (30 days), did you or any household
#' member go a whole day and night without eating anything at all because there was not enough food?".
#' It has to be a string.
#' @param hhs_alldaynight_freq The name of the column "How often did this happen in the past (4 weeks/30 days)?".
#' It has to be a string.
#' @param hdds_cereals the name of the variable that indicates if cereals were consumed in the last 24 hours
#' @param hdds_tubers the name of the variable that indicates if roots or tubers were consumed in the last 24 hours
#' @param hdds_legumes the name of the variable that indicates if pulses or legumes were consumed in the last 24 hours
#' @param hdds_veg the name of the variable that indicates if vegetables were consumed in the last 24 hours
#' @param hdds_fruit the name of the variable that indicates if fruit were consumed in the last 24 hours
#' @param hdds_meat the name of the variable that indicates if meat were consumed in the last 24 hours
#' @param hdds_fish the name of the variable that indicates if fish were consumed in the last 24 hours
#' @param hdds_dairy the name of the variable that indicates if dairy were consumed in the last 24 hours
#' @param hdds_eggs the name of the variable that indicates if eggs were consumed in the last 24 hours
#' @param hdds_sugar the name of the variable that indicates if sugar were consumed in the last 24 hours
#' @param hdds_oil the name of the variable that indicates if oil was consumed in the last 24 hours
#' @param hdds_condiments the name of the variable that indicates if condiments were consumed in the last 24 hours
#' @param lcsi_stress1 the name of the variable that indicates the first stress LCSI strategy
#' @param lcsi_stress2 the name of the variable that indicates the second stress LCSI strategy
#' @param lcsi_stress3 the name of the variable that indicates the third stress LCSI strategy
#' @param lcsi_stress4 the name of the variable that indicates the fourth stress LCSI strategy
#' @param lcsi_crisis1 the name of the variable that indicates the first crisis LCSI strategy
#' @param lcsi_crisis2 the name of the variable that indicates the second crisis LCSI strategy
#' @param lcsi_crisis3 the name of the variable that indicates the third crisis LCSI strategy
#' @param lcsi_emergency1 the name of the variable that indicates the first emergency LCSI strategy
#' @param lcsi_emergency2 the name of the variable that indicates the second emergency LCSI strategy
#' @param lcsi_emergency3 the name of the variable that indicates the third emergency LCSI strategy
#' @param lcsi_stress the name of the variable that indicates the calculation of the stress LCSI strategy
#' @param lcsi_crisis the name of the variable that indicates the calculation of the crisis LCSI strategy
#' @param lcsi_emergency the name of the variable that indicates the calculation of the stress LCSI strategy
#' @param lcsi_cat_yes the name of the variable that indicates the highest category of the LCSI strategy used yes
#' @param lcsi_cat_exhaust the name of the variable that indicates the highest category of the LCSI strategy used exhaustively
#' @param lcsi_cat the name of the variable that indicates the highest category of the LCSI strategy
#' @param fcs_cat the name of the variable that indicates the food consumption score category
#' @param fcs_score the name of the variable that indicates the food consumption score
#' @param rcsi_cat the name of the variable that indicates the reduced coping strategy index category
#' @param rcsi_score the name of the variable that indicates the reduced coping strategy index score
#' @param hhs_cat the name of the variable that indicates the household hunger scale category
#' @param hhs_score the name of the variable that indicates the household hunger scale score
#' @param hdds_cat the name of the variable that indicates the household dietary diversity score category
#' @param hdds_score the name of the variable that indicates the household dietary diversity score
#' @param fc_cell the name of the variable that indicates the food consumption matrix score
#' @param fc_phase the name of the variable that indicates the food consumption matrix phase
#' @param num_children the name of the variable that indicates the number of children available in each household
#' @param grouping the name of the variable that indicates the grouping variable - usually "enumerator"
#' @param tool.survey This is the tool.survey dataset. By default NULL
#' @param uuid uuid variable
#'
#' @return a dataframe that includes all the logical flags related to food security and livelihoods.
#' This includes:
#' - flag_meat_cereal_ratio
#' - flag_low_cereal
#' - flag_low_fcs
#' - flag_high_fcs
#' - flag_low_oil
#' - flag_sd_foodgroup
#' - flag_protein_rcsi
#' - flag_fcs_rcsi
#' - flag_high_rcsi
#' - flag_rcsi_children
#' - flag_fcsrcsi_box
#' - flag_sd_rcsicoping
#' - flag_severe_hhs
#' - flag_lcsi_coherence
#' - flag_lcsi_severity
#' - flag_lcsi_na
#' - flag_lcsi_liv_agriculture
#' - flag_lcsi_liv_livestock
#' - flag_lcsi_displ
#' - flag_fc_cell
#' - flag_low_sugar_cond_hdds
#' @export
#'
#' @examples
#' tool.survey <- impactR4PHU_survey_template
#' df <- data.frame(
#'   fsl_fcs_cereal = c(1, 4),
#'   fsl_fcs_legumes = c(3, 3),
#'   fsl_fcs_dairy = c(1, 6),
#'   fsl_fcs_meat = c(2, 3),
#'   fsl_fcs_veg = c(5, 4),
#'   fsl_fcs_fruit = c(1, 4),
#'   fsl_fcs_oil = c(1, 4),
#'   fsl_fcs_sugar = c(6, 2),
#'   fsl_rcsi_lessquality = c(2,4),
#'   fsl_rcsi_borrow = c(1,4),
#'   fsl_rcsi_mealsize = c(4,5),
#'   fsl_rcsi_mealadult = c(2,4),
#'   fsl_rcsi_mealnb = c(3,2),
#'   fsl_hhs_nofoodhh = c("yes","no"),
#'   fsl_hhs_nofoodhh_freq = c("rarely", NA),
#'   fsl_hhs_sleephungry= c("no","no"),
#'   fsl_hhs_sleephungry_freq = c(NA, NA),
#'   fsl_hhs_alldaynight= c("no","yes"),
#'   fsl_hhs_alldaynight_freq = c(NA, "often"),
#'   fsl_hdds_cereals = c("yes","no"),
#'   fsl_hdds_tubers = c("yes","no"),
#'   fsl_hdds_legumes = c("yes","no"),
#'   fsl_hdds_veg = c("no","no"),
#'   fsl_hdds_fruit = c("yes","no"),
#'   fsl_hdds_meat = c("yes","no"),
#'   fsl_hdds_fish = c("yes","no"),
#'   fsl_hdds_dairy = c("no","no"),
#'   fsl_hdds_eggs = c("yes","no"),
#'   fsl_hdds_sugar = c("yes","no"),
#'   fsl_hdds_oil = c("yes","no"),
#'   fsl_hdds_condiments = c("yes","no"),
#'   fsl_lcsi_stress1 = c("not_applicable","yes"),
#'   fsl_lcsi_stress2 = c("no_had_no_need","not_applicable"),
#'   fsl_lcsi_stress3 = c("not_applicable","yes"),
#'   fsl_lcsi_stress4 = c("no_exhausted","yes"),
#'   fsl_lcsi_crisis1 = c("no_exhausted","yes"),
#'   fsl_lcsi_crisis2 = c("no_had_no_need","yes"),
#'   fsl_lcsi_crisis3 = c("no_had_no_need","yes"),
#'   fsl_lcsi_emergency1 = c("no_had_no_need","yes"),
#'   fsl_lcsi_emergency2 = c("not_applicable","no_exhausted"),
#'   fsl_lcsi_emergency3 = c("not_applicable","yes"),
#'   fsl_lcsi_stress = c(1, 1),
#'   fsl_lcsi_crisis = c(1, 1),
#'   fsl_lcsi_emergency = c(0, 1),
#'   fsl_lcsi_cat_yes = c("None","Emergency"),
#'   fsl_lcsi_cat_exhaust = c("Crisis","Emergency"),
#'   fsl_lcsi_cat = c("Crisis","Emergency"),
#'   fsl_fcs_cat = c("Borderline","Acceptable"),
#'   fsl_fcs_score = c(32.5,64.0),
#'   fsl_rcsi_cat= c("Medium","High"),
#'   fsl_rcsi_score= c(17,31),
#'   fsl_hhs_cat= c("No or Little","Moderate"),
#'   fsl_hhs_score= c(1,2),
#'   fsl_hdds_cat= c("High","Low"),
#'   fsl_hdds_score= c(10,0),
#'   fc_cell= c(22,33),
#'   fc_phase= c("Phase 2 FC","Phase 3 FC"),
#'   num_children= c(2,3),
#'   enumerator = c("team1","team2"),
#'   uuid= c("31d0cfb8-21d7-414b4f-94999f-04a15ce39d78","205d37b1-5a6f-44484d-b3b1ba-4eafbdc50873")
#'   )
#'
#' check_fsl_flags(.dataset = df, grouping = "enumerator", tool.survey = tool.survey)

check_fsl_flags <- function(.dataset,
                           fcs_cereal = "fsl_fcs_cereal",
                           fcs_legumes = "fsl_fcs_legumes",
                           fcs_dairy = "fsl_fcs_dairy",
                           fcs_meat = "fsl_fcs_meat",
                           fcs_veg = "fsl_fcs_veg",
                           fcs_fruit = "fsl_fcs_fruit",
                           fcs_oil = "fsl_fcs_oil",
                           fcs_sugar = "fsl_fcs_sugar",
                           rcsi_lessquality = "fsl_rcsi_lessquality",
                           rcsi_borrow = "fsl_rcsi_borrow",
                           rcsi_mealsize = "fsl_rcsi_mealsize",
                           rcsi_mealadult = "fsl_rcsi_mealadult",
                           rcsi_mealnb = "fsl_rcsi_mealnb",
                           hhs_nofoodhh = "fsl_hhs_nofoodhh",
                           hhs_nofoodhh_freq = "fsl_hhs_nofoodhh_freq",
                           hhs_sleephungry = "fsl_hhs_sleephungry",
                           hhs_sleephungry_freq = "fsl_hhs_sleephungry_freq",
                           hhs_alldaynight = "fsl_hhs_alldaynight",
                           hhs_alldaynight_freq = "fsl_hhs_alldaynight_freq",
                           hdds_cereals = "fsl_hdds_cereals",
                           hdds_tubers = "fsl_hdds_tubers",
                           hdds_legumes = "fsl_hdds_legumes",
                           hdds_veg = "fsl_hdds_veg",
                           hdds_fruit = "fsl_hdds_fruit",
                           hdds_meat = "fsl_hdds_meat",
                           hdds_fish = "fsl_hdds_fish",
                           hdds_dairy = "fsl_hdds_dairy",
                           hdds_eggs = "fsl_hdds_eggs",
                           hdds_sugar = "fsl_hdds_sugar",
                           hdds_oil = "fsl_hdds_oil",
                           hdds_condiments = "fsl_hdds_condiments",
                           lcsi_stress1 = "fsl_lcsi_stress1",
                           lcsi_stress2 = "fsl_lcsi_stress2",
                           lcsi_stress3 = "fsl_lcsi_stress3",
                           lcsi_stress4 = "fsl_lcsi_stress4",
                           lcsi_crisis1 = "fsl_lcsi_crisis1",
                           lcsi_crisis2 = "fsl_lcsi_crisis2",
                           lcsi_crisis3 = "fsl_lcsi_crisis3",
                           lcsi_emergency1 = "fsl_lcsi_emergency1",
                           lcsi_emergency2 = "fsl_lcsi_emergency2",
                           lcsi_emergency3 = "fsl_lcsi_emergency3",
                           lcsi_stress = "fsl_lcsi_stress",
                           lcsi_crisis = "fsl_lcsi_crisis",
                           lcsi_emergency = "fsl_lcsi_emergency",
                           lcsi_cat_yes = "fsl_lcsi_cat_yes",
                           lcsi_cat_exhaust = "fsl_lcsi_cat_exhaust",
                           lcsi_cat = "fsl_lcsi_cat",
                           fcs_cat ="fsl_fcs_cat",
                           fcs_score = "fsl_fcs_score",
                           rcsi_cat = "fsl_rcsi_cat",
                           rcsi_score = "fsl_rcsi_score",
                           hhs_cat = "fsl_hhs_cat",
                           hhs_score = "fsl_hhs_score",
                           hdds_cat = "fsl_hdds_cat",
                           hdds_score = "fsl_hdds_score",
                           fc_cell = "fc_cell",
                           fc_phase = "fc_phase",
                           num_children = "num_children",
                           grouping = NULL,
                           tool.survey = NULL,
                           uuid = "uuid") {

  ## Throw an error if a dataset wasn't provided as a first argument
  if (!is.data.frame(.dataset)) {
    stop("First argument should be a dataset")
  }

  options(warn = -1)
  ## Throw an error if the tool is empty
  if(is.null(tool.survey)) stop("Tool survey not available")

  if (!is.data.frame(tool.survey)) {
    stop("tool.survey should be a dataset")
  }

  if (nrow(tool.survey) == 0) {
    stop("tool.survey is empty")
  }

  ## Throw an error if the dataset is empty
  if (nrow(.dataset) == 0) {
    stop("Dataset is empty")
  }

  if (!uuid %in% names(.dataset)) stop("uuid argument incorrect, or not available in the dataset")

  if (!methods::hasArg(grouping)) {
    df <- df %>% dplyr::mutate(group = "All")
    grouping <- "group"
  }

  ## initiate the return output
  results <- .dataset %>%
    dplyr::select(uuid, grouping)

  # combine all fcs_columns together
  fcs_flag_columns <- c(fcs_cereal,fcs_legumes,fcs_dairy,fcs_meat,fcs_veg,fcs_fruit,fcs_oil,fcs_sugar,fcs_score)

  ## Test if all columns are in the dataset
  if(!all(fcs_flag_columns %in% names(.dataset))) {
    stop("Missing fcs columns")
  } else{
    ## flag issues in data with FCS
    results2 <- .dataset %>%
      dplyr::mutate_at(dplyr::vars(fcs_flag_columns),as.numeric)%>%
      dplyr::mutate(flag_meat_cereal_ratio = ifelse(is.na(!!rlang::sym(fcs_cereal)), NA, ifelse(!!rlang::sym(fcs_cereal) < fcs_meat, 1, 0)),
                    flag_low_cereal = ifelse(is.na(!!rlang::sym(fcs_cereal)), NA, ifelse(!!rlang::sym(fcs_cereal) < 5, 1, 0)),
                    flag_low_fcs = ifelse(is.na(!!rlang::sym(fcs_score)),NA, ifelse(!!rlang::sym(fcs_score)<=10,1,0)),
                    flag_high_fcs = ifelse(is.na(!!rlang::sym(fcs_score)),NA, ifelse(!!rlang::sym(fcs_score)>=56,1,0)),
                    flag_low_oil = ifelse(is.na(!!rlang::sym(fcs_cereal)), NA, ifelse(!!rlang::sym(fcs_oil) < 5, 1, 0))) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(sd_foods = stats::sd(c(!!rlang::sym(fcs_cereal), !!rlang::sym(fcs_legumes), !!rlang::sym(fcs_dairy),
                                    !!rlang::sym(fcs_meat), !!rlang::sym(fcs_veg), !!rlang::sym(fcs_fruit),
                                    !!rlang::sym(fcs_oil), !!rlang::sym(fcs_sugar)), na.rm = TRUE),
                    flag_sd_foodgroup = dplyr::case_when(sd_foods < 0.8 ~ 1,
                                                         .default = 0,
                                                         TRUE ~ NA)) %>%
      dplyr::ungroup() %>%
      dplyr::select(fcs_flag_columns,
                    fcs_cat,
                    flag_meat_cereal_ratio,
                    flag_low_cereal,
                    flag_low_oil,
                    flag_low_fcs,
                    flag_high_fcs,
                    flag_sd_foodgroup)

    if(!exists("results")){
      results <- results2
    } else {
      results <- cbind(results,results2)
    }
  }
  ## flag issues in data with rCSI

  rcsi_flag_columns <- c(rcsi_lessquality,rcsi_borrow,rcsi_mealsize,rcsi_mealadult,rcsi_mealnb,rcsi_score)
  if(!all(rcsi_flag_columns %in% names(.dataset))) {
    stop("Missing rcsi columns")
  } else {
    results2 <- .dataset %>%
      dplyr::mutate_at(dplyr::vars(rcsi_flag_columns),as.numeric)%>%
      dplyr::mutate(flag_protein_rcsi = ifelse(is.na(!!rlang::sym(rcsi_score)), NA,
                                               ifelse(is.na(!!rlang::sym(fcs_cereal)), NA,
                                                      ifelse(!!rlang::sym(rcsi_score) >= 19 & ( !!rlang::sym(fcs_dairy) >= 5 | !!rlang::sym(fcs_meat) >= 5), 1, 0 ))),
                    flag_fcs_rcsi = ifelse(is.na(!!rlang::sym(rcsi_score)), NA,
                                           ifelse(is.na(!!rlang::sym(fcs_score)), NA,
                                                  ifelse(!!rlang::sym(fcs_score) < 35 & !!rlang::sym(rcsi_score) <= 4, 1, 0 ))),
                    flag_high_rcsi = ifelse(is.na(!!rlang::sym(rcsi_score)), NA, ifelse(!!rlang::sym(rcsi_score) >= 43, 1, 0)))
    if(!num_children %in% names(.dataset)) {
      warning("num_children argument incorrect or not available in the dataset.")
      results2 <- results2 %>%
        dplyr::mutate(flag_rcsi_children = NA)
    } else{
      results2 <- results2 %>%
        dplyr::mutate(flag_rcsi_children = ifelse(is.na(!!rlang::sym(rcsi_mealadult)), NA, ifelse(!is.na(!!rlang::sym(rcsi_mealadult)) & as.numeric(num_children) == 0, 1,0)))
    }
    results2 <- results2 %>%
      dplyr::mutate(flag_fcsrcsi_box = dplyr::case_when(as.numeric(!!rlang::sym(rcsi_score)) > 18 & as.numeric(!!rlang::sym(fcs_score)) > 56 ~ 1, .default = 0,
                                                        TRUE ~ NA)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(sd_rcsicoping = stats::sd(c(!!rlang::sym(rcsi_lessquality), !!rlang::sym(rcsi_borrow), !!rlang::sym(rcsi_mealsize),
                                         !!rlang::sym(rcsi_mealadult), !!rlang::sym(rcsi_mealnb)), na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(flag_sd_rcsicoping = dplyr::case_when(sd_rcsicoping < 0.8 & !!rlang::sym(rcsi_score) < 4 ~ 1, .default = 0, TRUE ~ NA)) %>%
      dplyr::select(rcsi_flag_columns,rcsi_cat,flag_protein_rcsi,flag_fcs_rcsi,flag_high_rcsi,flag_rcsi_children,flag_fcsrcsi_box,flag_sd_rcsicoping)

    if(!exists("results")){
      results <- results2
    } else {
      results <- cbind(results,results2)
    }
  }

  ## flag issue in data with HHS
  hhs_flag_columns <- c(hhs_nofoodhh,hhs_nofoodhh_freq,hhs_sleephungry,
                        hhs_sleephungry_freq,hhs_alldaynight,hhs_alldaynight_freq,hhs_score,hhs_cat)
  if(!all(hhs_flag_columns %in% names(.dataset))) {
    stop("Missing hhs columns")
  } else {
    results2 <- .dataset %>%
      dplyr::mutate(flag_severe_hhs = ifelse(is.na(!!rlang::sym(hhs_score)), NA, ifelse(!!rlang::sym(hhs_score) >= 5, 1, 0))) %>%
      dplyr::select(hhs_flag_columns,flag_severe_hhs)

    if(!exists("results")){
      results <- results2
    } else {
      results <- cbind(results,results2)
    }
  }
  ## flag issues with LCSI
  lcs_flag_columns <- c(lcsi_stress1,lcsi_stress2,lcsi_stress3,lcsi_stress4,lcsi_crisis1,lcsi_crisis2,
                        lcsi_crisis3,lcsi_emergency1,lcsi_emergency2,lcsi_emergency3,lcsi_stress,
                        lcsi_crisis,lcsi_emergency,lcsi_cat_yes,lcsi_cat_exhaust,lcsi_cat)
  if(!all(lcs_flag_columns %in% names(.dataset))) {
    stop("Missing lcsi columns")
  } else {
    results2 <- .dataset %>%
      dplyr::mutate(flag_lcsi_coherence = ifelse(is.na(!!rlang::sym(lcsi_emergency)), NA,
                                                 ifelse(!!rlang::sym(lcsi_emergency) == 1 & !!rlang::sym(lcsi_stress) == 0 |
                                                          !!rlang::sym(lcsi_emergency) == 1 & !!rlang::sym(lcsi_crisis) == 0 |
                                                          !!rlang::sym(lcsi_crisis) == 1 & !!rlang::sym(lcsi_stress) == 0, 1, 0)),
                    flag_lcsi_severity = dplyr::case_when(!!rlang::sym(lcsi_emergency) == 1 ~ 1, .default = 0,
                                                          TRUE ~ NA))

    lcs_variables <- c("fsl_lcsi_stress1","fsl_lcsi_stress2","fsl_lcsi_stress3","fsl_lcsi_stress4","fsl_lcsi_crisis1",
                       "fsl_lcsi_crisis2","fsl_lcsi_crisis3","fsl_lcsi_emergency1","fsl_lcsi_emergency2","fsl_lcsi_emergency3")
    results2$lcsi.count.na <-  apply(results2[c(lcs_variables)], 1, function(x) sum(x == "not_applicable"))

    results2 <- results2 %>%
      dplyr::mutate(flag_lcsi_na = dplyr::case_when(lcsi.count.na == 10 ~ 1, .default = 0, TRUE ~ NA))

    income_types <- c("first_income_types","second_income_types","third_income_types")
    suppressWarnings(
      agric <- lcs_variables[which(grepl("agriculture|crop|crops|farm",get.label(lcs_variables, tool.survey = tool.survey)))]
    )

    suppressWarnings(
      livest <- lcs_variables[which(grepl("livestock|livestocks|animal",get.label(lcs_variables, tool.survey = tool.survey)))]

    )

    suppressWarnings(
      displ <- lcs_variables[which(grepl("displaced|migration|migrated",get.label(lcs_variables, tool.survey = tool.survey)))]
    )

    if(length(agric)>0){
      results2$flag_lcsi_liv_agriculture <- dplyr::case_when(rowSums(sapply(results2[agric], function(i) grepl("yes",i))) > 0 & any(results2[income_types] == "sell_agri_prod") > 0  ~ 1, .default = 0, TRUE ~ NA) ## Fix second part to take only select_one from three columns
    }

    if(length(livest)>0){
      results2$flag_lcsi_liv_livestock  <- dplyr::case_when(rowSums(sapply(results2[livest], function(i) grepl("yes",i))) > 0 & any(results2[income_types] == "sell_anim_prod") > 0 ~ 1, .default = 0, TRUE ~ NA) ## Fix second part to take only select_one from three columns
    }

    if(length(displ)>0){
      results2$flag_lcsi_displ  <- dplyr::case_when(rowSums(sapply(results2[displ], function(i) grepl("yes",i))) > 0 & results2["residency_status"] == "idp" ~ 1, .default = 0, TRUE ~ NA) ## Fix second part to take only select_one from three columns
    }

    if(length(livest)>0 & length(agric)>0 & length(displ)>0){
      results2 <- results2 %>%
        dplyr::select(lcs_flag_columns,flag_lcsi_coherence,flag_lcsi_severity,flag_lcsi_na,flag_lcsi_liv_agriculture,flag_lcsi_liv_livestock,flag_lcsi_displ)
    } else if (length(livest)>0 & length(agric)>0 & length(displ) == 0){
      results2 <- results2 %>%
        dplyr::select(lcs_flag_columns,flag_lcsi_coherence,flag_lcsi_severity,flag_lcsi_na,flag_lcsi_liv_livestock,flag_lcsi_liv_agriculture)
    } else if (length(agric)>0 & length(displ)>0 & length(livest) == 0){
      results2 <- results2 %>%
        dplyr::select(lcs_flag_columns,flag_lcsi_coherence,flag_lcsi_severity,flag_lcsi_na,flag_lcsi_liv_agriculture,flag_lcsi_displ)
    } else if (length(displ)>0 & length(livest)>0 & length(agric) == 0){
      results2 <- results2 %>%
        dplyr::select(lcs_flag_columns,flag_lcsi_coherence,flag_lcsi_severity,flag_lcsi_na,flag_lcsi_displ,flag_lcsi_liv_livestock)
    } else if (length(livest)>0 & length(agric) ==0 & length(displ) == 0){
      results2 <- results2 %>%
        dplyr::select(lcs_flag_columns,flag_lcsi_coherence,flag_lcsi_severity,flag_lcsi_na,flag_lcsi_liv_livestock)
    } else if (length(agric)>0 & length(livest) == 0 & length(displ) == 0){
      results2 <- results2 %>%
        dplyr::select(lcs_flag_columns,flag_lcsi_coherence,flag_lcsi_severity,flag_lcsi_na,flag_lcsi_liv_agriculture)
    } else if (length(displ)>0 & length(livest) == 0 & length(agric) == 0){
      results2 <- results2 %>%
        dplyr::select(lcs_flag_columns,flag_lcsi_coherence,flag_lcsi_severity,flag_lcsi_na,flag_lcsi_displ)
    }  else {
      results2 <- results2 %>%
        dplyr::select(lcs_flag_columns,flag_lcsi_coherence,flag_lcsi_severity,flag_lcsi_na)
    }

    if(!exists("results")){
      results <- results2
    } else {
      results <- cbind(results,results2)
    }
  }
  fc_phase_col <- c(fc_cell,fc_phase)
  if(!all(fc_phase_col %in% names(.dataset))) {
    stop("Missing fc_cell and fc_phase columns")
  } else {
    ## flag phase
    results2 <- .dataset %>%
      dplyr::mutate(flag_fc_cell = ifelse(is.na(fc_cell), NA,
                                          ifelse(fc_cell %in% c(3,4,5,8,9,10), 1, 0))) %>%
      dplyr::select(fc_phase_col, flag_fc_cell)
    if(!exists("results")){
      results <- results2
    } else {
      results <- cbind(results,results2)
    }
  }
  ## flag hhds
  hdds_flag_columns <- c(hdds_cereals,hdds_tubers,hdds_legumes,hdds_veg,hdds_fruit,
                         hdds_meat,hdds_fish,hdds_dairy,hdds_eggs,hdds_sugar,
                         hdds_oil,hdds_condiments,hdds_cat,hdds_score)
  if(!all(hdds_flag_columns %in% names(.dataset))) {
    stop("Missing fc_cell and fc_phase columns")
  } else{
    results2 <- .dataset %>%
      dplyr::mutate(flag_low_sugar_cond_hdds = ifelse(is.na(!!rlang::sym(hdds_score)), NA,
                                                      ifelse((!!rlang::sym(hdds_score) <= 2 & !!rlang::sym(hdds_sugar) == "yes" & !!rlang::sym(hdds_condiments) == "yes") |
                                                               (!!rlang::sym(hdds_score) <= 1 & !!rlang::sym(hdds_sugar) == "yes") |
                                                               (!!rlang::sym(hdds_score) <= 1 & !!rlang::sym(hdds_condiments) == "yes"), 1, 0))) %>%
      dplyr::select(hdds_flag_columns,flag_low_sugar_cond_hdds)
    if(!exists("results")){
      results <- results2
    } else {
      results <- cbind(results,results2)
    }
  }
  options(warn = 0)
  return(results)
}
