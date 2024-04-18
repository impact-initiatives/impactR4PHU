reformat_mortality <- function(df) {
 if("sex" %in% names(df)){
   sex_codes <- unique(df$sex)
   ideal_codes <- c("1", "2")
   sex_recodes <- c("1", "2", "NA")

   if(length(setdiff(sex_codes, ideal_codes))==0) {
     print("Good - Sex coded as 1/2 for male/female")
   } else {
     for(i in 1:length(sex_codes)) {
       a <- svDialogs::dlg_input(message= paste0("\n RE-FORMATTING VARIABLE : SEX \n How is '",sex_codes[[i]], "' coded? Please input either\n '1' for male, \n '2' for 'female' or \n 'NA' for missing. " ))$res
       while(length(setdiff(a, sex_recodes))==1) {
         a <- svDialogs::dlg_input(message= paste0("\n RE-FORMATTING VARIABLE : SEX \n Invalid input. ", "How is '", sex_codes[[i]], "' coded? Please input either\n '1' for male, \n '2' for 'female' or \n 'NA' for missing. "))$res
       }
       if(!is.na(sex_codes[[i]])){
         if(a == "NA") {df <- df %>% dplyr::mutate(sex = ifelse(.data$sex == sex_codes[[i]], NA, .data$sex))
         } else {df <- df %>% dplyr::mutate(sex = ifelse(.data$sex == sex_codes[[i]], a, .data$sex))}
       }
     }
   }
 }

  if(c("date_dc") %in% names(df)) {

    date_recodes <- c("mdy", "dmy", "ymd", "ydm")
    unique_dates <- df %>% dplyr::filter(!is.na(.data$date_dc)) %>% dplyr::select(.data$date_dc) %>% t %>% c %>% unique
    a <- svDialogs::dlg_input(message= paste0("Example of Date of Data collection values: ", if(length(unique_dates)>0) {unique_dates[[1]]}, " ", if(length(unique_dates)>1) {unique_dates[[2]]}, " ",if(length(unique_dates)>2) {unique_dates[[3]]},
                                              "\n RE-FORMATTING VARIABLE : DATE OF DATA COLLECTION \n What is the date format for the DATE OF DATA COLLECTION column? Please input : \n 'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month."))$res
    while(length(setdiff(a, date_recodes))==1) {
      a <- svDialogs::dlg_input(message= paste0("Invalid input. \n ", "\n RE-FORMATTING VARIABLE : DATE OF DATA COLLECTION \n How is DATE OF DATA COLLECTION formatted? Please input : \n  'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month."))$res
    }


    if(is.character(df$date_dc)) {df <- df %>% dplyr::mutate(date_dc = ifelse(.data$date_dc == "", NA, .data$date_dc))}

    df <- df %>%
      dplyr::mutate(date_dc_date = lubridate::parse_date_time(.data$date_dc, orders = a)) %>%
      dplyr::mutate(date_dc_month = lubridate::month(.data$date_dc_date),
                    date_dc_day = lubridate::day(.data$date_dc_date),
                    date_dc_year = lubridate::year(.data$date_dc_date)) %>%
      dplyr::mutate(date_dc_char = paste(.data$date_dc_month, .data$date_dc_day, .data$date_dc_year, sep = "/"),
                    date_dc_char = ifelse(is.na(.data$date_dc_char), NA, ifelse(.data$date_dc_char == "NA/NA/NA", NA, .data$date_dc_char)))

  }

  ### TO CONTINUE HERE
}


create_mortality_long_df <- function(df_main,date_dc = "today",date_recall_event = "recall_date",
                                     enumerator = NULL,cluster = NULL,admin1 = NULL,admin2 = NULL,
                                     uuid_main = NULL,df_roster,sex_roster = "sex_roster",age_roster = "calc_final_age_years",
                                     joined_roster = "joined",birth_roster = "ind_born",birthdate_roster = "final_ind_dob",
                                     joined_date_roster = "final_date_join", uuid_roster = NULL, df_left,sex_left = "sex_left",
                                     age_left = "calc_final_age_years_left",birth_left = "ind_born_left",
                                     joined_left = "left_present",joined_date_left = "final_date_join_left",
                                     left_date_left = "date_left_exact",birthdate_left = "final_ind_dob_left",
                                     uuid_left = NULL, df_died,sex_died = "sex_died",age_died = "calc_final_age_years_died",
                                     birth_died = "ind_born_died",joined_died = "died_present",death_cause = "cause_death",
                                     death_location = "location_death",date_death = "final_date_death",
                                     joined_date_died = "date_join_final_death",birthdate_died = "dob_died",uuid_died = NULL) {

  options(warn = -1)

  if(is.null(date_recall_event)) {
    stop("A date for recall event is required. Please input a character date with a format like dd/mm/yyyy. E.g 28/12/2020. Please check your input.")
  }
  ## Throw an error if a dataset wasn't provided as a first argument
  if (!is.data.frame(df_main)) {
    stop("Main data should be a dataframe")
  }

  ## Throw an error if the dataset is empty
  if (nrow(df_main) == 0) {
    stop("Main data is empty")
  }

  ## Throw an error if a dataset wasn't provided as a first argument
  if (!is.data.frame(df_roster)) {
    stop("Roster Data should be a dataframe")
  }

  ## Throw an error if the dataset is empty
  if (nrow(df_roster) == 0) {
    stop("Roster Data is empty")
  }

  ## Throw an error if a dataset wasn't provided as a first argument
  if (!is.data.frame(df_left)) {
    stop("Left Data should be a dataframe")
  }

  ## Throw an error if the dataset is empty
  if (nrow(df_left) == 0) {
    warning("Left Data is empty")
  }

  ## Throw an error if a dataset wasn't provided as a first argument
  if (!is.data.frame(df_died)) {
    stop("Died Data should be a dataframe")
  }

  ## Throw an error if the dataset is empty
  if (nrow(df_died) == 0) {
    stop("Died Data is empty")
  }

  if (!uuid_main %in% names(df_main)) stop("uuid argument incorrect, or not available in the main dataset")
  if (!uuid_roster %in% names(df_roster)) stop("uuid argument incorrect, or not available in the roster")
  if (!uuid_left %in% names(df_left)) stop("uuid argument incorrect, or not available in the left data")
  if (!uuid_died %in% names(df_died)) stop("uuid argument incorrect, or not available in the died data")

  #prepare df_main columns to left_join with all other loop dataframes
  if(!cluster %in% names(df_main)) {
    warning("Cluster was not find in the main data. Creating a cluster column with NA.")
    df_main <- df_main %>%
      dplyr::mutate(cluster = NA)
  }

  if(!admin1 %in% names(df_main)) {
    warning("Admin1 was not find in the main data. Creating a admin1 column with NA.")
    df_main <- df_main %>%
      dplyr::mutate(admin1 = NA)
  }

  if(!admin2 %in% names(df_main)) {
    warning("Admin2 was not find in the main data. Creating a admin2 column with NA.")
    df_main <- df_main %>%
      dplyr::mutate(admin2 = NA)
  }

  if(!all(c(date_dc,date_recall_event,enumerator) %in% names(df_main))) {
    stop("Check date_dc, date_recall_event, or enumerator arguments. Couldn't find in main data.")
  } else {
    main_to_join <- df_main %>%
      dplyr::rename(date_dc = !!rlang::sym(date_dc),
                    date_recall_event = !!rlang::sym(date_recall_event),
                    enumerator = !!rlang::sym(enumerator),
                    cluster = !!rlang::sym(cluster),
                    admin1 = !!rlang::sym(admin1),
                    admin2 = !!rlang::sym(admin2),
                    uuid = uuid_main) %>%
      dplyr::select(uuid, date_dc, date_recall_event, enumerator, cluster, admin1, admin2)
  }


  df_roster <- df_roster %>%
    dplyr::rename(sex = sex_roster,
                  age_years = age_roster,
                  join = joined_roster,
                  birth = birth_roster,
                  date_join = joined_date_roster,
                  date_birth = birthdate_roster,
                  uuid = uuid_roster) %>%
    dplyr::left_join(main_to_join) %>%
    dplyr::mutate(date_recall = date_recall_event)


  df_left <- df_left %>%
    dplyr::rename(sex = sex_left,
                  age_years = age_left,
                  join = joined_left,
                  birth = birth_left,
                  date_join = joined_date_left,
                  date_left = left_date_left,
                  date_birth = birthdate_left,
                  uuid = uuid_left) %>%
    dplyr::left_join(main_to_join) %>%
    dplyr::mutate(date_recall = date_recall_event)


  df_died <- df_died %>%
    dplyr::rename(sex = sex_died,
                  age_years = age_died,
                  join = joined_died,
                  birth = birth_died,
                  death_cause = death_cause,
                  death_location = death_location,
                  date_death = date_death,
                  date_join = joined_date_died,
                  date_birth = birthdate_died,
                  uuid = uuid_died) %>%
    dplyr::left_join(main_to_join) %>%
    dplyr::mutate(date_recall = date_recall_event)

  # if dates included in any, make sure all dfs have columns for dates of death, birth, left, join

  date_vars <- c("date_death", "date_birth", "date_join", "date_left")

  if(length(intersect(date_vars, colnames(df_roster))) > 0 | length(intersect(date_vars, colnames(df_left))) > 0 | length(intersect(date_vars, colnames(df_died))) > 0) {

    if("date_death" %in% names(df_roster)) {df_roster <- df_roster %>% dplyr::mutate(date_death = NA)}
    if("date_join" %in% names(df_roster)) {df_roster <- df_roster %>% dplyr::mutate(date_join = NA)}
    if("date_left" %in% names(df_roster)) {df_roster <- df_roster %>% dplyr::mutate(date_left = NA)}
    if("date_birth" %in% names(df_roster)) {df_roster <- df_roster %>% dplyr::mutate(date_birth = NA)}

    if("date_death" %in% names(df_left)) {df_left <- df_left %>% dplyr::mutate(date_death = NA)}
    if("date_join" %in% names(df_left)) {df_left <- df_left %>% dplyr::mutate(date_join = NA)}
    if("date_left" %in% names(df_left)) {df_left <- df_left %>% dplyr::mutate(date_left = NA)}
    if("date_birth" %in% names(df_left)) {df_left <- df_left %>% dplyr::mutate(date_birth = NA)}

    if("date_death" %in% names(df_died)) {df_died <- df_died %>% dplyr::mutate(date_death = NA)}
    if("date_join" %in% names(df_died)) {df_died <- df_died %>% dplyr::mutate(date_join = NA)}
    if("date_left" %in% names(df_died)) {df_died <- df_died %>% dplyr::mutate(date_left = NA)}
    if("date_birth" %in% names(df_died)) {df_died <- df_died %>% dplyr::mutate(date_birth = NA)}

    df_roster[date_vars] <- lapply(df_roster[date_vars], as.character)
    df_left[date_vars] <- lapply(df_left[date_vars], as.character)
    df_died[date_vars] <- lapply(df_died[date_vars], as.character)

  }

  req_roster <- c("date_dc", "enumerator", "cluster", "sex", "age_years", "birth")
  req_left <- c("sex", "age_years", "birth")
  req_died <- c("sex", "age_years", "birth", "death_cause", "death_location")
  additional_cols <- c("join", "left", "death", "death_cause", "death_location")

  if(all(req_roster %in% names(df_roster))) {print("Sex, Age and Births available for current roster.")} else {stop("Missing minimum information (SEX, AGE, Births) for current household roster. Please check input.")}
  if(all(req_left %in% names(df_left))) {print("Sex, Age and Births available for Left people.")} else {stop("Missing minimum information (SEX, AGE, Births) for left people roster. Please check input.")}
  if(all(req_died %in% names(df_died))) {print("Sex, Age, Births, Cause and Location of Death available for Deceased people.")} else {stop("Missing minimum information (SEX, AGE, Births, Cause of Death, Location of Death) for death roster. Please check input.")}

  # Adding missing columns to current roster data
  if(all(additional_cols %in% names(df_roster))) {

    cols_to_add <- setdiff(additional_cols, colnames(df_roster))

    if("join" %in% cols_to_add) {df_roster <- df_roster %>% dplyr::mutate(join = NA)}
    if("left" %in% cols_to_add) {df_roster <- df_roster %>% dplyr::mutate(left = NA)}
    if("death" %in% cols_to_add) {df_roster <- df_roster %>% dplyr::mutate(death = NA)}
    if("death_cause" %in% cols_to_add) {df_roster <- df_roster %>% dplyr::mutate(death_cause = NA)}
    if("death_location" %in% cols_to_add) {df_roster <- df_roster %>% dplyr::mutate(death_location = NA)}

  }


  # Adding missing columns to left roster data
  if(all(additional_cols %in% names(df_left)))  {

    cols_to_add <- setdiff(additional_cols, colnames(df_left))

    if("join" %in% cols_to_add) {df_left <- df_left %>% dplyr::mutate(join = NA)}
    if("left" %in% cols_to_add) {df_left <- df_left %>% dplyr::mutate(left = "1")}
    if("death" %in% cols_to_add) {df_left <- df_left %>% dplyr::mutate(death = NA)}
    if("death_cause" %in% cols_to_add) {df_left <- df_left %>% dplyr::mutate(death_cause = NA)}
    if("death_location" %in% cols_to_add) {df_left <- df_left %>% dplyr::mutate(death_location = NA)}

  }


  # Adding missing columns to died roster data
  if(all(additional_cols %in% names(df_died)))  {

    cols_to_add <- setdiff(additional_cols, colnames(df_died))

    if("join" %in% cols_to_add) {df_died <- df_died %>% dplyr::mutate(join = NA)}
    if("left" %in% cols_to_add) {df_died <- df_died %>% dplyr::mutate(left = NA)}
    if("death" %in% cols_to_add) {df_died <- df_died %>% dplyr::mutate(death = "1")}

  }

  # adjusting col order if admin are included or not

  if(all(date_vars %in% names(df_roster)) |
     all(date_vars %in% names(df_left)) |
     all(date_vars %in% names(df_died))) {

    if(is.null(admin1_roster)) {

      if(is.null(admin2_roster)) {

        col_order <- c("date_dc", "date_recall", "enumerator", "cluster", "uuid", "sex", "age_years", "join", "date_join", "left", "date_left", "birth", "date_birth", "death", "date_death", "death_cause", "death_location")
      } else {
        col_order <- c("date_dc", "date_recall", "enumerator", "admin2", "cluster", "uuid", "sex", "age_years", "join", "date_join", "left", "date_left", "birth", "date_birth", "death", "date_death", "death_cause", "death_location")

      }

    } else {

      if(is.null(admin2_roster)) {
        col_order <- c("date_dc", "date_recall", "enumerator", "admin1", "cluster", "uuid", "sex", "age_years", "join", "date_join", "left", "date_left", "birth", "date_birth", "death", "date_death", "death_cause", "death_location")

      } else {
        col_order <- c("date_dc", "date_recall", "enumerator", "admin1", "admin2", "cluster", "uuid", "sex", "age_years", "join", "date_join", "left", "date_left", "birth", "date_birth", "death", "date_death", "death_cause", "death_location")

      }

    }

  } else {

    if(is.null(admin1_roster)) {

      if(is.null(admin2_roster)) {

        col_order <- c("date_dc", "date_recall", "enumerator", "cluster", "uuid", "sex", "age_years", "join", "left", "birth", "death", "death_cause", "death_location")
      } else {
        col_order <- c("date_dc", "date_recall", "enumerator", "admin2", "cluster", "uuid", "sex", "age_years", "join", "left", "birth", "death", "death_cause", "death_location")

      }

    } else {

      if(is.null(admin2_roster)) {
        col_order <- c("date_dc", "date_recall", "enumerator", "admin1", "cluster", "uuid", "sex", "age_years", "join", "left", "birth", "death", "death_cause", "death_location")

      } else {
        col_order <- c("date_dc", "date_recall", "enumerator", "admin1", "admin2", "cluster", "uuid", "sex", "age_years", "join", "left", "birth", "death", "death_cause", "death_location")

      }

    }
  }

  df_roster <- df_roster %>% dplyr::select(col_order) %>% dplyr::mutate(age_years = as.character(age_years))
  df_left <- df_left %>% dplyr::select(col_order) %>% dplyr::mutate(age_years = as.character(age_years))
  df_died <- df_died %>% dplyr::select(col_order) %>% dplyr::mutate(age_years = as.character(age_years))

  df_roster <- lapply(df_roster, as.character)
  df_left <- lapply(df_left, as.character)
  df_died <- lapply(df_died, as.character)

  df_mortality <- dplyr::bind_rows(df_roster, df_left)
  df_mortality <- dplyr::bind_rows(df_mortality, df_died)

  # df_mortality <- healthyr::reformat_mortality_current_census(df_mortality)

  # calculating person time

  if(length(setdiff(c(date_vars), colnames(df_mortality))) != 4) {

    # print("I AM INSIDE THE DATE MORTALITY PART")

    df_mortality <- df_mortality %>%
      dplyr::mutate(
        age_years = as.numeric(age_years),
        # default person time calculations
        person_time = as.numeric(date_dc_date - date_recall_date),

        person_time = ifelse(is.na(date_join_date), person_time,
                             ifelse(!is.na(date_death_date) & !is.na(death) & !is.na(join), as.numeric(date_death_date - date_join_date),
                                    ifelse(!is.na(date_left_date) & !is.na(left) & !is.na(join), as.numeric(date_left_date - date_join_date),
                                           ifelse(!is.na(join), as.numeric(date_dc_date - date_join_date), person_time)))),

        # leaver person time calculations - join_left situaiton taken care above, so it defaults to person_time here
        person_time = ifelse(is.na(date_left_date), person_time,
                             ifelse(!is.na(date_join_date) & !is.na(join), person_time,
                                    ifelse(!is.na(left), as.numeric(date_left_date - date_recall_date), person_time))),

        # # birth person time calculations
        person_time = ifelse(is.na(date_birth_date), person_time,
                             ifelse( date_birth_date < date_recall_date, person_time,
                                     ifelse(!is.na(date_death_date)  & !is.na(death) & !is.na(birth), as.numeric(date_death_date - date_birth_date),
                                            ifelse(!is.na(date_left_date) & !is.na(left) & !is.na(birth), as.numeric(date_left_date - date_birth_date),
                                                   ifelse(!is.na(birth), as.numeric(date_dc_date - date_birth_date), person_time))))),
        #
        # # death person time calculations
        person_time = ifelse(is.na(date_death_date), person_time,
                             ifelse(!is.na(date_join_date) & !is.na(join), person_time,
                                    ifelse(!is.na(date_birth_date) & !is.na(birth), person_time,
                                           ifelse(!is.na(death), as.numeric(date_death_date - date_recall_date), person_time))))  ,

      )

    df_mortality <- df_mortality %>%
      dplyr::mutate(
        under_5 = ifelse(is.na(age_years), NA, ifelse(as.numeric(age_years) < 5, 1, NA)),
        under_5_pt = ifelse(is.na(under_5), NA, ifelse(under_5 == 1, person_time, NA)))

  } else {

    # print("I AM OUTSIDE THE DATE MORTALITY PART")

    df_mortality <- df_mortality %>%
      dplyr::mutate(
        age_years = as.numeric(age_years),
        person_time = date_dc_date - date_recall_date,
        person_time = as.numeric(person_time),
        person_time = ifelse(!is.na(join) | !is.na(left) | !is.na(birth) | !is.na(death), person_time*0.5, person_time),
        under_5 = ifelse(is.na(age_years), NA, ifelse(as.numeric(age_years) < 5, 1, NA)),
        under_5_pt = ifelse(is.na(under_5), NA, ifelse(under_5 == 1, person_time, NA)))

  }


  df_mortality <- df_mortality %>%
    dplyr::mutate(
      join_under5 = ifelse(is.na(under_5), NA, join),
      left_under5 = ifelse(is.na(under_5), NA, left),
      birth_under5 = ifelse(is.na(under_5), NA, birth),
      death_under5 = ifelse(is.na(under_5), NA, death),
      age_0to2 = ifelse(is.na(age_years), NA, ifelse(age_years >= 0 & age_years < 2, 1, NA)),
      age_2to5 = ifelse(is.na(age_years), NA, ifelse(age_years >= 2 & age_years < 5, 1, NA)),
      age_5to10 = ifelse(is.na(age_years), NA, ifelse(age_years >= 5 & age_years < 10, 1, NA)),
      age_0to5 = ifelse(is.na(age_years), NA, ifelse(age_years >= 0 & age_years < 5, 1, NA)),
      age_5plus = ifelse(is.na(age_years), NA, ifelse(age_years >= 5 & age_years < 200, 1, NA)),
    )

  df_mortality$age_group <- cut(as.numeric(df_mortality$age_years),
                                breaks = c(-1,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84, Inf),
                                labels = c("0-4", "5-9", "10-14", "15-19",
                                           "20-24", "25-29", "30-34", "35-39","40-44", "45-49", "50-54", "55-59",
                                           "60-64", "65-69", "70-74", "75-79", "80-84", "85+"))

  df_mortality <- healthyr::flag_mortality_issues(df = df_mortality)

  # create unique id

  df_mortality <- df_mortality %>%
    dplyr::group_by(hh_id) %>%
    dplyr::mutate(individual_id = dplyr::row_number()) %>%
    ungroup() %>%
    dplyr::mutate(id = paste0(hh_id, "_", individual_id), individual_id = NULL) %>%
    dplyr::select(id, dplyr::everything())

  # df_mortality <- df_mortality %>% dplyr::mutate()

  # Saving the new dataframe to a xlsx, if specified
  if(!is.null(file_path)) {writexl::write_xlsx(df_mortality, file_path)}

  return(df_mortality)

}
