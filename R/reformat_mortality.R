#' Reformat Mortality Data
#'
#' @param df long dataframe containing (roster/left/died dfs)
#'
#' @return dataframe with recoded values for:
#' -sex
#' -date_dc
#' -date_recall
#' -date_join
#' -date_birth
#' -date_left
#' -date_death
#' -join
#' -left
#' -birth
#' -death
#' -cause_death
#' -location_death
#' @export
#'
#' @examples
#' \dontrun{
#'   reformat_mortality(df_mortality)
#' }

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
          if(a == "NA") {df <- df %>% dplyr::mutate(sex = ifelse(sex == sex_codes[[i]], NA, sex))
          } else {df <- df %>% dplyr::mutate(sex = ifelse(sex == sex_codes[[i]], a, sex))}
        }
      }
    }
  }

  if(c("date_dc") %in% names(df)) {

    date_recodes <- c("mdy", "dmy", "ymd", "ydm", "NA")
    unique_dates <- df %>% dplyr::filter(!is.na(date_dc)) %>% dplyr::select(date_dc) %>% t %>% c %>% unique
    a <- svDialogs::dlg_input(message= paste0("Example of Date of Data collection values: ", if(length(unique_dates)>0) {unique_dates[[1]]}, " ", if(length(unique_dates)>1) {unique_dates[[2]]}, " ",if(length(unique_dates)>2) {unique_dates[[3]]},
                                              "\n RE-FORMATTING VARIABLE : DATE OF DATA COLLECTION \n What is the date format for the DATE OF DATA COLLECTION column? Please input : \n 'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month."))$res
    while(length(setdiff(a, date_recodes))==1) {
      a <- svDialogs::dlg_input(message= paste0("Example of Date of Data collection values: ", if(length(unique_dates)>0) {unique_dates[[1]]}, " ", if(length(unique_dates)>1) {unique_dates[[2]]}, " ",if(length(unique_dates)>2) {unique_dates[[3]]},
                                                "Invalid input. \n ", "\n RE-FORMATTING VARIABLE : DATE OF DATA COLLECTION \n How is DATE OF DATA COLLECTION formatted? Please input : \n  'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month."))$res
    }


    if(is.character(df$date_dc)) {df <- df %>% dplyr::mutate(date_dc = ifelse(date_dc == "", NA, date_dc))}

    df <- df %>%
      dplyr::mutate(date_dc_date = lubridate::parse_date_time(date_dc, orders = a)) %>%
      dplyr::mutate(date_dc_month = lubridate::month(date_dc_date),
                    date_dc_day = lubridate::day(date_dc_date),
                    date_dc_year = lubridate::year(date_dc_date)) %>%
      dplyr::mutate(date_dc_char = paste(date_dc_month, date_dc_day, date_dc_year, sep = "/"),
                    date_dc_char = ifelse(is.na(date_dc_char), NA, ifelse(date_dc_char == "NA/NA/NA", NA, date_dc_char)))

  }

  # Checking Recall Event Date

  if(c("date_recall") %in% names(df)) {

    dob_recodes <- c("mdy", "dmy", "ymd", "ydm", "NA")
    unique_dates <- df %>% dplyr::filter(!is.na(date_recall)) %>% dplyr::select(date_recall) %>% t %>% c %>% unique



    a <- svDialogs::dlg_input(message= paste0("Example of RECALL DATE value: ", if(length(unique_dates)>0) {unique_dates[[1]]}, " ", if(length(unique_dates)>1) {unique_dates[[2]]}, " ",if(length(unique_dates)>2) {unique_dates[[3]]},
                                              "\n RE-FORMATTING VARIABLE : RECALL DATE \n What is the date format for the RECALL DATE formatted? Please input : \n 'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month."))$res
    while(length(setdiff(a, dob_recodes))==1) {
      a <- svDialogs::dlg_input(message= paste0("Example of RECALL DATE value: ", if(length(unique_dates)>0) {unique_dates[[1]]}, " ", if(length(unique_dates)>1) {unique_dates[[2]]}, " ",if(length(unique_dates)>2) {unique_dates[[3]]},
                                                "Invalid input. \n", "\n RE-FORMATTING VARIABLE : RECALL DATE \n How is RECALL DATE formatted? Please input : \n  'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month."))$res
    }

    if(is.character(df$date_recall)) {df <- df %>% dplyr::mutate(date_recall = ifelse(date_recall == "", NA, date_recall))}

    df <- df %>%
      dplyr::mutate(date_recall_date = lubridate::parse_date_time(date_recall, orders = a)) %>%
      dplyr::mutate(date_recall_month = lubridate::month(date_recall_date),
                    date_recall_day = lubridate::day(date_recall_date),
                    date_recall_year = lubridate::year(date_recall_date)) %>%
      dplyr::mutate(date_recall = paste(date_recall_month, date_recall_day, date_recall_year, sep = "/"),
                    date_recall = ifelse(is.na(date_recall), NA, ifelse(date_recall == "NA/NA/NA", NA, date_recall)))

  }

  # Checking Date Join

  if(c("date_join") %in% names(df)) {

    dob_recodes <- c("mdy", "dmy", "ymd", "ydm", "NA")
    unique_dates <- df %>% dplyr::filter(!is.na(date_join)) %>% dplyr::select(date_join) %>% t %>% c %>% unique

    a <- svDialogs::dlg_input(message= paste0("Example of JOIN DATE value: ", if(length(unique_dates)>0) {unique_dates[[1]]}, " ", if(length(unique_dates)>1) {unique_dates[[2]]}, " ",if(length(unique_dates)>2) {unique_dates[[3]]},
                                              "\n RE-FORMATTING VARIABLE : JOIN DATE \n What is the date format for the JOIN DATE formatted? Please input : \n 'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month." ))$res
    while(length(setdiff(a, dob_recodes))==1) {
      a <- svDialogs::dlg_input(message= paste0("Example of JOIN DATE value: ", if(length(unique_dates)>0) {unique_dates[[1]]}, " ", if(length(unique_dates)>1) {unique_dates[[2]]}, " ",if(length(unique_dates)>2) {unique_dates[[3]]},
                                                "Invalid input. ", "\n RE-FORMATTING VARIABLE : JOIN DATE \n How is JOIN DATE formatted? Please input : \n  'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month."))$res
    }

    if(is.character(df$date_join)) {df <- df %>% dplyr::mutate(date_join = ifelse(date_join == "", NA, date_join))}


    df <- df %>%
      dplyr::mutate(date_join_date = lubridate::parse_date_time(date_join, orders = a)) %>%
      dplyr::mutate(date_join_month = lubridate::month(date_join_date),
                    date_join_day = lubridate::day(date_join_date),
                    date_join_year = lubridate::year(date_join_date)) %>%
      dplyr::mutate(date_join = paste(date_join_month, date_join_day, date_join_year, sep = "/"),
                    date_join = ifelse(is.na(date_join), NA, ifelse(date_join == "NA/NA/NA", NA, date_join)))

  }

  # Checking Date Left

  if(c("date_left") %in% names(df)) {

    dob_recodes <- c("mdy", "dmy", "ymd", "ydm", "NA")
    unique_dates <- df %>% dplyr::filter(!is.na(date_left)) %>% dplyr::select(date_left) %>% t %>% c %>% unique

    a <- svDialogs::dlg_input(message= paste0("Example of LEFT DATE value: ", if(length(unique_dates)>0) {unique_dates[[1]]}, " ", if(length(unique_dates)>1) {unique_dates[[2]]}, " ",if(length(unique_dates)>2) {unique_dates[[3]]},
                                              "\n RE-FORMATTING VARIABLE : LEFT DATE \n What is the date format for the LEFT DATE formatted? Please input : \n 'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month."))$res
    while(length(setdiff(a, dob_recodes))==1) {
      a <- svDialogs::dlg_input(message= paste0("Example of LEFT DATE value: ", if(length(unique_dates)>0) {unique_dates[[1]]}, " ", if(length(unique_dates)>1) {unique_dates[[2]]}, " ",if(length(unique_dates)>2) {unique_dates[[3]]},
                                                "Invalid input. ", "\n RE-FORMATTING VARIABLE : LEFT DATE \n How is LEFT DATE formatted? Please input : \n  'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month."))$res
    }

    if(is.character(df$date_left)) {df <- df %>% dplyr::mutate(date_left = ifelse(date_left == "", NA, date_left))}


    df <- df %>%
      dplyr::mutate(date_left_date = lubridate::parse_date_time(date_left, orders = a)) %>%
      dplyr::mutate(date_left_month = lubridate::month(date_left_date),
                    date_left_day = lubridate::day(date_left_date),
                    date_left_year = lubridate::year(date_left_date)) %>%
      dplyr::mutate(date_left = paste(date_left_month, date_left_day, date_left_year, sep = "/"),
                    date_left = ifelse(is.na(date_left), NA, ifelse(date_left == "NA/NA/NA", NA, date_left)))
  }

  # Checking Date Birth

  if(c("date_birth") %in% names(df)) {

    dob_recodes <- c("mdy", "dmy", "ymd", "ydm", "NA")
    unique_dates <- df %>% dplyr::filter(!is.na(date_birth)) %>% dplyr::select(date_birth) %>% t %>% c %>% unique

    a <- svDialogs::dlg_input(message= paste0("Example of BIRTH DATE value: ", if(length(unique_dates)>0) {unique_dates[[1]]}, " ", if(length(unique_dates)>1) {unique_dates[[2]]}, " ",if(length(unique_dates)>2) {unique_dates[[3]]},
                                              "\n RE-FORMATTING VARIABLE : BIRTH DATE \n What is the date format for the BIRTH DATE formatted? Please input : \n 'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month."))$res
    while(length(setdiff(a, dob_recodes))==1) {
      a <- svDialogs::dlg_input(message= paste0("Example of BIRTH DATE value: ", if(length(unique_dates)>0) {unique_dates[[1]]}, " ", if(length(unique_dates)>1) {unique_dates[[2]]}, " ",if(length(unique_dates)>2) {unique_dates[[3]]},
                                                "Invalid input. ", "\n RE-FORMATTING VARIABLE : BIRTH DATE \n How is BIRTH DATE formatted? Please input : \n  'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month."))$res
    }

    if(is.character(df$date_birth)) {df <- df %>% dplyr::mutate(date_birth = ifelse(date_birth == "", NA, date_birth))}


    df <- df %>%
      dplyr::mutate(date_birth_date = lubridate::parse_date_time(date_birth, orders = a)) %>%
      dplyr::mutate(date_birth_month = lubridate::month(date_birth_date),
                    date_birth_day = lubridate::day(date_birth_date),
                    date_birth_year = lubridate::year(date_birth_date)) %>%
      dplyr::mutate(date_birth = paste(date_birth_month, date_birth_day, date_birth_year, sep = "/"),
                    date_birth = ifelse(is.na(date_birth), NA, ifelse(date_birth == "NA/NA/NA", NA, date_birth)))

  }

  # Checking Date Death

  if(c("date_death") %in% names(df)) {

    dob_recodes <- c("mdy", "dmy", "ymd", "ydm", "NA")
    unique_dates <- df %>% dplyr::filter(!is.na(date_death)) %>% dplyr::select(date_death) %>% t %>% c %>% unique

    a <- svDialogs::dlg_input(message= paste0("Example of DEATH DATE value: ", if(length(unique_dates)>0) {unique_dates[[1]]}, " ", if(length(unique_dates)>1) {unique_dates[[2]]}, " ",if(length(unique_dates)>2) {unique_dates[[3]]},
                                              "\n RE-FORMATTING VARIABLE : DEATH DATE \n What is the date format for the DEATH DATE formatted? Please input : \n 'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month."))$res
    while(length(setdiff(a, dob_recodes))==1) {
      a <- svDialogs::dlg_input(message= paste0("Example of DEATH DATE value: ", if(length(unique_dates)>0) {unique_dates[[1]]}, " ", if(length(unique_dates)>1) {unique_dates[[2]]}, " ",if(length(unique_dates)>2) {unique_dates[[3]]},
                                                "Invalid input. ", "\n RE-FORMATTING VARIABLE : DEATH DATE \n How is DEATH DATE formatted? Please input : \n  'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month."))$res
    }

    if(is.character(df$date_death)) {df <- df %>% dplyr::mutate(date_death = ifelse(date_death == "", NA, date_death))}


    df <- df %>%
      dplyr::mutate(date_death_date = lubridate::parse_date_time(date_death, orders = a)) %>%
      dplyr::mutate(date_death_month = lubridate::month(date_death_date),
                    date_death_day = lubridate::day(date_death_date),
                    date_death_year = lubridate::year(date_death_date)) %>%
      dplyr::mutate(date_death = paste(date_death_month, date_death_day, date_death_year, sep = "/"),
                    date_death = ifelse(is.na(date_death), NA, ifelse(date_death == "NA/NA/NA", NA, date_death)))

  }

  # Checking Joined, Left, Birth, Dead

  demographic_vars <- c("join", "left", "birth", "death")

  list_to_check <- intersect(demographic_vars, colnames(df))
  df[list_to_check] <- lapply(df[list_to_check], as.character)
  list_to_check_codes <- df %>% dplyr::select(list_to_check) %>% t %>% c %>% unique

  ideal_codes <- c("1")
  demo_recodes <- c("1", "NA")

  if(length(setdiff(list_to_check_codes, ideal_codes))==0) {
    print("Good - The demographic variables are already coded as 1/0 for yes/no")
  } else {
    for(i in 1:length(list_to_check_codes)) {
      a <- svDialogs::dlg_input(message= paste0("\n Please define how each value is coded for the demographic variable (Yes/No) columns in the data.",
                                                "\n RE-FORMATTING VARIABLES : BIRTHS, JOINS, LEFTS, DEATHS \n How is '",list_to_check_codes[[i]], "' coded? Please input either \n '1' for yes, or \n 'NA' for no, missing or any other values."))$res
      while(length(setdiff(a, demo_recodes))==1) {
        a <- svDialogs::dlg_input(message= paste0("\n Please define how each value is coded for the demographic variable (Yes/No) columns in the data.",
                                                  "Invalid input. \n ", "\n RE-FORMATTING VARIABLES : BIRTHS, JOINS, LEFTS, DEATHS \n How is '", list_to_check_codes[[i]], "' coded? Please input either \n '1' for yes, or \n 'NA' for no, missing or any other values." ))$res
      }

      if(!is.na(list_to_check_codes[[i]])){
        if(a == "NA") {
          df <- df %>% dplyr::mutate(dplyr::across(list_to_check, ~ (ifelse(. == list_to_check_codes[[i]], NA, .))))
        } else {
          df <- df %>% dplyr::mutate(dplyr::across(list_to_check, ~(ifelse(. == list_to_check_codes[[i]], a, .))))
        }
      }
    }
  }

  # Checking Cause of Death Coding

  cause_codes <- unique(df$death_cause)
  ideal_codes <- c("1", "2", "3")
  cause_recodes <- c("1", "2", "3", "NA")

  df <- df %>% dplyr::mutate(death_cause_smart = "")

  if(length(setdiff(cause_codes, ideal_codes))==0) {
    print("Good - Cause of Death coded as 1/2/3 for unknown/injury/illness")
  } else {
    print(cat(paste0()))
    for(i in 1:length(cause_codes)) {
      a <- svDialogs::dlg_input(message= paste0("\n Please define how each value is coded for Cause of Death in the data,",
                                                "\n RE-FORMATTING VARIABLES : CAUSE OF DEATH \n How is '",cause_codes[[i]], "' coded? Please input either: \n '1' for unknown, \n '2' for injury/trauma, \n '3' for illness or \n 'NA' for missing. \n"))$res
      while(length(setdiff(a, cause_recodes))==1) {
        a <- svDialogs::dlg_input(message= paste0("\n Please define how each value is coded for Cause of Death in the data,",
                                                  "Invalid input. \n ", "\n RE-FORMATTING VARIABLES : CAUSE OF DEATH \n How is '", cause_codes[[i]], "' coded? Please choose either: \n '1' for unknown, \n '2' for injury/trauma, \n '3' for illness or \n 'NA' for missing. \n"))$res
      }


      if(!is.na(cause_codes[[i]])){
        if(a == "NA") {df <- df %>% dplyr::mutate(death_cause_smart = ifelse(death_cause == cause_codes[[i]], NA, death_cause_smart))
        } else {df <- df %>% dplyr::mutate(death_cause_smart = ifelse(death_cause == cause_codes[[i]], a, death_cause_smart))}
      }

    }
  }

  # Checking Location of Death Coding
  location_codes <- unique(df$death_location)
  ideal_codes <- c("1", "2", "3", "4")
  location_recodes <- c("1", "2", "3", "4", "NA")

  df <- df %>% dplyr::mutate(death_location_smart = "")

  if(length(setdiff(location_codes, ideal_codes))==0) {
    print("Good - Cause of Death coded as 1/2/3/4 for unknown/injury/illness")
  } else {
    print(cat(paste0()))
    for(i in 1:length(location_codes)) {
      a <- svDialogs::dlg_input(message= paste0("\n Please define how each value is coded for Location of Death in the data,",
                                                "\n RE-FORMATTING VARIABLES : LOCATION OF DEATH \n How is '",location_codes[[i]], "' coded? Please input either: \n '1' for Died in Current Location, \n '2' for Died During Migration, \n '3' for Died in place of last residence, \n '4' Other location of death or \n 'NA' for missing. \n" ))$res
      while(length(setdiff(a, location_recodes))==1) {
        a <- svDialogs::dlg_input(message= paste0("\n Please define how each value is coded for Location of Death in the data,",
                                                  "Invalid input. \n ", "\n RE-FORMATTING VARIABLES : LOCATION OF DEATH \n How is '", location_codes[[i]], "' coded? Please choose either: \n '1' for Died in Current Location, \n '2' for Died During Migration, \n '3' for Died in place of last residence, \n '4' Other location of death or \n 'NA' for missing. \n"))$res
      }
      if(!is.na(location_codes[[i]])){
        if(a == "NA") {df <- df %>% dplyr::mutate(death_location_smart = ifelse(death_location == location_codes[[i]], NA, death_location_smart))
        } else {df <- df %>% dplyr::mutate(death_location_smart = ifelse(death_location == location_codes[[i]], a, death_location_smart))}
      }
    }
  }

  return(df)
}
