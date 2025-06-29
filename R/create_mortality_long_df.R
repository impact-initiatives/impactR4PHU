#' create_mortality_long_df
#'
#' @param df_main Main Dataset
#' @param date_dc the name of the variable that indicates the date of data collection
#' By default: "today"
#' @param date_recall_event the name of the variable that indicates the recall date
#' By default: "recall_date"
#' @param enumerator the name of the variable that indicates the enumerator
#' By default: NULL
#' @param cluster the name of the variable that indicates the cluster
#' By default: NULL
#' @param admin1 the name of the variable that indicates the admin 1
#' By default: NULL
#' @param admin2 the name of the variable that indicates the admin 2
#' By default: NULL
#' @param uuid_main the name of the variable that indicates the unique uuid
#' By default: NULL
#' @param df_roster Roster Dataset
#' @param sex_roster the name of the variable that indicates the sex of the individuals
#' By default: "sex_roster"
#' @param age_roster the name of the variable that indicates the age by year of the individuals
#' By default: "calc_final_age_years"
#' @param joined_roster the name of the variable that indicates if the individuals joined the HH
#' By default: "joined"
#' @param birth_roster the name of the variable that indicates if the individuals joined the HH
#' By default: "joined"
#' @param birthdate_roster the name of the variable that indicates if the date of birth of child
#' under 6. By default: "final_ind_dob"
#' @param joined_date_roster the name of the variable that indicates if the date individuals
#' joined the HH. By default: "final_date_join"
#' @param uuid_roster the name of the variable that indicates the unique uuid of HH
#' By default: NULL
#' @param collected_df_left Boolean value to check if df_left is collected. By default: T
#' @param df_left Left Dataset
#' @param sex_left the name of the variable that indicates the sex of the individuals
#' By default: "sex_left"
#' @param age_left the name of the variable that indicates the age by year of the individuals
#' By default: "calc_final_age_years_left"
#' @param birth_left the name of the variable that indicates if any of the leavers were born.
#' By default: "ind_born_left"
#' @param joined_left the name of the variable that indicates if any of the leavers joined
#' the HH. By default: "left_present"
#' @param joined_date_left the name of the variable that indicates the date the leavers
#' joined the HH. By default: "final_date_join_left"
#' @param left_date_left the name of the variable that indicates the date the leavers
#' left the HH. By default: "final_date_left"
#' @param birthdate_left the name of the variable that indicates the date the leavers
#' were born. By default: "final_ind_dob_left"
#' @param uuid_left the name of the variable that indicates the unique uuid of HH
#' By default: NULL
#' @param df_died Death Dataset
#' @param sex_died the name of the variable that indicates the sex of the individuals
#' By default: "sex_died"
#' @param age_died the name of the variable that indicates the age by year of the individuals
#' By default: "calc_final_age_years_died"
#' @param birth_died the name of the variable that indicates if any of the death were born.
#' By default: "ind_born_died"
#' @param joined_died the name of the variable that indicates if any of the death joined
#' the HH. By default: "died_present"
#' @param death_cause the name of the variable that indicates the cause of death
#' By default: "cause_death"
#' @param death_location the name of the variable that indicates the location of death
#' By default: "location_death"
#' @param date_death the name of the variable that indicates the date of death
#' By default: "final_date_death"
#' @param joined_date_died the name of the variable that indicates the date death joiners
#' joined the HH, By default: "date_join_final_death"
#' @param birthdate_died the name of the variable that indicates the date of birth of the
#' death individuals. By default: "dob_died"
#' @param uuid_died the name of the variable that indicates the unique uuid of HH
#' By default: NULL
#'
#' @return return a long reformated dataframe including all roster/left/death individuals
#' @export
#'
#' @examples
#' \dontrun{
#'   create_mortality_long_df(df_main,date_dc = "today",
#'   date_recall_event = "recall_date",
#'   enumerator = "enumerator",cluster = "cluster",
#'   admin1 = "admin1",admin2 = "admin2",
#'   uuid_main = "uuid", df_roster,sex_roster = "sex_roster",
#'   age_roster = "calc_final_age_years",
#'   joined_roster = "joined",birth_roster = "ind_born",
#'   birthdate_roster = "final_ind_dob",
#'   joined_date_roster = "final_date_join",
#'   uuid_roster = "_submission__uuid", df_left,sex_left = "sex_left",
#'   age_left = "calc_final_age_years_left",birth_left = "ind_born_left",
#'   joined_left = "left_present",joined_date_left = "final_date_join_left",
#'   left_date_left = "final_date_left",birthdate_left = "final_ind_dob_left",
#'   uuid_left = "_submission__uuid", df_died,sex_died = "sex_died",
#'   age_died = "calc_final_age_years_died",
#'   birth_died = "ind_born_died",joined_died = "died_present",
#'   death_cause = "cause_death", death_location = "location_death",
#'   date_death = "final_date_death", joined_date_died = "date_join_final_death",
#'   birthdate_died = "dob_died",uuid_died = "_submission__uuid")
#' }

create_mortality_long_df <- function(
  df_main,
  date_dc = "today",
  date_recall_event = "recall_date",
  enumerator = NULL,
  cluster = NULL,
  admin1 = NULL,
  admin2 = NULL,
  uuid_main = NULL,
  df_roster,
  sex_roster = "sex_roster",
  age_roster = "calc_final_age_years",
  joined_roster = "joined",
  birth_roster = "ind_born",
  birthdate_roster = "final_ind_dob",
  joined_date_roster = "final_date_join",
  uuid_roster = NULL,
  collected_df_left = TRUE,
  df_left = NULL,
  sex_left = "sex_left",
  age_left = "calc_final_age_years_left",
  birth_left = "ind_born_left",
  joined_left = "left_present",
  joined_date_left = "final_date_join_left",
  left_date_left = "final_date_left",
  birthdate_left = "final_ind_dob_left",
  uuid_left = NULL,
  df_died,
  sex_died = "sex_died",
  age_died = "calc_final_age_years_died",
  birth_died = "ind_born_died",
  joined_died = "died_present",
  death_cause = "cause_death",
  death_location = "location_death",
  date_death = "final_date_death",
  joined_date_died = "date_join_final_death",
  birthdate_died = "dob_died",
  uuid_died = NULL
) {
  options(warn = -1)

  if (is.null(date_recall_event)) {
    stop(
      "A date for recall event is required. Please input a character date with a format like dd/mm/yyyy. E.g 28/12/2020. Please check your input."
    )
  }

  if (!is.data.frame(df_main)) {
    stop("Main data should be a dataframe")
  }

  if (nrow(df_main) == 0) {
    stop("Main data is empty")
  }

  if (!is.data.frame(df_roster)) {
    stop("Roster Data should be a dataframe")
  }

  if (nrow(df_roster) == 0) {
    stop("Roster Data is empty")
  }

  if (collected_df_left) {
    if (!is.data.frame(df_left)) {
      stop("Left Data should be a dataframe")
    }
    if (nrow(df_left) == 0) {
      stop("Left Data is empty")
    }
  }

  if (!is.data.frame(df_died)) {
    stop("Died Data should be a dataframe")
  }

  if (nrow(df_died) == 0) {
    stop("Died Data is empty")
  }

  if (!is.null(uuid_main)) {
    if (!uuid_main %in% names(df_main)) {
      stop("uuid argument incorrect, or not available in the main dataset")
    }
  }

  if (!is.null(uuid_roster)) {
    if (!uuid_roster %in% names(df_roster)) {
      stop("uuid argument incorrect, or not available in the roster")
    }
  }

  if (collected_df_left) {
    if (!uuid_left %in% names(df_left)) {
      stop("uuid argument incorrect, or not available in the left data")
    }
  }

  if (!is.null(uuid_died)) {
    if (!uuid_died %in% names(df_died)) {
      stop("uuid argument incorrect, or not available in the died data")
    }
  }

  if (!is.null(cluster)) {
    if (!cluster %in% names(df_main)) {
      warning(
        "Cluster was not find in the main data. Creating a cluster column with NA."
      )
      df_main <- df_main %>% dplyr::mutate(cluster = NA)
      cluster <- "cluster"
    }
  } else {
    warning(
      "Cluster was not find in the main data. Creating a cluster column with NA."
    )
    df_main <- df_main %>% dplyr::mutate(cluster = NA)
    cluster <- "cluster"
  }

  if (!is.null(admin1)) {
    if (!admin1 %in% names(df_main)) {
      warning(
        "Admin1 was not find in the main data. Creating a admin1 column with NA."
      )
      df_main <- df_main %>% dplyr::mutate(admin1 = NA)
      admin1 <- "admin1"
    }
  } else {
    warning(
      "Admin1 was not find in the main data. Creating a admin1 column with NA."
    )
    df_main <- df_main %>% dplyr::mutate(admin1 = NA)
    admin1 <- "admin1"
  }

  if (!is.null(admin2)) {
    if (!admin2 %in% names(df_main)) {
      warning(
        "Admin2 was not find in the main data. Creating a admin2 column with NA."
      )
      df_main <- df_main %>% dplyr::mutate(admin2 = NA)
      admin2 <- "admin2"
    }
  } else {
    warning(
      "Cluster was not find in the main data. Creating a cluster column with NA."
    )
    df_main <- df_main %>% dplyr::mutate(admin2 = NA)
    admin2 <- "admin2"
  }

  if (
    !all(
      c(date_dc, date_recall_event, enumerator) %in%
        names(df_main)
    ) |
      length(c(date_dc, date_recall_event, enumerator)) - 3 != 0
  ) {
    stop(
      "Check date_dc, date_recall_event, or enumerator arguments. Couldn't find in main data."
    )
  } else {
    main_to_join <- df_main %>%
      dplyr::rename(
        date_dc = !!rlang::sym(date_dc),
        date_recall_event = !!rlang::sym(date_recall_event),
        enumerator = !!rlang::sym(enumerator),
        cluster = !!rlang::sym(cluster),
        admin1 = !!rlang::sym(admin1),
        admin2 = !!rlang::sym(admin2),
        uuid = uuid_main
      ) %>%
      dplyr::select(
        uuid,
        date_dc,
        date_recall_event,
        enumerator,
        cluster,
        admin1,
        admin2
      )
  }

  df_roster <- df_roster %>%
    dplyr::rename(
      sex = sex_roster,
      age_years = age_roster,
      join = joined_roster,
      birth = birth_roster,
      date_join = joined_date_roster,
      date_birth = birthdate_roster,
      uuid = uuid_roster
    ) %>%
    dplyr::left_join(main_to_join) %>%
    dplyr::mutate(date_recall = date_recall_event)
  if (collected_df_left) {
    df_left <- df_left %>%
      dplyr::rename(
        sex = sex_left,
        age_years = age_left,
        join = joined_left,
        birth = birth_left,
        date_join = joined_date_left,
        date_left = left_date_left,
        date_birth = birthdate_left,
        uuid = uuid_left
      ) %>%
      dplyr::left_join(main_to_join) %>%
      dplyr::mutate(date_recall = date_recall_event)
  }
  df_died <- df_died %>%
    dplyr::rename(
      sex = sex_died,
      age_years = age_died,
      join = joined_died,
      birth = birth_died,
      death_cause = death_cause,
      death_location = death_location,
      date_death = date_death,
      date_join = joined_date_died,
      date_birth = birthdate_died,
      uuid = uuid_died
    ) %>%
    dplyr::left_join(main_to_join) %>%
    dplyr::mutate(date_recall = date_recall_event)

  date_vars <- c("date_death", "date_birth", "date_join", "date_left")
  if (collected_df_left) {
    if (
      length(intersect(date_vars, colnames(df_roster))) > 0 |
        length(intersect(date_vars, colnames(df_left))) > 0 |
        length(intersect(date_vars, colnames(df_died))) > 0
    ) {
      if (!"date_death" %in% names(df_roster)) {
        df_roster <- df_roster %>% dplyr::mutate(date_death = NA)
      }
      if (!"date_join" %in% names(df_roster)) {
        df_roster <- df_roster %>% dplyr::mutate(date_join = NA)
      }
      if (!"date_left" %in% names(df_roster)) {
        df_roster <- df_roster %>% dplyr::mutate(date_left = NA)
      }
      if (!"date_birth" %in% names(df_roster)) {
        df_roster <- df_roster %>% dplyr::mutate(date_birth = NA)
      }
      if (!"date_death" %in% names(df_left)) {
        df_left <- df_left %>% dplyr::mutate(date_death = NA)
      }
      if (!"date_join" %in% names(df_left)) {
        df_left <- df_left %>% dplyr::mutate(date_join = NA)
      }
      if (!"date_left" %in% names(df_left)) {
        df_left <- df_left %>% dplyr::mutate(date_left = NA)
      }
      if (!"date_birth" %in% names(df_left)) {
        df_left <- df_left %>% dplyr::mutate(date_birth = NA)
      }
      if (!"date_death" %in% names(df_died)) {
        df_died <- df_died %>% dplyr::mutate(date_death = NA)
      }
      if (!"date_join" %in% names(df_died)) {
        df_died <- df_died %>% dplyr::mutate(date_join = NA)
      }
      if (!"date_left" %in% names(df_died)) {
        df_died <- df_died %>% dplyr::mutate(date_left = NA)
      }
      if (!"date_birth" %in% names(df_died)) {
        df_died <- df_died %>% dplyr::mutate(date_birth = NA)
      }
      df_roster[date_vars] <- lapply(df_roster[date_vars], as.character)
      if (collected_df_left) {
        df_left[date_vars] <- lapply(df_left[date_vars], as.character)
      }
      df_died[date_vars] <- lapply(df_died[date_vars], as.character)
    }
  } else {
    if (
      length(intersect(date_vars, colnames(df_roster))) > 0 |
        length(intersect(date_vars, colnames(df_died))) > 0
    ) {
      if (!"date_death" %in% names(df_roster)) {
        df_roster <- df_roster %>% dplyr::mutate(date_death = NA)
      }
      if (!"date_join" %in% names(df_roster)) {
        df_roster <- df_roster %>% dplyr::mutate(date_join = NA)
      }
      if (!"date_left" %in% names(df_roster)) {
        df_roster <- df_roster %>% dplyr::mutate(date_left = NA)
      }
      if (!"date_birth" %in% names(df_roster)) {
        df_roster <- df_roster %>% dplyr::mutate(date_birth = NA)
      }
      if (!"date_death" %in% names(df_died)) {
        df_died <- df_died %>% dplyr::mutate(date_death = NA)
      }
      if (!"date_join" %in% names(df_died)) {
        df_died <- df_died %>% dplyr::mutate(date_join = NA)
      }
      if (!"date_left" %in% names(df_died)) {
        df_died <- df_died %>% dplyr::mutate(date_left = NA)
      }
      if (!"date_birth" %in% names(df_died)) {
        df_died <- df_died %>% dplyr::mutate(date_birth = NA)
      }
      df_roster[date_vars] <- lapply(df_roster[date_vars], as.character)
      if (collected_df_left) {
        df_left[date_vars] <- lapply(df_left[date_vars], as.character)
      }
      df_died[date_vars] <- lapply(df_died[date_vars], as.character)
    }
  }

  req_roster <- c(
    "date_dc",
    "enumerator",
    "cluster",
    "sex",
    "age_years",
    "birth"
  )
  if (collected_df_left) {
    req_left <- c("sex", "age_years", "birth")
  }
  req_died <- c("sex", "age_years", "birth", "death_cause", "death_location")
  additional_cols <- c("join", "left", "death", "death_cause", "death_location")
  if (all(req_roster %in% names(df_roster))) {
    print("Sex, Age and Births available for current roster.")
  } else {
    stop(
      "Missing minimum information (SEX, AGE, Births) for current household roster. Please check input."
    )
  }
  if (collected_df_left) {
    if (all(req_left %in% names(df_left))) {
      print("Sex, Age and Births available for Left people.")
    } else {
      stop(
        "Missing minimum information (SEX, AGE, Births) for left people roster. Please check input."
      )
    }
  }
  if (all(req_died %in% names(df_died))) {
    print(
      "Sex, Age, Births, Cause and Location of Death available for Deceased people."
    )
  } else {
    stop(
      "Missing minimum information (SEX, AGE, Births, Cause of Death, Location of Death) for death roster. Please check input."
    )
  }
  if (!all(additional_cols %in% names(df_roster))) {
    cols_to_add <- setdiff(additional_cols, colnames(df_roster))
    if ("join" %in% cols_to_add) {
      df_roster <- df_roster %>% dplyr::mutate(join = NA)
    }
    if ("left" %in% cols_to_add) {
      df_roster <- df_roster %>% dplyr::mutate(left = NA)
    }
    if ("death" %in% cols_to_add) {
      df_roster <- df_roster %>% dplyr::mutate(death = NA)
    }
    if ("death_cause" %in% cols_to_add) {
      df_roster <- df_roster %>% dplyr::mutate(death_cause = NA)
    }
    if ("death_location" %in% cols_to_add) {
      df_roster <- df_roster %>% dplyr::mutate(death_location = NA)
    }
  }
  if (collected_df_left) {
    if (!all(additional_cols %in% names(df_left))) {
      cols_to_add <- setdiff(additional_cols, colnames(df_left))
      if ("join" %in% cols_to_add) {
        df_left <- df_left %>% dplyr::mutate(join = NA)
      }
      if ("left" %in% cols_to_add) {
        df_left <- df_left %>% dplyr::mutate(left = "1")
      }
      if ("death" %in% cols_to_add) {
        df_left <- df_left %>% dplyr::mutate(death = NA)
      }
      if ("death_cause" %in% cols_to_add) {
        df_left <- df_left %>% dplyr::mutate(death_cause = NA)
      }
      if ("death_location" %in% cols_to_add) {
        df_left <- df_left %>% dplyr::mutate(death_location = NA)
      }
    }
  }
  if (!all(additional_cols %in% names(df_died))) {
    cols_to_add <- setdiff(additional_cols, colnames(df_died))
    if ("join" %in% cols_to_add) {
      df_died <- df_died %>% dplyr::mutate(join = NA)
    }
    if ("left" %in% cols_to_add) {
      df_died <- df_died %>% dplyr::mutate(left = NA)
    }
    if ("death" %in% cols_to_add) {
      df_died <- df_died %>% dplyr::mutate(death = "1")
    }
  }
  if (collected_df_left) {
    if (
      all(date_vars %in% names(df_roster)) |
        all(
          date_vars %in%
            names(df_left)
        ) |
        all(date_vars %in% names(df_died))
    ) {
      if (is.null(admin1)) {
        if (is.null(admin2)) {
          col_order <- c(
            "date_dc",
            "date_recall",
            "enumerator",
            "cluster",
            "uuid",
            "sex",
            "age_years",
            "join",
            "date_join",
            "left",
            "date_left",
            "birth",
            "date_birth",
            "death",
            "date_death",
            "death_cause",
            "death_location"
          )
        } else {
          col_order <- c(
            "date_dc",
            "date_recall",
            "enumerator",
            "admin2",
            "cluster",
            "uuid",
            "sex",
            "age_years",
            "join",
            "date_join",
            "left",
            "date_left",
            "birth",
            "date_birth",
            "death",
            "date_death",
            "death_cause",
            "death_location"
          )
        }
      } else {
        if (is.null(admin2)) {
          col_order <- c(
            "date_dc",
            "date_recall",
            "enumerator",
            "admin1",
            "cluster",
            "uuid",
            "sex",
            "age_years",
            "join",
            "date_join",
            "left",
            "date_left",
            "birth",
            "date_birth",
            "death",
            "date_death",
            "death_cause",
            "death_location"
          )
        } else {
          col_order <- c(
            "date_dc",
            "date_recall",
            "enumerator",
            "admin1",
            "admin2",
            "cluster",
            "uuid",
            "sex",
            "age_years",
            "join",
            "date_join",
            "left",
            "date_left",
            "birth",
            "date_birth",
            "death",
            "date_death",
            "death_cause",
            "death_location"
          )
        }
      }
    } else {
      if (is.null(admin1)) {
        if (is.null(admin2)) {
          col_order <- c(
            "date_dc",
            "date_recall",
            "enumerator",
            "cluster",
            "uuid",
            "sex",
            "age_years",
            "join",
            "left",
            "birth",
            "death",
            "death_cause",
            "death_location"
          )
        } else {
          col_order <- c(
            "date_dc",
            "date_recall",
            "enumerator",
            "admin2",
            "cluster",
            "uuid",
            "sex",
            "age_years",
            "join",
            "left",
            "birth",
            "death",
            "death_cause",
            "death_location"
          )
        }
      } else {
        if (is.null(admin2)) {
          col_order <- c(
            "date_dc",
            "date_recall",
            "enumerator",
            "admin1",
            "cluster",
            "uuid",
            "sex",
            "age_years",
            "join",
            "left",
            "birth",
            "death",
            "death_cause",
            "death_location"
          )
        } else {
          col_order <- c(
            "date_dc",
            "date_recall",
            "enumerator",
            "admin1",
            "admin2",
            "cluster",
            "uuid",
            "sex",
            "age_years",
            "join",
            "left",
            "birth",
            "death",
            "death_cause",
            "death_location"
          )
        }
      }
    }
  } else {
    if (
      all(date_vars %in% names(df_roster)) | all(date_vars %in% names(df_died))
    ) {
      if (is.null(admin1)) {
        if (is.null(admin2)) {
          col_order <- c(
            "date_dc",
            "date_recall",
            "enumerator",
            "cluster",
            "uuid",
            "sex",
            "age_years",
            "join",
            "date_join",
            "left",
            "date_left",
            "birth",
            "date_birth",
            "death",
            "date_death",
            "death_cause",
            "death_location"
          )
        } else {
          col_order <- c(
            "date_dc",
            "date_recall",
            "enumerator",
            "admin2",
            "cluster",
            "uuid",
            "sex",
            "age_years",
            "join",
            "date_join",
            "left",
            "date_left",
            "birth",
            "date_birth",
            "death",
            "date_death",
            "death_cause",
            "death_location"
          )
        }
      } else {
        if (is.null(admin2)) {
          col_order <- c(
            "date_dc",
            "date_recall",
            "enumerator",
            "admin1",
            "cluster",
            "uuid",
            "sex",
            "age_years",
            "join",
            "date_join",
            "left",
            "date_left",
            "birth",
            "date_birth",
            "death",
            "date_death",
            "death_cause",
            "death_location"
          )
        } else {
          col_order <- c(
            "date_dc",
            "date_recall",
            "enumerator",
            "admin1",
            "admin2",
            "cluster",
            "uuid",
            "sex",
            "age_years",
            "join",
            "date_join",
            "left",
            "date_left",
            "birth",
            "date_birth",
            "death",
            "date_death",
            "death_cause",
            "death_location"
          )
        }
      }
    } else {
      if (is.null(admin1)) {
        if (is.null(admin2)) {
          col_order <- c(
            "date_dc",
            "date_recall",
            "enumerator",
            "cluster",
            "uuid",
            "sex",
            "age_years",
            "join",
            "left",
            "birth",
            "death",
            "death_cause",
            "death_location"
          )
        } else {
          col_order <- c(
            "date_dc",
            "date_recall",
            "enumerator",
            "admin2",
            "cluster",
            "uuid",
            "sex",
            "age_years",
            "join",
            "left",
            "birth",
            "death",
            "death_cause",
            "death_location"
          )
        }
      } else {
        if (is.null(admin2)) {
          col_order <- c(
            "date_dc",
            "date_recall",
            "enumerator",
            "admin1",
            "cluster",
            "uuid",
            "sex",
            "age_years",
            "join",
            "left",
            "birth",
            "death",
            "death_cause",
            "death_location"
          )
        } else {
          col_order <- c(
            "date_dc",
            "date_recall",
            "enumerator",
            "admin1",
            "admin2",
            "cluster",
            "uuid",
            "sex",
            "age_years",
            "join",
            "left",
            "birth",
            "death",
            "death_cause",
            "death_location"
          )
        }
      }
    }
  }

  df_roster <- df_roster %>%
    dplyr::select(col_order) %>%
    dplyr::mutate(age_years = as.character(age_years))
  if (collected_df_left) {
    df_left <- df_left %>%
      dplyr::select(col_order) %>%
      dplyr::mutate(age_years = as.character(age_years))
  }
  df_died <- df_died %>%
    dplyr::select(col_order) %>%
    dplyr::mutate(age_years = as.character(age_years))
  df_roster <- lapply(df_roster, as.character)
  if (collected_df_left) {
    df_left <- lapply(df_left, as.character)
  }
  df_died <- lapply(df_died, as.character)

  df_mortality <- dplyr::bind_rows(df_roster, df_died)
  if (collected_df_left) {
    df_mortality <- dplyr::bind_rows(df_mortality, df_left)
  }

  return(df_mortality)
}
