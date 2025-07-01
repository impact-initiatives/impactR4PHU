# source

calculate_roster_person_time_column <- function(data, start_col, end_col,
                                         new_col = "person_time_days",
                                         default_start = as.Date("2020-01-01")) {
  # Check that columns exist
  if (!all(c(start_col, end_col) %in% names(data))) {
    stop("Start and/or end column names not found in the dataset.")
  }
  
  # Coerce to Date
  start_dates <- suppressWarnings(as.Date(data[[start_col]]))
  end_dates <- suppressWarnings(as.Date(data[[end_col]]))
  
  # Replace invalid or too-early start dates
  start_dates[is.na(start_dates) | start_dates < default_start] <- default_start
  
  # Calculate raw difference
  person_time <- as.numeric(difftime(end_dates, start_dates, units = "days"))
  
  # Create flag: 1 if person_time is negative, else 0
  flag <- ifelse(!is.na(person_time) & person_time < 0, 1, 0)
  
  # Set negative person_time to NA
  person_time[person_time < 0] <- NA
  
  # Add columns to dataset
  data[[new_col]] <- person_time
  data[["flag_persontime"]] <- flag
  
  return(data)
}

fix_excel_dates2 <- function(x) {
  # If it's already Date, return as-is
  if (inherits(x, "Date")) {
    return(x)
  }
  
  # 1. Handle character strings that look like valid dates (e.g. "2023-01-01")
  if (is.character(x)) {
    
    print("I am here")
    
    # Try converting to Date first
    x_date <- suppressWarnings(as.Date(x))
    valid_date <- !is.na(x_date) & grepl("^\\d{4}-\\d{2}-\\d{2}$", x)
    
    # If some values are valid date strings, return the result
    if (any(valid_date)) {
      out <- x
      out[valid_date] <- as.character(x_date[valid_date])
      return(as.Date(out, origin = "1970-01-01"))
    }
    
    # If conversion failed, treat as numeric for Excel-style dates
    x_num <- suppressWarnings(as.numeric(x))
  } else {
    # Treat as numeric (for Excel-style dates)
    x_num <- x
  }
  
  # 2. Handle Excel-style numeric dates (safe range check)
  if (is.numeric(x_num) && all(x_num > 40000 & x_num < 50000, na.rm = TRUE)) {
    # Split into date (whole part) and time (decimal part)
    whole_part <- floor(x_num)
    decimal_part <- x_num - whole_part
    
    # Convert whole part to Date (Excel date)
    dates <- as.Date(whole_part, origin = "1899-12-30")
    
    # Convert decimal part to time (fraction of a day, in seconds)
    times <- decimal_part * 86400  # 86400 seconds in a day
    
    # Combine the date with the time into POSIXct datetime format
    datetime <- as.POSIXct(dates) + times
    
    return(datetime)
  }
  
  # 3. Fallback: return original vector untouched if neither case matched
  return(x)
}

fix_excel_dates <- function(x) {
  # If it's already Date, return as-is
  if (inherits(x, "Date")) {
    return(x)
  }
  
  # 1. Character strings that look like valid dates (e.g. "2023-01-01")
  if (is.character(x)) {
    x_date <- suppressWarnings(as.Date(x))
    valid_date <- !is.na(x_date) & grepl("^\\d{4}-\\d{2}-\\d{2}$", x)
    
    # If some values are valid date strings, only convert those
    if (any(valid_date)) {
      out <- x
      out[valid_date] <- as.character(x_date[valid_date])
      return(as.Date(out, origin = "1970-01-01"))
    }
    
    # 2. Try converting to numeric for Excel-style detection
    x_num <- suppressWarnings(as.numeric(x))
  } else {
    x_num <- x
  }
  
  # 3. Excel-style numeric dates (safe range check)
  if (is.numeric(x_num) && all(x_num > 40000 & x_num < 50000, na.rm = TRUE)) {
    return(as.Date(x_num, origin = "1899-12-30"))
  }
  
  # 4. Fallback: return original vector untouched
  return(x)
}

#' create_mortality_long_df
#'
#' @param df_main Main Dataset
#' @param date_dc the name of the variable that indicates the date of data collection
#' By default: "today"
#' @param date_recall_event the name of the variable that indicates the recall date
#' By default: "recall_date"
#' @param uuid_main the name of the variable that indicates the unique uuid
#' By default: NULL
#' @param df_roster Roster Dataset
#' @param sex_roster the name of the variable that indicates the sex of the individuals
#' By default: "sex_roster"
#' @param age_roster the name of the variable that indicates the age by year of the individuals
#' By default: "calc_final_age_years"
#' @param birthdate_roster the name of the variable that indicates if the date of birth of child
#' under 6. By default: "final_ind_dob"
#' @param uuid_roster the name of the variable that indicates the unique uuid of HH
#' By default: NULL
#' @param uuid_left the name of the variable that indicates the unique uuid of HH
#' By default: NULL
#' @param df_died Death Dataset
#' @param sex_died the name of the variable that indicates the sex of the individuals
#' By default: "sex_died"
#' @param age_died the name of the variable that indicates the age by year of the individuals
#' By default: "calc_final_age_years_died"
#' @param date_death the name of the variable that indicates the date of death
#' By default: "final_date_death"
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

local_create_mortality_long_df <- function (df_main, date_dc = "today", date_recall_event = "recall_date",
                                      uuid_main = NULL, 
                                      
                                      df_roster, sex_roster = "sex_roster",
                                      age_roster = "calc_final_age_years",
                                      birthdate_roster = "final_ind_dob", uuid_roster = NULL,
                                      
                                      df_died, sex_died = "sex_died", age_died = "calc_final_age_years_died",
                                      date_death = "final_date_death",
                                      birthdate_died = "dob_died", uuid_died = NULL,
                                      
                                      main_cols_to_add = NULL
                                    
                                      ) {
  # options(warn = -1)
  
  if (is.null(date_recall_event)) {
    stop("A date for recall event is required. Please input a character date with a format like dd/mm/yyyy. E.g 28/12/2020. Please check your input.")
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
  
  if (!is.data.frame(df_died)) {
    stop("Died Data should be a dataframe")
  }
  
  if (nrow(df_died) == 0) {
    stop("Died Data is empty")
  }
  
  if(!is.null(uuid_main)){
    if (!uuid_main %in% names(df_main))
      stop("uuid argument incorrect, or not available in the main dataset")
  }
  
  if(!is.null(uuid_roster)){
    if (!uuid_roster %in% names(df_roster))
      stop("uuid argument incorrect, or not available in the roster")
  }
  
  if(!is.null(uuid_died)){
    if (!uuid_died %in% names(df_died))
      stop("uuid argument incorrect, or not available in the died data")
  }

  if (!all(c(date_dc, date_recall_event) %in%
           names(df_main))|
      length(c(date_dc, date_recall_event))- 2 != 0) 
    
    {
    
    stop("Check date_dc, date_recall_event arguments. Couldn't find in main data.")
    
    } else {
    
    main_to_join <- df_main %>% dplyr::rename(date_dc = !!rlang::sym(date_dc),
                                              date_recall_event = !!rlang::sym(date_recall_event),
                                              uuid = uuid_main) %>% 
      dplyr::select(uuid, date_dc, date_recall_event, main_cols_to_add)
    
    
  }

  df_roster <- df_roster %>% dplyr::rename(sex = sex_roster,
                                           age_years = age_roster,
                                           date_birth = birthdate_roster,
                                           uuid = uuid_roster) %>% 
    dplyr::left_join(main_to_join) %>%
    dplyr::mutate(date_recall = date_recall_event)
  
  df_died <- df_died %>% dplyr::rename(sex = sex_died, 
                                       age_years = age_died,
                                       date_death = date_death,
                                       date_birth = birthdate_died,
                                       uuid = uuid_died) %>% 
    dplyr::left_join(main_to_join) %>%
    dplyr::mutate(date_recall = date_recall_event)
  
  req_cols <- c("sex", "age_years", "date_birth", "uuid")

  if (all(req_cols %in% names(df_roster))) {
    print("Sex, Age and Births available for current roster.")
  }
  else {
    stop("Missing minimum information (SEX, AGE, Date of Birth) for current household roster. Please check input.")
  }
  
 
  if (all(req_cols %in% names(df_died))) {
    print("Sex, Age, Births, available for Deceased people.")
  }
  else {
    stop("Missing minimum information (SEX, AGE, Dates of Birth or Death) for death roster. Please check input.")
  }

  df_roster <- lapply(df_roster, as.character)
  df_died <- lapply(df_died, as.character)
  
  df_mortality_long <- bind_rows(df_roster, df_died) %>%
    dplyr::mutate(
      date_dc = fix_excel_dates(date_dc),
      entry_date = dplyr::case_when(
        !is.na(date_birth) & date_birth >= date_recall ~ as.Date(date_birth),
        !is.na(date_recall) ~ as.Date(date_recall),
        TRUE ~ NA_Date_
      ),
      exit_date = dplyr::case_when(
        !is.na(date_death) ~ as.Date(date_death),
        !is.na(date_dc) ~ as.Date(date_dc),
        TRUE ~ NA_Date_
      ),
      person_time = as.numeric(difftime(exit_date, entry_date, units = "days")),
      flag_persont_time = dplyr::case_when(
        !is.na(person_time) & person_time < 0 ~ 1,
        TRUE ~ 0
      ),
      ind_born = dplyr::case_when(
        !is.na(date_birth) & date_birth >= date_recall ~ 1,
        !is.na(date_birth) ~ 0,
      )
    )
  
  to_merge_hh <- df_mortality_long %>%
    dplyr::mutate(
      pt_negative = dplyr::case_when(
      person_time < 0 ~ 1,
      TRUE ~ 0
    ),
      age_years = as.numeric(age_years),
      pt_u5 = dplyr::case_when(
      age_years < 5 ~ person_time,
      TRUE ~ 0
    )) %>%
    dplyr::group_by(uuid) %>%
    dplyr::summarize(
      person_time = sum(person_time, na.rm = TRUE),
      person_time_under5 = sum(pt_u5, na.rm = TRUE),
      num_deaths = sum(!is.na(date_death), na.rm = TRUE),
      num_under5_deaths = sum(age_years < 5 & !is.na(date_death), na.rm = TRUE),
      num_births = sum(ind_born, na.rm = TRUE),
      sum_hh = n(),
      sum_01 = sum(age_years < 2, na.rm = TRUE),
      sum_25 = sum(age_years >= 2 & age_years < 5, na.rm = TRUE),
      sum_05 = sum(age_years >= 0 & age_years < 5, na.rm = TRUE),
      sum_510 = sum(age_years >= 5 & age_years < 10, na.rm = TRUE),
      sum_5plus = sum(age_years >= 5, na.rm = TRUE),
      sum_male = sum(sex == "male", na.rm = TRUE),
      sum_female = sum(sex == "female", na.rm = TRUE),
      flag_pt_neg = sum(pt_negative, na.rm = TRUE)  
    ) %>% dplyr::rename(hh_uuid = uuid) %>%
    dplyr::mutate(
      flag_multiple_deaths = dplyr::case_when(
        num_deaths > 1 ~ 1,
        TRUE ~ 0
      ),
      flag_multiple_births = dplyr::case_when(
        num_births > 1 ~ 1,
        TRUE ~ 0
      )
    ) 
  
  df_mortality_hh <- df_main %>%
    dplyr::left_join(to_merge_hh, by = "hh_uuid")

  dfs_mortality <- list(df_mortality_long, df_mortality_hh)
  
  return(dfs_mortality)
}

fisher_summary <- function(data, event_col, total_col, group_col) {
  # Capture column names
  event_col <- rlang::ensym(event_col)
  total_col <- rlang::ensym(total_col)
  group_col <- rlang::ensym(group_col)
  
  # Extract values
  event_counts <- dplyr::pull(data, !!event_col)
  total_counts <- dplyr::pull(data, !!total_col)
  group_labels <- dplyr::pull(data, !!group_col)
  
  # Aggregate by group
  summary_df <- data %>%
    dplyr::group_by(!!group_col) %>%
    dplyr::summarise(
      events = sum(!!event_col),
      total = sum(!!total_col),
      .groups = "drop"
    )
  
  # Create 2-row matrix: Events / Non-events
  matrix_data <- rbind(
    Events = summary_df$events,
    NonEvents = summary_df$total - summary_df$events
  )
  colnames(matrix_data) <- summary_df[[rlang::as_string(group_col)]]
  
  # Run Fisher's exact test
  test <- fisher.test(matrix_data, simulate.p.value = ncol(matrix_data) > 2)
  
  list(
    table = matrix_data,
    p_value = test$p.value,
    method = test$method
  )
}

# anova_summary <- function(data, outcome_col, group_col) {
#   # Ensure proper symbol conversion
#   group_col <- rlang::sym(group_col)
#   outcome_col <- rlang::ensym(outcome_col)
#   
#   # Safely check that columns exist
#   if (!(rlang::as_string(group_col) %in% names(data)) ||
#       !(rlang::as_string(outcome_col) %in% names(data))) {
#     warning("One or both specified columns do not exist in the data.")
#     return(tibble::tibble(DF = NA, `F value` = NA, `Pr(>F)` = NA))
#   }
#   
#   # Filter out rows with missing values in either variable
#   filtered_data <- data %>%
#     dplyr::filter(!is.na(!!group_col), !is.na(!!outcome_col))
#   
#   # Check if there is enough data
#   if (nrow(filtered_data) < 2) {
#     warning("Not enough non-NA data to run ANOVA.")
#     return(tibble::tibble(DF = NA, `F value` = NA, `Pr(>F)` = NA))
#   }
#   
#   # Construct formula dynamically
#   formula <- as.formula(paste0(rlang::as_string(outcome_col), " ~ ", rlang::as_string(group_col)))
#   
#   # Try running ANOVA
#   result <- tryCatch({
#     model <- aov(formula, data = filtered_data)
#     summary_df <- broom::tidy(model)
#     summary_df %>%
#       dplyr::filter(term != "Residuals") %>%
#       dplyr::select(DF = df, `F value` = statistic, `Pr(>F)` = p.value)
#   }, error = function(e) {
#     warning(paste("ANOVA failed:", e$message))
#     tibble::tibble(DF = NA, `F value` = NA, `Pr(>F)` = NA)
#   })
#   
#   return(result)
# }

anova_summary <- function(data, outcome_col, group_col) {
  tryCatch({
    # Ensure the column names are interpreted correctly
    group_col_sym <- rlang::sym(group_col)
    outcome_col_sym <- rlang::ensym(outcome_col)
    
    # Construct formula dynamically
    formula <- as.formula(paste0(rlang::as_string(outcome_col_sym), " ~ ", rlang::as_string(group_col_sym)))
    
    # Run ANOVA
    model <- aov(formula, data = data)
    
    # Return ANOVA summary
    summary(model)
    
  }, error = function(e) {
    warning(paste("ANOVA failed:", e$message))
    return(NULL)
  })
}

chi_squared_test_binary <- function(data, group_col, cat1_col, cat2_col) {
  # Safely attempt everything inside tryCatch
  result <- tryCatch({
    # Convert column names safely
    group_col_sym <- rlang::sym(group_col)
    cat1_col_sym <- rlang::ensym(cat1_col)
    cat2_col_sym <- rlang::ensym(cat2_col)
    
    # Prepare the contingency table
    contingency_table <- data %>%
      dplyr::select(!!group_col_sym, !!cat1_col_sym, !!cat2_col_sym) %>%
      tibble::column_to_rownames(var = rlang::as_string(group_col_sym)) %>%
      as.matrix()
    
    # Perform Chi-squared test
    chisq.test(contingency_table)
    
  }, error = function(e) {
    warning(paste("Chi-squared test failed:", e$message))
    return(NULL)
  })
  
  return(result)
}


binomial_ratio_test_rowwise <- function(data, 
                                        success_col, 
                                        total_col, 
                                        expected_ratio = 0.5, 
                                        alternative = "two.sided",
                                        pval_colname = "p_value") {
  success_col <- rlang::ensym(success_col)
  total_col <- rlang::ensym(total_col)
  
  results <- data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      .temp_pval = tryCatch({
        if (
          !is.na(!!success_col) && 
          !is.na(!!total_col) && 
          !!total_col >= !!success_col && 
          !!total_col > 0 && 
          !!total_col == floor(!!total_col) && 
          !!success_col == floor(!!success_col)
        ) {
          round(
            binom.test(
              x = !!success_col,
              n = !!total_col,
              p = expected_ratio,
              alternative = alternative
            )$p.value,
            digits = 5
          )
        } else {
          NA_real_
        }
      }, error = function(e) {
        NA_real_
      })
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(!!pval_colname := .temp_pval)
  
  return(results)
}

t_test_rowwise_from_summary <- function(data,
                                        mean_col,
                                        sd_col,
                                        n_col,
                                        expected_mean = 0,
                                        alternative = "two.sided",
                                        pval_colname = "p_value") {
  mean_col <- rlang::ensym(mean_col)
  sd_col <- rlang::ensym(sd_col)
  n_col <- rlang::ensym(n_col)
  
  results <- data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      .temp_pval = tryCatch({
        t_stat <- (!!mean_col - expected_mean) / (!!sd_col / sqrt(!!n_col))
        df <- !!n_col - 1
        
        round(
          dplyr::case_when(
            alternative == "two.sided" ~ 2 * pt(-abs(t_stat), df),
            alternative == "greater" ~ pt(t_stat, df, lower.tail = FALSE),
            alternative == "less" ~ pt(t_stat, df, lower.tail = TRUE),
            TRUE ~ NA_real_
          ),
          digits = 3
        )
      }, error = function(e) {
        NA_real_
      })
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(!!pval_colname := .temp_pval)
  
  return(results)
}

poisson_ratio_test_rowwise <- function(data, 
                                       event_col, 
                                       exposure_col, 
                                       expected_rate = 0.02,  # expected rate per unit
                                       alternative = "two.sided",
                                       pval_colname = "p_value") {
  event_col <- rlang::ensym(event_col)
  exposure_col <- rlang::ensym(exposure_col)
  
  # Perform row-wise Poisson test and round p-values
  results <- data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      .temp_pval = tryCatch({
        if (!is.na(!!event_col) && !is.na(!!exposure_col) &&
            !!exposure_col > 0 && !!event_col >= 0) {
          round(
            poisson.test(
              x = !!event_col,      # number of events
              T = !!exposure_col,   # exposure units (e.g., households)
              r = expected_rate,
              alternative = alternative
            )$p.value,
            digits = 5
          )
        } else {
          NA_real_
        }
      }, error = function(e) {
        NA_real_
      })
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(!!pval_colname := .temp_pval)
  
  return(results)
}

convert_to_ordinal <- function(data, target_var, levels, na_values = NULL) {
  # Ensure the target variable exists in the dataframe
  if (!target_var %in% names(data)) {
    stop("The target variable does not exist in the dataframe.")
  }
  
  # Convert the target variable to character
  data <- data %>%
    mutate(!!target_var := as.character(.data[[target_var]]))
  
  # Set specified responses to NA
  if (!is.null(na_values)) {
    data <- data %>%
      mutate(!!target_var := case_when(
        .data[[target_var]] %in% na_values ~ NA_character_,
        TRUE ~ .data[[target_var]]
      ))
  }
  
  # Convert the target variable to an ordered factor with specified levels
  data <- data %>%
    mutate(!!target_var := factor(.data[[target_var]], levels = levels, ordered = TRUE))
  
  return(data)
}


convert_to_numeric <- function(data, columns) {
  # Function to check if a column can be safely converted to numeric
  can_convert_to_numeric <- function(column) {
    converted <- suppressWarnings(as.numeric(column))
    all(!is.na(converted) | is.na(column))
  }
  
  # Loop through the specified columns
  for (col in columns) {
    if (!is.numeric(data[[col]])) {
      if (can_convert_to_numeric(data[[col]])) {
        data[[col]] <- as.numeric(data[[col]])
      } else {
        warning(paste("Column", col, "cannot be safely converted to numeric and will be excluded."))
      }
    }
  }
  
  # Set values of -999 to NA and convert to numeric
  data <- data %>%
    mutate(across(all_of(columns), ~ ifelse(is.na(.), 0, as.numeric(.)))) %>%
    mutate(across(all_of(columns), ~ ifelse(. == -999, NA, as.numeric(.))))
  
  return(data)
}

summarize_numeric_vars <- function(data, columns) {
  # Function to check if a column can be safely converted to numeric
  can_convert_to_numeric <- function(column) {
    converted <- suppressWarnings(as.numeric(column))
    all(!is.na(converted) | is.na(column))
  }
  
  # Loop through the specified columns
  for (col in columns) {
    if (!is.numeric(data[[col]])) {
      if (can_convert_to_numeric(data[[col]])) {
        data[[col]] <- as.numeric(data[[col]])
      } else {
        warning(paste("Column", col, "cannot be safely converted to numeric and will be excluded from the summary."))
      }
    }
  }
  
  # Select only numeric variables
  numeric_vars <- data %>% select(all_of(columns)) %>% select(where(is.numeric))
  
  # Summarize descriptive statistics
  if (ncol(numeric_vars) == 0) {
    stop("No numeric variables available for summary.")
  }
  
  summary_list <- lapply(numeric_vars, function(x) {
    c(
      mean = round(mean(x, na.rm = TRUE), 3),
      sd = round(sd(x, na.rm = TRUE), 3),
      min = round(min(x, na.rm = TRUE), 3),
      q1 = round(quantile(x, 0.25, na.rm = TRUE), 3),
      median = round(median(x, na.rm = TRUE), 3),
      q3 = round(quantile(x, 0.75, na.rm = TRUE), 3),
      max = round(max(x, na.rm = TRUE), 3),
      iqr = round(IQR(x, na.rm = TRUE), 3),
      n_valid = sum(!is.na(x))
    )
  })
  
  summary_table <- do.call(rbind, summary_list)
  summary_table <- as.data.frame(summary_table)
  summary_table$variable <- rownames(summary_table)
  rownames(summary_table) <- NULL
  summary_table <- summary_table %>% select(variable, everything())
  
  return(summary_table)
}


correlation_summary <- function(data, outcome_var, numeric_vars, weight_var, cluster_var) {
  # Load necessary libraries
  library(dplyr)
  library(tibble)
  library(survey)
  
  # Ensure the outcome variable, weight variable, and cluster variable exist in the dataframe
  if (!outcome_var %in% names(data)) {
    stop("The outcome variable does not exist in the dataframe.")
  }
  if (!weight_var %in% names(data)) {
    stop("The weight variable does not exist in the dataframe.")
  }
  if (!cluster_var %in% names(data)) {
    stop("The cluster variable does not exist in the dataframe.")
  }
  
  # Replace NA values in outcome_var with 0
  data[[outcome_var]][is.na(data[[outcome_var]])] <- 0
  
  # Filter numeric variables that exist in the dataframe
  numeric_vars <- numeric_vars[numeric_vars %in% names(data)]
  if (length(numeric_vars) == 0) {
    stop("No numeric variables available for correlation analysis.")
  }
  
  # Initialize an empty list to store the results
  results_list <- list()
  
  # Create survey design object
  survey_design <- tryCatch({
    svydesign(ids = ~get(cluster_var), data = data, weights = ~get(weight_var))
  }, error = function(e) {
    stop(paste("Error in creating survey design:", e$message))
  })
  
  # Loop through each numeric variable and calculate the correlation
  for (var in numeric_vars) {
    message(paste("Processing variable:", var))
    
    # Check for sufficient finite observations
    valid_obs <- sum(is.finite(data[[outcome_var]]) & is.finite(data[[var]]))
    if (valid_obs < 2) {
      warning(paste("Skipping variable", var, "due to insufficient finite observations."))
      next
    }
    
    cor_test <- tryCatch({
      # Calculate covariance matrix
      cov_matrix <- svyvar(~get(outcome_var) + get(var), survey_design)
      message(paste("Covariance matrix for", var, "calculated successfully."))
      print(head(cov_matrix))
      
      # Extract correlation coefficient
      r_value <- cov_matrix[1, 2] / sqrt(cov_matrix[1, 1] * cov_matrix[2, 2])
      message(paste("Correlation coefficient for", var, "calculated successfully."))
      
      # Calculate p-value (approximation)
      t_value <- r_value * sqrt((valid_obs - 2) / (1 - r_value^2))
      p_value <- 2 * pt(-abs(t_value), df = valid_obs - 2)
      message(paste("P-value for", var, "calculated successfully."))
      
      list(r_value = r_value, p_value = p_value)
    }, error = function(e) {
      warning(paste("Error in correlation test for variable", var, ":", e$message))
      return(NULL)
    })
    
    if (!is.null(cor_test)) {
      r_value <- cor_test$r_value
      p_value <- cor_test$p_value
      r_squared <- round(r_value^2, 3)
      sd_value <- round(sd(data[[var]], na.rm = TRUE), 3)
      
      results_list[[var]] <- tibble(
        variable = var,
        r_value = round(r_value, 3),
        p_value = round(p_value, 3),
        r_squared = r_squared,
        sd_value = sd_value,
        n_valid = valid_obs
      )
    }
  }
  
  # Combine the results into a single dataframe
  summary_table <- bind_rows(results_list)
  
  return(summary_table)
}

summarize_categorical_vars <- function(data, categorical_vars, weights_col) {
  # Ensure the categorical variables and weights column exist in the dataframe
  categorical_vars <- categorical_vars[categorical_vars %in% names(data)]
  if (length(categorical_vars) == 0) {
    stop("No categorical variables available for summary.")
  }
  if (!weights_col %in% names(data)) {
    stop("The weights column does not exist in the dataframe.")
  }
  
  # Initialize an empty list to store the results
  results_list <- list()
  
  # Loop through each categorical variable and calculate the frequency, weighted proportion, and valid responses
  for (var in categorical_vars) {
    summary_table <- data %>%
      group_by(.data[[var]]) %>%
      summarise(
        frequency = n(),
        weighted_proportion = round(sum(.data[[weights_col]], na.rm = TRUE) / sum(data[[weights_col]], na.rm = TRUE), 3),
        valid_responses = sum(!is.na(.data[[var]]))
      ) %>%
      mutate(variable = var, value = as.character(.data[[var]])) %>%
      select(variable, value, frequency, weighted_proportion, valid_responses)
    
    results_list[[var]] <- summary_table
  }
  
  # Combine the results into a single dataframe
  final_summary <- bind_rows(results_list)
  
  return(final_summary)
}

perform_chi_squared_tests <- function(data, count_var, categorical_vars, weight_var, cluster_var) {
  # Load necessary libraries
  library(dplyr)
  library(tibble)
  library(caret)  # For creating dummy variables
  library(survey)  # For survey design corrections
  
  # Ensure the count variable, weight variable, and cluster variable exist in the dataframe
  if (!count_var %in% names(data)) {
    stop("The count variable does not exist in the dataframe.")
  }
  if (!weight_var %in% names(data)) {
    stop("The weight variable does not exist in the dataframe.")
  }
  if (!cluster_var %in% names(data)) {
    stop("The cluster variable does not exist in the dataframe.")
  }
  
  # Replace NA values in count_var with 0
  data[[count_var]][is.na(data[[count_var]])] <- 0
  
  # Filter categorical variables that exist in the dataframe
  categorical_vars <- categorical_vars[categorical_vars %in% names(data)]
  if (length(categorical_vars) == 0) {
    stop("No categorical variables available for Chi-Square tests.")
  }
  
  # Initialize an empty list to store the results
  results_list <- list()
  
  # Loop through each categorical variable
  for (var in categorical_vars) {
    message(paste("Processing variable:", var))
    
    # Create dummy variables for the categorical variable
    dummies <- dummyVars(~ ., data = data[, var, drop = FALSE], fullRank = FALSE)
    dummy_data <- predict(dummies, newdata = data)
    dummy_data <- as.data.frame(dummy_data)  # Convert to data frame
    print(head(dummy_data))  # Debugging step: print the head of dummy_data
    
    # Add dummy variables to the original data
    data_with_dummies <- cbind(data, dummy_data)
    
    # Create survey design object with dummy variables
    survey_design <- tryCatch({
      svydesign(ids = ~get(cluster_var), data = data_with_dummies, weights = ~get(weight_var))
    }, error = function(e) {
      stop(paste("Error in creating survey design:", e$message))
    })
    
    # Loop through each dummy variable and perform the Chi-Square test
    for (dummy_var in colnames(dummy_data)) {
      message(paste("Processing dummy variable:", dummy_var))
      
      chi_test <- tryCatch({
        svychisq(as.formula(paste("~", count_var, "+", dummy_var)), survey_design)
      }, error = function(e) {
        warning(paste("Error in Chi-Square test for dummy variable", dummy_var, ":", e$message))
        return(NULL)
      })
      
      if (!is.null(chi_test)) {
        results_list[[dummy_var]] <- tibble(
          variable = dummy_var,
          chi_square_statistic = round(chi_test$statistic, 3),
          degrees_of_freedom = chi_test$parameter,
          p_value = round(chi_test$p.value, 3)
        )
      }
    }
  }
  
  # Combine the results into a single dataframe
  summary_table <- bind_rows(results_list)
  
  print(summary_table)  # Debugging step: print the summary table
  
  return(summary_table)
}

replace_na_and_convert_to_factor <- function(data, columns) {
  for (col_name in columns) {
    if (col_name %in% names(data)) {
      col <- data[[col_name]]
      
      # Convert to character first to handle NA replacement
      col_char <- as.character(col)
      col_char[is.na(col_char)] <- "NA"
      
      if (is.factor(col)) {
        if (is.ordered(col)) {
          # Preserve order
          unique_levels <- unique(c(levels(col), "NA"))
          col_factor <- factor(col_char, levels = unique_levels, ordered = TRUE)
        } else {
          unique_levels <- unique(c(levels(col), "NA"))
          col_factor <- factor(col_char, levels = unique_levels)
        }
      } else {
        # Convert to unordered factor with NA as a level
        col_factor <- factor(col_char)
      }
      
      data[[col_name]] <- col_factor
    } else {
      warning(sprintf("Column '%s' not found in data. Skipping.", col_name))
    }
  }
  return(data)
}

summarize_missing_values <- function(df) {
  missing_summary <- df %>%
    summarise(across(everything(), ~ sum(is.na(.)))) %>%
    pivot_longer(cols = everything(), names_to = "column", values_to = "missing_values")
  return(missing_summary)
}

required_sample_size_mmr <- function(expected_mmr = 500,
                                     desired_se = 100,
                                     crude_birth_rate = 35,
                                     recall_months = 12,
                                     avg_hh_size = 5,
                                     design_effect = 1.5,
                                     non_response_rate = 0.1) {
  # Adjust crude birth rate for recall period
  adjusted_cbr <- crude_birth_rate * (recall_months / 12)
  
  # Step 1: Calculate required number of live births (B)
  required_births <- (1e5 * sqrt(expected_mmr)) / desired_se
  
  # Step 2: Estimate total population required
  total_population_required <- required_births / (adjusted_cbr / 1000)
  
  # Step 3: Estimate number of households
  total_households_required <- total_population_required / avg_hh_size
  
  # Step 4: Adjust for design effect
  total_households_deff <- total_households_required * design_effect
  
  # Step 5: Adjust for non-response
  final_household_sample <- total_households_deff / (1 - non_response_rate)
  
  # Output
  cat("Expected MMR:", expected_mmr, "\n")
  cat("Desired SE:", desired_se, "\n")
  cat("Crude Birth Rate:", crude_birth_rate, "per 1,000/year\n")
  cat("Recall Period:", recall_months, "months\n")
  cat("Average Household Size:", avg_hh_size, "\n")
  cat("Design Effect:", design_effect, "\n")
  cat("Non-Response Rate:", non_response_rate * 100, "%\n")
  cat("Adjusted Crude Birth Rate:", round(adjusted_cbr, 2), "per 1,000 for recall period\n\n")
  cat("Required number of live births:", round(required_births), "\n")
  cat("Estimated population sample size:", round(total_population_required), "\n")
  cat("Estimated household sample size (pre-adjustment):", round(total_households_required), "\n")
  cat("After design effect:", round(total_households_deff), "\n")
  cat("Final household sample size (adjusted for non-response):", ceiling(final_household_sample), "\n")
  
  return(invisible(list(
    required_births = required_births,
    total_population_required = total_population_required,
    total_households_required = total_households_required,
    total_households_deff = total_households_deff,
    final_household_sample = final_household_sample
  )))
}

required_sisterhood_households <- function(
    expected_mmr = 1000,                  # Expected MMR (per 100,000 live births)
    absolute_precision = 150,            # Desired precision (± deaths per 100,000 live births)
    crude_birth_rate = 35,               # CBR (per 1,000 people per year)
    sisters_per_woman = 2.5,             # Avg number of sisters per respondent
    avg_years_exposure = 10,             # Avg years of exposure for sisters
    design_effect = 1.5,                 # Design effect
    non_response_rate = 0.10,            # Non-response rate
    avg_household_size = 5.0,            # Average household size
    prop_eligible_women_per_hh = 0.4     # Proportion of women 15–49 in household
) {
  # Step 1: Convert MMR precision to relative precision
  relative_precision <- absolute_precision / expected_mmr
  
  # Step 2: Estimate maternal deaths per sister-year
  births_per_woman_year <- crude_birth_rate / 1000
  maternal_deaths_per_sister_year <- (expected_mmr / 100000) * births_per_woman_year
  
  # Step 3: Target maternal deaths based on variance rule of thumb
  target_maternal_deaths <- 1 / (relative_precision^2)
  
  # Step 4: Required sister-years
  required_sister_years <- target_maternal_deaths / maternal_deaths_per_sister_year
  
  # Step 5: Number of sisters and respondents
  required_sisters <- required_sister_years / avg_years_exposure
  required_respondents <- required_sisters / sisters_per_woman
  
  # Step 6: Adjust for design effect and non-response
  respondents_adjusted <- required_respondents * design_effect
  respondents_final <- respondents_adjusted / (1 - non_response_rate)
  
  # Step 7: Estimate number of households
  eligible_women_per_hh <- avg_household_size * prop_eligible_women_per_hh
  total_households <- respondents_final / eligible_women_per_hh
  
  # Output
  cat("Expected MMR:", expected_mmr, "per 100,000 live births\n")
  cat("Desired absolute precision: ±", absolute_precision, "\n")
  cat("Target maternal deaths for this precision:", ceiling(target_maternal_deaths), "\n")
  cat("Required sister-years:", ceiling(required_sister_years), "\n")
  cat("Required respondents:", ceiling(respondents_final), "\n")
  cat("Estimated total households to sample:", ceiling(total_households), "\n")
  
  return(invisible(list(
    target_maternal_deaths = target_maternal_deaths,
    required_sister_years = required_sister_years,
    required_respondents = respondents_final,
    total_households = total_households
  )))
}

render_quality_report <- function(main_data = NULL,
                                  roster_data = NULL,
                                  deaths_data = NULL,
                                  lang = "en",
                                  start_date = "2024-06-01",
                                  end_date = "2025-07-30",
                                  group_var = "enum",
                                  
                                  exp_sexRatio = 1,
                                  exp_ageRatio_01_35 = 0.42,
                                  exp_ageRatio_05_10 = 0.51,
                                  exp_ageRatio_05_5plus = 0.15,
                                  exp_meanHH_size = 5,
                                  exp_deathsPer_hh = 0.0792,
                                  exp_birthsPer_hh = 0.05,
                                  
                                  output_file = "quality_report.pdf",
                                  output_dir = "reports") {
  
  if (is.null(main_data)) {
    stop("Error: 'dataset_main' must be provided.")
  }
  
  if (is.null(roster_data)) {
    stop("Error: 'rosterData' must be provided.")
  }
  
  if (is.null(deaths_data)) {
    stop("Error: 'deathsData' must be provided.")
  }
  
  if (is.null(uuid_main)) {
    stop("Error: 'uuid_main' must be provided.")
  }
  
  if (is.null(uuid_roster)) {
    stop("Error: 'uuid_roster' must be provided.")
  }
  
  if (is.null(uuid_deaths)) {
    stop("Error: 'uuid_deaths' must be provided.")
  }
  
  rmarkdown::render(
    input = "quality_report_draft.Rmd",         # Replace with your actual .Rmd file path
    output_file = output_file,         # Name of the output file
    output_dir = output_dir,           # Directory to save the report
    params = list(
      mainData = main_data,
      rosterData = roster_data,
      deathsData = deaths_data,
      startDate = start_date,
      endDate = end_date,
      language = lang,
      GroupVar = group_var,
      exp_sex_ratio = exp_sexRatio,
      exp_age_ratio_01_35 = exp_ageRatio_01_35,
      exp_age_ratio_05_10 = exp_ageRatio_05_10,
      exp_age_ratio_05_5plus = exp_ageRatio_05_5plus,
      exp_mean_hh_size = exp_meanHH_size,
      exp_deaths_per_hh = exp_deathsPer_hh,
      exp_births_per_hh = exp_birthsPer_hh
    ),
    envir = new.env()                  # Use a clean environment to avoid conflicts
  )
}

