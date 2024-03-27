#' check_wash_flags
#'
#' @param .dataset the raw dataset
#' @param data_container_loop loop_dataset for containers. By default NULL
#' @param container_type the name of the variable that indicates the type of the container including in the choice
#' the amount per liter (ex: choicex_20l)
#' @param container_litre_other the name of the variable that indicates the amount per liter for other containers
#' @param container_journey_collection the name of the variable that indicates the amount of journeys done
#' @param num_containers the name of the variable that indicates the number of containers per HH
#' @param wash_water_collect_time the name of the variable that indicates if water on premise
#' @param value_on_premise the value of the choice indicating the water is collected on premise
#' @param num_hh the name of the variable that indicates the number of people per HH
#' @param grouping the name of the variable that indicates the grouping variable - usually "enumerator"
#' @param uuid uuid variable
#'
#' @return a dataframe that includes all the logical flags related to WASH
#' This includes:
#' If loop containers available:
#' - litre_per_day_per_person
#' - litre_z_score
#' - flag_sd_litre
#' - flag_low_litre
#' - flag_high_litre
#' - flag_high_container
#' - flag_no_container
#' @export
#'
#' @examples
#' df <- data.frame(
#'   num_hh = c("3","4"),
#'   wash_num_containers = c("2","3"),
#'   enumerator = c("1","2"),
#'   wash_water_collect_time = c("on_premise", "num_minutes"),
#'   uuid = c("uuid_1","uuid_2")
#' )
#'
#' df_containers <- data.frame(
#'   uuid = c("uuid_1","uuid_1","uuid_2","uuid_2","uuid_2"),
#'   wash_container_type = c("bucket_20l", "bucket_14l", "jerry_can_10l",
#'                           "collapsible_jerry_can_5l" ,"other"),
#'   wash_container_litre_other = c(NA,NA,NA,NA,"30"),
#'   wash_container_journey_collection = c("1","2", "1","1","1")
#' )
#'
#' check_wash_flags(
#'   .dataset = df,
#'   data_container_loop = df_containers,
#'   grouping = "enumerator"
#' )

check_wash_flags <- function(.dataset,
                             data_container_loop = NULL,
                             container_type = "wash_container_type",
                             container_litre_other = "wash_container_litre_other",
                             container_journey_collection = "wash_container_journey_collection",
                             num_containers = "wash_num_containers",
                             wash_water_collect_time = "wash_water_collect_time",
                             value_on_premise = "on_premise",
                             num_hh = "num_hh",
                             grouping = NULL,
                             uuid = "uuid") {

  options(warn = -1)
  ## Throw an error if a dataset wasn't provided as a first argument
  if (!is.data.frame(.dataset)) {
    stop("First argument should be a dataset")
  }


  ## Throw an error if the dataset is empty
  if (nrow(.dataset) == 0) {
    stop("Dataset is empty")
  }

  if(!is.null(data_container_loop)){
    ## Throw an error if the dataset is empty
    if (!is.data.frame(data_container_loop)) {
      stop("data_container_loop should be a dataset")
    }
  }

  if(!is.null(data_container_loop)){
    ## Throw an error if the dataset is empty
    if (nrow(data_container_loop) == 0) {
      stop("raw.water_count_loop is empty")
    }
  }

  if (!uuid %in% names(.dataset)) stop("uuid argument incorrect, or not available in the dataset")

  if (is.null(grouping)) {
    .dataset <- .dataset %>% dplyr::mutate(group = "All")
  } else {
    .dataset <- .dataset %>% dplyr::mutate(group = !!rlang::sym(grouping))
  }

  results <- .dataset %>%
    dplyr::select(uuid, group)

  if(!is.null(data_container_loop)){
    ## calculate liters per person per day
    calculate_data_container_loop <- data_container_loop %>%
      dplyr::rowwise() %>%
      mutate(container_type_litre = stringr::str_remove(stringr::str_extract(!!rlang::sym(container_type), "([^\\__]+$)"), "l"),
             litre = ifelse(!!rlang::sym(container_type) == "other", as.numeric(!!rlang::sym(container_litre_other)),as.numeric(container_type_litre)),
             litre_per_day = ifelse(is.na(!!rlang::sym(container_journey_collection)), litre, litre * as.numeric(!!rlang::sym(container_journey_collection)))) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(uuid) %>%
      dplyr::summarise(litre_per_day_per_hh = sum(litre_per_day))

    results2 <- .dataset %>%
      dplyr::left_join(calculate_data_container_loop) %>%
      dplyr::mutate(litre_per_day_per_person = litre_per_day_per_hh / as.numeric(!!rlang::sym(num_hh)))

    ## FLAGS (Litres per person per day)
    mean_litre_dataset <-  mean(results2$litre_per_day_per_person, na.rm = T)
    sd_litre_dataset <- stats::sd(results2$litre_per_day_per_person, na.rm = T)

    results2 <- results2 %>%
      dplyr::mutate(litre_z_score = (litre_per_day_per_person - mean_litre_dataset) / sd_litre_dataset)

    mean_litre_zscore <- mean(results2$litre_z_score, na.rm = T)


    results2 <- results2 %>%
      dplyr::mutate(flag_sd_litre = ifelse(is.na(litre_z_score), NA,
                                           ifelse(litre_z_score < mean_litre_zscore - 3 | litre_z_score > mean_litre_zscore + 3, 1, 0)),
                    flag_low_litre = ifelse(is.na(litre_per_day_per_person), NA,
                                            ifelse(litre_per_day_per_person <= 1, 1, 0)),
                    flag_high_litre = ifelse(is.na(litre_per_day_per_person),NA,
                                             ifelse(litre_per_day_per_person >=50, 1, 0)),
                    flag_high_container = ifelse(is.na(!!rlang::sym(num_containers)),NA ,
                                                 ifelse(as.numeric(!!rlang::sym(num_containers)) > 20, 1, 0)),
                    flag_no_container = dplyr::case_when(is.na(!!rlang::sym(wash_water_collect_time)) & is.na(!!rlang::sym(num_containers)) ~ NA,
                                                         !!rlang::sym(wash_water_collect_time) != value_on_premise & !!rlang::sym(num_containers) == "0" ~ 1,
                                                         TRUE ~ 0)) %>%
      dplyr::select(wash_water_collect_time,num_containers,litre_per_day_per_person,
                    litre_z_score,flag_sd_litre,flag_low_litre,
                    flag_high_litre,flag_high_container,flag_no_container)

    if(!exists("results")){
      results <- results2
    } else {
      results <- cbind(results,results2)
    }
  } else {
    warning("No Container loop to check WASH flags.")
  }
  options(warn = 0)
  return(results)
}
