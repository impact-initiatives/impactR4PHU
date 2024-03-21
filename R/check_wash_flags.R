#' check_wash_flags
#'
#' @param .dataset the raw dataset
#' @param data_container_loop loop_dataset for containers. By default NULL
#' @param container_type the name of the variable that indicates the type of the container including in the choice
#' the amount per liter (ex: choicex_20l)
#' @param container_litre_other the name of the variable that indicates the amount per liter for other containers
#' @param container_journey_collection the name of the variable that indicates the amount of journeys done
#' @param num_containers the name of the variable that indicates the number of containers per HH
#' @param water_source the name of the variable that indicates the main water source
#' @param different_water_source the name of the variable that indicates the other water source (Select_multiple up to 2)
#' by default NULL, but better to call it if you collect this indicator.
#' @param water_collect_time the name of the variable that indicates the collection time
#' @param choice_inside_compound the name of the choice inside the water_collection_time variable that indicate
#' that the collection time is none because water source is inside the compound. By default: (inside_compound)
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
#' - flag_not_immediate
#' If loop containers not available:
#' - flag_not_immediate
#' @export
#'
#' @examples
#' df <- data.frame(
#'   num_hh = c("3","4"),
#'   wash_num_containers = c("2","3"),
#'   enumerator = c("1","2"),
#'   wash_water_source = c("asdasd", "piped_dwelling"),
#'   wash_water_collect_time = c("inside_compound", "aasdasd"),
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
                             water_source = "wash_water_source",
                             different_water_source = NULL,
                             water_collect_time = "wash_water_collect_time",
                             choice_inside_compound = "inside_compound",
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
    if (nrow(data_container_loop) == 0) {
      stop("raw.water_count_loop is empty")
    }
  }


  if (is.null(grouping)) {
    .dataset <- .dataset %>% dplyr::mutate(group = "All")
    grouping <- "group"
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
    if(is.null(different_water_source)){
      results2 <- results2 %>%
        dplyr::mutate(on_premise = dplyr::case_when(is.na(!!rlang::sym(water_source)) ~ NA,
                                                    !!rlang::sym(water_source) %in% c("piped_dwelling",
                                                                                      "piped_compound",
                                                                                      "rainwater_collection") ~ 1,
                                                    TRUE ~ 0))
    } else {
      results2 <- results2 %>%
        dplyr::mutate(on_premise = dplyr::case_when(is.na(!!rlang::sym(water_source)) &
                                                      is.na(!!rlang::sym(different_water_source)) ~ NA,
                                                    !!rlang::sym(water_source) %in% c("piped_dwelling",
                                                                                      "piped_compound",
                                                                                      "rainwater_collection") |
                                                      stringr::str_detect(!!rlang::sym(different_water_source),
                                                                          "piped_dwelling|piped_compound|rainwater_collection") ~ 1,
                                                    TRUE ~ 0))
    }

    results2 <- results2 %>%
      dplyr::mutate(flag_sd_litre = ifelse(is.na(litre_z_score), NA,
                                           ifelse(litre_z_score < mean_litre_zscore - 3 | litre_z_score > mean_litre_zscore + 3, 1, 0)),
                    flag_low_litre = ifelse(is.na(litre_per_day_per_person), NA,
                                            ifelse(litre_per_day_per_person <= 1, 1, 0)),
                    flag_high_litre = ifelse(is.na(litre_per_day_per_person),NA,
                                             ifelse(litre_per_day_per_person >=50, 1, 0)),
                    flag_high_container = ifelse(is.na(!!rlang::sym(num_containers)),NA ,
                                                 ifelse(as.numeric(!!rlang::sym(num_containers)) > 20, 1, 0)),
                    flag_no_container = dplyr::case_when(is.na(on_premise) & is.na(!!rlang::sym(num_containers)) ~ NA,
                                                         on_premise == 1 & is.na(!!rlang::sym(num_containers)) ~ 1,
                                                         TRUE ~ 0),
                    flag_not_immediate = dplyr::case_when(is.na(on_premise) & is.na(!!rlang::sym(water_collect_time)) ~ NA,
                                                          on_premise == 1 & !!rlang::sym(water_collect_time) != "inside_compound" ~ 1,
                                                          TRUE ~ 0)) %>%
      dplyr::select(water_source,different_water_source,
                    num_containers,litre_per_day_per_person,
                    litre_z_score,water_collect_time,flag_sd_litre,flag_low_litre,
                    flag_high_litre,flag_high_container,flag_no_container,flag_not_immediate)

    if(!exists("results")){
      results <- results2
    } else {
      results <- cbind(results,results2)
    }
  } else {
    results2 <- .dataset %>%
      dplyr::mutate(flag_not_immediate = dplyr::case_when(is.na(on_premise) & is.na(!!rlang::sym(water_collect_time)) ~ NA,
                                                          on_premise == 1 & !!rlang::sym(water_collect_time) != "inside_compound" ~ 1,
                                                          TRUE ~ 0)) %>%
      dplyr::select(water_source,water_collect_time,flag_not_immediate)
    if(!exists("results")){
      results <- results2
    } else {
      results <- cbind(results,results2)
    }
  }
  options(warn = 0)
  return(results)
}
