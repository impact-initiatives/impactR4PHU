add_iycf <- function(.dataset,
                     age_months = "age_months",
                     grouping = NULL,
                     uuid = "uuid",
                     loop_index = NULL){
  ## Throw an error if a dataset wasn't provided as a first argument
  if (!is.data.frame(.dataset)) {
    stop("First argument should be a dataset")
  }

  if (nrow(.dataset) == 0) {
    stop("Dataset is empty")
  }


  if (is.null(grouping)) {
    .dataset <- .dataset %>% dplyr::mutate(group = "All")
  } else {
    .dataset <- .dataset %>% dplyr::mutate(group = !!rlang::sym(grouping))
  }

  if(!age_months %in% names(.dataset)) stop("Missing age_months column in dataset")

  if (is.null(loop_index)) {
    ## initiate the return output
    results <- .dataset %>%
      dplyr::mutate(loop_index = paste0("loop_nut_",dplyr::row_number())) %>%
      dplyr::select(uuid, group, loop_index,age_months)
  } else {
    results <- .dataset %>%
      dplyr::select(uuid, group, loop_index,age_months)
  }

  # "IYCF Indicator 1: Ever Breastfed; YES"

}


