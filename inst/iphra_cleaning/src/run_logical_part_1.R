rm(list = ls())

load("output/data_log/final_translation.rda")

## SET FILENAMES AND OTHER STRINGS  --------------------------------------------
strings <- c(
  out_date = stringr::str_sub(stringr::str_remove_all(Sys.Date(), '-'), 3)    # this one is appended to the end of filenames
)

# <- additional indicators and grouping variables are added here 
## TABULAR  -------------------------------------------------------------------
source('src/first_part_logical_checks.R')
# -----------------------------------------------------------------------------`