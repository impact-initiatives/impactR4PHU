rm(list = ls())

load("output/data_log/first_logical.rda")

## SET FILENAMES AND OTHER STRINGS  --------------------------------------------
strings <- c(
  out_date = stringr::str_sub(stringr::str_remove_all(Sys.Date(), '-'), 3)    # this one is appended to the end of filenames
)

# <- additional indicators and grouping variables are added here 
## TABULAR  -------------------------------------------------------------------
source('src/second_part_fixing_logical.R')
# -----------------------------------------------------------------------------`