rm(list = ls())
chooseCRANmirror(ind = 1)
utils::install.packages("renv")
options(renv.consent = TRUE)
renv::update(prompt = F)
source("src/init.R")
library(tidyverse)
strings <- c(
  out_date = stringr::str_sub(stringr::str_remove_all(Sys.Date(), '-'), 3)
)

rmarkdown::render(
  "fsl_cleaning_markdown.Rmd",
  output_file = paste0("output/", "FSL_Cleaning_", strings['out_date'], ".html")
)
cat("\n> Cleaning completed! You can check your output folder.")
