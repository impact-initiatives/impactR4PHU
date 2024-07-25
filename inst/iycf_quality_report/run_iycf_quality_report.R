rm(list = ls())
chooseCRANmirror(ind = 1)
utils::install.packages("renv")
options(renv.consent = TRUE)
renv::update(prompt = F)
source("src/utils.R")
library(tidyverse)
strings <- c(
  out_date = stringr::str_sub(stringr::str_remove_all(Sys.Date(), '-'), 3)
)

rmarkdown::render("iycf_quality_report_markdown.Rmd",
                  output_file = paste0("output/", "IYCF_Quality_Check_and_Plausibility_", strings['out_date'],".html"))
cat("\n> Quality Check completed! You can check your output folder.")
