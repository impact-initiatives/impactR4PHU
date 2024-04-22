rm(list = ls())
chooseCRANmirror(ind = 1)
utils::install.packages("renv")
options(renv.consent = TRUE)
renv::restore(prompt = F)
source("src/utils.R")
library(tidyverse)
strings <- c(
  out_date = stringr::str_sub(stringr::str_remove_all(Sys.Date(), '-'), 3)
)
file <- list.files(pattern = ".Rmd", full.names = T)

rmarkdown::render(file,
                  output_file = paste0("output/", "Mort_Quality_Check_and_Plausibility_", strings['out_date'],".html"))
cat("\n> Quality Check completed! You can check your output folder.")
