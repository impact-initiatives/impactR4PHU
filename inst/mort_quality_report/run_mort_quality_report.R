rm(list = ls())
chooseCRANmirror(ind = 1)
utils::install.packages("renv")
options(renv.consent = TRUE)
renv::update(prompt = F)
source("src/utils.R")
source("src/utils/kobo_utils.R")
source("src/utils/utils_cleaning.R")
source("src/utils/misc_utils.R")
library(tidyverse)
strings <- c(
  out_date = stringr::str_sub(stringr::str_remove_all(Sys.Date(), '-'), 3)
)

type_assessment <- tcltk::tk_select.list(
  c("Household", "Individual"),
  title = "Asessment Level"
)
if (type_assessment == "Individual") {
  rmarkdown::render(
    "mort_quality_report_markdown_ind.Rmd",
    output_file = paste0(
      "output/",
      "Mort_Quality_Check_and_Plausibility_",
      strings['out_date'],
      ".html"
    )
  )
} else if (type_assessment == "Household") {
  rmarkdown::render(
    "mort_quality_report_markdown_hh.Rmd",
    output_file = paste0(
      "output/",
      "Mort_Quality_Check_and_Plausibility_",
      strings['out_date'],
      ".html"
    )
  )
}
cat("\n> Quality Check completed! You can check your output folder.")
