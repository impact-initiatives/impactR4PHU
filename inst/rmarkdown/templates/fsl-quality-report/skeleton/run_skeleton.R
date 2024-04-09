setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())
chooseCRANmirror(ind = 1)
utils::install.packages("rmarkdown")
utils::install.packages("htmltools", )
options(renv.consent = TRUE)
strings <- c(
  out_date = stringr::str_sub(stringr::str_remove_all(Sys.Date(), '-'), 3)
)
file <- list.files(pattern = ".Rmd", full.names = T)
if(!dir.exists("output")){
  dir.create("output")
}
rmarkdown::render(file,
                  output_file = paste0("output/", "FSL_Quality_Check_and_Plausibility_", strings['out_date'],".html"))
cat("\n> Quality Check completed! You can check your output folder.")
