setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())
strings <- c(
  out_date = stringr::str_sub(stringr::str_remove_all(Sys.Date(), '-'), 3)
)
file <- list.files(pattern = ".Rmd", full.names = T)
if(!dir.exists("output")){
  dir.create("output")
}
rmarkdown::render(file,
                  output_file = paste0("output/", "FSL_Logical_Checks_Cleaning_LogBook_", strings['out_date'],".html"))
cat("\n> Logical Checks and Cleaning LogBook completed! You can check your output folder.")
