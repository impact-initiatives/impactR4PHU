
anon_pattern <- paste0(".xlsx")
# packaging data:

filename_kobo_raw <- list.files("data/inputs/kobo_export/", full.names = T)
if(length(filename_kobo_raw) == 0) stop("Error: couldn't find filename_kobo_raw")
filename_data_final <- list.files("output/final/", full.names = T, pattern = anon_pattern)
if(length(filename_data_final) == 0) stop("Error: couldn't find filename_data_log")

filename_checking_audit <- list.files("output/checking/audit/", full.names = T)
filename_checking_outliers <- list.files("output/checking/outliers/", full.names = T)
filename_checking_requests <- list.files("output/checking/requests/", full.names = T)
filename_checking_responses <- list.files("output/checking/responses/", full.names = T)
# packaging cleaning and deletion logs, enum performance
filenames_dlog <- list.files("output/deletion_log/", full.names = T)
filenames_clog <- list.files("output/cleaning_log/", full.names = T)


# combine all cleaning log, except sensitive and already combined files



filenames_enum_performance <- list.files("output/enum_performance/", full.names = T)

# resources:
filenames_resources <- list.files("resources", "(template)|(tool)|(readme)", full.names = T)

# source:
filenames_R <- list.files(pattern = "*.R$|*.Rmd$", recursive = T, full.names = T)
filenames_R <- filenames_R[stringr::str_detect(filenames_R, "(api\\.key)|(validation)|(renv)", T)]

files_to_zip <- c(filename_kobo_raw,
                  filename_data_final,
                  filename_checking_audit,
                  filename_checking_outliers,
                  filename_checking_requests,
                  filename_checking_responses,
                  filenames_dlog,
                  filenames_clog,
                  filenames_enum_performance,
                  filenames_resources,
                  filenames_R)

####### WARNING - SENSITIVE ######
pwd <- strings['pwd']  # <- this is a password to the zip archive, kept in plain text. this means that obviously you don't push this file to git and don't share it
##################################
  
  
zip(paste0(dataset.name.short), files_to_zip, flags = paste("-P", pwd))

#    D O N E
cat("\n DONE \n")
#    D O N E
