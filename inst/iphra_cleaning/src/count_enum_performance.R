filename.dataset <- strings['filename.dataset']

kobo.raw <- readxl::read_excel(filename.dataset, col_types = "text") 
if("_uuid" %in% names(kobo.raw)) {
  kobo.raw <- kobo.raw %>%
    rename(uuid ="_uuid")
} else if ("_index" %in% names(kobo.raw)) {
  kobo.raw <- kobo.raw %>%
    rename(index = "_index")
}
deletion.log <- load.requests("output/deletion_log/","deletion_log")

cleaning.log <- load.requests("output/cleaning_log/","cleaning_log")


create.count_collected_enu(kobo.raw,     enum_colname)
create.count_deleted_enu(deletion.log, enum_colname)
create.count_enu_cleaning(cleaning.log, enum_colname)
if(language_assessment == "English"){
  cat("\nDone. Created 3 files in output/enum_performance.")
}else {
  cat("\nTerminé. Création de 3 fichiers dans output/enum_performance.")
}
