source("src/utils/misc_utils.R")
source("src/utils/utils_cleaning.R")

library(dplyr)

options(scipen = 999)
options(dplyr.summarise.inform = FALSE)

# ------------------------------------------------------------------------------
## Detect Enumerator column
enum_colname <- names(raw.flag.fcs)[grepl("enum|team",names(raw.flag.fcs))]

if(length(enum_colname) == 1){
  yes_no <- svDialogs::dlg_message(paste0("Is '", enum_colname, "' the correct enumerator column?"), type = "yesno")$res
  enum_colname <- enum_colname
} else if (length(enum_colname) > 1){
  enum_colname <- tcltk::tk_select.list(enum_colname, title = "Enumerator Columns")
} else if (length(enum_colname) == 0 | yes_no == "no") {
  enum_colname <- svDialogs::dlg_input(message= "Enter the name of the Enumerator Column","enumerator")$res
}

