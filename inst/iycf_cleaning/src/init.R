source("src/utils/kobo_utils.R")
source("src/utils/misc_utils.R")
source("src/utils/utils_cleaning.R")

library(dplyr)

options(scipen = 999)
options(dplyr.summarise.inform = FALSE)

## Detect empty variables
is_empty_new <- function(x) {
  obj <- get(x)
  length(obj) == 0 || is.null(obj) || (is.character(obj) && all(obj == ""))
}
