options(scipen = 999)
options(dplyr.summarise.inform = FALSE)

## Detect empty variables
is_empty_new <- function(x) {
  obj <- get(x)
  length(obj) == 0 || is.null(obj) || (is.character(obj) && all(obj == ""))
}
