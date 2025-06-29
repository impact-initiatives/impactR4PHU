#' Negate in pipe
#'
#' @param a - a variable present in the environment
#' @param b - a variable present in the environment
#'
#' @return not in
#'
#' @examples
#' \dontrun{
#' a <- 2
#' a %notin% c(3,4)
#' }

"%notin%" <- function(a, b) {
  !a %in% b
}
