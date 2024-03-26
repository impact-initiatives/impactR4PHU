#' Negate %in%
#'
#' @return not in
#' @export
#'
#' @examples
#' a <- 2
#' a %notin% c(3,4)

`%notin%` <- Negate(`%in%`)
