#' Create the plausibility table for FSL
#'
#' @param .flextable A flextable format of the return value of create_fsl_plaus
#'
#' @return a Flextable of the FSL plausibility report
#' @export
#'
#' @examples
#' \dontrun{
#'   create_plaus_table_fsl(fsl_flextable)
#' }
create_plaus_table_fsl <- function(.flextable) {
  .flextable <- flextable::merge_at(.flextable, i = 1:12, j = 9)
  .flextable <- flextable::merge_at(.flextable, i = 13:22, j = 9)
  .flextable <- flextable::merge_at(.flextable, i = 23:24, j = 9)
  .flextable <- flextable::merge_at(.flextable, i = 25:34, j = 9)
  .flextable <- flextable::merge_at(.flextable, i = 35:46, j = 9)
  .flextable <- flextable::merge_at(.flextable, i = 47, j = 4:5)
  .flextable <- flextable::merge_at(.flextable, i = 48, j = 4:5)
  .flextable <- flextable::merge_at(.flextable, i = 47, j = 8:9)
  .flextable <- flextable::merge_at(.flextable, i = 48, j = 8:9)
  for (i in 1:36) {
    .flextable <- flextable::merge_at(.flextable, i = i, j = 2:3)
    .flextable <- flextable::merge_at(.flextable, i = i, j = 6:7)
  }

  for (n in 1:46) {
    k <- as.numeric(n + 1)
    if ((n %% 2) == 0) {
      next
    } else {
      .flextable <- flextable::merge_at(.flextable, i = n:k, j = 1)
      .flextable <- flextable::merge_at(.flextable, i = n:k, j = 8)
    }
  }

  for (i in 43:48) {
    .flextable <- flextable::merge_at(.flextable, i = i, j = 2:3)
    .flextable <- flextable::merge_at(.flextable, i = i, j = 6:7)
  }

  .flextable <- .flextable %>%
    flextable::delete_part(part = "header")%>%
    flextable::add_header_lines() %>%
    flextable::add_header_row(
      values = c("Criteria", "Excellent", "Good","Acceptable","Problematic","Score","Indicator Score"),
      colwidths = c(1,2,1,1,2,1,1), top = TRUE) %>%
    flextable::add_header_row(
      values = c("", "Values", "", ""),
      colwidths = c(1,6,1,1), top = TRUE)%>%
    flextable::align(
      j = 2:9,
      align = "center",
      part = "body") %>%
    flextable::align(
      j = 2:9,
      align = "center",
      part = "header") %>%
    flextable::align(
      j = 7,
      align = "center",
      part = "header") %>%
    flextable::border_inner_h() %>%
    flextable::border_inner_v() %>%
    flextable::border_outer() %>% flextable::border_inner_h(part="header") %>%
    flextable::surround(part = "header", border.top = flextable::fp_border_default(color= "black",
                                                                                   style = "solid",
                                                                                   width = 2)) %>%
    flextable::surround(part = "header", border.bottom = flextable::fp_border_default(color= "black",
                                                                                      style = "solid",
                                                                                      width = 2)) %>%
    flextable::surround(part = "header", border.left  = flextable::fp_border_default(color= "black",
                                                                                     style = "solid",
                                                                                     width = 2)) %>%
    flextable::surround(part = "header", border.right  = flextable::fp_border_default(color= "black",
                                                                                      style = "solid",
                                                                                      width = 2)) %>%
    flextable::surround(i = 1, j = 1:9, border.top = flextable::fp_border_default(color= "black",
                                                                                  style = "solid",
                                                                                  width = 3)) %>%
    flextable::surround(i = 12, j = 1:9, border.bottom = flextable::fp_border_default(color= "black",
                                                                                      style = "solid",
                                                                                      width = 3)) %>%
    flextable::surround(i = 1:12, j = 1, border.left  = flextable::fp_border_default(color= "black",
                                                                                     style = "solid",
                                                                                     width = 3)) %>%
    flextable::surround(i = 1:12, j = 9, border.right  = flextable::fp_border_default(color= "black",
                                                                                      style = "solid",
                                                                                      width = 3)) %>%
    flextable::surround(i = 13, j = 1:9, border.top = flextable::fp_border_default(color= "black",
                                                                                   style = "solid",
                                                                                   width = 3)) %>%
    flextable::surround(i = 22, j = 1:9, border.bottom = flextable::fp_border_default(color= "black",
                                                                                      style = "solid",
                                                                                      width = 3)) %>%
    flextable::surround(i = 13:22, j = 1, border.left  = flextable::fp_border_default(color= "black",
                                                                                      style = "solid",
                                                                                      width = 3)) %>%
    flextable::surround(i = 13:22, j = 9, border.right  = flextable::fp_border_default(color= "black",
                                                                                       style = "solid",
                                                                                       width = 3)) %>%
    flextable::surround(i = 23, j = 1:9, border.top = flextable::fp_border_default(color= "black",
                                                                                   style = "solid",
                                                                                   width = 3)) %>%
    flextable::surround(i = 24, j = 1:9, border.bottom = flextable::fp_border_default(color= "black",
                                                                                      style = "solid",
                                                                                      width = 3)) %>%
    flextable::surround(i = 23:24, j = 1, border.left  = flextable::fp_border_default(color= "black",
                                                                                      style = "solid",
                                                                                      width = 3)) %>%
    flextable::surround(i = 23:24, j = 9, border.right  = flextable::fp_border_default(color= "black",
                                                                                       style = "solid",
                                                                                       width = 3)) %>%
    flextable::surround(i = 25, j = 1:9, border.top = flextable::fp_border_default(color= "black",
                                                                                   style = "solid",
                                                                                   width = 3)) %>%
    flextable::surround(i = 34, j = 1:9, border.bottom = flextable::fp_border_default(color= "black",
                                                                                      style = "solid",
                                                                                      width = 3)) %>%
    flextable::surround(i = 25:34, j = 1, border.left  = flextable::fp_border_default(color= "black",
                                                                                      style = "solid",
                                                                                      width = 3)) %>%
    flextable::surround(i = 25:34, j = 9, border.right  = flextable::fp_border_default(color= "black",
                                                                                       style = "solid",
                                                                                       width = 3)) %>%
    flextable::surround(i = 35, j = 1:9, border.top = flextable::fp_border_default(color= "black",
                                                                                   style = "solid",
                                                                                   width = 3)) %>%
    flextable::surround(i = 46, j = 1:9, border.bottom = flextable::fp_border_default(color= "black",
                                                                                      style = "solid",
                                                                                      width = 3)) %>%
    flextable::surround(i = 35:46, j = 1, border.left  = flextable::fp_border_default(color= "black",
                                                                                      style = "solid",
                                                                                      width = 3)) %>%
    flextable::surround(i = 35:46, j = 9, border.right  = flextable::fp_border_default(color= "black",
                                                                                       style = "solid",
                                                                                       width = 3)) %>%
    flextable::surround(i = 47, j = 1:9, border.top = flextable::fp_border_default(color= "black",
                                                                                   style = "solid",
                                                                                   width = 3)) %>%
    flextable::surround(i = 48, j = 1:9, border.bottom = flextable::fp_border_default(color= "black",
                                                                                      style = "solid",
                                                                                      width = 3)) %>%
    flextable::surround(i = 47:48, j = 8, border.right  = flextable::fp_border_default(color= "black",
                                                                                       style = "solid",
                                                                                       width = 3)) %>%
    flextable::surround(i = 47:48, j = 1, border.left  = flextable::fp_border_default(color= "black",
                                                                                      style = "solid",
                                                                                      width = 3)) %>%

    flextable::bold(j = 1) %>%
    flextable::bold(j = "Score") %>%
    flextable::bold(part = "header")

  return(.flextable)
}
