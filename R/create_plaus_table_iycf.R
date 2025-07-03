#' Create the plausibility table for IYCF
#'
#' @param .flextable A flextable format of the return value of create_iycf_plaus
#'
#' @return a Flextable of the IYCF plausibility report
#' @export
#'
#' @examples
#' \dontrun{
#'   create_plaus_table_iycf(iycf_flextable)
#' }

create_plaus_table_iycf <- function(.flextable) {
  for (n in 1:12) {
    k <- as.numeric(n + 1)
    if ((n %% 2) == 0) {
      next
    } else {
      .flextable <- flextable::merge_at(.flextable, i = n:k, j = 1)
    }
  }
  for (n in 1:12) {
    k <- as.numeric(n + 1)
    if ((n %% 2) == 0) {
      next
    } else {
      .flextable <- flextable::merge_at(.flextable, i = n:k, j = 6)
    }
  }

  .flextable <- flextable::merge_at(.flextable, i = 1, j = 3:4)
  .flextable <- flextable::merge_at(.flextable, i = 2, j = 3:4)

  .flextable <- .flextable %>%
    flextable::delete_part(part = "header") %>%
    flextable::add_header_row(
      values = c("", "Values", ""),
      colwidths = c(1, 4, 1),
      top = TRUE
    ) %>%
    flextable::align(
      j = 2:6,
      align = "center",
      part = "body"
    ) %>%
    flextable::align(
      j = 2:6,
      align = "center",
      part = "header"
    ) %>%
    flextable::border_inner_h() %>%
    flextable::border_inner_v() %>%
    flextable::border_inner_h(part = "header") %>%
    flextable::surround(
      part = "header",
      border.top = flextable::fp_border_default(
        color = "black",
        style = "solid",
        width = 2
      )
    ) %>%
    flextable::surround(
      part = "header",
      border.bottom = flextable::fp_border_default(
        color = "black",
        style = "solid",
        width = 2
      )
    ) %>%
    flextable::surround(
      part = "header",
      border.left = flextable::fp_border_default(
        color = "black",
        style = "solid",
        width = 2
      )
    ) %>%
    flextable::surround(
      part = "header",
      border.right = flextable::fp_border_default(
        color = "black",
        style = "solid",
        width = 2
      )
    ) %>%
    flextable::surround(
      i = 1,
      j = 1:6,
      border.top = flextable::fp_border_default(
        color = "black",
        style = "solid",
        width = 3
      )
    ) %>%
    flextable::surround(
      i = 12,
      j = 1:6,
      border.bottom = flextable::fp_border_default(
        color = "black",
        style = "solid",
        width = 3
      )
    ) %>%
    flextable::surround(
      i = 1:12,
      j = 1,
      border.left = flextable::fp_border_default(
        color = "black",
        style = "solid",
        width = 3
      )
    ) %>%
    flextable::surround(
      i = 1:12,
      j = 6,
      border.right = flextable::fp_border_default(
        color = "black",
        style = "solid",
        width = 3
      )
    ) %>%
    flextable::surround(
      i = 14,
      j = 1:6,
      border.bottom = flextable::fp_border_default(
        color = "black",
        style = "solid",
        width = 3
      )
    ) %>%
    flextable::surround(
      i = 13:14,
      j = 1,
      border.left = flextable::fp_border_default(
        color = "black",
        style = "solid",
        width = 3
      )
    ) %>%
    flextable::surround(
      i = 13:14,
      j = 6,
      border.right = flextable::fp_border_default(
        color = "black",
        style = "solid",
        width = 3
      )
    ) %>%
    flextable::bold(j = 1) %>%
    flextable::bold(j = "Score") %>%
    flextable::bold(part = "header")

  return(.flextable)
}
