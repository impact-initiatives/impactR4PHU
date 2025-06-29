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
  .flextable <- flextable::merge_at(.flextable, i = 1:14, j = 9)
  .flextable <- flextable::merge_at(.flextable, i = 15:22, j = 9)
  .flextable <- flextable::merge_at(.flextable, i = 23:24, j = 9)
  .flextable <- flextable::merge_at(.flextable, i = 25:30, j = 9)
  .flextable <- flextable::merge_at(.flextable, i = 31:42, j = 9)
  .flextable <- flextable::merge_at(.flextable, i = 43, j = 4:5)
  .flextable <- flextable::merge_at(.flextable, i = 44, j = 4:5)
  .flextable <- flextable::merge_at(.flextable, i = 43, j = 8:9)
  .flextable <- flextable::merge_at(.flextable, i = 44, j = 8:9)

  for (i in 1:32) {
    .flextable <- flextable::merge_at(.flextable, i = i, j = 2:3)
    .flextable <- flextable::merge_at(.flextable, i = i, j = 6:7)
  }

  for (i in 1:24) {
    .flextable <- flextable::merge_at(.flextable, i = i, j = 4:5)
  }

  for (i in 27:32) {
    .flextable <- flextable::merge_at(.flextable, i = i, j = 4:5)
  }

  for (n in 1:42) {
    k <- as.numeric(n + 1)
    if ((n %% 2) == 0) {
      next
    } else {
      .flextable <- flextable::merge_at(.flextable, i = n:k, j = 1)
      .flextable <- flextable::merge_at(.flextable, i = n:k, j = 8)
    }
  }

  for (i in 39:44) {
    .flextable <- flextable::merge_at(.flextable, i = i, j = 2:3)
    .flextable <- flextable::merge_at(.flextable, i = i, j = 4:5)
    .flextable <- flextable::merge_at(.flextable, i = i, j = 6:7)
  }

  .flextable <- .flextable %>%
    flextable::delete_part(part = "header") %>%
    flextable::add_header_lines() %>%
    flextable::add_header_row(
      values = c("Criteria", "Values", "Score", "Indicator Score"),
      colwidths = c(1, 6, 1, 1),
      top = TRUE
    ) %>%
    flextable::align(
      j = 2:9,
      align = "center",
      part = "body"
    ) %>%
    flextable::align(
      j = 2:9,
      align = "center",
      part = "header"
    ) %>%
    flextable::align(
      j = 7,
      align = "center",
      part = "header"
    ) %>%
    flextable::border_inner_h() %>%
    flextable::border_inner_v() %>%
    flextable::border_outer() %>%
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
      j = 1:9,
      border.top = flextable::fp_border_default(
        color = "black",
        style = "solid",
        width = 3
      )
    ) %>%
    flextable::surround(
      i = 14,
      j = 1:9,
      border.bottom = flextable::fp_border_default(
        color = "black",
        style = "solid",
        width = 3
      )
    ) %>%
    flextable::surround(
      i = 1:14,
      j = 1,
      border.left = flextable::fp_border_default(
        color = "black",
        style = "solid",
        width = 3
      )
    ) %>%
    flextable::surround(
      i = 1:14,
      j = 9,
      border.right = flextable::fp_border_default(
        color = "black",
        style = "solid",
        width = 3
      )
    ) %>%
    flextable::surround(
      i = 15,
      j = 1:9,
      border.top = flextable::fp_border_default(
        color = "black",
        style = "solid",
        width = 3
      )
    ) %>%
    flextable::surround(
      i = 22,
      j = 1:9,
      border.bottom = flextable::fp_border_default(
        color = "black",
        style = "solid",
        width = 3
      )
    ) %>%
    flextable::surround(
      i = 15:22,
      j = 1,
      border.left = flextable::fp_border_default(
        color = "black",
        style = "solid",
        width = 3
      )
    ) %>%
    flextable::surround(
      i = 15:22,
      j = 9,
      border.right = flextable::fp_border_default(
        color = "black",
        style = "solid",
        width = 3
      )
    ) %>%
    flextable::surround(
      i = 23,
      j = 1:9,
      border.top = flextable::fp_border_default(
        color = "black",
        style = "solid",
        width = 3
      )
    ) %>%
    flextable::surround(
      i = 24,
      j = 1:9,
      border.bottom = flextable::fp_border_default(
        color = "black",
        style = "solid",
        width = 3
      )
    ) %>%
    flextable::surround(
      i = 23:24,
      j = 1,
      border.left = flextable::fp_border_default(
        color = "black",
        style = "solid",
        width = 3
      )
    ) %>%
    flextable::surround(
      i = 23:24,
      j = 9,
      border.right = flextable::fp_border_default(
        color = "black",
        style = "solid",
        width = 3
      )
    ) %>%
    flextable::surround(
      i = 25,
      j = 1:9,
      border.top = flextable::fp_border_default(
        color = "black",
        style = "solid",
        width = 3
      )
    ) %>%
    flextable::surround(
      i = 30,
      j = 1:9,
      border.bottom = flextable::fp_border_default(
        color = "black",
        style = "solid",
        width = 3
      )
    ) %>%
    flextable::surround(
      i = 25:30,
      j = 1,
      border.left = flextable::fp_border_default(
        color = "black",
        style = "solid",
        width = 3
      )
    ) %>%
    flextable::surround(
      i = 25:30,
      j = 9,
      border.right = flextable::fp_border_default(
        color = "black",
        style = "solid",
        width = 3
      )
    ) %>%
    flextable::surround(
      i = 31,
      j = 1:9,
      border.top = flextable::fp_border_default(
        color = "black",
        style = "solid",
        width = 3
      )
    ) %>%
    flextable::surround(
      i = 42,
      j = 1:9,
      border.bottom = flextable::fp_border_default(
        color = "black",
        style = "solid",
        width = 3
      )
    ) %>%
    flextable::surround(
      i = 31:42,
      j = 1,
      border.left = flextable::fp_border_default(
        color = "black",
        style = "solid",
        width = 3
      )
    ) %>%
    flextable::surround(
      i = 31:42,
      j = 9,
      border.right = flextable::fp_border_default(
        color = "black",
        style = "solid",
        width = 3
      )
    ) %>%
    flextable::surround(
      i = 43,
      j = 1:9,
      border.top = flextable::fp_border_default(
        color = "black",
        style = "solid",
        width = 3
      )
    ) %>%
    flextable::surround(
      i = 44,
      j = 1:9,
      border.bottom = flextable::fp_border_default(
        color = "black",
        style = "solid",
        width = 3
      )
    ) %>%
    flextable::surround(
      i = 43:44,
      j = 9,
      border.right = flextable::fp_border_default(
        color = "black",
        style = "solid",
        width = 3
      )
    ) %>%
    flextable::surround(
      i = 43:44,
      j = 1,
      border.left = flextable::fp_border_default(
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
