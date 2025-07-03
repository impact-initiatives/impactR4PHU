#' subch
#'
#' @param g figure to be included as a subchunk in rmarkdown
#' @param fig_height height of the figure
#' @param fig_width width of the figure
#'
#' @return a figure to be included as a subchunk in rmarkdown
#' @export

subch <- function(g, fig_height=7, fig_width=5) {
  g_deparsed <- paste0(deparse(function() {g}), collapse = '')
  sub_chunk <- paste0("\n\n<center>\n", "```{r sub_chunk_", as.numeric(Sys.time())*1000,
                      ", fig.height=", fig_height, ", fig.width=", fig_width, ", echo=FALSE}\n(",
                      g_deparsed, ")()\n```", "\n</center>")
  cat(knitr::knit(text = knitr::knit_expand(text = sub_chunk), quiet = TRUE))
}
