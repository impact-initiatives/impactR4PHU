#------------------------------------------------------------------------------------------------------------
# utility operators & other miscellaneous functions
#------------------------------------------------------------------------------------------------------------

"%==%" <- function(a, b) ifelse(!is.na(a), a == b, F)
"%!=%" <- function(a, b) ifelse(!is.na(a), a != b, F)
"%_<_%" <- function(a, b) ifelse(!is.na(a), as.numeric(a) < b, F)
"%_<=_%" <- function(a, b) ifelse(!is.na(a), as.numeric(a) <= b, F)
"%_>_%" <- function(a, b) ifelse(!is.na(a), as.numeric(a) > b, F)
"%_>=_%" <- function(a, b) ifelse(!is.na(a), as.numeric(a) >= b, F)
"%_+_%" <- function(a, b) as.numeric(a) + as.numeric(b)
"%==na%" <- function(e1, e2) {
  ifelse(is.na(e1 == e2), is.na(e1) == is.na(e2), e1 == e2)
}
"%!=na%" <- function(e1, e2) {
  (e1 != e2 | (is.na(e1) & !is.na(e2)) | (is.na(e2) & !is.na(e1))) &
    !(is.na(e1) & is.na(e2))
}


qview <- function(datum, title = NULL, ..., n = NULL) {
  view(
    datum %>% select(-contains("/")),
    title = ifelse(is.null(title), "qview", title),
    ...,
    n = NULL
  )
}


# just a shorthand
isna <- function(x) is.na(x)

# utility to make an anychoice pattern from a vector of choices:
anychoice_pattern <- function(choices) paste0("(", choices, ")", collapse = "|")

# function to convert floats to percentages - requires 'scales' library
as_perc <- scales::label_percent(
  accuracy = 0.1,
  decimal.mark = ".",
  suffix = "%"
)

# transposing tibbles - function from https://stackoverflow.com/questions/42790219/how-do-i-transpose-a-tibble-in-r
transpose_df <- function(df) {
  t_df <- data.table::transpose(df)
  colnames(t_df) <- rownames(df)
  rownames(t_df) <- colnames(df)
  t_df <- t_df %>%
    tibble::rownames_to_column(.data = .) %>%
    tibble::as_tibble(.)
  return(t_df)
}

factorize <- function(
  x, # vector to be transformed
  min_freq = .01, # all levels < this % of records will be bucketed
  min_n = 1, # all levels < this # of records will be bucketed
  NA_level = '(NA)', # level created for NA values
  blank_level = '(blank)', # level created for "" values
  infrequent_level = 'Other', # level created for bucketing rare values
  infrequent_can_include_blank_and_NA = F, # default NA and blank are not bucketed
  order = T, # default to ordered
  reverse_order = F # default to increasing order
) {
  if (class(x) != 'factor') {
    x <- as.factor(x)
  }
  # suspect this is faster than reassigning new factor object
  levels(x) <- c(levels(x), NA_level, infrequent_level, blank_level)

  # Swap out the NA and blank categories
  x[is.na(x)] <- NA_level
  x[x == ''] <- blank_level

  # Going to use this table to reorder
  f_tb <- table(x, useNA = 'always')

  # Which levels will be bucketed?
  infreq_set <- c(
    names(f_tb[f_tb < min_n]),
    names(f_tb[(f_tb / sum(f_tb)) < min_freq])
  )

  # If NA and/or blank were infrequent levels above, this prevents bucketing
  if (!infrequent_can_include_blank_and_NA) {
    infreq_set <- infreq_set[!infreq_set %in% c(NA_level, blank_level)]
  }

  # Relabel all the infrequent choices
  x[x %in% infreq_set] <- infrequent_level

  # Return the reordered factor
  reorder(
    droplevels(x),
    rep(1 - (2 * reverse_order), length(x)),
    FUN = sum,
    order = order
  )
}


# add table to HTML
# add table to HTML
subch <- function(g, fig_height = 7, fig_width = 5) {
  g_deparsed <- paste0(
    deparse(function() {
      g
    }),
    collapse = ''
  )
  sub_chunk <- paste0(
    "\n\n<center>\n",
    "```{r sub_chunk_",
    as.numeric(Sys.time()) * 1000,
    ", fig.height=",
    fig_height,
    ", fig.width=",
    fig_width,
    ", echo=FALSE}\n(",
    g_deparsed,
    ")()\n```",
    "\n</center>"
  )
  cat(knitr::knit(text = knitr::knit_expand(text = sub_chunk), quiet = TRUE))
}

# add section to HTML
add_to_html.section <- function(name) cat(paste0("\n\n## ", name, " {.tabset}"))

# add title to HTML
add_to_html.title <- function(title) {
  cat(paste0(
    paste0(rep("\n", 2), collapse = ""),
    paste0(rep("#", 4), collapse = ""),
    " ",
    "<strong>",
    title,
    "</strong>"
  ))
}

# add subtitle to HTML
add_to_html.sub_title <- function(disaggregate.variable) {
  if (is.na(disaggregate.variable)) {
    cat(paste0(
      paste0(rep("\n", 2), collapse = ""),
      paste0(rep("#", 5), collapse = ""),
      " No disaggregation"
    ))
  } else {
    cat(paste0(
      paste0(rep("\n", 2), collapse = ""),
      paste0(rep("#", 5), collapse = ""),
      " Disaggregated by ",
      disaggregate.variable
    ))
  }
}
#--------------------------------------------------------------------------------------------------------------
# list to add to datatable to style the table
tableFormat <- list(
  dom = 'T<"clear">lfrtip',
  scrollX = TRUE
)
