# Plot Age Distribution

Plot Age Distribution

## Usage

``` r
plot_age_distribution(
  .dataset,
  by_group = NULL,
  year_or_month = NULL,
  age_years = "age_years",
  age_months = "age_months",
  min_age = NULL,
  max_age = NULL,
  breaks = NULL,
  file_path = NULL,
  wdth = 5,
  hght = 5,
  title_name = NULL
)
```

## Arguments

- .dataset:

  HH_roster Data with Age and Sex of individuals

- by_group:

  Disaggregated variable

- year_or_month:

  Month or Year Plot. "year" for Year, "month" for Month. By deault it
  will calculate year.

- age_years:

  the variable name in the Data indicating the year age of the
  individual By default: "age_years"

- age_months:

  the variable name in the Data indicating the month age of the
  individual By default: "age_months"

- min_age:

  Minimum Age cutoff. By default 0 for year and month.

- max_age:

  Maximum Age cutoff. By default 5 for year and 59 for month.

- breaks:

  Breaks of the Bins. By default 1 for year and 12 for month.

- file_path:

  File path for saving the plot in a directory

- wdth:

  width of the plot. By default: 5

- hght:

  height of the plot. By default: 5

- title_name:

  Title of the plot

## Value

an age per year or month distribution plot

## Examples

``` r
if (FALSE) { # \dontrun{
  plot_age_distribution(hh_roster)
} # }
```
