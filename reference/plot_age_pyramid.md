# Plot Age Pyramid

Plot Age Pyramid

## Usage

``` r
plot_age_pyramid(
  .dataset,
  sex_column = "sex",
  age_years = "age_years",
  age_grouping = FALSE,
  file_path = NULL,
  wdth = 5,
  hght = 5,
  title_name = NULL
)
```

## Arguments

- .dataset:

  HH_roster Data with Age and Sex of individuals

- sex_column:

  the variable name in the Data indicating the sex of the individual By
  default: "sex"

- age_years:

  the variable name in the Data indicating the age of the individual By
  default: "age_years"

- age_grouping:

  If True, user using age_grouping in data with variable name
  "age_group". If default False, age_years will be grouped as followed.
  0-4/5-9/10-14/15-19/20-24/25-29/30-34/35-39/40-44/
  45-49/50-54/55-59/60-64/65-69/70-74/75-79/80-84/85+

- file_path:

  File path for saving the plot in a directory

- wdth:

  width of the plot. By default: 5

- hght:

  height of the plot. By default: 5

- title_name:

  Title of the plot

## Value

An Age Pyramid plot

## Examples

``` r
if (FALSE) { # \dontrun{
  plot_age_pyramid(hh_roster)
} # }
```
