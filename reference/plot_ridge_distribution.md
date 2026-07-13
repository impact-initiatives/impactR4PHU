# Plot Ridge Distribution

Plot Ridge Distribution

## Usage

``` r
plot_ridge_distribution(
  .dataset,
  numeric_cols = NULL,
  name_groups = NULL,
  name_units = NULL,
  grouping = NULL,
  file_path = NULL,
  wdth = 5,
  hght = 5,
  title_name = NULL
)
```

## Arguments

- .dataset:

  output of the create_fsl_flags functions

- numeric_cols:

  a vector of the same fsl indicator score columns By default: NULL.

- name_groups:

  Name of the groups. By default: "Groups"

- name_units:

  Name of the units. By default: "Units"

- grouping:

  Variable name from the output create_fsl_flags for grouping

- file_path:

  File path for saving the plot in a directory

- wdth:

  width of the plot. By default: 5

- hght:

  height of the plot. By default: 5

- title_name:

  Title of the plot

## Value

A Ridge Plot with the distribution of values

## Examples

``` r
if (FALSE) { # \dontrun{
  plot_ridge_distribution(df)
} # }
```
