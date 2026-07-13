# Plot Correlogram

Plot Correlogram

## Usage

``` r
plot_correlogram(
  .dataset,
  numeric_cols = c("fsl_fcs_score", "fsl_rcsi_score", "fsl_hhs_score"),
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

  a vector of the fsl indicator scores. By default: c("fsl_fcs_score",
  "fsl_rcsi_score", "fsl_hhs_score")

- file_path:

  File path for saving the plot in a directory

- wdth:

  width of the plot. By default: 5

- hght:

  height of the plot. By default: 5

- title_name:

  Title of the plot

## Value

a Correlogram plot

## Examples

``` r
if (FALSE) { # \dontrun{
  plot_correlogram(.dataset)
} # }
```
