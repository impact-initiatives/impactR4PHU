# Find the label of a variable

Find the label of a variable

## Usage

``` r
get.label(
  variable = NULL,
  tool.survey = NULL,
  label_colname = "label::English"
)
```

## Arguments

- variable:

  This is the name of the header from raw data. By default NULL

- tool.survey:

  This is the tool.survey dataset. By default NULL

- label_colname:

  This is the label column name in the tool survey. By default
  label::English

## Value

return label will be the one for the base question itself (e.g. if
variable == "water_source", the result is the label of "What are the
water sources in your household")

## Examples

``` r
if (FALSE) { # \dontrun{
label <- get.label("water_source", tool.survey = tool.survey)
} # }
```
