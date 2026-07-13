# create_iycf_plaus

create_iycf_plaus

## Usage

``` r
create_iycf_plaus(
  df_iycf,
  age_months = NULL,
  sex = NULL,
  iycf_8 = NULL,
  iycf_caregiver = NULL,
  yes_value_caregiver = NULL,
  no_value_caregiver = NULL,
  exp_prevalence_mad = NULL,
  exp_sex_ratio = NULL,
  exp_ratio_under6m_6to23m = NULL,
  grouping = NULL,
  uuid = "uuid",
  short_report = FALSE,
  file_path = NULL
)
```

## Arguments

- df_iycf:

  dataframe output of the check_iycf_flag function

- age_months:

  the name of the variable that indicates the age in month of the child

- sex:

  the name of the variable that indicates the sex of the child

- iycf_8:

  the name of the variable that indicates if the meal frequency the
  child had yesterday. By default "iycf_8"

- iycf_caregiver:

  the name of the variable that indicates if the caregiver of the child
  is present. By default NULL

- yes_value_caregiver:

  the value of the choice "yes" to all the caregiver column

- no_value_caregiver:

  the value of the choice "no" to all the caregiver column

- exp_prevalence_mad:

  Expected prevalence for Minimum Acceptable Diet (MAD) By default:
  0.3:0.7

- exp_sex_ratio:

  Expected sex ratio. By default: 1:1

- exp_ratio_under6m_6to23m:

  Expected age ratio between children under 6 month, and children
  between 6 and 23 months. By default: 1:4

- grouping:

  the name of the variable that indicates the grouping variable -
  usually "enumerator"

- uuid:

  uuid variable

- short_report:

  Inputs a boolean value TRUE or FALSE to return just key variables. If
  FALSE, returns a dataframe of all the variables calculated.

- file_path:

  Inputs an optional character value specifying the file location to
  save a copy of the results.

## Value

a dataframe with all IYCF related plausibility columns

## Examples

``` r
if (FALSE) { # \dontrun{
  create_iycf_plaus(df_iycf)
} # }
```
