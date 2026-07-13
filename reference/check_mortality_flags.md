# check_mortality_flags

check_mortality_flags

## Usage

``` r
check_mortality_flags(
  df,
  cause_death_f = c("post_partum", "during_pregnancy", "during_delivery")
)
```

## Arguments

- df:

  output dataframe long mortality from create_mortality_long_df

- cause_death_f:

  vector list of the cause of death options related to female

## Value

df_mortality with two extra flag columns: -flag_multiple_death
-flag_cause_death

## Examples

``` r
if (FALSE) { # \dontrun{
  check_mortality_flags(df_mortality,
  cause_death_f = c("post_partum","during_pregnancy","during_delivery"))
} # }
```
