# check_health_flags

check_health_flags

## Usage

``` r
check_health_flags(
  .dataset,
  monthly_expenditures = c("cm_expenditure_frequent_food",
    "cm_expenditure_frequent_rent", "cm_expenditure_frequent_water",
    "cm_expenditure_frequent_nfi", "cm_expenditure_frequent_utilitiues",
    "cm_expenditure_frequent_fuel", "cm_expenditure_frequent_transportation",
    "cm_expenditure_frequent_communication", "cm_expenditure_frequent_other"),
  health_expenditure_col = "cm_expenditure_infrequent_health",
  periodic_expenditures = c("cm_expenditure_infrequent_shelter",
    "cm_expenditure_infrequent_nfi", "cm_expenditure_infrequent_health",
    "cm_expenditure_infrequent_education", "cm_expenditure_infrequent_debt",
    "cm_expenditure_infrequent_other"),
  num_period_months = 6,
  uuid = "uuid"
)
```

## Arguments

- .dataset:

  the raw dataset with all add_x indicators functions called in

- monthly_expenditures:

  the vector of the frequent expenditure variables. By default including
  (food,rent,water,nfi,utilities,fuel,transportation,communication,others)

- health_expenditure_col:

  the name of the variable that indicates the infrequent expenditure on
  health.

- periodic_expenditures:

  the vector of the infrequent expenditure variables. By default
  including (shelter,nfi,health,education,debt,others)

- num_period_months:

  the number of months that cover the infrequent expenditure variables.
  By default: 6

- uuid:

  uuid variable

## Value

a dataframe that includes all the logical flags related to health This
includes: - flag_severe_health_exp - flag_catastrophic_health_exp

## Examples

``` r
if (FALSE) check_health_flags(.dataset = df1) # \dontrun{}
```
