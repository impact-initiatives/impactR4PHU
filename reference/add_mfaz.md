# add_mfaz

add_mfaz

## Usage

``` r
add_mfaz(
  .dataset,
  nut_muac_cm = "nut_muac_cm",
  edema_confirm = "nut_edema_confirm",
  child_age_months = "child_age_months",
  child_sex = "child_sex",
  value_male_sex = "m",
  value_edema_confirm = "yes"
)
```

## Arguments

- .dataset:

  Child Nutrition Loop Dataset

- nut_muac_cm:

  the name of the variable that indicates the MUAC measurement by CM. By
  default it is nut_muac_cm

- edema_confirm:

  the name of the variable that indicates that edema is confirmed By
  default it is nut_edema_confirm

- child_age_months:

  the name of the variable that indicates the age of child per month. By
  default it is child_age_months

- child_sex:

  the name of the variable that indicates the sex of the child By
  default it is child_sex

- value_male_sex:

  the value of the choice "male" of the sex indicator By default it is m

- value_edema_confirm:

  the value of the choice "yes" of the nut_edema_confirm indicator By
  default it is "yes

## Value

the dataset with the severe_mfaz, global_mfaz, and moderate_mfaz
calcualted

## Examples

``` r
df1 <- data.frame(
uuid = c("uuid_1","uuid_2"),
nut_muac_cm = c("12.5","10"),
child_sex = c("m","f"),
child_age_months = c("14","54"),
nut_edema_confirm = c("yes",NA))

add_mfaz(.dataset = df1)
#> ================================================================================
#>     uuid nut_muac_cm child_sex child_age_months nut_edema_confirm sex
#> 1 uuid_1        12.5         m               14               yes   1
#> 2 uuid_2        10.0         f               54              <NA>   2
#>   age_months age_days  mfaz severe_mfaz moderate_mfaz global_mfaz
#> 1         14    423.5 -2.09           1             1           1
#> 2         54   1633.5 -5.31           1             0           1
```
