# add_hdds

add_hdds

## Usage

``` r
add_hdds(
  .dataset,
  fsl_hdds_cereals = "fsl_hdds_cereals",
  fsl_hdds_tubers = "fsl_hdds_tubers",
  fsl_hdds_veg = "fsl_hdds_veg",
  fsl_hdds_fruit = "fsl_hdds_fruit",
  fsl_hdds_meat = "fsl_hdds_meat",
  fsl_hdds_eggs = "fsl_hdds_eggs",
  fsl_hdds_fish = "fsl_hdds_fish",
  fsl_hdds_legumes = "fsl_hdds_legumes",
  fsl_hdds_dairy = "fsl_hdds_dairy",
  fsl_hdds_oil = "fsl_hdds_oil",
  fsl_hdds_sugar = "fsl_hdds_sugar",
  fsl_hdds_condiments = "fsl_hdds_condiments",
  yes_val = "yes",
  no_val = "no"
)
```

## Arguments

- .dataset:

  Main Dataset

- fsl_hdds_cereals:

  the name of the variable that indicates if cereals were consumed in
  the last 24 hours

- fsl_hdds_tubers:

  the name of the variable that indicates if roots or tubers were
  consumed in the last 24 hours

- fsl_hdds_veg:

  the name of the variable that indicates if vergetables were consumed
  in the last 24 hours

- fsl_hdds_fruit:

  the name of the variable that indicates if fruits were consumed in the
  last 24 hours

- fsl_hdds_meat:

  the name of the variable that indicates if meat were consumed in the
  last 24 hours

- fsl_hdds_eggs:

  the name of the variable that indicates if eggs were consumed in the
  last 24 hours

- fsl_hdds_fish:

  the name of the variable that indicates if fish were consumed in the
  last 24 hours

- fsl_hdds_legumes:

  the name of the variable that indicates if pulses or legumes were
  consumed in the last 24 hours

- fsl_hdds_dairy:

  the name of the variable that indicates if dairy were consumed in the
  last 24 hours

- fsl_hdds_oil:

  the name of the variable that indicates if oil were consumed in the
  last 24 hours

- fsl_hdds_sugar:

  the name of the variable that indicates if sugar were consumed in the
  last 24 hours

- fsl_hdds_condiments:

  the name of the variable that indicates if condiments were consumed in
  the last 24 hours

- yes_val:

  A character value in the dataset associated with "yes"

- no_val:

  A character value in the dataset associated with "no"

## Value

the dataset with fsl_hdds_score and fsl_hdds_cat computed, as well as
all the hdds columns recoded to 1 and 0 for yes and no as
fsl_hdds_x_recoded

## Examples

``` r
df1 <- data.frame(fsl_hdds_cereals = c("yes", "yes", "yes", "no", "yes", "no"),
  fsl_hdds_tubers = c("yes", "yes", "yes", "no", "yes", "no"),
  fsl_hdds_veg = c("no", "yes", "yes", "no", "yes", "no"),
  fsl_hdds_fruit = c("yes", "yes", "yes", "no", "yes", "no"),
  fsl_hdds_meat = c("no", "no", "yes", "no", "yes", "no"),
  fsl_hdds_eggs = c("yes", "no", "yes", "no", "yes", "no"),
  fsl_hdds_fish = c("yes", "yes", "yes", "no", "yes", "no"),
  fsl_hdds_legumes = c("yes", "no", "yes", "no", "yes", "no"),
  fsl_hdds_dairy = c("no", "yes", "yes", "no", "yes", "no"),
  fsl_hdds_oil = c("yes", "yes", "yes", "no", "no", "no"),
  fsl_hdds_sugar = c("yes", "yes", "no", "no", "yes", "no"),
  fsl_hdds_condiments = c("no", "yes", "yes", "no", "yes", "no")
)
add_hdds(.dataset = df1
)
#>   fsl_hdds_cereals fsl_hdds_tubers fsl_hdds_veg fsl_hdds_fruit fsl_hdds_meat
#> 1              yes             yes           no            yes            no
#> 2              yes             yes          yes            yes            no
#> 3              yes             yes          yes            yes           yes
#> 4               no              no           no             no            no
#> 5              yes             yes          yes            yes           yes
#> 6               no              no           no             no            no
#>   fsl_hdds_eggs fsl_hdds_fish fsl_hdds_legumes fsl_hdds_dairy fsl_hdds_oil
#> 1           yes           yes              yes             no          yes
#> 2            no           yes               no            yes          yes
#> 3           yes           yes              yes            yes          yes
#> 4            no            no               no             no           no
#> 5           yes           yes              yes            yes           no
#> 6            no            no               no             no           no
#>   fsl_hdds_sugar fsl_hdds_condiments fsl_hdds_cereals_recoded
#> 1            yes                  no                        1
#> 2            yes                 yes                        1
#> 3             no                 yes                        1
#> 4             no                  no                        0
#> 5            yes                 yes                        1
#> 6             no                  no                        0
#>   fsl_hdds_tubers_recoded fsl_hdds_veg_recoded fsl_hdds_fruit_recoded
#> 1                       1                    0                      1
#> 2                       1                    1                      1
#> 3                       1                    1                      1
#> 4                       0                    0                      0
#> 5                       1                    1                      1
#> 6                       0                    0                      0
#>   fsl_hdds_meat_recoded fsl_hdds_eggs_recoded fsl_hdds_fish_recoded
#> 1                     0                     1                     1
#> 2                     0                     0                     1
#> 3                     1                     1                     1
#> 4                     0                     0                     0
#> 5                     1                     1                     1
#> 6                     0                     0                     0
#>   fsl_hdds_legumes_recoded fsl_hdds_dairy_recoded fsl_hdds_oil_recoded
#> 1                        1                      0                    1
#> 2                        0                      1                    1
#> 3                        1                      1                    1
#> 4                        0                      0                    0
#> 5                        1                      1                    0
#> 6                        0                      0                    0
#>   fsl_hdds_sugar_recoded fsl_hdds_condiments_recoded fsl_hdds_score
#> 1                      1                           0              8
#> 2                      1                           1              9
#> 3                      0                           1             11
#> 4                      0                           0              0
#> 5                      1                           1             11
#> 6                      0                           0              0
#>   fsl_hdds_cat
#> 1         High
#> 2         High
#> 3         High
#> 4          Low
#> 5         High
#> 6          Low
```
