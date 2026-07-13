# create_fsl_plaus

create_fsl_plaus

## Usage

``` r
create_fsl_plaus(
  .dataset,
  fsl_fcs_cereal = "fsl_fcs_cereal",
  fsl_fcs_legumes = "fsl_fcs_legumes",
  fsl_fcs_dairy = "fsl_fcs_dairy",
  fsl_fcs_meat = "fsl_fcs_meat",
  fsl_fcs_veg = "fsl_fcs_veg",
  fsl_fcs_fruit = "fsl_fcs_fruit",
  fsl_fcs_oil = "fsl_fcs_oil",
  fsl_fcs_sugar = "fsl_fcs_sugar",
  fsl_rcsi_lessquality = "fsl_rcsi_lessquality",
  fsl_rcsi_borrow = "fsl_rcsi_borrow",
  fsl_rcsi_mealsize = "fsl_rcsi_mealsize",
  fsl_rcsi_mealadult = "fsl_rcsi_mealadult",
  fsl_rcsi_mealnb = "fsl_rcsi_mealnb",
  fsl_fcs_score = "fsl_fcs_score",
  fsl_rcsi_score = "fsl_rcsi_score",
  fsl_hhs_score = "fsl_hhs_score",
  fsl_hdds_score = "fsl_hdds_score",
  grouping = NULL,
  uuid = "uuid",
  short_report = FALSE,
  file_path = NULL
)
```

## Arguments

- .dataset:

  raw/clean data with all calculated fcs/rcsi/hhs/hdds/fcm/fclcm add_x
  indicators

- fsl_fcs_cereal:

  the name of the variable that indicates the number of days cereals
  were consumed

- fsl_fcs_legumes:

  the name of the variable that indicates the number of days legumes
  were consumed

- fsl_fcs_dairy:

  the name of the variable that indicates the number of days dairy were
  consumed

- fsl_fcs_meat:

  the name of the variable that indicates the number of days meat were
  consumed

- fsl_fcs_veg:

  the name of the variable that indicates the number of days vegetables
  were consumed

- fsl_fcs_fruit:

  the name of the variable that indicates the number of days fruit were
  consumed

- fsl_fcs_oil:

  the name of the variable that indicates the number of days oil was
  consumed

- fsl_fcs_sugar:

  the name of the variable that indicates the number of days sugar was
  consumed

- fsl_rcsi_lessquality:

  Column representing question- During the last 7 days, were there days
  (and, if so, how many) when your household had to rely on less
  preferred and less expensive food to cope with a lack of food or money
  to buy it?

- fsl_rcsi_borrow:

  Column representing question- During the last 7 days, were there days
  (and, if so, how many) when your household had to borrow food or rely
  on help from a relative or friend to cope with a lack of food or money
  to buy it?

- fsl_rcsi_mealsize:

  Column representing question- During the last 7 days, were there days
  (and, if so, how many) when your household had to limit portion size
  of meals at meal times to cope with a lack of food or money to buy it?

- fsl_rcsi_mealadult:

  Column representing question- During the last 7 days, were there days
  (and, if so, how many) when your household had to restrict consumption
  by adults in order for small children to eat to cope with a lack of
  food or money to buy it?

- fsl_rcsi_mealnb:

  Column representing question - During the last 7 days, were there days
  (and, if so, how many) when your household had to reduce number of
  meals eaten in a day to cope with a lack of food or money to buy it?

- fsl_fcs_score:

  Column representing FCS Score

- fsl_rcsi_score:

  Column representing rCSI Score

- fsl_hhs_score:

  Column representing HHS Score

- fsl_hdds_score:

  Column representing HDDS Score

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

a dataframe with all fsl related plausibility columns

## Examples

``` r
if (FALSE) create_fsl_plaus(df) # \dontrun{}
```
