
## impactR4PHU

<!-- badges: start -->

[![Contributor
Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](code_of_conduct.md)
[![R-CMD-check](https://github.com/impact-initiatives/impactR4PHU/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/impact-initiatives/impactR4PHU/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/impact-initiatives/impactR4PHU/graph/badge.svg?token=BG57ECHOYX)](https://codecov.io/gh/impact-initiatives/impactR4PHU)
<!-- badges: end -->

## Overview

`impactR4PHU` is designed for creating quality check reports, cleaning,
analysing and outputing results of core outcome indicators of Public
Health Unit. This package will target mainly Food Security and
Livelihoods, WASH, Nutrition and Health Sectors.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("impact-initiatives/impactR4PHU")
```

## Examples

``` r
library(impactR4PHU)
df <- impactR4PHU_data_template
```

### Example:: Add Food Consumption Score (FCS)

``` r
df_with_fcs <- df %>% add_fcs(
  cutoffs = "normal",
  fsl_fcs_cereal = "fsl_fcs_cereal",
  fsl_fcs_legumes = "fsl_fcs_legumes",
  fsl_fcs_veg = "fsl_fcs_veg",
  fsl_fcs_fruit = "fsl_fcs_fruit",
  fsl_fcs_meat = "fsl_fcs_meat",
  fsl_fcs_dairy = "fsl_fcs_dairy",
  fsl_fcs_sugar = "fsl_fcs_sugar",
  fsl_fcs_oil = "fsl_fcs_oil"
)
df_with_fcs %>%
  dplyr::select(
    uuid, fsl_fcs_score, fsl_fcs_cat, fcs_weight_cereal1, fcs_weight_legume2,
    fcs_weight_dairy3, fcs_weight_meat4, fcs_weight_veg5,
    fcs_weight_fruit6, fcs_weight_oil7, fcs_weight_sugar8
  ) %>%
  head(20)
```

    ## # A tibble: 20 × 11
    ##    uuid          fsl_fcs_score fsl_fcs_cat fcs_weight_cereal1 fcs_weight_legume2
    ##    <chr>                 <dbl> <chr>                    <dbl>              <dbl>
    ##  1 0cfd1539-4be…          NA   <NA>                        NA                 NA
    ##  2 0fc8a427-f30…          NA   <NA>                        NA                 NA
    ##  3 14c3baf8-d4b…          32.5 Borderline                   6                  6
    ##  4 1a8de690-60a…          23   Borderline                   4                  6
    ##  5 1c92baf4-107…          23.5 Borderline                   4                  3
    ##  6 1d7ca542-5eb…          62.5 Acceptable                   4                  9
    ##  7 1ecfd059-c21…          29.5 Borderline                  14                  3
    ##  8 205d37b1-5a6…          40   Acceptable                   4                 12
    ##  9 218f7539-061…          92.5 Acceptable                  12                 18
    ## 10 2d56cf0a-a45…          NA   <NA>                        NA                 NA
    ## 11 3186cfde-19a…          35   Borderline                  14                  6
    ## 12 31d0cfb8-21d…          43   Acceptable                  12                  3
    ## 13 328e7cd6-651…          12.5 Poor                         4                  6
    ## 14 36584aec-f27…          23   Borderline                   6                  6
    ## 15 37b5a861-0f2…          40   Acceptable                   4                  3
    ## 16 38b615cf-0fd…          77   Acceptable                   4                 21
    ## 17 3aef5849-5ca…          29.5 Borderline                  14                  3
    ## 18 3b6948fe-340…          53   Acceptable                  14                 12
    ## 19 3c1704f5-247…          33.5 Borderline                  14                  3
    ## 20 3e02914b-eb2…          29   Borderline                  14                  6
    ## # ℹ 6 more variables: fcs_weight_dairy3 <dbl>, fcs_weight_meat4 <dbl>,
    ## #   fcs_weight_veg5 <dbl>, fcs_weight_fruit6 <dbl>, fcs_weight_oil7 <dbl>,
    ## #   fcs_weight_sugar8 <dbl>

### Example:: Add Household Hunger Scale (HHS)

``` r
df_with_hhs <- df_with_fcs %>% add_hhs(
  fsl_hhs_nofoodhh = "fsl_hhs_nofoodhh",
  fsl_hhs_nofoodhh_freq = "fsl_hhs_nofoodhh_freq",
  fsl_hhs_sleephungry = "fsl_hhs_sleephungry",
  fsl_hhs_sleephungry_freq = "fsl_hhs_sleephungry_freq",
  fsl_hhs_alldaynight = "fsl_hhs_alldaynight",
  fsl_hhs_alldaynight_freq = "fsl_hhs_alldaynight_freq",
  yes_answer = "yes",
  no_answer = "no",
  rarely_answer = "rarely",
  sometimes_answer = "sometimes",
  often_answer = "often"
)
df_with_hhs %>%
  dplyr::select(
    uuid, fsl_hhs_comp1, fsl_hhs_comp2, fsl_hhs_comp3,
    fsl_hhs_score, fsl_hhs_cat_ipc, fsl_hhs_cat, num_hh
  ) %>%
  head(20)
```

    ##                                        uuid fsl_hhs_comp1 fsl_hhs_comp2
    ## 1  0cfd1539-4be3-4c444a-8a8d8e-0d2a6bf74895            NA            NA
    ## 2  0fc8a427-f30e-4a4341-b3b5b4-08a6392ef4dc            NA            NA
    ## 3  14c3baf8-d4b0-43484c-8d8e8f-a5fd7134982e             0             0
    ## 4  1a8de690-60af-45494a-8b8487-78f45ec16b39             0             0
    ## 5  1c92baf4-107e-474c46-a3a8a5-6b2e815ad30c             0             0
    ## 6  1d7ca542-5ebf-434e44-949e9a-d3687ef9c145             0             0
    ## 7  1ecfd059-c215-4d4746-94999b-87920feb4a6c             0             0
    ## 8  205d37b1-5a6f-44484d-b3b1ba-4eafbdc50873             0             0
    ## 9  218f7539-061b-404f44-96989f-b345c89a6e21             0             0
    ## 10 2d56cf0a-a45c-444148-898e84-ab7f4de18259            NA            NA
    ## 11 3186cfde-19a7-434748-bbb7b1-e369754821cb             0             0
    ## 12 31d0cfb8-21d7-414b4f-94999f-04a15ce39d78             2             2
    ## 13 328e7cd6-6517-4f4044-8f8c86-c710a84e5639             1             1
    ## 14 36584aec-f271-47484b-999391-417e2a3d6b59             0             0
    ## 15 37b5a861-0f21-4e4942-909295-34826ecd950b             0             0
    ## 16 38b615cf-0fd3-4f4d4e-bfbab1-a07658b413ce             0             0
    ## 17 3aef5849-5ca7-4c4841-8a8584-e64b1a8d0c92             1             1
    ## 18 3b6948fe-3409-4f4143-b3bab2-86301b529fc7             0             0
    ## 19 3c1704f5-2473-474e4f-808982-f9830c51d7b2             1             1
    ## 20 3e02914b-eb25-484243-909498-dcfa793514b2             0             0
    ##    fsl_hhs_comp3 fsl_hhs_score fsl_hhs_cat_ipc  fsl_hhs_cat num_hh
    ## 1             NA            NA            <NA>         <NA>   <NA>
    ## 2             NA            NA            <NA>         <NA>   <NA>
    ## 3              0             0            None No or Little      3
    ## 4              0             0            None No or Little      3
    ## 5              0             0            None No or Little      4
    ## 6              0             0            None No or Little      4
    ## 7              0             0            None No or Little      4
    ## 8              0             0            None No or Little      4
    ## 9              1             1    No or Little No or Little      4
    ## 10            NA            NA            <NA>         <NA>   <NA>
    ## 11             0             0            None No or Little      4
    ## 12             2             6     Very Severe       Severe      4
    ## 13             1             3        Moderate     Moderate      4
    ## 14             0             0            None No or Little      4
    ## 15             0             0            None No or Little      3
    ## 16             0             0            None No or Little      4
    ## 17             0             2        Moderate     Moderate      3
    ## 18             0             0            None No or Little      4
    ## 19             1             3        Moderate     Moderate      4
    ## 20             0             0            None No or Little      3

### Example:: Add Livelihood Coping Strategy score (LCSI)

``` r
df_with_lcsi <- df_with_hhs %>% add_lcsi(
  fsl_lcsi_stress1 = "fsl_lcsi_stress1",
  fsl_lcsi_stress2 = "fsl_lcsi_stress2",
  fsl_lcsi_stress3 = "fsl_lcsi_stress3",
  fsl_lcsi_stress4 = "fsl_lcsi_stress4",
  fsl_lcsi_crisis1 = "fsl_lcsi_crisis1",
  fsl_lcsi_crisis2 = "fsl_lcsi_crisis2",
  fsl_lcsi_crisis3 = "fsl_lcsi_crisis3",
  fsl_lcsi_emergency1 = "fsl_lcsi_emergency1",
  fsl_lcsi_emergency2 = "fsl_lcsi_emergency2",
  fsl_lcsi_emergency3 = "fsl_lcsi_emergency3",
  yes_val = "yes",
  no_val = "no_had_no_need",
  exhausted_val = "no_exhausted",
  not_applicable_val = "not_applicable"
)
df_with_lcsi %>%
  dplyr::select(uuid, fsl_lcsi_cat, fsl_lcsi_cat_exhaust, fsl_lcsi_cat_yes) %>%
  head(20)
```

    ##                                        uuid fsl_lcsi_cat fsl_lcsi_cat_exhaust
    ## 1  0cfd1539-4be3-4c444a-8a8d8e-0d2a6bf74895         <NA>                 <NA>
    ## 2  0fc8a427-f30e-4a4341-b3b5b4-08a6392ef4dc         <NA>                 <NA>
    ## 3  14c3baf8-d4b0-43484c-8d8e8f-a5fd7134982e       Stress                 None
    ## 4  1a8de690-60af-45494a-8b8487-78f45ec16b39         None                 None
    ## 5  1c92baf4-107e-474c46-a3a8a5-6b2e815ad30c       Stress                 None
    ## 6  1d7ca542-5ebf-434e44-949e9a-d3687ef9c145    Emergency                 None
    ## 7  1ecfd059-c215-4d4746-94999b-87920feb4a6c       Stress               Stress
    ## 8  205d37b1-5a6f-44484d-b3b1ba-4eafbdc50873       Stress                 None
    ## 9  218f7539-061b-404f44-96989f-b345c89a6e21         None                 None
    ## 10 2d56cf0a-a45c-444148-898e84-ab7f4de18259         <NA>                 <NA>
    ## 11 3186cfde-19a7-434748-bbb7b1-e369754821cb    Emergency                 None
    ## 12 31d0cfb8-21d7-414b4f-94999f-04a15ce39d78    Emergency               Crisis
    ## 13 328e7cd6-6517-4f4044-8f8c86-c710a84e5639         None                 None
    ## 14 36584aec-f271-47484b-999391-417e2a3d6b59         None                 None
    ## 15 37b5a861-0f21-4e4942-909295-34826ecd950b       Stress                 None
    ## 16 38b615cf-0fd3-4f4d4e-bfbab1-a07658b413ce         None                 None
    ## 17 3aef5849-5ca7-4c4841-8a8584-e64b1a8d0c92       Stress                 None
    ## 18 3b6948fe-3409-4f4143-b3bab2-86301b529fc7         None                 None
    ## 19 3c1704f5-2473-474e4f-808982-f9830c51d7b2       Stress               Stress
    ## 20 3e02914b-eb25-484243-909498-dcfa793514b2       Stress               Stress
    ##    fsl_lcsi_cat_yes
    ## 1              <NA>
    ## 2              <NA>
    ## 3            Stress
    ## 4              None
    ## 5            Stress
    ## 6         Emergency
    ## 7              None
    ## 8            Stress
    ## 9              None
    ## 10             <NA>
    ## 11        Emergency
    ## 12        Emergency
    ## 13             None
    ## 14             None
    ## 15           Stress
    ## 16             None
    ## 17           Stress
    ## 18             None
    ## 19           Stress
    ## 20             None

### Example:: Add Reduced Household Coping Strategy score (rCSI)

``` r
df_with_rcsi <- df_with_lcsi %>% add_rcsi(
  fsl_rcsi_lessquality = "fsl_rcsi_lessquality",
  fsl_rcsi_borrow = "fsl_rcsi_borrow",
  fsl_rcsi_mealsize = "fsl_rcsi_mealsize",
  fsl_rcsi_mealadult = "fsl_rcsi_mealadult",
  fsl_rcsi_mealnb = "fsl_rcsi_mealnb"
)
df_with_rcsi %>%
  dplyr::select(uuid, fsl_rcsi_score, fsl_rcsi_cat) %>%
  head(20)
```

    ##                                        uuid fsl_rcsi_score fsl_rcsi_cat
    ## 1  0cfd1539-4be3-4c444a-8a8d8e-0d2a6bf74895             NA         <NA>
    ## 2  0fc8a427-f30e-4a4341-b3b5b4-08a6392ef4dc             NA         <NA>
    ## 3  14c3baf8-d4b0-43484c-8d8e8f-a5fd7134982e              9       Medium
    ## 4  1a8de690-60af-45494a-8b8487-78f45ec16b39             NA         <NA>
    ## 5  1c92baf4-107e-474c46-a3a8a5-6b2e815ad30c              7       Medium
    ## 6  1d7ca542-5ebf-434e44-949e9a-d3687ef9c145              7       Medium
    ## 7  1ecfd059-c215-4d4746-94999b-87920feb4a6c              6       Medium
    ## 8  205d37b1-5a6f-44484d-b3b1ba-4eafbdc50873             11       Medium
    ## 9  218f7539-061b-404f44-96989f-b345c89a6e21              6       Medium
    ## 10 2d56cf0a-a45c-444148-898e84-ab7f4de18259             NA         <NA>
    ## 11 3186cfde-19a7-434748-bbb7b1-e369754821cb              5       Medium
    ## 12 31d0cfb8-21d7-414b4f-94999f-04a15ce39d78             34         High
    ## 13 328e7cd6-6517-4f4044-8f8c86-c710a84e5639             16       Medium
    ## 14 36584aec-f271-47484b-999391-417e2a3d6b59              5       Medium
    ## 15 37b5a861-0f21-4e4942-909295-34826ecd950b             13       Medium
    ## 16 38b615cf-0fd3-4f4d4e-bfbab1-a07658b413ce             NA         <NA>
    ## 17 3aef5849-5ca7-4c4841-8a8584-e64b1a8d0c92             12       Medium
    ## 18 3b6948fe-3409-4f4143-b3bab2-86301b529fc7             NA         <NA>
    ## 19 3c1704f5-2473-474e4f-808982-f9830c51d7b2              8       Medium
    ## 20 3e02914b-eb25-484243-909498-dcfa793514b2              5       Medium

### Example:: Add Household Dietary Diversity Score (HDDS)

``` r
df_with_hdds <- df_with_rcsi %>% add_hdds(
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
   fsl_hdds_condiments = "fsl_hdds_condiments"
)
df_with_hdds %>%
  dplyr::select(uuid, fsl_hdds_score, fsl_hdds_cat) %>%
  head(20)
```

    ##                                        uuid fsl_hdds_score fsl_hdds_cat
    ## 1  0cfd1539-4be3-4c444a-8a8d8e-0d2a6bf74895             NA         <NA>
    ## 2  0fc8a427-f30e-4a4341-b3b5b4-08a6392ef4dc             NA         <NA>
    ## 3  14c3baf8-d4b0-43484c-8d8e8f-a5fd7134982e              6         High
    ## 4  1a8de690-60af-45494a-8b8487-78f45ec16b39              5         High
    ## 5  1c92baf4-107e-474c46-a3a8a5-6b2e815ad30c              7         High
    ## 6  1d7ca542-5ebf-434e44-949e9a-d3687ef9c145              6         High
    ## 7  1ecfd059-c215-4d4746-94999b-87920feb4a6c              5         High
    ## 8  205d37b1-5a6f-44484d-b3b1ba-4eafbdc50873              7         High
    ## 9  218f7539-061b-404f44-96989f-b345c89a6e21              8         High
    ## 10 2d56cf0a-a45c-444148-898e84-ab7f4de18259             NA         <NA>
    ## 11 3186cfde-19a7-434748-bbb7b1-e369754821cb              8         High
    ## 12 31d0cfb8-21d7-414b4f-94999f-04a15ce39d78              4       Medium
    ## 13 328e7cd6-6517-4f4044-8f8c86-c710a84e5639              7         High
    ## 14 36584aec-f271-47484b-999391-417e2a3d6b59              6         High
    ## 15 37b5a861-0f21-4e4942-909295-34826ecd950b              7         High
    ## 16 38b615cf-0fd3-4f4d4e-bfbab1-a07658b413ce              4       Medium
    ## 17 3aef5849-5ca7-4c4841-8a8584-e64b1a8d0c92              6         High
    ## 18 3b6948fe-3409-4f4143-b3bab2-86301b529fc7              6         High
    ## 19 3c1704f5-2473-474e4f-808982-f9830c51d7b2              9         High
    ## 20 3e02914b-eb25-484243-909498-dcfa793514b2              3       Medium

### Example:: Add Food Consumption Matrix (FCM) using FCS, RCSI, and HHS

**Notice that these functions are also pipable**

``` r
df_with_fcm_1 <- df_with_hdds %>%
  add_fcm_phase(
    fcs_column_name = "fsl_fcs_cat",
    rcsi_column_name = "fsl_rcsi_cat",
    hhs_column_name = "fsl_hhs_cat_ipc",
    fcs_categories_acceptable = "Acceptable",
    fcs_categories_poor = "Poor",
    fcs_categories_borderline = "Borderline",
    rcsi_categories_low = "No to Low",
    rcsi_categories_medium = "Medium",
    rcsi_categories_high = "High",
    hhs_categories_none = "None",
    hhs_categories_little = "No or Little",
    hhs_categories_moderate = "Moderate",
    hhs_categories_severe = "Severe",
    hhs_categories_very_severe = "Very Severe"
  )
df_with_fcm_1 %>%
  dplyr::select(uuid, fc_cell, fc_phase) %>%
  head(20)
```

    ##                                        uuid fc_cell   fc_phase
    ## 1  0cfd1539-4be3-4c444a-8a8d8e-0d2a6bf74895      NA       <NA>
    ## 2  0fc8a427-f30e-4a4341-b3b5b4-08a6392ef4dc      NA       <NA>
    ## 3  14c3baf8-d4b0-43484c-8d8e8f-a5fd7134982e      21 Phase 2 FC
    ## 4  1a8de690-60af-45494a-8b8487-78f45ec16b39      NA       <NA>
    ## 5  1c92baf4-107e-474c46-a3a8a5-6b2e815ad30c      21 Phase 2 FC
    ## 6  1d7ca542-5ebf-434e44-949e9a-d3687ef9c145      16 Phase 2 FC
    ## 7  1ecfd059-c215-4d4746-94999b-87920feb4a6c      21 Phase 2 FC
    ## 8  205d37b1-5a6f-44484d-b3b1ba-4eafbdc50873      16 Phase 2 FC
    ## 9  218f7539-061b-404f44-96989f-b345c89a6e21      17 Phase 2 FC
    ## 10 2d56cf0a-a45c-444148-898e84-ab7f4de18259      NA       <NA>
    ## 11 3186cfde-19a7-434748-bbb7b1-e369754821cb      21 Phase 2 FC
    ## 12 31d0cfb8-21d7-414b4f-94999f-04a15ce39d78      35 Phase 4 FC
    ## 13 328e7cd6-6517-4f4044-8f8c86-c710a84e5639      28 Phase 3 FC
    ## 14 36584aec-f271-47484b-999391-417e2a3d6b59      21 Phase 2 FC
    ## 15 37b5a861-0f21-4e4942-909295-34826ecd950b      16 Phase 2 FC
    ## 16 38b615cf-0fd3-4f4d4e-bfbab1-a07658b413ce      NA       <NA>
    ## 17 3aef5849-5ca7-4c4841-8a8584-e64b1a8d0c92      23 Phase 3 FC
    ## 18 3b6948fe-3409-4f4143-b3bab2-86301b529fc7      NA       <NA>
    ## 19 3c1704f5-2473-474e4f-808982-f9830c51d7b2      23 Phase 3 FC
    ## 20 3e02914b-eb25-484243-909498-dcfa793514b2      21 Phase 2 FC

### Example:: Add Food Consumption Matrix (FCM) using HDDS, RCSI, and HHS

**Notice that these functions are also pipable**

``` r
df_with_fcm_2 <- df_with_hdds %>%
  add_fcm_phase(
    hdds_column_name = "fsl_hdds_cat",
    rcsi_column_name = "fsl_rcsi_cat",
    hhs_column_name = "fsl_hhs_cat_ipc",
    hdds_categories_low = "Low",
    hdds_categories_medium = "Medium",
    hdds_categories_high = "High",
    rcsi_categories_low = "No to Low",
    rcsi_categories_medium = "Medium",
    rcsi_categories_high = "High",
    hhs_categories_none = "None",
    hhs_categories_little = "No or Little",
    hhs_categories_moderate = "Moderate",
    hhs_categories_severe = "Severe",
    hhs_categories_very_severe = "Very Severe"
  )
df_with_fcm_2 %>%
  dplyr::select(uuid, fc_cell, fc_phase) %>%
  head(20)
```

    ##                                        uuid fc_cell   fc_phase
    ## 1  0cfd1539-4be3-4c444a-8a8d8e-0d2a6bf74895      NA       <NA>
    ## 2  0fc8a427-f30e-4a4341-b3b5b4-08a6392ef4dc      NA       <NA>
    ## 3  14c3baf8-d4b0-43484c-8d8e8f-a5fd7134982e      21 Phase 2 FC
    ## 4  1a8de690-60af-45494a-8b8487-78f45ec16b39      NA       <NA>
    ## 5  1c92baf4-107e-474c46-a3a8a5-6b2e815ad30c      21 Phase 2 FC
    ## 6  1d7ca542-5ebf-434e44-949e9a-d3687ef9c145      16 Phase 2 FC
    ## 7  1ecfd059-c215-4d4746-94999b-87920feb4a6c      21 Phase 2 FC
    ## 8  205d37b1-5a6f-44484d-b3b1ba-4eafbdc50873      16 Phase 2 FC
    ## 9  218f7539-061b-404f44-96989f-b345c89a6e21      17 Phase 2 FC
    ## 10 2d56cf0a-a45c-444148-898e84-ab7f4de18259      NA       <NA>
    ## 11 3186cfde-19a7-434748-bbb7b1-e369754821cb      21 Phase 2 FC
    ## 12 31d0cfb8-21d7-414b4f-94999f-04a15ce39d78      35 Phase 4 FC
    ## 13 328e7cd6-6517-4f4044-8f8c86-c710a84e5639      28 Phase 3 FC
    ## 14 36584aec-f271-47484b-999391-417e2a3d6b59      21 Phase 2 FC
    ## 15 37b5a861-0f21-4e4942-909295-34826ecd950b      16 Phase 2 FC
    ## 16 38b615cf-0fd3-4f4d4e-bfbab1-a07658b413ce      NA       <NA>
    ## 17 3aef5849-5ca7-4c4841-8a8584-e64b1a8d0c92      23 Phase 3 FC
    ## 18 3b6948fe-3409-4f4143-b3bab2-86301b529fc7      NA       <NA>
    ## 19 3c1704f5-2473-474e4f-808982-f9830c51d7b2      23 Phase 3 FC
    ## 20 3e02914b-eb25-484243-909498-dcfa793514b2      21 Phase 2 FC

### Example:: Add Food Consumption Matrix (FCM) using FCS and HHS

**Notice that these functions are also pipable**

``` r
df_with_fcm_3 <- df_with_hdds %>%
  add_fcm_phase(
    fcs_column_name = "fsl_fcs_cat",
    hhs_column_name = "fsl_hhs_cat_ipc",
    fcs_categories_acceptable = "Acceptable",
    fcs_categories_poor = "Poor",
    fcs_categories_borderline = "Borderline",
    hhs_categories_none = "None",
    hhs_categories_little = "No or Little",
    hhs_categories_moderate = "Moderate",
    hhs_categories_severe = "Severe",
    hhs_categories_very_severe = "Very Severe"
  )
df_with_fcm_3 %>%
  dplyr::select(uuid, fc_cell, fc_phase) %>%
  head(20)
```

    ##                                        uuid fc_cell   fc_phase
    ## 1  0cfd1539-4be3-4c444a-8a8d8e-0d2a6bf74895      NA       <NA>
    ## 2  0fc8a427-f30e-4a4341-b3b5b4-08a6392ef4dc      NA       <NA>
    ## 3  14c3baf8-d4b0-43484c-8d8e8f-a5fd7134982e      21 Phase 2 FC
    ## 4  1a8de690-60af-45494a-8b8487-78f45ec16b39      NA       <NA>
    ## 5  1c92baf4-107e-474c46-a3a8a5-6b2e815ad30c      21 Phase 2 FC
    ## 6  1d7ca542-5ebf-434e44-949e9a-d3687ef9c145      16 Phase 2 FC
    ## 7  1ecfd059-c215-4d4746-94999b-87920feb4a6c      21 Phase 2 FC
    ## 8  205d37b1-5a6f-44484d-b3b1ba-4eafbdc50873      16 Phase 2 FC
    ## 9  218f7539-061b-404f44-96989f-b345c89a6e21      17 Phase 2 FC
    ## 10 2d56cf0a-a45c-444148-898e84-ab7f4de18259      NA       <NA>
    ## 11 3186cfde-19a7-434748-bbb7b1-e369754821cb      21 Phase 2 FC
    ## 12 31d0cfb8-21d7-414b4f-94999f-04a15ce39d78      35 Phase 4 FC
    ## 13 328e7cd6-6517-4f4044-8f8c86-c710a84e5639      28 Phase 3 FC
    ## 14 36584aec-f271-47484b-999391-417e2a3d6b59      21 Phase 2 FC
    ## 15 37b5a861-0f21-4e4942-909295-34826ecd950b      16 Phase 2 FC
    ## 16 38b615cf-0fd3-4f4d4e-bfbab1-a07658b413ce      NA       <NA>
    ## 17 3aef5849-5ca7-4c4841-8a8584-e64b1a8d0c92      23 Phase 3 FC
    ## 18 3b6948fe-3409-4f4143-b3bab2-86301b529fc7      NA       <NA>
    ## 19 3c1704f5-2473-474e4f-808982-f9830c51d7b2      23 Phase 3 FC
    ## 20 3e02914b-eb25-484243-909498-dcfa793514b2      21 Phase 2 FC

### Example:: Add Food Consumption Matrix (FCM) using HDDS and HHS

**Notice that these functions are also pipable**

``` r
df_with_fcm_4 <- df_with_hdds %>%
  add_fcm_phase(
    hdds_column_name = "fsl_hdds_cat",
    hhs_column_name = "fsl_hhs_cat_ipc",
    hdds_categories_low = "Low",
    hdds_categories_medium = "Medium",
    hdds_categories_high = "High",
    hhs_categories_none = "None",
    hhs_categories_little = "No or Little",
    hhs_categories_moderate = "Moderate",
    hhs_categories_severe = "Severe",
    hhs_categories_very_severe = "Very Severe"
  )
df_with_fcm_4 %>%
  dplyr::select(uuid, fc_cell, fc_phase) %>%
  head(20)
```

    ##                                        uuid fc_cell   fc_phase
    ## 1  0cfd1539-4be3-4c444a-8a8d8e-0d2a6bf74895      NA       <NA>
    ## 2  0fc8a427-f30e-4a4341-b3b5b4-08a6392ef4dc      NA       <NA>
    ## 3  14c3baf8-d4b0-43484c-8d8e8f-a5fd7134982e      21 Phase 2 FC
    ## 4  1a8de690-60af-45494a-8b8487-78f45ec16b39      NA       <NA>
    ## 5  1c92baf4-107e-474c46-a3a8a5-6b2e815ad30c      21 Phase 2 FC
    ## 6  1d7ca542-5ebf-434e44-949e9a-d3687ef9c145      16 Phase 2 FC
    ## 7  1ecfd059-c215-4d4746-94999b-87920feb4a6c      21 Phase 2 FC
    ## 8  205d37b1-5a6f-44484d-b3b1ba-4eafbdc50873      16 Phase 2 FC
    ## 9  218f7539-061b-404f44-96989f-b345c89a6e21      17 Phase 2 FC
    ## 10 2d56cf0a-a45c-444148-898e84-ab7f4de18259      NA       <NA>
    ## 11 3186cfde-19a7-434748-bbb7b1-e369754821cb      21 Phase 2 FC
    ## 12 31d0cfb8-21d7-414b4f-94999f-04a15ce39d78      35 Phase 4 FC
    ## 13 328e7cd6-6517-4f4044-8f8c86-c710a84e5639      28 Phase 3 FC
    ## 14 36584aec-f271-47484b-999391-417e2a3d6b59      21 Phase 2 FC
    ## 15 37b5a861-0f21-4e4942-909295-34826ecd950b      16 Phase 2 FC
    ## 16 38b615cf-0fd3-4f4d4e-bfbab1-a07658b413ce      NA       <NA>
    ## 17 3aef5849-5ca7-4c4841-8a8584-e64b1a8d0c92      23 Phase 3 FC
    ## 18 3b6948fe-3409-4f4143-b3bab2-86301b529fc7      NA       <NA>
    ## 19 3c1704f5-2473-474e4f-808982-f9830c51d7b2      23 Phase 3 FC
    ## 20 3e02914b-eb25-484243-909498-dcfa793514b2      21 Phase 2 FC

### Example:: Add FEWSNET Food Consumption-Livelihood Matrix (FCLCM)

**Notice that these functions are also pipable**

``` r
df_with_fclcm <- df_with_fcm_1 %>% ## Taken from previous Example
  add_fclcm_phase()
df_with_fclcm %>%
  dplyr::select(uuid, fclcm_phase) %>%
  head(20)
```

    ##                                        uuid  fclcm_phase
    ## 1  0cfd1539-4be3-4c444a-8a8d8e-0d2a6bf74895         <NA>
    ## 2  0fc8a427-f30e-4a4341-b3b5b4-08a6392ef4dc         <NA>
    ## 3  14c3baf8-d4b0-43484c-8d8e8f-a5fd7134982e Phase 2 FCLC
    ## 4  1a8de690-60af-45494a-8b8487-78f45ec16b39         <NA>
    ## 5  1c92baf4-107e-474c46-a3a8a5-6b2e815ad30c Phase 2 FCLC
    ## 6  1d7ca542-5ebf-434e44-949e9a-d3687ef9c145 Phase 3 FCLC
    ## 7  1ecfd059-c215-4d4746-94999b-87920feb4a6c Phase 2 FCLC
    ## 8  205d37b1-5a6f-44484d-b3b1ba-4eafbdc50873 Phase 2 FCLC
    ## 9  218f7539-061b-404f44-96989f-b345c89a6e21 Phase 2 FCLC
    ## 10 2d56cf0a-a45c-444148-898e84-ab7f4de18259         <NA>
    ## 11 3186cfde-19a7-434748-bbb7b1-e369754821cb Phase 3 FCLC
    ## 12 31d0cfb8-21d7-414b4f-94999f-04a15ce39d78 Phase 4 FCLC
    ## 13 328e7cd6-6517-4f4044-8f8c86-c710a84e5639 Phase 3 FCLC
    ## 14 36584aec-f271-47484b-999391-417e2a3d6b59 Phase 2 FCLC
    ## 15 37b5a861-0f21-4e4942-909295-34826ecd950b Phase 2 FCLC
    ## 16 38b615cf-0fd3-4f4d4e-bfbab1-a07658b413ce         <NA>
    ## 17 3aef5849-5ca7-4c4841-8a8584-e64b1a8d0c92 Phase 3 FCLC
    ## 18 3b6948fe-3409-4f4143-b3bab2-86301b529fc7         <NA>
    ## 19 3c1704f5-2473-474e4f-808982-f9830c51d7b2 Phase 3 FCLC
    ## 20 3e02914b-eb25-484243-909498-dcfa793514b2 Phase 2 FCLC

## Code of Conduct

Please note that the addindicators project is released with a
[Contributor Code of
Conduct](https://impact-initiatives.github.io/impactR4PHU/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
