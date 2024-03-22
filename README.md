
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

## Adding indicators (for both Analysis or Quality checks)

``` r
library(impactR4PHU)
df <- impactR4PHU_data_template
```

### FSL ADD Incicators

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

### NUTRITION ADD Incicators

``` r
df_nut <- impactR4PHU_data_nut_template
```

### Example:: Add MUAC

``` r
df_with_muac <- df_nut %>% 
  add_muac()
df_with_muac %>%
  dplyr::select(
    uuid, sam_muac, mam_muac, gam_muac) %>%
  head(20)
```

    ## # A tibble: 20 × 4
    ##    uuid                                     sam_muac mam_muac gam_muac
    ##    <chr>                                       <dbl>    <dbl>    <dbl>
    ##  1 7b4261fa-61a5-4a4948-999093-13bc7e9f0658        0        1        1
    ##  2 83c0a56b-15fd-4f4349-b2bcbd-806912fb3c5d        0        0        0
    ##  3 6401c279-8a6f-464b4d-919598-da125739e64c        0        0        0
    ##  4 1ecfd059-c215-4d4746-94999b-87920feb4a6c        0        0        0
    ##  5 4b038c2e-25a6-484641-aca6a7-cf387e4b29d1        0        0        0
    ##  6 3b6948fe-3409-4f4143-b3bab2-86301b529fc7        0        0        0
    ##  7 512bce03-78ea-404742-8e8d83-e53a8296c0d4        0        0        0
    ##  8 1a8de690-60af-45494a-8b8487-78f45ec16b39        0        0        0
    ##  9 53a2e761-34cb-434c46-b3bdbc-b0fc1295673d        0        0        0
    ## 10 4d5b1089-1aec-424f49-aba5a9-b3ade80461fc        0        0        0
    ## 11 ef2963c7-ef67-4e4446-bab5b7-7e9d0431fa8c        0        0        0
    ## 12 98fdb3a2-2c1a-4f424b-8d8782-b21d683ea94f        0        0        0
    ## 13 1d7ca542-5ebf-434e44-949e9a-d3687ef9c145        0        0        0
    ## 14 a725301d-21b7-444c42-919f95-2f769503b184        0        0        0
    ## 15 4b038c2e-25a6-484641-aca6a7-cf387e4b29d1        0        0        0
    ## 16 1d7ca542-5ebf-434e44-949e9a-d3687ef9c145        0        0        0
    ## 17 3c1704f5-2473-474e4f-808982-f9830c51d7b2       NA       NA       NA
    ## 18 ef0d36a5-493b-444048-bbbab9-bf719e4850a6        0        0        0
    ## 19 31d0cfb8-21d7-414b4f-94999f-04a15ce39d78       NA       NA       NA
    ## 20 31d0cfb8-21d7-414b4f-94999f-04a15ce39d78       NA       NA       NA

### Example:: Add MFAZ

``` r
df_with_mfaz <- df_with_muac %>% 
  add_mfaz()
```

    ## ================================================================================

``` r
df_with_mfaz %>%
  dplyr::select(
    uuid, mfaz, severe_mfaz, moderate_mfaz, global_mfaz) %>%
  head(20)
```

    ## # A tibble: 20 × 5
    ##    uuid                               mfaz severe_mfaz moderate_mfaz global_mfaz
    ##    <chr>                             <dbl>       <dbl>         <dbl>       <dbl>
    ##  1 7b4261fa-61a5-4a4948-999093-13bc… -3.4            1             0           1
    ##  2 83c0a56b-15fd-4f4349-b2bcbd-8069… -1.48           0             0           0
    ##  3 6401c279-8a6f-464b4d-919598-da12… -2.33           0             1           1
    ##  4 1ecfd059-c215-4d4746-94999b-8792… -2.38           0             1           1
    ##  5 4b038c2e-25a6-484641-aca6a7-cf38… -1.3            0             0           0
    ##  6 3b6948fe-3409-4f4143-b3bab2-8630… -0.62           0             0           0
    ##  7 512bce03-78ea-404742-8e8d83-e53a… -0.8            0             0           0
    ##  8 1a8de690-60af-45494a-8b8487-78f4… -1.88           0             0           0
    ##  9 53a2e761-34cb-434c46-b3bdbc-b0fc… -0.43           0             0           0
    ## 10 4d5b1089-1aec-424f49-aba5a9-b3ad…  0.42           0             0           0
    ## 11 ef2963c7-ef67-4e4446-bab5b7-7e9d… -0.57           0             0           0
    ## 12 98fdb3a2-2c1a-4f424b-8d8782-b21d…  0.51           0             0           0
    ## 13 1d7ca542-5ebf-434e44-949e9a-d368… -0.73           0             0           0
    ## 14 a725301d-21b7-444c42-919f95-2f76…  0.4            0             0           0
    ## 15 4b038c2e-25a6-484641-aca6a7-cf38…  1.05           0             0           0
    ## 16 1d7ca542-5ebf-434e44-949e9a-d368…  1.63           0             0           0
    ## 17 3c1704f5-2473-474e4f-808982-f983… NA             NA            NA          NA
    ## 18 ef0d36a5-493b-444048-bbbab9-bf71…  1.04           0             0           0
    ## 19 31d0cfb8-21d7-414b4f-94999f-04a1… NA             NA            NA          NA
    ## 20 31d0cfb8-21d7-414b4f-94999f-04a1… NA             NA            NA          NA

## Checking Flags (For Quality reports and Plausibility checks)

``` r
tool <- impactR4PHU_survey_template
```

### Example:: Check Food Security and Livelihoods Flags

``` r
fsl_flags <- df_with_fclcm %>% 
  check_fsl_flags(tool.survey = tool,
                  grouping = "enumerator")
fsl_flags %>% 
  dplyr::select(uuid, group,starts_with("flag_")) %>% 
  head(20)
```

    ##                                        uuid group flag_meat_cereal_ratio
    ## 1  0cfd1539-4be3-4c444a-8a8d8e-0d2a6bf74895     1                     NA
    ## 2  0fc8a427-f30e-4a4341-b3b5b4-08a6392ef4dc     5                     NA
    ## 3  14c3baf8-d4b0-43484c-8d8e8f-a5fd7134982e     2                      0
    ## 4  1a8de690-60af-45494a-8b8487-78f45ec16b39     2                      0
    ## 5  1c92baf4-107e-474c46-a3a8a5-6b2e815ad30c     2                      0
    ## 6  1d7ca542-5ebf-434e44-949e9a-d3687ef9c145     5                      1
    ## 7  1ecfd059-c215-4d4746-94999b-87920feb4a6c     2                      0
    ## 8  205d37b1-5a6f-44484d-b3b1ba-4eafbdc50873     2                      0
    ## 9  218f7539-061b-404f44-96989f-b345c89a6e21     2                      1
    ## 10 2d56cf0a-a45c-444148-898e84-ab7f4de18259     3                     NA
    ## 11 3186cfde-19a7-434748-bbb7b1-e369754821cb     4                      0
    ## 12 31d0cfb8-21d7-414b4f-94999f-04a15ce39d78     4                      0
    ## 13 328e7cd6-6517-4f4044-8f8c86-c710a84e5639     3                      0
    ## 14 36584aec-f271-47484b-999391-417e2a3d6b59     3                      0
    ## 15 37b5a861-0f21-4e4942-909295-34826ecd950b     4                      1
    ## 16 38b615cf-0fd3-4f4d4e-bfbab1-a07658b413ce     2                      1
    ## 17 3aef5849-5ca7-4c4841-8a8584-e64b1a8d0c92     4                      0
    ## 18 3b6948fe-3409-4f4143-b3bab2-86301b529fc7     5                      0
    ## 19 3c1704f5-2473-474e4f-808982-f9830c51d7b2     2                      0
    ## 20 3e02914b-eb25-484243-909498-dcfa793514b2     5                      0
    ##    flag_low_cereal flag_low_oil flag_low_fcs flag_high_fcs flag_sd_foodgroup
    ## 1               NA           NA           NA            NA                NA
    ## 2               NA           NA           NA            NA                NA
    ## 3                1            1            0             0                 1
    ## 4                1            1            0             0                 1
    ## 5                1            1            0             0                 1
    ## 6                1            0            0             1                NA
    ## 7                0            0            0             0                NA
    ## 8                1            1            0             0                NA
    ## 9                0            1            0             1                NA
    ## 10              NA           NA           NA            NA                NA
    ## 11               0            0            0             0                NA
    ## 12               0            1            0             0                NA
    ## 13               1            1            0             0                NA
    ## 14               1            0            0             0                NA
    ## 15               1            0            0             0                NA
    ## 16               1            1            0             1                NA
    ## 17               0            0            0             0                NA
    ## 18               0            0            0             0                NA
    ## 19               0            0            0             0                NA
    ## 20               0            0            0             0                NA
    ##    flag_protein_rcsi flag_fcs_rcsi flag_high_rcsi flag_rcsi_children
    ## 1                 NA            NA             NA                 NA
    ## 2                 NA            NA             NA                 NA
    ## 3                  0             0              0                 NA
    ## 4                 NA            NA             NA                 NA
    ## 5                  0             0              0                 NA
    ## 6                  0             0              0                 NA
    ## 7                  0             0              0                 NA
    ## 8                  0             0              0                 NA
    ## 9                  0             0              0                 NA
    ## 10                NA            NA             NA                 NA
    ## 11                 0             0              0                 NA
    ## 12                 0             0              0                 NA
    ## 13                 0             0              0                 NA
    ## 14                 0             0              0                 NA
    ## 15                 0             0              0                 NA
    ## 16                NA            NA             NA                 NA
    ## 17                 0             0              0                 NA
    ## 18                NA            NA             NA                 NA
    ## 19                 0             0              0                 NA
    ## 20                 0             0              0                 NA
    ##    flag_fcsrcsi_box flag_sd_rcsicoping flag_severe_hhs flag_lcsi_coherence
    ## 1                NA                 NA              NA                  NA
    ## 2                NA                 NA              NA                  NA
    ## 3                NA                 NA               0                   0
    ## 4                NA                 NA               0                   0
    ## 5                NA                 NA               0                   0
    ## 6                NA                 NA               0                   0
    ## 7                NA                 NA               0                   0
    ## 8                NA                 NA               0                   0
    ## 9                NA                 NA               0                   0
    ## 10               NA                 NA              NA                  NA
    ## 11               NA                 NA               0                   1
    ## 12               NA                 NA               1                   0
    ## 13               NA                 NA               0                   0
    ## 14               NA                 NA               0                   0
    ## 15               NA                 NA               0                   0
    ## 16               NA                 NA               0                   0
    ## 17               NA                 NA               0                   0
    ## 18               NA                 NA               0                   0
    ## 19               NA                 NA               0                   0
    ## 20               NA                 NA               0                   0
    ##    flag_lcsi_severity flag_lcsi_na flag_lcsi_liv_livestock
    ## 1                  NA           NA                      NA
    ## 2                  NA           NA                      NA
    ## 3                  NA           NA                      NA
    ## 4                  NA           NA                      NA
    ## 5                  NA           NA                      NA
    ## 6                   1           NA                      NA
    ## 7                  NA           NA                      NA
    ## 8                  NA           NA                       1
    ## 9                  NA           NA                      NA
    ## 10                 NA           NA                      NA
    ## 11                  1           NA                      NA
    ## 12                  1           NA                      NA
    ## 13                 NA           NA                      NA
    ## 14                 NA           NA                      NA
    ## 15                 NA           NA                      NA
    ## 16                 NA           NA                      NA
    ## 17                 NA           NA                      NA
    ## 18                 NA           NA                      NA
    ## 19                 NA           NA                      NA
    ## 20                 NA           NA                      NA
    ##    flag_lcsi_liv_agriculture flag_lcsi_displ flag_fc_cell
    ## 1                         NA              NA           NA
    ## 2                         NA              NA           NA
    ## 3                         NA              NA            0
    ## 4                         NA              NA           NA
    ## 5                         NA              NA            0
    ## 6                         NA              NA            0
    ## 7                         NA              NA            0
    ## 8                         NA              NA            0
    ## 9                         NA              NA            0
    ## 10                        NA              NA           NA
    ## 11                         1              NA            0
    ## 12                        NA              NA            0
    ## 13                        NA              NA            0
    ## 14                        NA              NA            0
    ## 15                        NA              NA            0
    ## 16                        NA              NA           NA
    ## 17                        NA              NA            0
    ## 18                        NA              NA           NA
    ## 19                        NA              NA            0
    ## 20                        NA              NA            0
    ##    flag_low_sugar_cond_hdds
    ## 1                        NA
    ## 2                        NA
    ## 3                         0
    ## 4                         0
    ## 5                         0
    ## 6                         0
    ## 7                         0
    ## 8                         0
    ## 9                         0
    ## 10                       NA
    ## 11                        0
    ## 12                        0
    ## 13                        0
    ## 14                        0
    ## 15                        0
    ## 16                        0
    ## 17                        0
    ## 18                        0
    ## 19                        0
    ## 20                        0

### Example:: Check Food Security and Livelihoods Flags

``` r
nut_flags <- df_with_mfaz %>% 
  check_nut_flags(loop_index = "loop_index")
nut_flags %>% 
  dplyr::select(uuid, group,starts_with("flag_"), ends_with("noflag")) %>% 
  head(20)
```

    ##                                        uuid group flag_sd_mfaz
    ## 1  7b4261fa-61a5-4a4948-999093-13bc7e9f0658   All            0
    ## 2  83c0a56b-15fd-4f4349-b2bcbd-806912fb3c5d   All            0
    ## 3  6401c279-8a6f-464b4d-919598-da125739e64c   All            0
    ## 4  1ecfd059-c215-4d4746-94999b-87920feb4a6c   All            0
    ## 5  4b038c2e-25a6-484641-aca6a7-cf387e4b29d1   All            0
    ## 6  3b6948fe-3409-4f4143-b3bab2-86301b529fc7   All            0
    ## 7  512bce03-78ea-404742-8e8d83-e53a8296c0d4   All            0
    ## 8  1a8de690-60af-45494a-8b8487-78f45ec16b39   All            0
    ## 9  53a2e761-34cb-434c46-b3bdbc-b0fc1295673d   All            0
    ## 10 4d5b1089-1aec-424f49-aba5a9-b3ade80461fc   All            0
    ## 11 ef2963c7-ef67-4e4446-bab5b7-7e9d0431fa8c   All            0
    ## 12 98fdb3a2-2c1a-4f424b-8d8782-b21d683ea94f   All            0
    ## 13 1d7ca542-5ebf-434e44-949e9a-d3687ef9c145   All            0
    ## 14 a725301d-21b7-444c42-919f95-2f769503b184   All            0
    ## 15 4b038c2e-25a6-484641-aca6a7-cf387e4b29d1   All            0
    ## 16 1d7ca542-5ebf-434e44-949e9a-d3687ef9c145   All            0
    ## 17 3c1704f5-2473-474e4f-808982-f9830c51d7b2   All           NA
    ## 18 ef0d36a5-493b-444048-bbbab9-bf719e4850a6   All            0
    ## 19 31d0cfb8-21d7-414b4f-94999f-04a15ce39d78   All           NA
    ## 20 31d0cfb8-21d7-414b4f-94999f-04a15ce39d78   All           NA
    ##    flag_extreme_muac flag_edema_pitting mfaz_noflag mean_mfaz_noflag
    ## 1                  0                 NA       -3.40           -0.639
    ## 2                  0                 NA       -1.48           -0.639
    ## 3                  0                  0       -2.33           -0.639
    ## 4                  0                 NA       -2.38           -0.639
    ## 5                  0                  0       -1.30           -0.639
    ## 6                  0                 NA       -0.62           -0.639
    ## 7                  0                 NA       -0.80           -0.639
    ## 8                  0                 NA       -1.88           -0.639
    ## 9                  0                  0       -0.43           -0.639
    ## 10                 0                  0        0.42           -0.639
    ## 11                 0                 NA       -0.57           -0.639
    ## 12                 0                  0        0.51           -0.639
    ## 13                 0                  0       -0.73           -0.639
    ## 14                 0                  0        0.40           -0.639
    ## 15                 0                 NA        1.05           -0.639
    ## 16                 0                  0        1.63           -0.639
    ## 17                NA                  0          NA           -0.639
    ## 18                 0                  0        1.04           -0.639
    ## 19                NA                 NA          NA           -0.639
    ## 20                NA                 NA          NA           -0.639
    ##    sd_mfaz_noflag global_mfaz_noflag moderate_mfaz_noflag severe_mfaz_noflag
    ## 1            1.38                  1                    0                  1
    ## 2            1.38                  0                    0                  0
    ## 3            1.38                  1                    1                  0
    ## 4            1.38                  1                    1                  0
    ## 5            1.38                  0                    0                  0
    ## 6            1.38                  0                    0                  0
    ## 7            1.38                  0                    0                  0
    ## 8            1.38                  0                    0                  0
    ## 9            1.38                  0                    0                  0
    ## 10           1.38                  0                    0                  0
    ## 11           1.38                  0                    0                  0
    ## 12           1.38                  0                    0                  0
    ## 13           1.38                  0                    0                  0
    ## 14           1.38                  0                    0                  0
    ## 15           1.38                  0                    0                  0
    ## 16           1.38                  0                    0                  0
    ## 17           1.38                 NA                   NA                 NA
    ## 18           1.38                  0                    0                  0
    ## 19           1.38                 NA                   NA                 NA
    ## 20           1.38                 NA                   NA                 NA
    ##    gam_muac_noflag mam_muac_noflag sam_muac_noflag muac_noflag
    ## 1                1               1               0        12.1
    ## 2                0               0               0        12.5
    ## 3                0               0               0        12.8
    ## 4                0               0               0        13.2
    ## 5                0               0               0        13.7
    ## 6                0               0               0        13.7
    ## 7                0               0               0        13.8
    ## 8                0               0               0        14.1
    ## 9                0               0               0        14.4
    ## 10               0               0               0        14.7
    ## 11               0               0               0        14.7
    ## 12               0               0               0        14.8
    ## 13               0               0               0        15.0
    ## 14               0               0               0        15.4
    ## 15               0               0               0        15.4
    ## 16               0               0               0        15.8
    ## 17              NA              NA              NA          NA
    ## 18               0               0               0        17.8
    ## 19              NA              NA              NA          NA
    ## 20              NA              NA              NA          NA

### Example:: Check WASH Flags

``` r
container_df <- impactR4PHU_data_wash_template
wash_flags <- df %>% 
  check_wash_flags(data_container_loop = container_df,
                   grouping = "enumerator")
```

    ## Joining with `by = join_by(uuid)`

``` r
wash_flags %>% 
  dplyr::select(uuid, group,starts_with("flag_")) %>% 
  head(20)
```

    ##                                        uuid group flag_sd_litre flag_low_litre
    ## 1  0cfd1539-4be3-4c444a-8a8d8e-0d2a6bf74895     1            NA             NA
    ## 2  0fc8a427-f30e-4a4341-b3b5b4-08a6392ef4dc     5            NA             NA
    ## 3  14c3baf8-d4b0-43484c-8d8e8f-a5fd7134982e     2            NA             NA
    ## 4  1a8de690-60af-45494a-8b8487-78f45ec16b39     2            NA             NA
    ## 5  1c92baf4-107e-474c46-a3a8a5-6b2e815ad30c     2             0              0
    ## 6  1d7ca542-5ebf-434e44-949e9a-d3687ef9c145     5            NA             NA
    ## 7  1ecfd059-c215-4d4746-94999b-87920feb4a6c     2            NA             NA
    ## 8  205d37b1-5a6f-44484d-b3b1ba-4eafbdc50873     2            NA             NA
    ## 9  218f7539-061b-404f44-96989f-b345c89a6e21     2            NA             NA
    ## 10 2d56cf0a-a45c-444148-898e84-ab7f4de18259     3            NA             NA
    ## 11 3186cfde-19a7-434748-bbb7b1-e369754821cb     4            NA             NA
    ## 12 31d0cfb8-21d7-414b4f-94999f-04a15ce39d78     4            NA             NA
    ## 13 328e7cd6-6517-4f4044-8f8c86-c710a84e5639     3            NA             NA
    ## 14 36584aec-f271-47484b-999391-417e2a3d6b59     3            NA             NA
    ## 15 37b5a861-0f21-4e4942-909295-34826ecd950b     4            NA             NA
    ## 16 38b615cf-0fd3-4f4d4e-bfbab1-a07658b413ce     2            NA             NA
    ## 17 3aef5849-5ca7-4c4841-8a8584-e64b1a8d0c92     4            NA             NA
    ## 18 3b6948fe-3409-4f4143-b3bab2-86301b529fc7     5            NA             NA
    ## 19 3c1704f5-2473-474e4f-808982-f9830c51d7b2     2            NA             NA
    ## 20 3e02914b-eb25-484243-909498-dcfa793514b2     5            NA             NA
    ##    flag_high_litre flag_high_container flag_no_container flag_not_immediate
    ## 1               NA                  NA                NA                 NA
    ## 2               NA                  NA                NA                 NA
    ## 3               NA                  NA                 0                  0
    ## 4               NA                  NA                 1                  0
    ## 5                0                   0                 0                  1
    ## 6               NA                   0                 0                  0
    ## 7               NA                  NA                 0                  0
    ## 8               NA                   0                 0                  0
    ## 9               NA                   0                 0                  0
    ## 10              NA                  NA                NA                 NA
    ## 11              NA                  NA                 1                  1
    ## 12              NA                   0                 0                  0
    ## 13              NA                   0                 0                  0
    ## 14              NA                   0                 0                  0
    ## 15              NA                  NA                 0                  0
    ## 16              NA                  NA                 0                  0
    ## 17              NA                  NA                 0                  0
    ## 18              NA                  NA                 0                  0
    ## 19              NA                  NA                 0                  0
    ## 20              NA                   0                 0                  0

## Code of Conduct

Please note that the addindicators project is released with a
[Contributor Code of
Conduct](https://impact-initiatives.github.io/impactR4PHU/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
