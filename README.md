
# impactR4PHU <a href='https://www.impact-initiatives.org'><img src='man/figures/impactR4PHU.png' align="right" height="138" /></a>

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

## Table of Contents

- [Installation](#installation)
- [Projects](#projects)
  - [Data Quality](#data-quality)
    - FSL
    - Mortality
    - IYCF
  - [Cleaning](#cleaning)
    - FSL
    - Mortality
    - IYCF
  - [Descriptive Analysis](#descriptive-analysis)
    - FSL
    - Mortality
    - IYCF
  - [IPHRA](#iphra)
    - Quality Report and Plausibility Checks
    - Cleaning
    - Analysis
  - [PH Integrated Tables](#ph-integrated-tables)
- [Potential Errors and How to fix
  them](#potential-errors-and-how-to-fix-them)
- [Standalone Functions](#standalone-functions)
  - [FSL ADD INDICATORS](#fsl-add-indicators)
    - FCS: Food Consumption Score
    - HHS: Household Hunger Scale
    - rCSI: Reduced Coping Strategy Index
    - LCSI: Livelihood Coping Strategy Index
    - HDDS: Household Dietary Diversity Score
    - FCM: Food Consumption Matrix
    - FCLCM: Food Consumption Livelihood Matrix
  - [Nutrition ADD INDICATORS](#nutrition-add-indicators)
    - MUAC: Mid-Upper Arm Circumference
    - MFAZ: MUAC for Age z-score
    - IYCF: Infant and Young Child Feeding Scores
  - [Checking Flags](#checking-flags)

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("impact-initiatives/impactR4PHU")
```

## Projects

Upon installing the impactR4PHU package, you will be able to access
pre-coded projects related to the various sectors of Public Health.
These projects aims to support country missions and the research
department to check/clean/and analyse indicatores related to Public
Health sectors.

To access these projects, follow the following instructions.

<figure>
<img src="./man/figures/projects_1.png"
alt="Go to File -&gt; New Project…" />
<figcaption aria-hidden="true">Go to File -&gt; New
Project…</figcaption>
</figure>

<figure>
<img src="./man/figures/projects_2.png" alt="Select New Directory" />
<figcaption aria-hidden="true">Select New Directory</figcaption>
</figure>

<figure>
<img src="./man/figures/projects_3.png"
alt="Scroll to find the respective projects" />
<figcaption aria-hidden="true">Scroll to find the respective
projects</figcaption>
</figure>

<figure>
<img src="./man/figures/projects_4.png"
alt="Select all the lines and click run" />
<figcaption aria-hidden="true">Select all the lines and click
run</figcaption>
</figure>

### Data Quality

The Data Quality and Plausibility Report serves as a crucial tool for
assessing the reliability and accuracy of the data collection of all
related public health indicators across different assessments. This
comprehensive analysis is designed to identify and address potential
issues within the data, ensuring that field teams are being informed on
potential issues detected in the data collection.

The report provides a detailed examination of the datasets, employing a
variety of metrics and methodologies to evaluate data quality and
plausibility. This includes checks for completeness, consistency, and
accuracy of the data collected. This report aims to uncover any
discrepancies, outliers, or anomalies that may suggest data collection,
entry errors, or underlying issues that could impact the integrity of
the findings.

#### FSL Section

This section includes:
<ul>
<li>
Overall Plausibility Report / By Enumerator
</li>
<li>
All the flags related to Food Security and Livelihoods (details shown
for each flag in the section)
</li>
<li>
Plots showing the distribution of the data.
</li>
</ul>

#### Mortality Section

This section includes:
<ul>
<li>
Overall Plausibility Report / By Enumerator
</li>
<li>
All the flags related to Mortality (details shown for each flag in the
section)
</li>
<li>
Number of deaths(overall and under 5)/joiners/leavers as well as person
times.
</li>
<li>
Plots showing the crude, under 5, and birth rates.
</li>
</ul>

#### IYCF Section

This section includes:
<ul>
<li>
Overall Plausibility Report / By Enumerator
</li>
<li>
All the flags related to IYCF (details shown for each flag in the
section)
</li>
<li>
Plots showing the distribution of the data.
</li>
</ul>

#### What to do next?

Please check each flag and the <strong>ACTION</strong> related to it and
act accordingly. Another output will be associated to this HTML, the
Excel file of the flags that were fired and requires follow-up with the
field team. Please check the README tab in the excel file. This file
will again be generated with the full data during the cleaning of the
dataset. So please do use this file during data collection and relate to
it in the final one to be filled.

### Cleaning

The Data Cleaning Template serves as a crucial tool for assessing the
data collection of all related public health indicators indicators
across different assessments. This comprehensive tool is designed to
identify and address potential issues within the data, ensuring that
field teams are being followed up on potential issues detected in the
data collection.

The report provides a detailed examination of the datasets, employing a
variety of metrics and methodologies to evaluate data quality. This
report aims to uncover any discrepancies, outliers, or anomalies that
may suggest data collection, entry errors, or underlying issues that
could impact the integrity of the findings.

#### FSL Section

<strong>Direct Checks:</strong>
<ul>
<li>
All FCS columns are 0. FCS Score is equal to 0. All values are changed
to NA.
</li>
<li>
All FCS columns are 7. All values are changed to NA.
</li>
<li>
All LCSI columns are not_applicable. All values are changed to NA.
</li>
<li>
The LCSI strategy related to Displacement/Migration but HH is not a
displaced group. Value of LCSI changed to NA.
</li>
<li>
The LCSI strategy related to Agriculture but HH do not have income type
related to agriculture. Value of LCSI changed to NA.
</li>
<li>
The LCSI strategy related to Livestock but HH do not have income type
related to livestock. Value of LCSI changed to NA.
</li>
</ul>
<strong>Follow-Up Checks:</strong>
<ul>
<li>
Check 1: rCSI Score is high while protein consumption is also reported
as frequent.
</li>
<li>
Check 2: HHs report using crisis or emergency strategies but not stress
strategies or Emergency and no crisis.
</li>
<li>
Check 3: HH that would have an acceptable FCS score and a high rCSI
score.
</li>
</ul>

#### Mortality Section (Individual Level)

<strong>Follow-Up Checks:</strong>
<ul>
<li>
Check 1: Respondent reported more than 2 death in the HH.
</li>
<li>
Check 2:Respondent reported sex of dead person male and a cause of death
related to female only.
</li>
<li>
Check 3:Respondent reported wrong dates leading to a negative person
time.
</li>
</ul>

#### Mortality Section (Household Level)

<strong>Follow-Up Checks</strong>
<ul>
<li>
Check 1: Respondent reported more than 2 death in the HH.
</li>
</ul>

#### What to do next?

Please check the files in the output/ folder. The cleaning_logbook.xlsx
already can contain some of the directly cleaned data. The
followup_request.xlsm file represent the checks that needs to be
followed up with the field team. Please follow the instruction in the
READ_ME tab to know how to fill the file. After filling the file, you
can merge the \[uuid/variable/old.value/new.value/issue\] from the
filled file with the cleaning_logbook.xlsx and add them to your cleaning
scripts.

### Descriptive Analysis

The Descriptive Analysis is an analytical platform that presents a
multitude of quantitative data tables. It encompasses a wide range of
public health indicators collected through the assessment process,
empowering users to examine and interpret complex datasets effectively.
This tool is structured to support understanding the distribution of
your data and support you writing your factsheets/outputs/reports, and
create other visualizations.

#### FSL Section

As you saw in the output folder, you will have another excel file
outputed from the analysis script.
<ul>
<li>
The Excel file includes 2 sheets. The first 2 are all the tables that
you see in the different sections of this HTML output. You can navigate
to respective tables through the first sheet “Table of Contents”.
</li>
<li>
Another output as well will include the IPC table.
</li>
</ul>

#### Mortality Section

As you saw in the output folder, you will have another excel file
outputed from the analysis script.
<ul>
<li>
The Excel file includes 2 sheets. The first 2 are all the tables that
you see in the different sections of this HTML output. You can navigate
to respective tables through the first sheet “Table of Contents”.
</li>
</ul>

### IPHRA

The use case for this toolkit is intended to be in acute crises where
there is a realistic possibility of deterioration of public health
outcomes in the population to be assessed. This is not intended to be an
urgent rapid assessment done within the first 72 hours, which tend to be
more qualitative, but instead the intended timeline should be after an
initial stabilization of a situation and population movements, maybe one
month after an initial shock or hazard, depending on the situation. The
general objective and purpose of an IPHRA assessment is “to assess the
severity of the public health situation and identify initial public
health priorities for response to mitigate excess morbidity,
malnutrition, and mortality.”

Please follow the instructions provided in the recorded training
sessions in the
<a href = "https://acted.sharepoint.com/sites/IMPACT-Public_health/SitePages/Toolkits.aspx">PHU
Intranet Page</a> to learn how to run the scripts.

#### Quality Report and Plausibility Checks

The Data Quality and Plausibility Report serves as a crucial tool for
assessing the reliability and accuracy of the IPHRA data collection
across different sectors such as Nutrition, Mortality, Water, Sanitation
and Hygiene (WASH), Food Security, and Livelihoods. This comprehensive
analysis is designed to identify and address potential issues within the
data, ensuring that field teams are being informed on potential issues
detected in the data collection.each of these sectors, the report
provides a detailed examination of the datasets, employing a variety of
metrics and methodologies to evaluate data quality and plausibility.
This includes checks for completeness, consistency, and accuracy of the
data collected. This report aims to uncover any discrepancies, outliers,
or anomalies that may suggest data collection, entry errors, or
underlying issues that could impact the integrity of the findings.

#### Cleaning

The IPHRA Cleaning toolkit is a tailored project to clean the collected
data following IMPACT’s guidance of quantitative data cleaning. The
project is divided in batches files that can be ran outside of R Studio
to reduce the interactions with R and allow any person run the scripts.
You are required to have R Tools and RStudio installed on your device to
be able to run the scripts.

#### Analysis

The IPHRA Tabular Analysis is an analytical platform that presents a
multitude of quantitative data tables. It encompasses a wide range of
indicators collected through the IPHRA assessment process, empowering
users to examine and interpret complex datasets effectively. This tool
is structured to support understanding the distribution of your data and
support you writing your factsheets/outputs/reports, and create other
visualizations.

### PH Integrated Tables

The Integrated Table serves as a comprehensive tool for evaluating
public health outcomes by assigning severity thresholds to various
indicators. These thresholds are categorized into different levels
(Extremely High, Very High, High, Moderate, Low) and are used to assess
the overall risk of excess mortality (RoEM).

Here is a table showing the different indicators and the thresholds

<figure>
<img src="./man/figures/ph_tables.png" alt="PH TABLES" />
<figcaption aria-hidden="true">PH TABLES</figcaption>
</figure>

#### Impact on Population (Health Outcomes)

<ul>
<li>
Mortality: Crude Mortality Rates
</li>
<li>
% non-trauma deaths: %s respresents non-trauma related causes of death
for HHs only reporting death
</li>
<li>
Children under-5 sick: %s of under-5 individuals reported being sick.
</li>
<li>
Unmet Healthcare Needs: %s of individuals reporting needing healthcare
needs but not receiving it.
</li>
<li>
relevant period AMN Phase if available: IPC AMN Phases (1-5)
</li>
</ul>

Mortality related indicators will only show if Mortality data was
collected. If so, another excel document will be required, and it is
generated by running the Mortality Descriptive Analysis project.

#### Direct Contributing Factors

<ul>
<li>
relevant period AFI Phase if available: IPC AFI Phases (1-5)
</li>
<li>
HH food consumption gap: %s of HH reported Food Consumption Matrix 3-4-5
</li>
<li>
Poor FCS: %s of HH reported poor Food Consumption Score
</li>
<li>
High rCSI: %s of HH reported high Reduced Coping Strategies Index
</li>
<li>
HHS score severe and very severe: %s of HH reported severe and very
severe Household Hunger Scale score
</li>
<li>
Emergency Coping Strategies: %s of HH reported emergency Livelihood
Coping Strategies Index
</li>
<li>
Improved Water Source: %s of HH reported having improved drinking water
sources
</li>
<li>
Water Quantity: %s of HH without access to a sufficient quantity of
drinking water
</li>
<li>
Improved Sanitation: %s of HH reported having improved sanitation
facilities
</li>
<li>
Access to handwashing facilities: %s of HH reported having access to
handwashing facilities
</li>
</ul>

Please note that Improved Water Source/Improved Sanitation/and Access to
handwashing facilities thresholds are dynamically calculated using the
distribution of the data (following the guidances from WASH cluster).

The median, 1st quantile, 3rd quantile, and the IQR (3rd quantile - 1st
quantile) are calculated and the thresholds are set as following:

<ul>
<li>
Extremely High: \< 1st quantile - IQR
</li>
<li>
Very High: \< 1st quantile
</li>
<li>
High: \< Median
</li>
<li>
Moderate: \< 3rd quantile
</li>
<li>
Low: \>= 3rd quantile
</li>
</ul>

However if (1st quantile - IQR) for the Extremely High threshold is
yielding a negative value, the IQR is flipped to be added to the Low
threshold as following:

<ul>
<li>
Extremely High: \< 1st quantile
</li>
<li>
Very High: \< Median
</li>
<li>
High: \< 3rd quantile
</li>
<li>
Moderate: \< 3rd quantile + IQR
</li>
<li>
Low: \>= 3rd quantile + IQR
</li>
</ul>

#### Contributing Factors

<ul>
<li>
Time to health facilities as per usual mode of transportation: %s of HHs
reported taking more than 60 min to reach health facilities
</li>
</ul>

The project will follow a user input requirements method. Some of the
WASH inputs might require visiting the humind package
(impact-initiatives-hppu/humind) to understand the categories of the
improved/unimproved drinking water and sanitation questions and
potentially other indicators.

The output will include 3 sheets:

<ul>
<li>
instructions: information about all the indicators and the thresholds,
even the WASH calculated ones
</li>
<li>
Data: %s and values by Admin 1 and National levels
</li>
<li>
Cat: Categories of the indicators by Admin 1 and National levels
</li>
</ul>

Here is an example (dummy Somalia Data):

<figure>
<img src="./man/figures/example_ph.png" alt="Example PH TABLES" />
<figcaption aria-hidden="true">Example PH TABLES</figcaption>
</figure>

## Potential Errors and How to fix them

During the run of the integrated projects, some errors might occur. <br>
Please see some of these errors that were already caught and the way to
solve them.

### lazy-load Error

![Lazy Load Error](./man/figures/just_restart.png) This error usually
appears after the scripts taking some time (5-10 mins) to load due to
the upload of the packages. <br> To solve this issue, only restart the
session or R by going to the tab part -\> Session -\> Restart R (or
CTRL + SHIFT + F10), and rerun the script again.

### ‘make’ not found

![make not found](./man/figures/make_error.png) This error usually
appears during the installation of packages the first time you are
running the scripts. The projects are wrapped within something called R
Environment that automatically install and load the necessary packages
for the project. Some of these packages are constantly maintained by
their owners and new versions are deployed regularly. The script try to
check for any updates in the package and upload the newest. However, if
the error still shows, specially with <strong>Error: Error Installing
package ‘XXXX’</strong>, you have two options.
<ul>
<li>
If you are comfortable handling some debugging, please try to find the
latest version of the mentioned package in the error in the web, usually
searching (PACKAGE NAME latest version in r) show you something called
the CRAN where you can see the latest version. Then, open renv.lock, and
target the actual package (attention, not where it is mentioned as
dependency to another package), then replace the version with the latest
one. Please do contact Abraham Azar
(<abraham.azar@impact-initiatives.org>) or the PHU team
(<impact.geneva.phu@impact-initiatives.org>) mentioning the updated
package name and the version.
</li>
<li>
If the first point was gibberish to you, directly contact Abraham Azar
(<abraham.azar@impact-initiatives.org>) or the PHU team
(<impact.geneva.phu@impact-initiatives.org>) with the error.
</li>
</ul>

### Wrong dates in mortality

<figure>
<img src="./man/figures/wrong_dates.png"
alt="Wrong dates in mortality" />
<figcaption aria-hidden="true">Wrong dates in mortality</figcaption>
</figure>

If the above error appears while running the mortality quality report or
descriptive analysis projects, this means that you have a possible issue
between the birth dates and the death dates in the death loop. Most
probably, one of the death have a recorded birth date after the recorded
death date. Make sure to fix the dates before running the scripts.

If any other error is hindering you to run the whole script, or any
other semantic issues (output do not make any sense), please contact
Abraham Azar (<abraham.azar@impact-initiatives.org>) or the PHU team
(<impact.geneva.phu@impact-initiatives.org>) with the error/issue.

## Standalone Functions (for both Analysis or Quality checks)

``` r
library(impactR4PHU)
df <- impactR4PHU_data_template
```

### FSL ADD Indicators

#### Example:: Add Food Consumption Score (FCS)

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
```

#### Example:: Add Household Hunger Scale (HHS)

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
```

#### Example:: Add Livelihood Coping Strategy score (LCSI)

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
```

#### Example:: Add Reduced Household Coping Strategy score (rCSI)

``` r
df_with_rcsi <- df_with_lcsi %>% add_rcsi(
  fsl_rcsi_lessquality = "fsl_rcsi_lessquality",
  fsl_rcsi_borrow = "fsl_rcsi_borrow",
  fsl_rcsi_mealsize = "fsl_rcsi_mealsize",
  fsl_rcsi_mealadult = "fsl_rcsi_mealadult",
  fsl_rcsi_mealnb = "fsl_rcsi_mealnb"
)
```

#### Example:: Add Household Dietary Diversity Score (HDDS)

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
```

#### Example:: Add Food Consumption Matrix (FCM) using FCS, RCSI, and HHS

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
    hhs_categories_little = "Little",
    hhs_categories_moderate = "Moderate",
    hhs_categories_severe = "Severe",
    hhs_categories_very_severe = "Very Severe"
  )
```

#### Example:: Add Food Consumption Matrix (FCM) using HDDS, RCSI, and HHS

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
    hhs_categories_little = "Little",
    hhs_categories_moderate = "Moderate",
    hhs_categories_severe = "Severe",
    hhs_categories_very_severe = "Very Severe"
  )
```

#### Example:: Add Food Consumption Matrix (FCM) using FCS and HHS

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
    hhs_categories_little = "Little",
    hhs_categories_moderate = "Moderate",
    hhs_categories_severe = "Severe",
    hhs_categories_very_severe = "Very Severe"
  )
```

#### Example:: Add Food Consumption Matrix (FCM) using HDDS and HHS

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
    hhs_categories_little = "Little",
    hhs_categories_moderate = "Moderate",
    hhs_categories_severe = "Severe",
    hhs_categories_very_severe = "Very Severe"
  )
```

#### Example:: Add FEWSNET Food Consumption-Livelihood Matrix (FCLCM)

**Notice that these functions are also pipable**

``` r
df_with_fclcm <- df_with_fcm_1 %>% ## Taken from previous Example
  add_fclcm_phase()
```

### NUTRITION ADD Indicators

``` r
df_nut <- impactR4PHU_data_nut_template
```

#### Example:: Add MUAC

``` r
df_with_muac <- df_nut %>% 
  add_muac()
```

#### Example:: Add MFAZ

``` r
df_with_mfaz <- df_with_muac %>% 
  add_mfaz()
```

    ## ================================================================================

#### Example:: Add IYCF

``` r
df_iycf <- impactR4PHU_iycf_template_data
df_with_iycf <- df_iycf %>% 
  add_iycf(uuid = "_submission__uuid",
           age_months = "child_age_months_2")
```

### Checking Flags

``` r
tool <- impactR4PHU_survey_template
```

#### Example:: Check Food Security and Livelihoods Flags

``` r
fsl_flags <- df_with_fclcm %>% 
  check_fsl_flags(tool.survey = tool)
```

#### Example:: Check Anthropometric Flags

``` r
anthro_flags <- df_with_mfaz %>% 
  check_anthro_flags(loop_index = "loop_index")
```

#### Example:: Check WASH Flags

``` r
container_df <- impactR4PHU_data_wash_template
wash_flags <- df %>% 
  check_wash_flags(data_container_loop = container_df)
```

    ## Joining with `by = join_by(uuid)`

#### Example:: Check Health Flags (to add more flags related to WGSS)

``` r
msna_data <- impactR4PHU_MSNA_template_data
health_flags <- check_health_flags(
  .dataset = msna_data
)
```

#### Example:: Check IYCF Flags

``` r
iycf_flags <- check_iycf_flags(
  .dataset = df_with_iycf,
  age_months = "child_age_months_2",
  uuid = "_submission__uuid"
)
```

## Code of Conduct

Please note that the impactR4PHU project is released with a [Contributor
Code of
Conduct](https://impact-initiatives.github.io/impactR4PHU/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
