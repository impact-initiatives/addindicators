
## addindicators

<!-- badges: start -->
[![R-CMD-check](https://github.com/impact-initiatives/addindicators/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/impact-initiatives/addindicators/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/impact-initiatives/addindicators/branch/master/graph/badge.svg?token=RlTJbum32D)](https://codecov.io/gh/impact-initiatives/addindicators)

<!-- badges: end -->

## Overview

`addindicators` is designed for creating and checking (occasionally)
composite indicators such as Food Consumption Indicators and others.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("impact-initiatives/addindicators")
```

## Examples

### Example:: Add Food Consumption Score (FCS)

``` r
library(addindicators)
df <- read.csv("[file path].csv")

df_with_fcs <- df %>% add_fcs(
  cutoffs = "normal 21.5-35",
  fcs_cereal = "cereal",
  fcs_legumes = "pulses",
  fcs_veg = "vegetables",
  fcs_fruit = "fruits",
  fcs_meat = "meat", 
  fcs_dairy = "milk",
  fcs_sugar = "sugar",
  fcs_oil = "oil"
 )
```

### Example:: Add Household Hunger Scale (HHS)

``` r
df_with_hhs <- df %>% add_hhs(
     hhs_nofoodhh_1 = "fs_hhs_nofood_yn",
     hhs_nofoodhh_1a = "fs_hhs_nofood_freq",
     hhs_sleephungry_2 = "fs_hhs_sleephungry_yn",
     hhs_sleephungry_2a = "fs_hhs_sleephungry_freq",
     hhs_alldaynight_3 = "fs_hhs_daynoteating_yn",
     hhs_alldaynight_3a = "fs_hhs_daynoteating_freq",
     yes_answer = "yes",
     no_answer = "no",
     rarely_answer = "rarely_1_2",
     sometimes_answer = "sometimes_3_10",
     often_answer = "often_10_times"
)
```

### Example:: Add Livelihood Coping Strategy score (LCSI)

``` r
df_with_lcsi <- df %>% add_lcsi(
 lcsi_stress_vars = c("stress1", "stress2", "stress3", "stress4"),
 lcsi_crisis_vars = c("crisis1", "crisis2", "crisis3"),
 lcsi_emergency_vars = c("emergency1", "emergency2", "emergency3"),
 yes_val = "Yes",
 no_val = "No",
 exhausted_val = "Exhausted",
 not_applicable_val = "Not Applicable"
)
```

### Example:: Add Reduced Household Coping Strategy score (rCSI)

``` r
df_with_rcsi <- df %>% add_rcsi(
  rCSILessQlty = "rCSILessQlty",
  rCSIBorrow = "rCSIBorrow",
  rCSIMealSize = "rCSIMealSize",
  rCSIMealAdult = "rCSIMealAdult",
  rCSIMealNb = "rCSIMealNb",
  new_colname = "rcsi"
)
```

### Example:: Add Food Consumption Matrix (FCM)

**Notice that these functions are also pipable**

``` r
df_with_fcm <- df %>%
  add_fcs() %>% 
  add_hhs() %>% 
  add_rcsi() %>% 
  add_lcsi() %>% 
  add_fcm_phase(
   fcs_column_name = "fcs_cat",
   rcsi_column_name = "rcsi_cat",
   hhs_column_name = "hhs_cat",
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

### Example:: Add FEWSNET Food Consumption-Livelihood Matrix (FCLCM)

**Notice that these functions are also pipable**

``` r
df_with_fclcm <- df_with_fcm %>% ## Taken from previous Example
  add_fclcm_phase()
```
