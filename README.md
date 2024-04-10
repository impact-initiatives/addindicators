
## addindicators

<!-- badges: start -->

[![Contributor
Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](code_of_conduct.md)
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

``` r
library(addindicators)
df <- addindicators_MSNA_template_data
```

### Example:: Add Food Consumption Score (FCS)

``` r
df_with_fcs <- df %>% add_fcs(
  cutoffs = "normal",
  fsl_fcs_cereal = "fs_fcs_cereals_grains_roots_tubers",
  fsl_fcs_legumes = "fs_fcs_beans_nuts",
  fsl_fcs_veg = "fs_fcs_vegetables_leaves",
  fsl_fcs_fruit = "fs_fcs_fruit",
  fsl_fcs_meat = "fs_fcs_meat_fish_eggs",
  fsl_fcs_dairy = "fs_fcs_dairy",
  fsl_fcs_sugar = "fs_fcs_sugar",
  fsl_fcs_oil = "fs_fcs_oil_fat_butter"
)
df_with_fcs %>%
  dplyr::select(
    uuid,  fsl_fcs_score, fsl_fcs_cat, fcs_weight_cereal1, fcs_weight_legume2,
    fcs_weight_dairy3, fcs_weight_meat4, fcs_weight_veg5,
    fcs_weight_fruit6, fcs_weight_oil7, fcs_weight_sugar8
  ) %>%
  head(20)
```

    ## # A tibble: 20 × 11
    ##    uuid          fsl_fcs_score fsl_fcs_cat fcs_weight_cereal1 fcs_weight_legume2
    ##    <chr>                 <dbl> <chr>                    <dbl>              <dbl>
    ##  1 eaf540cd-32b…          48.5 Acceptable                   0                  0
    ##  2 89e706c3-53d…          50.5 Acceptable                   4                  6
    ##  3 afd921c6-e54…          46.5 Acceptable                   8                  0
    ##  4 d8b05f39-ba8…          42.5 Acceptable                  14                  3
    ##  5 d6b42f9e-c20…          55.5 Acceptable                   6                  6
    ##  6 f1b9ec67-20d…          37.5 Acceptable                   0                  6
    ##  7 95ea286d-ae8…          50.5 Acceptable                   4                 15
    ##  8 85b4a96f-cea…          31.5 Borderline                   0                 15
    ##  9 ef13a764-0af…          49.5 Acceptable                  14                  0
    ## 10 1a69e87b-ec6…          52.5 Acceptable                   8                 12
    ## 11 5613d0fe-34d…          69   Acceptable                  12                  3
    ## 12 091aef7d-2b3…          34   Borderline                   4                  0
    ## 13 e21a34f5-1a4…          36   Acceptable                   0                  6
    ## 14 42dc8573-e2d…          39   Acceptable                  14                  3
    ## 15 3a180db5-d12…          47   Acceptable                  10                  9
    ## 16 789a632b-53d…          46.5 Acceptable                   4                  9
    ## 17 cd41675b-eb4…          61.5 Acceptable                  14                 18
    ## 18 f741c29d-b7c…          77   Acceptable                  10                 12
    ## 19 2516eba7-789…          76.5 Acceptable                   2                 15
    ## 20 c7896215-b36…          20.5 Poor                         2                  3
    ## # ℹ 6 more variables: fcs_weight_dairy3 <dbl>, fcs_weight_meat4 <dbl>,
    ## #   fcs_weight_veg5 <dbl>, fcs_weight_fruit6 <dbl>, fcs_weight_oil7 <dbl>,
    ## #   fcs_weight_sugar8 <dbl>

### Example:: Add Household Hunger Scale (HHS)

``` r
df_with_hhs <- df_with_fcs %>% add_hhs(
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
df_with_hhs %>%
  dplyr::select(
    uuid, hhs_comp1, hhs_comp2, hhs_comp3,
    hhs_score, hhs_cat_ipc, hhs_cat, hh_size
  ) %>%
  head(20)
```

    ##                                        uuid hhs_comp1 hhs_comp2 hhs_comp3
    ## 1  eaf540cd-32bd-41474b-b4beb5-d62fc987e45a         0         0         0
    ## 2  89e706c3-53d8-4a4049-898586-4926085db71e         1         0         0
    ## 3  afd921c6-e54a-4c4740-919c93-87f59bd0e63a         0         2         0
    ## 4  d8b05f39-ba85-494c4d-808c84-9dc57823a4f1         0         0         0
    ## 5  d6b42f9e-c209-4c4541-808a81-86bea53df142         0         0         0
    ## 6  f1b9ec67-20db-47404d-a3ada0-1a37e5c49d02         1         0         2
    ## 7  95ea286d-ae86-47404a-828487-feba6d1503c9         0         0         0
    ## 8  85b4a96f-cea2-4f4b48-9d929f-5d76892f31b0         0         0         0
    ## 9  ef13a764-0af7-4f494c-838b88-6cb31a50842e         1         0         1
    ## 10 1a69e87b-ec61-4e4a40-8f868a-fe24c6a705bd         1         1         1
    ## 11 5613d0fe-34dc-474c43-b4b0bd-36a4c8edf902         0         0         0
    ## 12 091aef7d-2b31-4f4741-a5a8af-36e8f1bd075a         0         2         0
    ## 13 e21a34f5-1a46-42404b-b7b6be-7bc9286d0f13         1         0         0
    ## 14 42dc8573-e2d0-43484b-aaada2-c37ef865d041         0         0         0
    ## 15 3a180db5-d126-4d4b49-808d88-b3e5c71d908f         1         1         1
    ## 16 789a632b-53da-4c4f40-a0a1ad-f53ca2e9074b         0         0         0
    ## 17 cd41675b-eb48-444e4f-b8b7b3-1e493cb02f5a         0         0         0
    ## 18 f741c29d-b7c5-424a4d-94999c-6018bac9274e         0         0         0
    ## 19 2516eba7-789c-4c4b41-afa0ad-f0a7365bd81c         2         0         0
    ## 20 c7896215-b36f-40444c-aaa2af-fa4d37c6502e         1         1         2
    ##    hhs_score hhs_cat_ipc      hhs_cat hh_size
    ## 1          0        None No or Little      19
    ## 2          1      Little No or Little      21
    ## 3          2    Moderate     Moderate      21
    ## 4          0        None No or Little      23
    ## 5          0        None No or Little      22
    ## 6          3    Moderate     Moderate      19
    ## 7          0        None No or Little      22
    ## 8          0        None No or Little      17
    ## 9          2    Moderate     Moderate      21
    ## 10         3    Moderate     Moderate      19
    ## 11         0        None No or Little      16
    ## 12         2    Moderate     Moderate      17
    ## 13         1      Little No or Little      19
    ## 14         0        None No or Little      20
    ## 15         3    Moderate     Moderate      19
    ## 16         0        None No or Little      19
    ## 17         0        None No or Little      17
    ## 18         0        None No or Little      22
    ## 19         2    Moderate     Moderate      23
    ## 20         4      Severe       Severe      21

### Example:: Add Livelihood Coping Strategy score (LCSI)

``` r
df_with_lcsi <- df_with_hhs %>% add_lcsi(
  lcsi_stress_vars = c("liv_stress_lcsi_1", "liv_stress_lcsi_2", "liv_stress_lcsi_3", "liv_stress_lcsi_4"),
  lcsi_crisis_vars = c("liv_crisis_lcsi_1", "liv_crisis_lcsi_2", "liv_crisis_lcsi_3"),
  lcsi_emergency_vars = c("liv_emerg_lcsi_1", "liv_emerg_lcsi_2", "liv_emerg_lcsi_3"),
  yes_val = "yes",
  no_val = "no_had_no_need",
  exhausted_val = "no_exhausted",
  not_applicable_val = "not_applicable"
)
df_with_lcsi %>%
  dplyr::select(uuid, lcsi_cat, lcsi_cat_exhaust, lcsi_cat_yes) %>%
  head(20)
```

    ##                                        uuid  lcsi_cat lcsi_cat_exhaust
    ## 1  eaf540cd-32bd-41474b-b4beb5-d62fc987e45a Emergency        Emergency
    ## 2  89e706c3-53d8-4a4049-898586-4926085db71e Emergency        Emergency
    ## 3  afd921c6-e54a-4c4740-919c93-87f59bd0e63a Emergency        Emergency
    ## 4  d8b05f39-ba85-494c4d-808c84-9dc57823a4f1 Emergency        Emergency
    ## 5  d6b42f9e-c209-4c4541-808a81-86bea53df142 Emergency        Emergency
    ## 6  f1b9ec67-20db-47404d-a3ada0-1a37e5c49d02 Emergency        Emergency
    ## 7  95ea286d-ae86-47404a-828487-feba6d1503c9 Emergency        Emergency
    ## 8  85b4a96f-cea2-4f4b48-9d929f-5d76892f31b0 Emergency           Crisis
    ## 9  ef13a764-0af7-4f494c-838b88-6cb31a50842e Emergency        Emergency
    ## 10 1a69e87b-ec61-4e4a40-8f868a-fe24c6a705bd Emergency        Emergency
    ## 11 5613d0fe-34dc-474c43-b4b0bd-36a4c8edf902 Emergency             None
    ## 12 091aef7d-2b31-4f4741-a5a8af-36e8f1bd075a    Crisis           Crisis
    ## 13 e21a34f5-1a46-42404b-b7b6be-7bc9286d0f13    Crisis           Crisis
    ## 14 42dc8573-e2d0-43484b-aaada2-c37ef865d041 Emergency        Emergency
    ## 15 3a180db5-d126-4d4b49-808d88-b3e5c71d908f Emergency             None
    ## 16 789a632b-53da-4c4f40-a0a1ad-f53ca2e9074b Emergency           Stress
    ## 17 cd41675b-eb48-444e4f-b8b7b3-1e493cb02f5a    Crisis           Crisis
    ## 18 f741c29d-b7c5-424a4d-94999c-6018bac9274e Emergency        Emergency
    ## 19 2516eba7-789c-4c4b41-afa0ad-f0a7365bd81c Emergency        Emergency
    ## 20 c7896215-b36f-40444c-aaa2af-fa4d37c6502e Emergency           Crisis
    ##    lcsi_cat_yes
    ## 1     Emergency
    ## 2     Emergency
    ## 3        Crisis
    ## 4        Crisis
    ## 5     Emergency
    ## 6        Stress
    ## 7        Crisis
    ## 8     Emergency
    ## 9     Emergency
    ## 10    Emergency
    ## 11    Emergency
    ## 12       Crisis
    ## 13       Stress
    ## 14       Stress
    ## 15    Emergency
    ## 16    Emergency
    ## 17       Crisis
    ## 18       Crisis
    ## 19    Emergency
    ## 20    Emergency

### Example:: Add Reduced Household Coping Strategy score (rCSI)

``` r
df_with_rcsi <- df_with_lcsi %>% add_rcsi(
  rCSILessQlty = "rCSILessQlty",
  rCSIBorrow = "rCSIBorrow",
  rCSIMealSize = "rCSIMealSize",
  rCSIMealAdult = "rCSIMealAdult",
  rCSIMealNb = "rCSIMealNb",
  new_colname = "rcsi"
)
```

    ## Variable name for rcsi score is rcsi_score

    ## Variable name for rcsi category is rcsi_cat

``` r
df_with_rcsi %>%
  dplyr::select(uuid, rcsi_score, rcsi_cat) %>%
  head(20)
```

    ##                                        uuid rcsi_score rcsi_cat
    ## 1  eaf540cd-32bd-41474b-b4beb5-d62fc987e45a         34     High
    ## 2  89e706c3-53d8-4a4049-898586-4926085db71e         37     High
    ## 3  afd921c6-e54a-4c4740-919c93-87f59bd0e63a         30     High
    ## 4  d8b05f39-ba85-494c4d-808c84-9dc57823a4f1         29     High
    ## 5  d6b42f9e-c209-4c4541-808a81-86bea53df142         14   Medium
    ## 6  f1b9ec67-20db-47404d-a3ada0-1a37e5c49d02         37     High
    ## 7  95ea286d-ae86-47404a-828487-feba6d1503c9         20     High
    ## 8  85b4a96f-cea2-4f4b48-9d929f-5d76892f31b0         31     High
    ## 9  ef13a764-0af7-4f494c-838b88-6cb31a50842e         32     High
    ## 10 1a69e87b-ec61-4e4a40-8f868a-fe24c6a705bd         17   Medium
    ## 11 5613d0fe-34dc-474c43-b4b0bd-36a4c8edf902          9   Medium
    ## 12 091aef7d-2b31-4f4741-a5a8af-36e8f1bd075a          8   Medium
    ## 13 e21a34f5-1a46-42404b-b7b6be-7bc9286d0f13         35     High
    ## 14 42dc8573-e2d0-43484b-aaada2-c37ef865d041         17   Medium
    ## 15 3a180db5-d126-4d4b49-808d88-b3e5c71d908f         40     High
    ## 16 789a632b-53da-4c4f40-a0a1ad-f53ca2e9074b         31     High
    ## 17 cd41675b-eb48-444e4f-b8b7b3-1e493cb02f5a         24     High
    ## 18 f741c29d-b7c5-424a4d-94999c-6018bac9274e         15   Medium
    ## 19 2516eba7-789c-4c4b41-afa0ad-f0a7365bd81c         34     High
    ## 20 c7896215-b36f-40444c-aaa2af-fa4d37c6502e         25     High

### Example:: Add Food Consumption Matrix (FCM)

**Notice that these functions are also pipable**

``` r
df_with_fcm <- df_with_rcsi %>%
  add_fcm_phase(
    fcs_column_name = "fsl_fcs_cat",
    rcsi_column_name = "rcsi_cat",
    hhs_column_name = "hhs_cat_ipc",
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
df_with_fcm %>%
  dplyr::select(uuid, fc_cell, fc_phase) %>%
  head(20)
```

    ##                                        uuid fc_cell   fc_phase
    ## 1  eaf540cd-32bd-41474b-b4beb5-d62fc987e45a      31 Phase 2 FC
    ## 2  89e706c3-53d8-4a4049-898586-4926085db71e      32 Phase 2 FC
    ## 3  afd921c6-e54a-4c4740-919c93-87f59bd0e63a      33 Phase 3 FC
    ## 4  d8b05f39-ba85-494c4d-808c84-9dc57823a4f1      31 Phase 2 FC
    ## 5  d6b42f9e-c209-4c4541-808a81-86bea53df142      16 Phase 2 FC
    ## 6  f1b9ec67-20db-47404d-a3ada0-1a37e5c49d02      33 Phase 3 FC
    ## 7  95ea286d-ae86-47404a-828487-feba6d1503c9      31 Phase 2 FC
    ## 8  85b4a96f-cea2-4f4b48-9d929f-5d76892f31b0      36 Phase 2 FC
    ## 9  ef13a764-0af7-4f494c-838b88-6cb31a50842e      33 Phase 3 FC
    ## 10 1a69e87b-ec61-4e4a40-8f868a-fe24c6a705bd      18 Phase 2 FC
    ## 11 5613d0fe-34dc-474c43-b4b0bd-36a4c8edf902      16 Phase 2 FC
    ## 12 091aef7d-2b31-4f4741-a5a8af-36e8f1bd075a      23 Phase 3 FC
    ## 13 e21a34f5-1a46-42404b-b7b6be-7bc9286d0f13      32 Phase 2 FC
    ## 14 42dc8573-e2d0-43484b-aaada2-c37ef865d041      16 Phase 2 FC
    ## 15 3a180db5-d126-4d4b49-808d88-b3e5c71d908f      33 Phase 3 FC
    ## 16 789a632b-53da-4c4f40-a0a1ad-f53ca2e9074b      31 Phase 2 FC
    ## 17 cd41675b-eb48-444e4f-b8b7b3-1e493cb02f5a      31 Phase 2 FC
    ## 18 f741c29d-b7c5-424a4d-94999c-6018bac9274e      16 Phase 2 FC
    ## 19 2516eba7-789c-4c4b41-afa0ad-f0a7365bd81c      33 Phase 3 FC
    ## 20 c7896215-b36f-40444c-aaa2af-fa4d37c6502e      44 Phase 4 FC

### Example:: Add FEWSNET Food Consumption-Livelihood Matrix (FCLCM)

**Notice that these functions are also pipable**

``` r
df_with_fclcm <- df_with_fcm %>% ## Taken from previous Example
  add_fclcm_phase()
df_with_fclcm %>%
  dplyr::select(uuid, fclcm_phase) %>%
  head(20)
```

    ##                                        uuid  fclcm_phase
    ## 1  eaf540cd-32bd-41474b-b4beb5-d62fc987e45a Phase 3 FCLC
    ## 2  89e706c3-53d8-4a4049-898586-4926085db71e Phase 3 FCLC
    ## 3  afd921c6-e54a-4c4740-919c93-87f59bd0e63a Phase 4 FCLC
    ## 4  d8b05f39-ba85-494c4d-808c84-9dc57823a4f1 Phase 3 FCLC
    ## 5  d6b42f9e-c209-4c4541-808a81-86bea53df142 Phase 3 FCLC
    ## 6  f1b9ec67-20db-47404d-a3ada0-1a37e5c49d02 Phase 4 FCLC
    ## 7  95ea286d-ae86-47404a-828487-feba6d1503c9 Phase 3 FCLC
    ## 8  85b4a96f-cea2-4f4b48-9d929f-5d76892f31b0 Phase 3 FCLC
    ## 9  ef13a764-0af7-4f494c-838b88-6cb31a50842e Phase 4 FCLC
    ## 10 1a69e87b-ec61-4e4a40-8f868a-fe24c6a705bd Phase 3 FCLC
    ## 11 5613d0fe-34dc-474c43-b4b0bd-36a4c8edf902 Phase 3 FCLC
    ## 12 091aef7d-2b31-4f4741-a5a8af-36e8f1bd075a Phase 3 FCLC
    ## 13 e21a34f5-1a46-42404b-b7b6be-7bc9286d0f13 Phase 3 FCLC
    ## 14 42dc8573-e2d0-43484b-aaada2-c37ef865d041 Phase 3 FCLC
    ## 15 3a180db5-d126-4d4b49-808d88-b3e5c71d908f Phase 4 FCLC
    ## 16 789a632b-53da-4c4f40-a0a1ad-f53ca2e9074b Phase 3 FCLC
    ## 17 cd41675b-eb48-444e4f-b8b7b3-1e493cb02f5a Phase 3 FCLC
    ## 18 f741c29d-b7c5-424a4d-94999c-6018bac9274e Phase 3 FCLC
    ## 19 2516eba7-789c-4c4b41-afa0ad-f0a7365bd81c Phase 4 FCLC
    ## 20 c7896215-b36f-40444c-aaa2af-fa4d37c6502e Phase 4 FCLC

### Example:: Review of indicators

The logic behind *review_variables* is to compare the results from 2
codes to create the composite variable.

In this example, the Food Consumption Score from the first example will
be compared.

``` r
review_df <- addindicators_MSNA_template_data %>% add_fcs(
  cutoffs = "normal",
  fsl_fcs_cereal = "fs_fcs_cereals_grains_roots_tubers",
  fsl_fcs_legumes = "fs_fcs_beans_nuts",
  fsl_fcs_veg = "fs_fcs_vegetables_leaves",
  fsl_fcs_fruit = "fs_fcs_fruit",
  fsl_fcs_meat = "fs_fcs_meat_fish_eggs",
  fsl_fcs_dairy = "fs_fcs_dairy",
  fsl_fcs_sugar = "fs_fcs_sugar",
  fsl_fcs_oil = "fs_fcs_oil_fat_butter"
)
```

The new results and the results to be reviewed are bound together by the
*uuid*.

``` r
binded_df <- df_with_fcs %>%
  dplyr::full_join(review_df, by = "uuid")
```

There are 2 functions to review: - review_one_variable, to review only
one variable - review_variables, a wrapper around review_one_variable to
be able to review several variables.

#### review_one_variable

``` r
review_one_variable <- review_one_variable(binded_df,
  column_to_review = "fsl_fcs_cat.x",
  column_to_compare_with = "fsl_fcs_cat.y"
)

review_one_variable$review_check_fsl_fcs_cat.x %>% mean()
```

    ## [1] 1

``` r
review_one_variable$review_comment_fsl_fcs_cat.x %>% table(useNA = "ifany")
```

    ## .
    ## Same results 
    ##          100

#### review_variables

``` r
review_results <- review_variables(binded_df,
  columns_to_review = c("fsl_fcs_score.x", "fsl_fcs_cat.x"),
  columns_to_compare_with = c("fsl_fcs_score.y", "fsl_fcs_cat.y")
)

review_results$review_table %>%
  dplyr::group_by(variable) %>%
  dplyr::summarise(prop_correction = mean(review_check))
```

    ## # A tibble: 2 × 2
    ##   variable        prop_correction
    ##   <chr>                     <dbl>
    ## 1 fsl_fcs_cat.x                 1
    ## 2 fsl_fcs_score.x               1

``` r
review_results$review_table %>%
  dplyr::group_by(variable, review_comment) %>%
  dplyr::tally(sort = T)
```

    ## # A tibble: 2 × 3
    ## # Groups:   variable [2]
    ##   variable        review_comment     n
    ##   <chr>           <glue>         <int>
    ## 1 fsl_fcs_cat.x   Same results     100
    ## 2 fsl_fcs_score.x Same results     100

#### Examples when differences exists

``` r
test_categorical <- data.frame(
  test = c(
    "test equality",
    "test difference",
    "test Missing in y",
    "test Missing in x",
    "test equality missing in both"
  ),
  var_x = c("A", "B", "C", NA, NA),
  var_y = c("A", "A", NA, "D", NA),
  uuid = letters[1:5]
)
review_one_variable(test_categorical,
  column_to_review = "var_x",
  column_to_compare_with = "var_y"
)
```

    ##   uuid review_check_var_x review_comment_var_x
    ## 1    a               TRUE         Same results
    ## 2    b              FALSE    Different results
    ## 3    c              FALSE     Missing in var_y
    ## 4    d              FALSE     Missing in var_x
    ## 5    e               TRUE         Same results

## Code of Conduct

Please note that the addindicators project is released with a
[Contributor Code of
Conduct](https://impact-initiatives.github.io/addindicators/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
