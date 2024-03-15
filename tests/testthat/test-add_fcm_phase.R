library(dplyr)


###### Sad Path #######

testthat::test_that("Check input type -- dataset", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))
  testthat::expect_error(add_fcm_phase(.dataset = 0))
  testthat::expect_error(add_fcm_phase(.dataset = "x"))
  testthat::expect_error(add_fcm_phase(.dataset = 1.0))
  testthat::expect_error(add_fcm_phase(.dataset = F))
  testthat::expect_error(add_fcm_phase(.dataset = list()))
})


testthat::test_that("Check dataframe empty", {
  df1 <- data.frame()
  testthat::expect_error(add_fcm_phase(.dataset = df1))
})

testthat::test_that("Check for missing columns", {
  load(testthat::test_path("testdata", "test_df_with_calculation.rda"))

  testthat::expect_error(add_fcm_phase(
    .dataset = test_df_calculated_values %>% dplyr::select(-fsl_hhs_cat),
    hhs_column_name = "fsl_hhs_cat"
  ))

})

testthat::test_that("Checking column values hhs - [None/No or Little/Moderate/Severe/Very Severe]", {
  load(testthat::test_path("testdata", "test_df_with_calculation.rda"))

  set.seed(30)
  test_df_calculated_values[sample.int(nrow(test_df_calculated_values), 3), c("fsl_hhs_cat")] <- "none"
  set.seed(29)
  test_df_calculated_values[sample.int(nrow(test_df_calculated_values), 3), c("fsl_hhs_cat")] <- "little"

  set.seed(12)
  test_df_calculated_values[sample.int(nrow(test_df_calculated_values), 3), c("fsl_hhs_cat")] <- "random_value"
  testthat::expect_error(add_fcm_phase(
    .dataset = test_df_calculated_values
    ))
})

testthat::test_that("Checking column values rcsi - [No to Low/Medium/High]", {
  load(testthat::test_path("testdata", "test_df_with_calculation.rda"))

  set.seed(30)
  test_df_calculated_values[sample.int(nrow(test_df_calculated_values), 3), c("fsl_rcsi_cat")] <- "none"
  set.seed(29)
  test_df_calculated_values[sample.int(nrow(test_df_calculated_values), 3), c("fsl_rcsi_cat")] <- "med"

  set.seed(12)
  test_df_calculated_values[sample.int(nrow(test_df_calculated_values), 3), c("fsl_rcsi_cat")] <- "random_value"
  testthat::expect_error(add_fcm_phase(
    .dataset = test_df_calculated_values
  ))
})

testthat::test_that("Checking column values fcs - [Acceptable/Poor/Borderline]", {
  load(testthat::test_path("testdata", "test_df_with_calculation.rda"))

  set.seed(30)
  test_df_calculated_values[sample.int(nrow(test_df_calculated_values), 3), c("fsl_fcs_cat")] <- "accep"
  set.seed(29)
  test_df_calculated_values[sample.int(nrow(test_df_calculated_values), 3), c("fsl_fcs_cat")] <- "poor"

  set.seed(12)
  test_df_calculated_values[sample.int(nrow(test_df_calculated_values), 3), c("fsl_fcs_cat")] <- "random_value"
  testthat::expect_error(add_fcm_phase(
    .dataset = test_df_calculated_values
  ))
})

testthat::test_that("Checking column values hdds - [Low/Medium/High]", {
  load(testthat::test_path("testdata", "test_df_with_calculation.rda"))

  set.seed(30)
  test_df_calculated_values[sample.int(nrow(test_df_calculated_values), 3), c("fsl_hdds_cat")] <- "low"
  set.seed(29)
  test_df_calculated_values[sample.int(nrow(test_df_calculated_values), 3), c("fsl_hdds_cat")] <- "Very High"

  set.seed(12)
  test_df_calculated_values[sample.int(nrow(test_df_calculated_values), 3), c("fsl_hdds_cat")] <- "random_value"
  testthat::expect_error(add_fcm_phase(
    .dataset = test_df_calculated_values
  ))
})

test_df <- expand.grid(
      fcs_cat = c("Acceptable", "Poor", "Borderline"),
      rcsi_cat = c("No to Low", "Medium", "High"),
      hhs_cat = c("None", "Moderate", "Very Severe", "Severe", "No or Little")
    ) |>
      as.data.frame()
#### Happy Path ####

testthat::test_that("check if function producing expected output with (FCS/RCSI/HHS)", {
  test_df <- expand.grid(
    fsl_fcs_cat = c("Acceptable", "Poor", "Borderline"),
    fsl_rcsi_cat = c("No to Low", "Medium", "High"),
    fsl_hhs_cat = c("None", "Moderate", "Very Severe", "Severe", "No or Little")
  ) |>
    as.data.frame()
  expected_output_success <- test_df |>
    dplyr::mutate(fc_cell = dplyr::case_when(
      fsl_fcs_cat == "Acceptable" & fsl_rcsi_cat == "No to Low" & fsl_hhs_cat == "None" ~ 1,
      fsl_fcs_cat == "Poor" & fsl_rcsi_cat == "No to Low" & fsl_hhs_cat == "None" ~ 11,
      fsl_fcs_cat == "Borderline" & fsl_rcsi_cat == "No to Low" & fsl_hhs_cat == "None" ~ 6,
      fsl_fcs_cat == "Acceptable" & fsl_rcsi_cat == "Medium" & fsl_hhs_cat == "None" ~ 16,
      fsl_fcs_cat == "Poor" & fsl_rcsi_cat == "Medium" & fsl_hhs_cat == "None" ~ 26,
      fsl_fcs_cat == "Borderline" & fsl_rcsi_cat == "Medium" & fsl_hhs_cat == "None" ~ 21,
      fsl_fcs_cat == "Acceptable" & fsl_rcsi_cat == "High" & fsl_hhs_cat == "None" ~ 31,
      fsl_fcs_cat == "Poor" & fsl_rcsi_cat == "High" & fsl_hhs_cat == "None" ~ 41,
      fsl_fcs_cat == "Borderline" & fsl_rcsi_cat == "High" & fsl_hhs_cat == "None" ~ 36,
      fsl_fcs_cat == "Acceptable" & fsl_rcsi_cat == "No to Low" & fsl_hhs_cat == "No or Little" ~ 2,
      fsl_fcs_cat == "Poor" & fsl_rcsi_cat == "No to Low" & fsl_hhs_cat == "No or Little" ~ 12,
      fsl_fcs_cat == "Borderline" & fsl_rcsi_cat == "No to Low" & fsl_hhs_cat == "No or Little" ~ 7,
      fsl_fcs_cat == "Acceptable" & fsl_rcsi_cat == "Medium" & fsl_hhs_cat == "No or Little" ~ 17,
      fsl_fcs_cat == "Poor" & fsl_rcsi_cat == "Medium" & fsl_hhs_cat == "No or Little" ~ 27,
      fsl_fcs_cat == "Borderline" & fsl_rcsi_cat == "Medium" & fsl_hhs_cat == "No or Little" ~ 22,
      fsl_fcs_cat == "Acceptable" & fsl_rcsi_cat == "High" & fsl_hhs_cat == "No or Little" ~ 32,
      fsl_fcs_cat == "Poor" & fsl_rcsi_cat == "High" & fsl_hhs_cat == "No or Little" ~ 42,
      fsl_fcs_cat == "Borderline" & fsl_rcsi_cat == "High" & fsl_hhs_cat == "No or Little" ~ 37,
      fsl_fcs_cat == "Acceptable" & fsl_rcsi_cat == "No to Low" & fsl_hhs_cat == "Moderate" ~ 3,
      fsl_fcs_cat == "Poor" & fsl_rcsi_cat == "No to Low" & fsl_hhs_cat == "Moderate" ~ 13,
      fsl_fcs_cat == "Borderline" & fsl_rcsi_cat == "No to Low" & fsl_hhs_cat == "Moderate" ~ 8,
      fsl_fcs_cat == "Acceptable" & fsl_rcsi_cat == "Medium" & fsl_hhs_cat == "Moderate" ~ 18,
      fsl_fcs_cat == "Poor" & fsl_rcsi_cat == "Medium" & fsl_hhs_cat == "Moderate" ~ 28,
      fsl_fcs_cat == "Borderline" & fsl_rcsi_cat == "Medium" & fsl_hhs_cat == "Moderate" ~ 23,
      fsl_fcs_cat == "Acceptable" & fsl_rcsi_cat == "High" & fsl_hhs_cat == "Moderate" ~ 33,
      fsl_fcs_cat == "Poor" & fsl_rcsi_cat == "High" & fsl_hhs_cat == "Moderate" ~ 43,
      fsl_fcs_cat == "Borderline" & fsl_rcsi_cat == "High" & fsl_hhs_cat == "Moderate" ~ 38,
      fsl_fcs_cat == "Acceptable" & fsl_rcsi_cat == "No to Low" & fsl_hhs_cat == "Severe" ~ 4,
      fsl_fcs_cat == "Poor" & fsl_rcsi_cat == "No to Low" & fsl_hhs_cat == "Severe" ~ 14,
      fsl_fcs_cat == "Borderline" & fsl_rcsi_cat == "No to Low" & fsl_hhs_cat == "Severe" ~ 9,
      fsl_fcs_cat == "Acceptable" & fsl_rcsi_cat == "Medium" & fsl_hhs_cat == "Severe" ~ 19,
      fsl_fcs_cat == "Poor" & fsl_rcsi_cat == "Medium" & fsl_hhs_cat == "Severe" ~ 29,
      fsl_fcs_cat == "Borderline" & fsl_rcsi_cat == "Medium" & fsl_hhs_cat == "Severe" ~ 24,
      fsl_fcs_cat == "Acceptable" & fsl_rcsi_cat == "High" & fsl_hhs_cat == "Severe" ~ 34,
      fsl_fcs_cat == "Poor" & fsl_rcsi_cat == "High" & fsl_hhs_cat == "Severe" ~ 44,
      fsl_fcs_cat == "Borderline" & fsl_rcsi_cat == "High" & fsl_hhs_cat == "Severe" ~ 39,
      fsl_fcs_cat == "Acceptable" & fsl_rcsi_cat == "No to Low" & fsl_hhs_cat == "Very Severe" ~ 5,
      fsl_fcs_cat == "Poor" & fsl_rcsi_cat == "No to Low" & fsl_hhs_cat == "Very Severe" ~ 15,
      fsl_fcs_cat == "Borderline" & fsl_rcsi_cat == "No to Low" & fsl_hhs_cat == "Very Severe" ~ 10,
      fsl_fcs_cat == "Acceptable" & fsl_rcsi_cat == "Medium" & fsl_hhs_cat == "Very Severe" ~ 20,
      fsl_fcs_cat == "Poor" & fsl_rcsi_cat == "Medium" & fsl_hhs_cat == "Very Severe" ~ 30,
      fsl_fcs_cat == "Borderline" & fsl_rcsi_cat == "Medium" & fsl_hhs_cat == "Very Severe" ~ 25,
      fsl_fcs_cat == "Acceptable" & fsl_rcsi_cat == "High" & fsl_hhs_cat == "Very Severe" ~ 35,
      fsl_fcs_cat == "Poor" & fsl_rcsi_cat == "High" & fsl_hhs_cat == "Very Severe" ~ 45,
      fsl_fcs_cat == "Borderline" & fsl_rcsi_cat == "High" & fsl_hhs_cat == "Very Severe" ~ 40,
      TRUE ~ NA_real_
    )) |>
    dplyr::mutate(fc_phase = dplyr::case_when(
      fc_cell %in% c(1, 6) ~ "Phase 1 FC",
      fc_cell %in% c(2, 3, 7, 11, 12, 16, 17, 18, 21, 22, 26, 31, 32, 36) ~ "Phase 2 FC",
      fc_cell %in% c(4, 5, 8, 9, 13, 19, 20, 23, 24, 27, 28, 33, 34, 37, 38, 41, 42, 43) ~ "Phase 3 FC",
      fc_cell %in% c(10, 14, 15, 25, 29, 35, 39, 40, 44) ~ "Phase 4 FC",
      fc_cell %in% c(30, 45) ~ "Phase 5 FC",
      TRUE ~ NA_character_
    ))

  testthat::expect_equal(
    add_fcm_phase(.dataset = test_df,
      fcs_column_name = "fsl_fcs_cat",
      rcsi_column_name = "fsl_rcsi_cat",
      hhs_column_name = "fsl_hhs_cat",
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
    ),
    expected_output_success
  )
})


testthat::test_that("check if function producing expected output with (HDDS/RCSI/HHS)", {
  test_df <- expand.grid(
    fsl_hdds_cat = c("Low", "Medium", "High"),
    fsl_rcsi_cat = c("No to Low", "Medium", "High"),
    fsl_hhs_cat = c("None", "Moderate", "Very Severe", "Severe", "No or Little")
  ) |>
    as.data.frame()
  expected_output_success <- test_df |>
    dplyr::mutate(fc_cell = dplyr::case_when(
      fsl_hdds_cat == "High" & fsl_rcsi_cat == "No to Low" & fsl_hhs_cat == "None" ~ 1,
      fsl_hdds_cat == "Low" & fsl_rcsi_cat == "No to Low" & fsl_hhs_cat == "None" ~ 11,
      fsl_hdds_cat == "Medium" & fsl_rcsi_cat == "No to Low" & fsl_hhs_cat == "None" ~ 6,
      fsl_hdds_cat == "High" & fsl_rcsi_cat == "Medium" & fsl_hhs_cat == "None" ~ 16,
      fsl_hdds_cat == "Low" & fsl_rcsi_cat == "Medium" & fsl_hhs_cat == "None" ~ 26,
      fsl_hdds_cat == "Medium" & fsl_rcsi_cat == "Medium" & fsl_hhs_cat == "None" ~ 21,
      fsl_hdds_cat == "High" & fsl_rcsi_cat == "High" & fsl_hhs_cat == "None" ~ 31,
      fsl_hdds_cat == "Low" & fsl_rcsi_cat == "High" & fsl_hhs_cat == "None" ~ 41,
      fsl_hdds_cat == "Medium" & fsl_rcsi_cat == "High" & fsl_hhs_cat == "None" ~ 36,
      fsl_hdds_cat == "High" & fsl_rcsi_cat == "No to Low" & fsl_hhs_cat == "No or Little" ~ 2,
      fsl_hdds_cat == "Low" & fsl_rcsi_cat == "No to Low" & fsl_hhs_cat == "No or Little" ~ 12,
      fsl_hdds_cat == "Medium" & fsl_rcsi_cat == "No to Low" & fsl_hhs_cat == "No or Little" ~ 7,
      fsl_hdds_cat == "High" & fsl_rcsi_cat == "Medium" & fsl_hhs_cat == "No or Little" ~ 17,
      fsl_hdds_cat == "Low" & fsl_rcsi_cat == "Medium" & fsl_hhs_cat == "No or Little" ~ 27,
      fsl_hdds_cat == "Medium" & fsl_rcsi_cat == "Medium" & fsl_hhs_cat == "No or Little" ~ 22,
      fsl_hdds_cat == "High" & fsl_rcsi_cat == "High" & fsl_hhs_cat == "No or Little" ~ 32,
      fsl_hdds_cat == "Low" & fsl_rcsi_cat == "High" & fsl_hhs_cat == "No or Little" ~ 42,
      fsl_hdds_cat == "Medium" & fsl_rcsi_cat == "High" & fsl_hhs_cat == "No or Little" ~ 37,
      fsl_hdds_cat == "High" & fsl_rcsi_cat == "No to Low" & fsl_hhs_cat == "Moderate" ~ 3,
      fsl_hdds_cat == "Low" & fsl_rcsi_cat == "No to Low" & fsl_hhs_cat == "Moderate" ~ 13,
      fsl_hdds_cat == "Medium" & fsl_rcsi_cat == "No to Low" & fsl_hhs_cat == "Moderate" ~ 8,
      fsl_hdds_cat == "High" & fsl_rcsi_cat == "Medium" & fsl_hhs_cat == "Moderate" ~ 18,
      fsl_hdds_cat == "Low" & fsl_rcsi_cat == "Medium" & fsl_hhs_cat == "Moderate" ~ 28,
      fsl_hdds_cat == "Medium" & fsl_rcsi_cat == "Medium" & fsl_hhs_cat == "Moderate" ~ 23,
      fsl_hdds_cat == "High" & fsl_rcsi_cat == "High" & fsl_hhs_cat == "Moderate" ~ 33,
      fsl_hdds_cat == "Low" & fsl_rcsi_cat == "High" & fsl_hhs_cat == "Moderate" ~ 43,
      fsl_hdds_cat == "Medium" & fsl_rcsi_cat == "High" & fsl_hhs_cat == "Moderate" ~ 38,
      fsl_hdds_cat == "High" & fsl_rcsi_cat == "No to Low" & fsl_hhs_cat == "Severe" ~ 4,
      fsl_hdds_cat == "Low" & fsl_rcsi_cat == "No to Low" & fsl_hhs_cat == "Severe" ~ 14,
      fsl_hdds_cat == "Medium" & fsl_rcsi_cat == "No to Low" & fsl_hhs_cat == "Severe" ~ 9,
      fsl_hdds_cat == "High" & fsl_rcsi_cat == "Medium" & fsl_hhs_cat == "Severe" ~ 19,
      fsl_hdds_cat == "Low" & fsl_rcsi_cat == "Medium" & fsl_hhs_cat == "Severe" ~ 29,
      fsl_hdds_cat == "Medium" & fsl_rcsi_cat == "Medium" & fsl_hhs_cat == "Severe" ~ 24,
      fsl_hdds_cat == "High" & fsl_rcsi_cat == "High" & fsl_hhs_cat == "Severe" ~ 34,
      fsl_hdds_cat == "Low" & fsl_rcsi_cat == "High" & fsl_hhs_cat == "Severe" ~ 44,
      fsl_hdds_cat == "Medium" & fsl_rcsi_cat == "High" & fsl_hhs_cat == "Severe" ~ 39,
      fsl_hdds_cat == "High" & fsl_rcsi_cat == "No to Low" & fsl_hhs_cat == "Very Severe" ~ 5,
      fsl_hdds_cat == "Low" & fsl_rcsi_cat == "No to Low" & fsl_hhs_cat == "Very Severe" ~ 15,
      fsl_hdds_cat == "Medium" & fsl_rcsi_cat == "No to Low" & fsl_hhs_cat == "Very Severe" ~ 10,
      fsl_hdds_cat == "High" & fsl_rcsi_cat == "Medium" & fsl_hhs_cat == "Very Severe" ~ 20,
      fsl_hdds_cat == "Low" & fsl_rcsi_cat == "Medium" & fsl_hhs_cat == "Very Severe" ~ 30,
      fsl_hdds_cat == "Medium" & fsl_rcsi_cat == "Medium" & fsl_hhs_cat == "Very Severe" ~ 25,
      fsl_hdds_cat == "High" & fsl_rcsi_cat == "High" & fsl_hhs_cat == "Very Severe" ~ 35,
      fsl_hdds_cat == "Low" & fsl_rcsi_cat == "High" & fsl_hhs_cat == "Very Severe" ~ 45,
      fsl_hdds_cat == "Medium" & fsl_rcsi_cat == "High" & fsl_hhs_cat == "Very Severe" ~ 40,
      TRUE ~ NA_real_
    )) |>
    dplyr::mutate(fc_phase = dplyr::case_when(
      fc_cell %in% c(1, 6) ~ "Phase 1 FC",
      fc_cell %in% c(2, 3, 7, 11, 12, 16, 17, 18, 21, 22, 26, 31, 32, 36) ~ "Phase 2 FC",
      fc_cell %in% c(4, 5, 8, 9, 13, 19, 20, 23, 24, 27, 28, 33, 34, 37, 38, 41, 42, 43) ~ "Phase 3 FC",
      fc_cell %in% c(10, 14, 15, 25, 29, 35, 39, 40, 44) ~ "Phase 4 FC",
      fc_cell %in% c(30, 45) ~ "Phase 5 FC",
      TRUE ~ NA_character_
    ))

  testthat::expect_equal(
    add_fcm_phase(.dataset = test_df,
                  hdds_column_name = "fsl_hdds_cat",
                  rcsi_column_name = "fsl_rcsi_cat",
                  hhs_column_name = "fsl_hhs_cat",
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
    ),
    expected_output_success
  )
})

testthat::test_that("check if function producing expected output with (FCS/HHS)", {
  test_df <- expand.grid(
    fsl_fcs_cat = c("Acceptable", "Poor", "Borderline"),
    fsl_hhs_cat = c("None", "Moderate", "Very Severe", "Severe", "No or Little")
  ) |>
    as.data.frame()
  expected_output_success <- test_df |>
    dplyr::mutate(fc_cell = dplyr::case_when(
      fsl_fcs_cat == "Acceptable" & fsl_hhs_cat == "None" ~ 1,
      fsl_fcs_cat == "Poor" & fsl_hhs_cat == "None" ~ 11,
      fsl_fcs_cat == "Borderline" & fsl_hhs_cat == "None" ~ 6,
      fsl_fcs_cat == "Acceptable" & fsl_hhs_cat == "No or Little" ~ 2,
      fsl_fcs_cat == "Poor" & fsl_hhs_cat == "No or Little" ~ 12,
      fsl_fcs_cat == "Borderline" & fsl_hhs_cat == "No or Little" ~ 7,
      fsl_fcs_cat == "Acceptable" & fsl_hhs_cat == "Moderate" ~ 3,
      fsl_fcs_cat == "Poor" & fsl_hhs_cat == "Moderate" ~ 13,
      fsl_fcs_cat == "Borderline" & fsl_hhs_cat == "Moderate" ~ 8,
      fsl_fcs_cat == "Acceptable" & fsl_hhs_cat == "Severe" ~ 4,
      fsl_fcs_cat == "Poor" & fsl_hhs_cat == "Severe" ~ 14,
      fsl_fcs_cat == "Borderline" & fsl_hhs_cat == "Severe" ~ 9,
      fsl_fcs_cat == "Acceptable" & fsl_hhs_cat == "Very Severe" ~ 5,
      fsl_fcs_cat == "Poor" & fsl_hhs_cat == "Very Severe" ~ 15,
      fsl_fcs_cat == "Borderline" & fsl_hhs_cat == "Very Severe" ~ 10,
      TRUE ~ NA_real_
    )) |>
    dplyr::mutate(fc_phase = dplyr::case_when(fc_cell %in% c(1, 6) ~ "Phase 1 FC",
                                              fc_cell %in% c(2, 3, 7, 11) ~ "Phase 2 FC",
                                              fc_cell %in% c(4, 8, 12, 13) ~ "Phase 3 FC",
                                              fc_cell %in% c(5, 9, 10, 14) ~ "Phase 4 FC",
                                              fc_cell %in% c(15) ~ "Phase 5 FC",
                                              TRUE ~ NA))

  testthat::expect_equal(
    add_fcm_phase(.dataset = test_df,
                  fcs_column_name = "fsl_fcs_cat",
                  hhs_column_name = "fsl_hhs_cat",
                  fcs_categories_acceptable = "Acceptable",
                  fcs_categories_poor = "Poor",
                  fcs_categories_borderline = "Borderline",
                  hhs_categories_none = "None",
                  hhs_categories_little = "No or Little",
                  hhs_categories_moderate = "Moderate",
                  hhs_categories_severe = "Severe",
                  hhs_categories_very_severe = "Very Severe"
    ),
    expected_output_success
  )
})


testthat::test_that("check if function producing expected output with (HDDS/HHS)", {
  test_df <- expand.grid(
    fsl_hdds_cat = c("High", "Low", "Medium"),
    fsl_hhs_cat = c("None", "Moderate", "Very Severe", "Severe", "No or Little")
  ) |>
    as.data.frame()
  expected_output_success <- test_df |>
    dplyr::mutate(fc_cell = dplyr::case_when(
      fsl_hdds_cat == "High" & fsl_hhs_cat == "None" ~ 1,
      fsl_hdds_cat == "Low" & fsl_hhs_cat == "None" ~ 11,
      fsl_hdds_cat == "Medium" & fsl_hhs_cat == "None" ~ 6,
      fsl_hdds_cat == "High" & fsl_hhs_cat == "No or Little" ~ 2,
      fsl_hdds_cat == "Low" & fsl_hhs_cat == "No or Little" ~ 12,
      fsl_hdds_cat == "Medium" & fsl_hhs_cat == "No or Little" ~ 7,
      fsl_hdds_cat == "High" & fsl_hhs_cat == "Moderate" ~ 3,
      fsl_hdds_cat == "Low" & fsl_hhs_cat == "Moderate" ~ 13,
      fsl_hdds_cat == "Medium" & fsl_hhs_cat == "Moderate" ~ 8,
      fsl_hdds_cat == "High" & fsl_hhs_cat == "Severe" ~ 4,
      fsl_hdds_cat == "Low" & fsl_hhs_cat == "Severe" ~ 14,
      fsl_hdds_cat == "Medium" & fsl_hhs_cat == "Severe" ~ 9,
      fsl_hdds_cat == "High" & fsl_hhs_cat == "Very Severe" ~ 5,
      fsl_hdds_cat == "Low" & fsl_hhs_cat == "Very Severe" ~ 15,
      fsl_hdds_cat == "Medium" & fsl_hhs_cat == "Very Severe" ~ 10,
      TRUE ~ NA_real_
    )) |>
    dplyr::mutate(fc_phase = dplyr::case_when(fc_cell %in% c(1, 6) ~ "Phase 1 FC",
                                              fc_cell %in% c(2, 3, 7, 11) ~ "Phase 2 FC",
                                              fc_cell %in% c(4, 8, 12, 13) ~ "Phase 3 FC",
                                              fc_cell %in% c(5, 9, 10, 14) ~ "Phase 4 FC",
                                              fc_cell %in% c(15) ~ "Phase 5 FC",
                                              TRUE ~ NA))

  testthat::expect_equal(
    add_fcm_phase(.dataset = test_df,
                  hdds_column_name = "fsl_hdds_cat",
                  hhs_column_name = "fsl_hhs_cat",
                  fcs_categories_acceptable = "High",
                  fcs_categories_poor = "Low",
                  fcs_categories_borderline = "Medium",
                  hhs_categories_none = "None",
                  hhs_categories_little = "No or Little",
                  hhs_categories_moderate = "Moderate",
                  hhs_categories_severe = "Severe",
                  hhs_categories_very_severe = "Very Severe"
    ),
    expected_output_success
  )
})

