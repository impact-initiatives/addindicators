testthat::test_that("Check if indicators are available in the data", {
  test_df <- expand.grid(
    fcs_cat = c("Acceptable", "Poor", "Borderline"),
    rcsi_cat = c("No to Low", "Medium", "High"),
    hhs_cat = c("None", "Moderate", "Very Severe", "Severe", "Little")
  ) |>
    as.data.frame() |>
    dplyr::mutate(
      fc_cell = c(1:45),
      fc_phase = ""
    )

  test_df_warning <- expand.grid(
    fcs_ca = c("Acceptable", "Poor", "Borderline"),
    rcsi_cat = c("No to Low", "Medium", "High"),
    hhs_cat = c("None", "Moderate", "Very Severe", "Severe", "Little")
  ) |>
    as.data.frame() |>
    dplyr::mutate(
      fc_cell = c(1:45),
      fc_phase = ""
    )


  actual_output <- add_fcm_phase(dataset = test_df)

  testthat::expect_equal(names(actual_output), colnames(test_df))
  add_fcm_phase(dataset = test_df_warning) |>
    testthat::expect_warning() |>
    testthat::expect_warning() |>
    testthat::expect_error()
})



testthat::test_that("check if function producing expected output", {
  test_df <- expand.grid(
    fcs_cat = c("Acceptable", "Poor", "Borderline"),
    rcsi_cat = c("No to Low", "Medium", "High"),
    hhs_cat = c("None", "Moderate", "Very Severe", "Severe", "Little")
  ) |>
    as.data.frame()
  expected_output_success <- test_df |>
    dplyr::mutate(fc_cell = dplyr::case_when(
      fcs_cat == "Acceptable" & rcsi_cat == "No to Low" & hhs_cat == "None" ~ 1,
      fcs_cat == "Poor" & rcsi_cat == "No to Low" & hhs_cat == "None" ~ 11,
      fcs_cat == "Borderline" & rcsi_cat == "No to Low" & hhs_cat == "None" ~ 6,
      fcs_cat == "Acceptable" & rcsi_cat == "Medium" & hhs_cat == "None" ~ 16,
      fcs_cat == "Poor" & rcsi_cat == "Medium" & hhs_cat == "None" ~ 26,
      fcs_cat == "Borderline" & rcsi_cat == "Medium" & hhs_cat == "None" ~ 21,
      fcs_cat == "Acceptable" & rcsi_cat == "High" & hhs_cat == "None" ~ 31,
      fcs_cat == "Poor" & rcsi_cat == "High" & hhs_cat == "None" ~ 41,
      fcs_cat == "Borderline" & rcsi_cat == "High" & hhs_cat == "None" ~ 36,
      fcs_cat == "Acceptable" & rcsi_cat == "No to Low" & hhs_cat == "Little" ~ 2,
      fcs_cat == "Poor" & rcsi_cat == "No to Low" & hhs_cat == "Little" ~ 12,
      fcs_cat == "Borderline" & rcsi_cat == "No to Low" & hhs_cat == "Little" ~ 7,
      fcs_cat == "Acceptable" & rcsi_cat == "Medium" & hhs_cat == "Little" ~ 17,
      fcs_cat == "Poor" & rcsi_cat == "Medium" & hhs_cat == "Little" ~ 27,
      fcs_cat == "Borderline" & rcsi_cat == "Medium" & hhs_cat == "Little" ~ 22,
      fcs_cat == "Acceptable" & rcsi_cat == "High" & hhs_cat == "Little" ~ 32,
      fcs_cat == "Poor" & rcsi_cat == "High" & hhs_cat == "Little" ~ 42,
      fcs_cat == "Borderline" & rcsi_cat == "High" & hhs_cat == "Little" ~ 37,
      fcs_cat == "Acceptable" & rcsi_cat == "No to Low" & hhs_cat == "Moderate" ~ 3,
      fcs_cat == "Poor" & rcsi_cat == "No to Low" & hhs_cat == "Moderate" ~ 13,
      fcs_cat == "Borderline" & rcsi_cat == "No to Low" & hhs_cat == "Moderate" ~ 8,
      fcs_cat == "Acceptable" & rcsi_cat == "Medium" & hhs_cat == "Moderate" ~ 18,
      fcs_cat == "Poor" & rcsi_cat == "Medium" & hhs_cat == "Moderate" ~ 28,
      fcs_cat == "Borderline" & rcsi_cat == "Medium" & hhs_cat == "Moderate" ~ 23,
      fcs_cat == "Acceptable" & rcsi_cat == "High" & hhs_cat == "Moderate" ~ 33,
      fcs_cat == "Poor" & rcsi_cat == "High" & hhs_cat == "Moderate" ~ 43,
      fcs_cat == "Borderline" & rcsi_cat == "High" & hhs_cat == "Moderate" ~ 38,
      fcs_cat == "Acceptable" & rcsi_cat == "No to Low" & hhs_cat == "Severe" ~ 4,
      fcs_cat == "Poor" & rcsi_cat == "No to Low" & hhs_cat == "Severe" ~ 14,
      fcs_cat == "Borderline" & rcsi_cat == "No to Low" & hhs_cat == "Severe" ~ 9,
      fcs_cat == "Acceptable" & rcsi_cat == "Medium" & hhs_cat == "Severe" ~ 19,
      fcs_cat == "Poor" & rcsi_cat == "Medium" & hhs_cat == "Severe" ~ 29,
      fcs_cat == "Borderline" & rcsi_cat == "Medium" & hhs_cat == "Severe" ~ 24,
      fcs_cat == "Acceptable" & rcsi_cat == "High" & hhs_cat == "Severe" ~ 34,
      fcs_cat == "Poor" & rcsi_cat == "High" & hhs_cat == "Severe" ~ 44,
      fcs_cat == "Borderline" & rcsi_cat == "High" & hhs_cat == "Severe" ~ 39,
      fcs_cat == "Acceptable" & rcsi_cat == "No to Low" & hhs_cat == "Very Severe" ~ 5,
      fcs_cat == "Poor" & rcsi_cat == "No to Low" & hhs_cat == "Very Severe" ~ 15,
      fcs_cat == "Borderline" & rcsi_cat == "No to Low" & hhs_cat == "Very Severe" ~ 10,
      fcs_cat == "Acceptable" & rcsi_cat == "Medium" & hhs_cat == "Very Severe" ~ 20,
      fcs_cat == "Poor" & rcsi_cat == "Medium" & hhs_cat == "Very Severe" ~ 30,
      fcs_cat == "Borderline" & rcsi_cat == "Medium" & hhs_cat == "Very Severe" ~ 25,
      fcs_cat == "Acceptable" & rcsi_cat == "High" & hhs_cat == "Very Severe" ~ 35,
      fcs_cat == "Poor" & rcsi_cat == "High" & hhs_cat == "Very Severe" ~ 45,
      fcs_cat == "Borderline" & rcsi_cat == "High" & hhs_cat == "Very Severe" ~ 40,
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
    add_fcm_phase(test_df,
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
    ),
    expected_output_success
  )
})
