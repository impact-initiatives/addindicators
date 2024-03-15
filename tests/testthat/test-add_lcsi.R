library(dplyr)

###### Sad Path #######

testthat::test_that("Check input type -- dataset", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))
  testthat::expect_error(add_lcsi(.dataset = 0))
  testthat::expect_error(add_lcsi(.dataset = "x"))
  testthat::expect_error(add_lcsi(.dataset = 1.0))
  testthat::expect_error(add_lcsi(.dataset = F))
  testthat::expect_error(add_lcsi(.dataset = list()))
})


testthat::test_that("Check dataframe empty", {
  df1 <- data.frame()
  testthat::expect_error(add_lcsi(.dataset = df1))
})

testthat::test_that("Check for missing columns", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  testthat::expect_error(add_lcsi(
    .dataset = test_df %>% dplyr::select(-fsl_lcsi_stress1),
    fsl_lcsi_stress1 = "fsl_lcsi_stress1"
  ))

})

testthat::test_that("Checking column values - [yes/no_had_no_need/no_exhausted/not_applicable]", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  set.seed(30)
  test_df[sample.int(nrow(test_df), 3), c("fsl_lcsi_stress1")] <- "random"
  test_df[sample.int(nrow(test_df), 3), c("fsl_lcsi_stress1")] <- "9"
  test_df[sample.int(nrow(test_df), 3), c("fsl_lcsi_stress1")] <- "YES"
  testthat::expect_error(add_lcsi(
    .dataset = test_df
  ))
})
testthat::test_that("Checking column values - [yes/no_had_no_need/no_exhausted/not_applicable]", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  set.seed(29)
  test_df[sample.int(nrow(test_df), 3), c("fsl_lcsi_stress2")] <- "random"
  test_df[sample.int(nrow(test_df), 3), c("fsl_lcsi_stress2")] <- "9"
  test_df[sample.int(nrow(test_df), 3), c("fsl_lcsi_stress2")] <- "YES"
  testthat::expect_error(add_lcsi(
    .dataset = test_df
  ))
})
testthat::test_that("Checking column values - [yes/no_had_no_need/no_exhausted/not_applicable]", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  set.seed(28)
  test_df[sample.int(nrow(test_df), 3), c("fsl_lcsi_stress3")] <- "random"
  test_df[sample.int(nrow(test_df), 3), c("fsl_lcsi_stress3")] <- "9"
  test_df[sample.int(nrow(test_df), 3), c("fsl_lcsi_stress3")] <- "YES"
  testthat::expect_error(add_lcsi(
    .dataset = test_df
  ))
})
testthat::test_that("Checking column values - [yes/no_had_no_need/no_exhausted/not_applicable]", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  set.seed(27)
  test_df[sample.int(nrow(test_df), 3), c("fsl_lcsi_stress4")] <- "random"
  test_df[sample.int(nrow(test_df), 3), c("fsl_lcsi_stress4")] <- "9"
  test_df[sample.int(nrow(test_df), 3), c("fsl_lcsi_stress4")] <- "YES"
  testthat::expect_error(add_lcsi(
    .dataset = test_df
  ))
})
testthat::test_that("Checking column values - [yes/no_had_no_need/no_exhausted/not_applicable]", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  set.seed(26)
  test_df[sample.int(nrow(test_df), 3), c("fsl_lcsi_crisis1")] <- "random"
  test_df[sample.int(nrow(test_df), 3), c("fsl_lcsi_crisis1")] <- "9"
  test_df[sample.int(nrow(test_df), 3), c("fsl_lcsi_crisis1")] <- "YES"
  testthat::expect_error(add_lcsi(
    .dataset = test_df
  ))
})
testthat::test_that("Checking column values - [yes/no_had_no_need/no_exhausted/not_applicable]", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  set.seed(25)
  test_df[sample.int(nrow(test_df), 3), c("fsl_lcsi_crisis2")] <- "random"
  test_df[sample.int(nrow(test_df), 3), c("fsl_lcsi_crisis2")] <- "9"
  test_df[sample.int(nrow(test_df), 3), c("fsl_lcsi_crisis2")] <- "YES"
  testthat::expect_error(add_lcsi(
    .dataset = test_df
  ))
})
testthat::test_that("Checking column values - [yes/no_had_no_need/no_exhausted/not_applicable]", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  set.seed(24)
  test_df[sample.int(nrow(test_df), 3), c("fsl_lcsi_crisis3")] <- "random"
  test_df[sample.int(nrow(test_df), 3), c("fsl_lcsi_crisis3")] <- "9"
  test_df[sample.int(nrow(test_df), 3), c("fsl_lcsi_crisis3")] <- "YES"
  testthat::expect_error(add_lcsi(
    .dataset = test_df
  ))
})
testthat::test_that("Checking column values - [yes/no_had_no_need/no_exhausted/not_applicable]", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  set.seed(23)
  test_df[sample.int(nrow(test_df), 3), c("fsl_lcsi_emergency1")] <- "random"
  test_df[sample.int(nrow(test_df), 3), c("fsl_lcsi_emergency1")] <- "9"
  test_df[sample.int(nrow(test_df), 3), c("fsl_lcsi_emergency1")] <- "YES"
  testthat::expect_error(add_lcsi(
    .dataset = test_df
  ))
})
testthat::test_that("Checking column values - [yes/no_had_no_need/no_exhausted/not_applicable]", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  set.seed(22)
  test_df[sample.int(nrow(test_df), 3), c("fsl_lcsi_emergency2")] <- "random"
  test_df[sample.int(nrow(test_df), 3), c("fsl_lcsi_emergency2")] <- "9"
  test_df[sample.int(nrow(test_df), 3), c("fsl_lcsi_emergency2")] <- "YES"
  testthat::expect_error(add_lcsi(
    .dataset = test_df
  ))
})
testthat::test_that("Checking column values - [yes/no_had_no_need/no_exhausted/not_applicable]", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  set.seed(21)
  test_df[sample.int(nrow(test_df), 3), c("fsl_lcsi_emergency3")] <- "random"
  test_df[sample.int(nrow(test_df), 3), c("fsl_lcsi_emergency3")] <- "9"
  test_df[sample.int(nrow(test_df), 3), c("fsl_lcsi_emergency3")] <- "YES"
  testthat::expect_error(add_lcsi(
    .dataset = test_df
  ))
})



#### Happy Path ####


testthat::test_that("testing add_lcsi", {
  df1 <- data.frame(
    fsl_lcsi_stress1 = c("No", "No", "Exhausted", "Not Applicable", "No"),
    fsl_lcsi_stress2 = c("No", "Yes", "Not Applicable", "No", "No"),
    fsl_lcsi_stress3 = c("Not Applicable", "Not Applicable", "Yes", "No", "No"),
    fsl_lcsi_stress4 = c("Not Applicable", "No", "Yes", "Yes", "No"),
    fsl_lcsi_crisis1 = c("No", "Not Applicable", "Yes", "Exhausted", "No"),
    fsl_lcsi_crisis2 = c("No", "No", "No", "No", "No"),
    fsl_lcsi_crisis3 = c("No", "No", "Yes", "Not Applicable", "No"),
    fsl_lcsi_emergency1 = c("No", "Not Applicable", "Not Applicable", "No", "No"),
    fsl_lcsi_emergency2 = c("No", "Not Applicable", "Yes", "Not Applicable", "No"),
    fsl_lcsi_emergency3 = c("Not Applicable", "No", "Not Applicable", "No", "Exhausted")
  )

  result <- add_lcsi(.dataset = df1,
                      yes_val = "Yes",
                      no_val = "No",
                      exhausted_val = "Exhausted",
                      not_applicable_val = "Not Applicable")

  expected_result <- data.frame(
    fsl_lcsi_stress1 = c("No", "No", "Exhausted", "Not Applicable", "No"),
    fsl_lcsi_stress2 = c("No", "Yes", "Not Applicable", "No", "No"),
    fsl_lcsi_stress3 = c("Not Applicable", "Not Applicable", "Yes", "No", "No"),
    fsl_lcsi_stress4 = c("Not Applicable", "No", "Yes", "Yes", "No"),
    fsl_lcsi_crisis1 = c("No", "Not Applicable", "Yes", "Exhausted", "No"),
    fsl_lcsi_crisis2 = c("No", "No", "No", "No", "No"),
    fsl_lcsi_crisis3 = c("No", "No", "Yes", "Not Applicable", "No"),
    fsl_lcsi_emergency1 = c("No", "Not Applicable", "Not Applicable", "No", "No"),
    fsl_lcsi_emergency2 = c("No", "Not Applicable", "Yes", "Not Applicable", "No"),
    fsl_lcsi_emergency3 = c("Not Applicable", "No", "Not Applicable", "No", "Exhausted"),
    fsl_lcsi_stress_yes = c("0", "1", "1", "1", "0"),
    fsl_lcsi_stress_exhaust = c("0", "0", "1", "0", "0"),
    fsl_lcsi_stress = c("0", "1", "1", "1", "0"),
    fsl_lcsi_crisis_yes = c("0", "0", "1", "0", "0"),
    fsl_lcsi_crisis_exhaust = c("0", "0", "0", "1", "0"),
    fsl_lcsi_crisis = c("0", "0", "1", "1", "0"),
    fsl_lcsi_emergency_yes = c("0", "0", "1", "0", "0"),
    fsl_lcsi_emergency_exhaust = c("0", "0", "0", "0", "1"),
    fsl_lcsi_emergency = c("0", "0", "1", "0", "1"),
    fsl_lcsi_cat_yes = c("None", "Stress", "Emergency", "Stress", "None"),
    fsl_lcsi_cat_exhaust = c("None", "None", "Stress", "Crisis", "Emergency"),
    fsl_lcsi_cat = c("None", "Stress", "Emergency", "Crisis", "Emergency")
  )

  testthat::expect_equal(result, expected_result)
})
