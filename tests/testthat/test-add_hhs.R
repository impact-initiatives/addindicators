library(dplyr)

##### Sad Path #####

testthat::test_that("Check input type -- dataset", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))
  testthat::expect_error(add_hhs(.dataset = 0))
  testthat::expect_error(add_hhs(.dataset = "x"))
  testthat::expect_error(add_hhs(.dataset = 1.0))
  testthat::expect_error(add_hhs(.dataset = FALSE))
  testthat::expect_error(add_hhs(.dataset = list()))
})


testthat::test_that("Check dataframe empty", {
  df1 <- data.frame()
  testthat::expect_error(add_hhs(.dataset = df1))
})

testthat::test_that("Check missing columns", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  testthat::expect_error(add_hhs(
    .dataset = test_df %>% dplyr::select(-fsl_hhs_nofoodhh),
    fsl_hhs_nofoodhh = "fsl_hhs_nofoodhh"
  ))

  testthat::expect_error(add_hhs(
    .dataset = test_df %>% dplyr::select(-fsl_hhs_nofoodhh_freq),
    fsl_hhs_nofoodhh_freq = "fsl_hhs_nofoodhh_freq"
  ))

  testthat::expect_error(add_hhs(
    .dataset = test_df %>% dplyr::select(-fsl_hhs_sleephungry),
    fsl_hhs_sleephungry = "fsl_hhs_sleephungry"
  ))

  testthat::expect_error(add_hhs(
    .dataset = test_df %>% dplyr::select(-fsl_hhs_sleephungry_freq),
    fsl_hhs_sleephungry_freq = "fsl_hhs_sleephungry_freq"
  ))

  testthat::expect_error(add_hhs(
    .dataset = test_df %>% dplyr::select(-fsl_hhs_alldaynight),
    fsl_hhs_alldaynight = "fsl_hhs_alldaynight"
  ))

  testthat::expect_error(add_hhs(
    .dataset = test_df %>% dplyr::select(-fsl_hhs_alldaynight_freq),
    fsl_hhs_alldaynight_freq = "fsl_hhs_alldaynight_freq"
  ))
})



testthat::test_that("Check columns values - Yes/No", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  set.seed(30)
  test_df[sample.int(nrow(test_df), 3), c("fsl_hhs_nofoodhh")] <- "YES"
  test_df[sample.int(nrow(test_df), 3), c("fsl_hhs_nofoodhh")] <- "NO"
  test_df[sample.int(nrow(test_df), 3), c("fsl_hhs_nofoodhh")] <- "random_value"

  testthat::expect_error(add_hhs(
    .dataset = test_df,
  ))
})
testthat::test_that("Check columns values - Yes/No", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  set.seed(29)
  test_df[sample.int(nrow(test_df), 3), c("fsl_hhs_alldaynight")] <- "YES"
  test_df[sample.int(nrow(test_df), 3), c("fsl_hhs_alldaynight")] <- "NO"
  test_df[sample.int(nrow(test_df), 3), c("fsl_hhs_alldaynight")] <- "random_value"

  testthat::expect_error(add_hhs(
    .dataset = test_df,

  ))
})
testthat::test_that("Check columns values - Yes/No", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  set.seed(12)
  test_df[sample.int(nrow(test_df), 3), c("fsl_hhs_sleephungry")] <- "YES"
  test_df[sample.int(nrow(test_df), 3), c("fsl_hhs_sleephungry")] <- "NO"
  test_df[sample.int(nrow(test_df), 3), c("fsl_hhs_sleephungry")] <- "random_value"

  testthat::expect_error(add_hhs(
    .dataset = test_df,
  ))
})

testthat::test_that("Check columns values - Frequencies", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  set.seed(30)
  test_df[sample.int(nrow(test_df), 3), c("fsl_hhs_nofoodhh_freq")] <- "rarely_1_2"
  test_df[sample.int(nrow(test_df), 3), c("fsl_hhs_nofoodhh_freq")] <- "sometimes_3_10"
  test_df[sample.int(nrow(test_df), 3), c("fsl_hhs_nofoodhh_freq")] <- "often_10_times"


  testthat::expect_error(add_hhs(
    .dataset = test_df
  ))
})

testthat::test_that("Check columns values - Frequencies", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  set.seed(29)
  test_df[sample.int(nrow(test_df), 3), c("fsl_hhs_sleephungry_freq")] <- "rarely_1_2"
  test_df[sample.int(nrow(test_df), 3), c("fsl_hhs_sleephungry_freq")] <- "sometimes_3_10"
  test_df[sample.int(nrow(test_df), 3), c("fsl_hhs_sleephungry_freq")] <- "often_10_times"


  testthat::expect_error(add_hhs(
    .dataset = test_df
  ))
})

testthat::test_that("Check columns values - Frequencies", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  set.seed(16)
  test_df[sample.int(nrow(test_df), 3), c("fsl_hhs_alldaynight_freq")] <- "rarely_1_2"
  test_df[sample.int(nrow(test_df), 3), c("fsl_hhs_alldaynight_freq")] <- "sometimes_3_10"
  test_df[sample.int(nrow(test_df), 3), c("fsl_hhs_alldaynight_freq")] <- "often_10_times"

  testthat::expect_error(add_hhs(
    .dataset = test_df
  ))
})


##### Happy Path #####

testthat::test_that("Check calculation", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))
  load(testthat::test_path("testdata", "test_df_with_calculation_hhs.rda"))


  testthat::expect_equal(
    object = add_hhs(
      .dataset = test_df,
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
    ) %>% dplyr::pull("fsl_hhs_cat_ipc"),
    expected = test_df_with_calculation %>% dplyr::pull("fsl_hhs_cat_ipc")
  )


  testthat::expect_equal(
    object = add_hhs(
      .dataset = test_df,
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
    ) %>% dplyr::pull("fsl_hhs_cat"),
    expected = test_df_with_calculation %>% dplyr::pull("fsl_hhs_cat")
  )
})

