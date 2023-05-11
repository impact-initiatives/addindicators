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

testthat::test_that("Check missing columns", {

  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  testthat::expect_error(add_hhs(.dataset = test_df %>% dplyr::select(-fs_hhs_nofood_yn),
                       hhs_nofoodhh_1 = "fs_hhs_nofood_yn"))

  testthat::expect_error(add_hhs(.dataset = test_df %>% dplyr::select(-fs_hhs_nofood_freq),
                       hhs_nofoodhh_1a = "fs_hhs_nofood_freq"))

  testthat::expect_error(add_hhs(.dataset = test_df %>% dplyr::select(-fs_hhs_sleephungry_yn),
                       hhs_sleephungry_2 = "fs_hhs_sleephungry_yn"))

  testthat::expect_error(add_hhs(.dataset = test_df %>% dplyr::select(-fs_hhs_sleephungry_freq),
                       hhs_sleephungry_2a = "fs_hhs_sleephungry_freq"))

  testthat::expect_error(add_hhs(.dataset = test_df %>% dplyr::select(-fs_hhs_daynoteating_yn),
                       hhs_alldaynight_3 = "fs_hhs_daynoteating_yn"))

  testthat::expect_error(add_hhs(.dataset = test_df %>% dplyr::select(-fs_hhs_daynoteating_freq),
                       hhs_alldaynight_3a = "fs_hhs_daynoteating_freq"))

})



testthat::test_that("Check columns values - Yes/No", {

  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  set.seed(30);test_df[sample.int(nrow(test_df),3),c("fs_hhs_nofood_yn","fs_hhs_sleephungry_yn","fs_hhs_daynoteating_yn")] = "YES"
  set.seed(29);test_df[sample.int(nrow(test_df),3),c("fs_hhs_nofood_yn","fs_hhs_sleephungry_yn","fs_hhs_daynoteating_yn")] = "NO"
  set.seed(12);test_df[sample.int(nrow(test_df),3),c("fs_hhs_nofood_yn","fs_hhs_sleephungry_yn","fs_hhs_daynoteating_yn")] = "random_value"

  testthat::expect_error(add_hhs(.dataset = test_df,
                       hhs_nofoodhh_1 = "fs_hhs_nofood_yn",
                       yes_answer = "yes",
                       no_answer = "no"))

  testthat::expect_error(add_hhs(.dataset = test_df,
                       hhs_sleephungry_2 = "fs_hhs_sleephungry_yn",
                       yes_answer = "yes",
                       no_answer = "no"))

  testthat::expect_error(add_hhs(.dataset = test_df,
                       hhs_alldaynight_3  = "fs_hhs_daynoteating_yn",
                       yes_answer = "yes",
                       no_answer = "no"))

})

testthat::test_that("Check columns values - Frequencies", {

  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  set.seed(30);test_df[sample.int(nrow(test_df),3),c("fs_hhs_nofood_freq","fs_hhs_sleephungry_freq","fs_hhs_daynoteating_freq")] = "rarely"
  set.seed(29);test_df[sample.int(nrow(test_df),3),c("fs_hhs_nofood_freq","fs_hhs_sleephungry_freq","fs_hhs_daynoteating_freq")] = "sometimes"
  set.seed(16);test_df[sample.int(nrow(test_df),3),c("fs_hhs_nofood_freq","fs_hhs_sleephungry_freq","fs_hhs_daynoteating_freq")] = "often"

  testthat::expect_error(add_hhs(.dataset = test_df,
                       hhs_nofoodhh_1a  = "fs_hhs_nofood_freq",
                       rarely_answer = "rarely_1_2",
                       sometimes_answer = "sometimes_3_10",
                       often_answer = "often_10_times"))

  testthat::expect_error(add_hhs(.dataset = test_df,
                       hhs_sleephungry_2a  = "fs_hhs_sleephungry_freq",
                       rarely_answer = "rarely_1_2",
                       sometimes_answer = "sometimes_3_10",
                       often_answer = "often_10_times"))

  testthat::expect_error(add_hhs(.dataset = test_df,
                       hhs_alldaynight_3a = "fs_hhs_daynoteating_freq",
                       rarely_answer = "rarely_1_2",
                       sometimes_answer = "sometimes_3_10",
                       often_answer = "often_10_times"))

})


##### Happy Path #####

testthat::test_that("Check calculation", {

  load(testthat::test_path("testdata", "test_df_hhs.rda"))
  load(testthat::test_path("testdata", "test_df_with_calculation_hhs.rda"))


  testthat::expect_equal(object = add_hhs(.dataset = test_df,
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
  ) %>% dplyr::pull("hhs_cat_ipc"),
  expected = test_df_with_calculation %>% dplyr::pull("hhs_cat_ipc"))


  testthat::expect_equal(object = add_hhs(.dataset = test_df,
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
  ) %>% dplyr::pull("hhs_cat"),
  expected = test_df_with_calculation %>% dplyr::pull("hhs_cat"))



})


