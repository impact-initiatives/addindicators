library(dplyr)

### sad path ###

testthat::test_that("Check input type -- dataset", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))
  testthat::expect_error(add_rcsi(.dataset = 0))
  testthat::expect_error(add_rcsi(.dataset = "x"))
  testthat::expect_error(add_rcsi(.dataset = 1.0))
  testthat::expect_error(add_rcsi(.dataset = FALSE))
  testthat::expect_error(add_rcsi(.dataset = list()))
})


testthat::test_that("Check dataframe empty", {
  df1 <- data.frame()
  testthat::expect_error(add_rcsi(.dataset = df1))
})

testthat::test_that("Check missing columns", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  testthat::expect_error(add_rcsi(
    .dataset = test_df %>% dplyr::select(-fsl_rcsi_lessquality),
    fsl_rcsi_lessquality = "fsl_rcsi_lessquality"
  ))

  testthat::expect_error(add_rcsi(
    .dataset = test_df %>% dplyr::select(-fsl_rcsi_borrow),
    fsl_rcsi_borrow = "fsl_rcsi_borrow"
  ))

  testthat::expect_error(add_rcsi(
    .dataset = test_df %>% dplyr::select(-fsl_rcsi_mealsize),
    fsl_rcsi_mealsize = "fsl_rcsi_mealsize"
  ))

  testthat::expect_error(add_rcsi(
    .dataset = test_df %>% dplyr::select(-fsl_rcsi_mealadult),
    fsl_rcsi_mealadult = "fsl_rcsi_mealadult"
  ))

  testthat::expect_error(add_rcsi(
    .dataset = test_df %>% dplyr::select(-fsl_rcsi_mealnb),
    fsl_rcsi_mealnb = "fsl_rcsi_mealnb"
  ))

})

testthat::test_that("Checking column values - [1:7]", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  set.seed(30)
  test_df[sample.int(nrow(test_df), 3), c("fsl_rcsi_lessquality")] <- 8
  test_df[sample.int(nrow(test_df), 3), c("fsl_rcsi_lessquality")] <- 9
  test_df[sample.int(nrow(test_df), 3), c("fsl_rcsi_lessquality")] <- 0
  testthat::expect_error(add_rcsi(
    .dataset = test_df,
  ))
})
testthat::test_that("Checking column values - [1:7]", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  set.seed(26)
  test_df[sample.int(nrow(test_df), 3), c("fsl_rcsi_borrow")] <- 8
  test_df[sample.int(nrow(test_df), 3), c("fsl_rcsi_borrow")] <- 9
  test_df[sample.int(nrow(test_df), 3), c("fsl_rcsi_borrow")] <- 0
  testthat::expect_error(add_rcsi(
    .dataset = test_df,
  ))
})
testthat::test_that("Checking column values - [1:7]", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  set.seed(25)
  test_df[sample.int(nrow(test_df), 3), c("fsl_rcsi_mealsize")] <- 8
  test_df[sample.int(nrow(test_df), 3), c("fsl_rcsi_mealsize")] <- 9
  test_df[sample.int(nrow(test_df), 3), c("fsl_rcsi_mealsize")] <- 0
  testthat::expect_error(add_rcsi(
    .dataset = test_df,
  ))
})
testthat::test_that("Checking column values - [1:7]", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  set.seed(23)
  test_df[sample.int(nrow(test_df), 3), c("fsl_rcsi_mealadult")] <- 8
  test_df[sample.int(nrow(test_df), 3), c("fsl_rcsi_mealadult")] <- 9
  test_df[sample.int(nrow(test_df), 3), c("fsl_rcsi_mealadult")] <- 0
  testthat::expect_error(add_rcsi(
    .dataset = test_df,
  ))
})
testthat::test_that("Checking column values - [1:7]", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  set.seed(22)
  test_df[sample.int(nrow(test_df), 3), c("fsl_rcsi_mealnb")] <- 8
  test_df[sample.int(nrow(test_df), 3), c("fsl_rcsi_mealnb")] <- 9
  test_df[sample.int(nrow(test_df), 3), c("fsl_rcsi_mealnb")] <- 0
  testthat::expect_error(add_rcsi(
    .dataset = test_df,
  ))
})

#### Happy Path ####




# if the inputed variables are characters, but can be made numeric
testthat::test_that("if variables are as character, the function stll works", {
  df1 <- data.frame(
    fsl_rcsi_lessquality = c(1, 2, 3, 1),
    fsl_rcsi_borrow = c(0, 0, 3, 0),
    fsl_rcsi_mealsize = c(4, 2, 6, 1),
    fsl_rcsi_mealadult = c(4, 3, 5, 0),
    fsl_rcsi_mealnb = c(2, 5, NA, 1)
  )
  df1 <- sapply(df1, as.character) %>% as.data.frame()
  result <- add_rcsi(
    .dataset = df1
  ) %>%
    dplyr::select(-ends_with("_weighted"))
  expected_result <- data.frame(
    fsl_rcsi_lessquality = c(1, 2, 3, 1),
    fsl_rcsi_borrow = c(0, 0, 3, 0),
    fsl_rcsi_mealsize = c(4, 2, 6, 1),
    fsl_rcsi_mealadult = c(4, 3, 5, 0),
    fsl_rcsi_mealnb = c(2, 5, NA, 1),
    fsl_rcsi_score = c(19, 18, NA, 3),
    fsl_rcsi_cat = c("High", "Medium", NA, "No to Low")
  )
  testthat::expect_equal(result, expected_result)
})




# test warning existing variable fsl_rcsi_score
testthat::test_that("Check and warns when fsl_rcsi_score is already a variable in the df", {
  df1 <- data.frame(
    fsl_rcsi_lessquality = c(1, 2, 3, 1),
    fsl_rcsi_borrow = c(0, 0, 3, 0),
    fsl_rcsi_mealsize = c(4, 2, 6, 1),
    fsl_rcsi_mealadult = c(4, 3, 5, 0),
    fsl_rcsi_mealnb = c(2, 5, NA, 1),
    fsl_rcsi_score = NA_character_
  )
  testthat::expect_warning(
    add_rcsi(.dataset = df1
    )
  )
})

# test warning existing variable fsl_rcsi_cat
testthat::test_that("Check and warns when fsl_rcsi_cat is already a variable in the df", {
  df1 <- data.frame(
    fsl_rcsi_lessquality = c(1, 2, 3, 1),
    fsl_rcsi_borrow = c(0, 0, 3, 0),
    fsl_rcsi_mealsize = c(4, 2, 6, 1),
    fsl_rcsi_mealadult = c(4, 3, 5, 0),
    fsl_rcsi_mealnb = c(2, 5, NA, 1),
    fsl_rcsi_cat = NA_character_
  )
  testthat::expect_warning(
    add_rcsi(.dataset = df1
    )
  )
})




