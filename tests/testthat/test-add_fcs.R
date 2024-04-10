###### Sad Path #######

testthat::test_that("Check input type -- dataset", {
  testthat::expect_error(add_fcs(.dataset = 0), "First argument should be a dataset")
  testthat::expect_error(add_fcs(.dataset = "x"), "First argument should be a dataset")
  testthat::expect_error(add_fcs(.dataset = 1.0), "First argument should be a dataset")
  testthat::expect_error(add_fcs(.dataset = F), "First argument should be a dataset")
  testthat::expect_error(add_fcs(.dataset = list()), "First argument should be a dataset")
})

testthat::test_that("Check dataframe empty", {
  df1 <- data.frame()
  testthat::expect_error(add_fcs(.dataset = df1, "Dataset is empty"))
})

testthat::test_that("Check for missing columns", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  testthat::expect_error(add_fcs(
    .dataset = test_df %>% dplyr::select(-fsl_fcs_cereal),
    fsl_fcs_cereal = "fsl_fcs_cereal"
  ), "Missing fcs columns")

  testthat::expect_error(add_fcs(
    .dataset = test_df %>% dplyr::select(-fsl_fcs_legumes),
    fsl_fcs_legumes = "fsl_fcs_legumes"
  ), "Missing fcs columns")

  testthat::expect_error(add_fcs(
    .dataset = test_df %>% dplyr::select(-fsl_fcs_veg),
    fsl_fcs_veg = "fsl_fcs_veg"
  ), "Missing fcs columns")

  testthat::expect_error(add_fcs(
    .dataset = test_df %>% dplyr::select(-fsl_fcs_fruit),
    fsl_fcs_fruit = "fsl_fcs_fruit"
  ), "Missing fcs columns")

  testthat::expect_error(add_fcs(
    .dataset = test_df %>% dplyr::select(-fsl_fcs_meat),
    fsl_fcs_meat = "fsl_fcs_meat"
  ), "Missing fcs columns")

  testthat::expect_error(add_fcs(
    .dataset = test_df %>% dplyr::select(-fsl_fcs_dairy),
    fsl_fcs_dairy = "fsl_fcs_dairy"
  ), "Missing fcs columns")

  testthat::expect_error(add_fcs(
    .dataset = test_df %>% dplyr::select(-fsl_fcs_sugar),
    fsl_fcs_sugar = "fsl_fcs_sugar"
  ), "Missing fcs columns")

  testthat::expect_error(add_fcs(
    .dataset = test_df %>% dplyr::select(-fsl_fcs_oil),
    fsl_fcs_oil = "fsl_fcs_oil"
  ), "Missing fcs columns")
})

testthat::test_that("Checking column values - [1:7]", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  set.seed(30)
  test_df[sample.int(nrow(test_df), 3), c("fsl_fcs_cereal")] <- 8
  test_df[sample.int(nrow(test_df), 3), c("fsl_fcs_cereal")] <- 9
  test_df[sample.int(nrow(test_df), 3), c("fsl_fcs_cereal")] <- 10

  testthat::expect_error(add_fcs(
    .dataset = test_df,
    cutoffs = "normal"
  ), "Wrong values in fsl_fcs_cereal: 9/10/8")
})

testthat::test_that("Checking column values - [1:7]", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  set.seed(29)
  test_df[sample.int(nrow(test_df), 3), c("fsl_fcs_legumes")] <- 8
  test_df[sample.int(nrow(test_df), 3), c("fsl_fcs_legumes")] <- 9
  test_df[sample.int(nrow(test_df), 3), c("fsl_fcs_legumes")] <- 10

  testthat::expect_error(add_fcs(
    .dataset = test_df,
    cutoffs = "normal"
  ), "Wrong values in fsl_fcs_legumes: 9/8/10")
})

testthat::test_that("Checking column values - [1:7]", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  set.seed(28)
  test_df[sample.int(nrow(test_df), 3), c("fsl_fcs_veg")] <- 8
  test_df[sample.int(nrow(test_df), 3), c("fsl_fcs_veg")] <- 9
  test_df[sample.int(nrow(test_df), 3), c("fsl_fcs_veg")] <- 10

  testthat::expect_error(add_fcs(
    .dataset = test_df,
    cutoffs = "normal"
  ), "Wrong values in fsl_fcs_veg: 10/8/9")
})

testthat::test_that("Checking column values - [1:7]", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  set.seed(27)
  test_df[sample.int(nrow(test_df), 3), c("fsl_fcs_fruit")] <- 8
  test_df[sample.int(nrow(test_df), 3), c("fsl_fcs_fruit")] <- 9
  test_df[sample.int(nrow(test_df), 3), c("fsl_fcs_fruit")] <- 10

  testthat::expect_error(add_fcs(
    .dataset = test_df,
    cutoffs = "normal"
  ), "Wrong values in fsl_fcs_fruit: 9/10/8")
})

testthat::test_that("Checking column values - [1:7]", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  set.seed(26)
  test_df[sample.int(nrow(test_df), 3), c("fsl_fcs_meat")] <- 8
  test_df[sample.int(nrow(test_df), 3), c("fsl_fcs_meat")] <- 9
  test_df[sample.int(nrow(test_df), 3), c("fsl_fcs_meat")] <- 10

  testthat::expect_error(add_fcs(
    .dataset = test_df,
    cutoffs = "normal"
  ), "Wrong values in fsl_fcs_meat: 8/10/9")
})

testthat::test_that("Checking column values - [1:7]", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  set.seed(25)
  test_df[sample.int(nrow(test_df), 3), c("fsl_fcs_dairy")] <- 8
  test_df[sample.int(nrow(test_df), 3), c("fsl_fcs_dairy")] <- 9
  test_df[sample.int(nrow(test_df), 3), c("fsl_fcs_dairy")] <- 10

  testthat::expect_error(add_fcs(
    .dataset = test_df,
    cutoffs = "normal"
  ), "Wrong values in fsl_fcs_dairy: 8/10/9")
})

testthat::test_that("Checking column values - [1:7]", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  set.seed(24)
  test_df[sample.int(nrow(test_df), 3), c("fsl_fcs_sugar")] <- 8
  test_df[sample.int(nrow(test_df), 3), c("fsl_fcs_sugar")] <- 9
  test_df[sample.int(nrow(test_df), 3), c("fsl_fcs_sugar")] <- 10

  testthat::expect_error(add_fcs(
    .dataset = test_df,
    cutoffs = "normal"
  ), "Wrong values in fsl_fcs_sugar: 10/8/9")
})

testthat::test_that("Checking column values - [1:7]", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  set.seed(23)
  test_df[sample.int(nrow(test_df), 3), c("fsl_fcs_oil")] <- 8
  test_df[sample.int(nrow(test_df), 3), c("fsl_fcs_oil")] <- 9
  test_df[sample.int(nrow(test_df), 3), c("fsl_fcs_oil")] <- 10

  testthat::expect_error(add_fcs(
    .dataset = test_df,
    cutoffs = "normal"
  ), "Wrong values in fsl_fcs_oil: 10/8/9")
})
#### Happy Path ####

testthat::test_that("Check calculations of fcs are correct", {
  df1 <- data.frame(
    fsl_fcs_cereal = c(1, 2, 3, 2, 5, 6, 7),
    fsl_fcs_legumes = c(3, 4, 5, 6, 1, 6, 5),
    fsl_fcs_veg = c(3, 2, 1, 6, 5, 4, 3),
    fsl_fcs_fruit = c(1, 4, 6, 2, 2, 2, 4),
    fsl_fcs_meat = c(5, 4, 3, 2, 7, 4, 5),
    fsl_fcs_dairy = c(1, 2, 6, 7, 3, 4, 2),
    fsl_fcs_sugar = c(1, 7, 6, 5, 2, 3, 4),
    fsl_fcs_oil = c(2, 3, 6, 5, 1, 7, 4)
  )
  result <- add_fcs(
    .dataset = df1,
    cutoffs = "normal"
  ) %>%
    dplyr::select(-starts_with("fcs_weight"), -fsl_fcs_cat)
  expected_result <- data.frame(
    fsl_fcs_cereal = c(1, 2, 3, 2, 5, 6, 7),
    fsl_fcs_legumes = c(3, 4, 5, 6, 1, 6, 5),
    fsl_fcs_veg = c(3, 2, 1, 6, 5, 4, 3),
    fsl_fcs_fruit = c(1, 4, 6, 2, 2, 2, 4),
    fsl_fcs_meat = c(5, 4, 3, 2, 7, 4, 5),
    fsl_fcs_dairy = c(1, 2, 6, 7, 3, 4, 2),
    fsl_fcs_sugar = c(1, 7, 6, 5, 2, 3, 4),
    fsl_fcs_oil = c(2, 3, 6, 5, 1, 7, 4),
    fsl_fcs_score = c(40.5, 51.0, 70.0, 71.0, 61.5, 73.0, 68.0)
  )
  testthat::expect_equal(result, expected_result)
})



# test 2 NA values
testthat::test_that("Check if the function doesnt compute FCS when a value is NA", {
  df1 <- data.frame(
    fsl_fcs_cereal = c(NA, 2, 3, 2, 5, 6, 7),
    fsl_fcs_legumes = c(3, 4, 5, 6, 1, 6, 5),
    fsl_fcs_veg = c(3, 2, 1, 6, 5, 4, 3),
    fsl_fcs_fruit = c(1, 4, 6, 2, 2, 2, 4),
    fsl_fcs_meat = c(5, 4, 3, 2, 7, 4, 5),
    fsl_fcs_dairy = c(1, 2, 6, 7, 3, 4, 2),
    fsl_fcs_sugar = c(1, 7, 6, 5, 2, 3, 4),
    fsl_fcs_oil = c(2, 3, 6, 5, 1, 7, 4)
  )
  result <- add_fcs(
    .dataset = df1,
    cutoffs = "normal"
  ) %>%
    dplyr::select(-starts_with("fcs_weight"), -fsl_fcs_cat)
  expected_result <- data.frame(
    fsl_fcs_cereal = c(NA, 2, 3, 2, 5, 6, 7),
    fsl_fcs_legumes = c(3, 4, 5, 6, 1, 6, 5),
    fsl_fcs_veg = c(3, 2, 1, 6, 5, 4, 3),
    fsl_fcs_fruit = c(1, 4, 6, 2, 2, 2, 4),
    fsl_fcs_meat = c(5, 4, 3, 2, 7, 4, 5),
    fsl_fcs_dairy = c(1, 2, 6, 7, 3, 4, 2),
    fsl_fcs_sugar = c(1, 7, 6, 5, 2, 3, 4),
    fsl_fcs_oil = c(2, 3, 6, 5, 1, 7, 4),
    fsl_fcs_score = c(NA, 51.0, 70.0, 71.0, 61.5, 73.0, 68.0)
  )
  testthat::expect_equal(result, expected_result)
})


# test warning existing variable fcs_score
testthat::test_that("Check and warns when fcs_score is already a variable in the df", {
  df1 <- data.frame(
    fsl_fcs_cereal = c(NA, 2, 3, 2, 5, 6, 7),
    fsl_fcs_legumes = c(3, 4, 5, 6, 1, 6, 5),
    fsl_fcs_veg = c(3, 2, 1, 6, 5, 4, 3),
    fsl_fcs_fruit = c(1, 4, 6, 2, 2, 2, 4),
    fsl_fcs_meat = c(5, 4, 3, 2, 7, 4, 5),
    fsl_fcs_dairy = c(1, 2, 6, 7, 3, 4, 2),
    fsl_fcs_sugar = c(1, 7, 6, 5, 2, 3, 4),
    fsl_fcs_oil = c(2, 3, 6, 5, 1, 7, 4),
    fsl_fcs_score = NA_character_
  )
  testthat::expect_warning(
    add_fcs(.dataset = df1,
            cutoffs = "normal"
    ),
    "There is already a variable called fsl_fcs_score in your dataset, it will be overwritten"
  )
})

# test warning existing variable fcs_cat
testthat::test_that("Check and warns when fcs_cat is already a variable in the df", {
  df1 <- data.frame(
    fsl_fcs_cereal = c(NA, 2, 3, 2, 5, 6, 7),
    fsl_fcs_legumes = c(3, 4, 5, 6, 1, 6, 5),
    fsl_fcs_veg = c(3, 2, 1, 6, 5, 4, 3),
    fsl_fcs_fruit = c(1, 4, 6, 2, 2, 2, 4),
    fsl_fcs_meat = c(5, 4, 3, 2, 7, 4, 5),
    fsl_fcs_dairy = c(1, 2, 6, 7, 3, 4, 2),
    fsl_fcs_sugar = c(1, 7, 6, 5, 2, 3, 4),
    fsl_fcs_oil = c(2, 3, 6, 5, 1, 7, 4),
    fsl_fcs_cat = NA_character_
  )
  testthat::expect_warning(
    add_fcs(.dataset = df1,
            cutoffs = "normal"
    ),
    "There is already a variable called fsl_fcs_cat in your dataset, it will be overwritten"
  )
})


# if the inputed variables are characters, but can be made numeric
testthat::test_that("if variables are as character, the function still works", {
  df1 <- data.frame(
    fsl_fcs_cereal = c(1, 2, 3, 2, 5, 6, 7),
    fsl_fcs_legumes = c(3, 4, 5, 6, 1, 6, 5),
    fsl_fcs_veg = c(3, 2, 1, 6, 5, 4, 3),
    fsl_fcs_fruit = c(1, 4, 6, 2, 2, 2, 4),
    fsl_fcs_meat = c(5, 4, 3, 2, 7, 4, 5),
    fsl_fcs_dairy = c(1, 2, 6, 7, 3, 4, 2),
    fsl_fcs_sugar = c(1, 7, 6, 5, 2, 3, 4),
    fsl_fcs_oil = c(2, 3, 6, 5, 1, 7, 4)
  )
  df1 <- sapply(df1, as.character) %>% as.data.frame()
  result <- add_fcs(
    .dataset = df1,
    cutoffs = "normal"
  ) %>%
    dplyr::select(-starts_with("fcs_weight"), -fsl_fcs_cat)
  expected_result <- data.frame(
    fsl_fcs_cereal = c(1, 2, 3, 2, 5, 6, 7),
    fsl_fcs_legumes = c(3, 4, 5, 6, 1, 6, 5),
    fsl_fcs_veg = c(3, 2, 1, 6, 5, 4, 3),
    fsl_fcs_fruit = c(1, 4, 6, 2, 2, 2, 4),
    fsl_fcs_meat = c(5, 4, 3, 2, 7, 4, 5),
    fsl_fcs_dairy = c(1, 2, 6, 7, 3, 4, 2),
    fsl_fcs_sugar = c(1, 7, 6, 5, 2, 3, 4),
    fsl_fcs_oil = c(2, 3, 6, 5, 1, 7, 4),
    fsl_fcs_score = c(40.5, 51.0, 70.0, 71.0, 61.5, 73.0, 68.0)
  )
  testthat::expect_equal(result, expected_result)
})


# test fcs_cat calculated correctly

testthat::test_that("Check calculations of fcs are correct -- normal cutoffs", {
  df1 <- data.frame(
    fsl_fcs_cereal = c(1, 2, 3, 2, 5, 6, 7),
    fsl_fcs_legumes = c(1, 4, 5, 6, 1, 6, 5),
    fsl_fcs_veg = c(2, 2, 1, 6, 5, 4, 3),
    fsl_fcs_fruit = c(1, 4, 6, 2, 2, 2, 4),
    fsl_fcs_meat = c(2, 4, 3, 2, 7, 4, 5),
    fsl_fcs_dairy = c(1, 2, 6, 7, 3, 4, 2),
    fsl_fcs_sugar = c(1, 7, 6, 5, 2, 3, 4),
    fsl_fcs_oil = c(2, 3, 6, 5, 1, 7, 4)
  )
  result <- add_fcs(
    .dataset = df1,
    cutoffs = "normal"
  ) %>%
    dplyr::select(-starts_with("fcs_weight"))
  expected_result <- data.frame(
    fsl_fcs_cereal = c(1, 2, 3, 2, 5, 6, 7),
    fsl_fcs_legumes = c(1, 4, 5, 6, 1, 6, 5),
    fsl_fcs_veg = c(2, 2, 1, 6, 5, 4, 3),
    fsl_fcs_fruit = c(1, 4, 6, 2, 2, 2, 4),
    fsl_fcs_meat = c(2, 4, 3, 2, 7, 4, 5),
    fsl_fcs_dairy = c(1, 2, 6, 7, 3, 4, 2),
    fsl_fcs_sugar = c(1, 7, 6, 5, 2, 3, 4),
    fsl_fcs_oil = c(2, 3, 6, 5, 1, 7, 4),
    fsl_fcs_score = c(21.5, 51.0, 70.0, 71.0, 61.5, 73.0, 68.0),
    fsl_fcs_cat = c("Borderline","Acceptable","Acceptable","Acceptable","Acceptable","Acceptable","Acceptable")
  )
  testthat::expect_equal(result, expected_result)
})

# test fcs_cat calculated correctly

testthat::test_that("Check calculations of fcs are correct -- alternative cutoffs", {
  df1 <- data.frame(
    fsl_fcs_cereal = c(1, 2, 3, 2, 5, 6, 7),
    fsl_fcs_legumes = c(1, 4, 5, 6, 1, 6, 5),
    fsl_fcs_veg = c(2, 2, 1, 6, 5, 4, 3),
    fsl_fcs_fruit = c(1, 4, 6, 2, 2, 2, 4),
    fsl_fcs_meat = c(2, 4, 3, 2, 7, 4, 5),
    fsl_fcs_dairy = c(1, 2, 6, 7, 3, 4, 2),
    fsl_fcs_sugar = c(1, 7, 6, 5, 2, 3, 4),
    fsl_fcs_oil = c(2, 3, 6, 5, 1, 7, 4)
  )
  result <- add_fcs(
    .dataset = df1,
    cutoffs = "alternative"
  ) %>%
    dplyr::select(-starts_with("fcs_weight"))
  expected_result <- data.frame(
    fsl_fcs_cereal = c(1, 2, 3, 2, 5, 6, 7),
    fsl_fcs_legumes = c(1, 4, 5, 6, 1, 6, 5),
    fsl_fcs_veg = c(2, 2, 1, 6, 5, 4, 3),
    fsl_fcs_fruit = c(1, 4, 6, 2, 2, 2, 4),
    fsl_fcs_meat = c(2, 4, 3, 2, 7, 4, 5),
    fsl_fcs_dairy = c(1, 2, 6, 7, 3, 4, 2),
    fsl_fcs_sugar = c(1, 7, 6, 5, 2, 3, 4),
    fsl_fcs_oil = c(2, 3, 6, 5, 1, 7, 4),
    fsl_fcs_score = c(21.5, 51.0, 70.0, 71.0, 61.5, 73.0, 68.0),
    fsl_fcs_cat = c("Poor","Acceptable","Acceptable","Acceptable","Acceptable","Acceptable","Acceptable")
  )
  testthat::expect_equal(result, expected_result)
})
