library(dplyr)

###### Sad Path #######

testthat::test_that("Check input type -- dataset", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))
  testthat::expect_error(add_hdds(.dataset = 0))
  testthat::expect_error(add_hdds(.dataset = "x"))
  testthat::expect_error(add_hdds(.dataset = 1.0))
  testthat::expect_error(add_hdds(.dataset = F))
  testthat::expect_error(add_hdds(.dataset = list()))
})


testthat::test_that("Check dataframe empty", {
  df1 <- data.frame()
  testthat::expect_error(add_hdds(.dataset = df1))
})

testthat::test_that("Check for missing columns", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))

  testthat::expect_error(add_hdds(
    .dataset = test_df %>% dplyr::select(-fsl_hdds_cereals),
    fsl_hdds_cereals = "fsl_hdds_cereals"
  ))

  testthat::expect_error(add_hdds(
    .dataset = test_df %>% dplyr::select(-fsl_hdds_tubers),
    fsl_hdds_tubers = "fsl_hdds_tubers"
  ))

  testthat::expect_error(add_hdds(
    .dataset = test_df %>% dplyr::select(-fsl_hdds_veg),
    fsl_hdds_veg = "fsl_hdds_veg"
  ))

  testthat::expect_error(add_hdds(
    .dataset = test_df %>% dplyr::select(-fsl_hdds_fruit),
    fsl_hdds_fruit = "fsl_hdds_fruit"
  ))

  testthat::expect_error(add_hdds(
    .dataset = test_df %>% dplyr::select(-fsl_hdds_meat),
    fsl_hdds_meat = "fsl_hdds_meat"
  ))

  testthat::expect_error(add_hdds(
    .dataset = test_df %>% dplyr::select(-fsl_hdds_eggs),
    fsl_hdds_eggs = "fsl_hdds_eggs"
  ))

  testthat::expect_error(add_hdds(
    .dataset = test_df %>% dplyr::select(-fsl_hdds_fish),
    fsl_hdds_fish = "fsl_hdds_fish"
  ))

  testthat::expect_error(add_hdds(
    .dataset = test_df %>% dplyr::select(-fsl_hdds_legumes),
    fsl_hdds_legumes = "fsl_hdds_legumes"
  ))

  testthat::expect_error(add_hdds(
    .dataset = test_df %>% dplyr::select(-fsl_hdds_dairy),
    fsl_hdds_dairy = "fsl_hdds_dairy"
  ))

  testthat::expect_error(add_hdds(
    .dataset = test_df %>% dplyr::select(-fsl_hdds_oil),
    fsl_hdds_oil = "fsl_hdds_oil"
  ))

  testthat::expect_error(add_hdds(
    .dataset = test_df %>% dplyr::select(-fsl_hdds_sugar),
    fsl_hdds_sugar = "fsl_hdds_sugar"
  ))

  testthat::expect_error(add_hdds(
    .dataset = test_df %>% dplyr::select(-fsl_hdds_condiments),
    fsl_hdds_condiments = "fsl_hdds_condiments"
  ))
})

testthat::test_that("Checking column values - [yes/no]", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))
  set.seed(30)
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_cereals"] <- "YES"
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_cereals"] <- "NO"
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_cereals"] <- "random_value"
  testthat::expect_error(add_hdds(
    .dataset = test_df
  ))
})
testthat::test_that("Checking column values - [yes/no]", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))
  set.seed(29)
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_tubers"] <- "YES"
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_tubers"] <- "NO"
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_tubers"] <- "random_value"
  testthat::expect_error(add_hdds(
    .dataset = test_df
  ))
})
testthat::test_that("Checking column values - [yes/no]", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))
  set.seed(28)
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_veg"] <- "YES"
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_veg"] <- "NO"
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_veg"] <- "random_value"
  testthat::expect_error(add_hdds(
    .dataset = test_df
  ))
})
testthat::test_that("Checking column values - [yes/no]", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))
  set.seed(27)
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_fruit"] <- "YES"
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_fruit"] <- "NO"
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_fruit"] <- "random_value"
  testthat::expect_error(add_hdds(
    .dataset = test_df
  ))
})
testthat::test_that("Checking column values - [yes/no]", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))
  set.seed(26)
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_meat"] <- "YES"
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_meat"] <- "NO"
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_meat"] <- "random_value"
  testthat::expect_error(add_hdds(
    .dataset = test_df
  ))
})
testthat::test_that("Checking column values - [yes/no]", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))
  set.seed(25)
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_eggs"] <- "YES"
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_eggs"] <- "NO"
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_eggs"] <- "random_value"
  testthat::expect_error(add_hdds(
    .dataset = test_df
  ))
})
testthat::test_that("Checking column values - [yes/no]", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))
  set.seed(24)
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_fish"] <- "YES"
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_fish"] <- "NO"
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_fish"] <- "random_value"
  testthat::expect_error(add_hdds(
    .dataset = test_df
  ))
})
testthat::test_that("Checking column values - [yes/no]", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))
  set.seed(23)
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_legumes"] <- "YES"
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_legumes"] <- "NO"
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_legumes"] <- "random_value"
  testthat::expect_error(add_hdds(
    .dataset = test_df
  ))
})
testthat::test_that("Checking column values - [yes/no]", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))
  set.seed(22)
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_dairy"] <- "YES"
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_dairy"] <- "NO"
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_dairy"] <- "random_value"
  testthat::expect_error(add_hdds(
    .dataset = test_df
  ))
})
testthat::test_that("Checking column values - [yes/no]", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))
  set.seed(21)
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_oil"] <- "YES"
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_oil"] <- "NO"
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_oil"] <- "random_value"
  testthat::expect_error(add_hdds(
    .dataset = test_df
  ))
})
testthat::test_that("Checking column values - [yes/no]", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))
  set.seed(20)
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_sugar"] <- "YES"
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_sugar"] <- "NO"
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_sugar"] <- "random_value"
  testthat::expect_error(add_hdds(
    .dataset = test_df
  ))
})
testthat::test_that("Checking column values - [yes/no]", {
  load(testthat::test_path("testdata", "test_df_hhs.rda"))
  set.seed(19)
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_condiments"] <- "YES"
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_condiments"] <- "NO"
  test_df[sample.int(nrow(test_df), 3),"fsl_hdds_condiments"] <- "random_value"
  testthat::expect_error(add_hdds(
    .dataset = test_df
  ))
})




#### Happy Path ####

testthat::test_that("Check calculations of hdds are correct", {
  df1 <- data.frame(
    fsl_hdds_cereals = c("no", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_tubers = c("no", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_veg = c("no", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_fruit = c("yes", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_meat = c("no", "no", "yes", "no", "yes", "no"),
    fsl_hdds_eggs = c("no", "no", "yes", "no", "yes", "no"),
    fsl_hdds_fish = c("no", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_legumes = c("yes", "no", "yes", "no", "yes", "no"),
    fsl_hdds_dairy = c("no", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_oil = c("yes", "yes", "yes", "no", "no", "no"),
    fsl_hdds_sugar = c("yes", "yes", "no", "no", "yes", "no"),
    fsl_hdds_condiments = c("no", "yes", "yes", "no", "yes", "no")
  )
  result <- add_hdds(
    .dataset = df1
  ) %>%
    dplyr::select(-ends_with("_recoded"))
  expected_result <- data.frame(
    fsl_hdds_cereals = c("no", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_tubers = c("no", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_veg = c("no", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_fruit = c("yes", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_meat = c("no", "no", "yes", "no", "yes", "no"),
    fsl_hdds_eggs = c("no", "no", "yes", "no", "yes", "no"),
    fsl_hdds_fish = c("no", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_legumes = c("yes", "no", "yes", "no", "yes", "no"),
    fsl_hdds_dairy = c("no", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_oil = c("yes", "yes", "yes", "no", "no", "no"),
    fsl_hdds_sugar = c("yes", "yes", "no", "no", "yes", "no"),
    fsl_hdds_condiments = c("no", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_score = c(4, 9, 11, 0, 11, 0),
    fsl_hdds_cat = c("Medium","High","High","Low","High","Low")
  )
  testthat::expect_equal(result, expected_result)
})

testthat::test_that("HDDS NA when value is NA", {
  df1 <- data.frame(
    fsl_hdds_cereals = c(NA, "yes", "yes", "no", "yes", "no"),
    fsl_hdds_tubers = c("no", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_veg = c("no", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_fruit = c("yes", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_meat = c("no", "no", "yes", "no", "yes", "no"),
    fsl_hdds_eggs = c("no", "no", "yes", "no", "yes", "no"),
    fsl_hdds_fish = c("no", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_legumes = c("yes", "no", "yes", "no", "yes", "no"),
    fsl_hdds_dairy = c("no", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_oil = c("yes", "yes", "yes", "no", "no", "no"),
    fsl_hdds_sugar = c("yes", "yes", "no", "no", "yes", "no"),
    fsl_hdds_condiments = c("no", "yes", "yes", "no", "yes", "no")
  )
  result <- add_hdds(
    .dataset = df1
  ) %>%
    dplyr::select(-ends_with("_recoded"))
  expected_result <- data.frame(
    fsl_hdds_cereals = c(NA, "yes", "yes", "no", "yes", "no"),
    fsl_hdds_tubers = c("no", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_veg = c("no", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_fruit = c("yes", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_meat = c("no", "no", "yes", "no", "yes", "no"),
    fsl_hdds_eggs = c("no", "no", "yes", "no", "yes", "no"),
    fsl_hdds_fish = c("no", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_legumes = c("yes", "no", "yes", "no", "yes", "no"),
    fsl_hdds_dairy = c("no", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_oil = c("yes", "yes", "yes", "no", "no", "no"),
    fsl_hdds_sugar = c("yes", "yes", "no", "no", "yes", "no"),
    fsl_hdds_condiments = c("no", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_score = c(NA, 9, 11, 0, 11, 0),
    fsl_hdds_cat = c(NA,"High","High","Low","High","Low")
  )
  testthat::expect_equal(result, expected_result)
})

# test warning existing variable fsl_hdds_score
testthat::test_that("HDDS NA when value is NA", {
  df1 <- data.frame(
    fsl_hdds_cereals = c(NA, "yes", "yes", "no", "yes", "no"),
    fsl_hdds_tubers = c("no", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_veg = c("no", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_fruit = c("yes", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_meat = c("no", "no", "yes", "no", "yes", "no"),
    fsl_hdds_eggs = c("no", "no", "yes", "no", "yes", "no"),
    fsl_hdds_fish = c("no", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_legumes = c("yes", "no", "yes", "no", "yes", "no"),
    fsl_hdds_dairy = c("no", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_oil = c("yes", "yes", "yes", "no", "no", "no"),
    fsl_hdds_sugar = c("yes", "yes", "no", "no", "yes", "no"),
    fsl_hdds_condiments = c("no", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_score = NA_character_
  )
  testthat::expect_warning(add_hdds(
    .dataset = df1
  ))
})
# test warning existing variable fsl_hdds_cat
testthat::test_that("HDDS NA when value is NA", {
  df1 <- data.frame(
    fsl_hdds_cereals = c(NA, "yes", "yes", "no", "yes", "no"),
    fsl_hdds_tubers = c("no", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_veg = c("no", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_fruit = c("yes", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_meat = c("no", "no", "yes", "no", "yes", "no"),
    fsl_hdds_eggs = c("no", "no", "yes", "no", "yes", "no"),
    fsl_hdds_fish = c("no", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_legumes = c("yes", "no", "yes", "no", "yes", "no"),
    fsl_hdds_dairy = c("no", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_oil = c("yes", "yes", "yes", "no", "no", "no"),
    fsl_hdds_sugar = c("yes", "yes", "no", "no", "yes", "no"),
    fsl_hdds_condiments = c("no", "yes", "yes", "no", "yes", "no"),
    fsl_hdds_cat = NA_character_
  )
  testthat::expect_warning(add_hdds(
    .dataset = df1
  ))
})
