test_that("review_one_variable returns correct results for numeric values", {
  test_numeric <- data.frame(
    test = c(
      "test equality",
      "test difference",
      "test Missing in y",
      "test Missing in x",
      "test equality missing in both",
      "test equality rounding in x",
      "test equality rounding in y",
      "test difference rounding in x",
      "test difference rounding in y"
    ),
    var_x = c(0, 1, 2, NA, NA, 0.00019, 0.0002, 0.00035, 0.0003),
    var_y = c(0, 2, NA, 3, NA, 0.0002, 0.00019, 0.0003, 0.00035),
    uuid = letters[1:9]
  )
  expected_results <- data.frame(
    uuid = letters[1:9],
    review_check_var_x = c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE),
    review_comment_var_x = c(
      "Same results",
      "Different results",
      "Missing in var_y",
      "Missing in var_x",
      "Same results",
      "Same results",
      "Same results",
      "Different results",
      "Different results"
    )
  )
  actual_results <- review_one_variable(test_numeric,
    column_to_review = "var_x",
    column_to_compare_with = "var_y"
  )
  expect_equal(actual_results, expected_results)

  actual_results_returns_table <- review_one_variable(test_numeric,
    column_to_review = "var_x",
    column_to_compare_with = "var_y",
    return_dataset = TRUE
  )

  expected_return_table <- dplyr::left_join(test_numeric, expected_results)
  expect_equal(actual_results_returns_table, expected_return_table)

  new_prefix_expected_results <- expected_results %>%
    `names<-`(c("uuid", "my_review_check_var_x", "my_review_comment_var_x"))
  new_prefix_actual_results <- review_one_variable(test_numeric,
    column_to_review = "var_x",
    column_to_compare_with = "var_y",
    prefix = "my_review"
  )
  expect_equal(new_prefix_actual_results, new_prefix_expected_results)
})

test_that("review_one_variable returns correct results for categorical values", {
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
  expected_results <- data.frame(
    uuid = letters[1:5],
    review_check_var_x = c(TRUE, FALSE, FALSE, FALSE, TRUE),
    review_comment_var_x = c(
      "Same results",
      "Different results",
      "Missing in var_y",
      "Missing in var_x",
      "Same results"
    )
  )
  actual_results <- review_one_variable(test_categorical,
    column_to_review = "var_x",
    column_to_compare_with = "var_y"
  )
  expect_equal(actual_results, expected_results)
})

test_that("review_one_variable returns correct error messages", {
  test_numeric <- data.frame(
    test = c(
      "test equality",
      "test difference",
      "test Missing in y",
      "test Missing in x",
      "test equality rounding in x",
      "test equality rounding in y",
      "test difference rounding in x",
      "test difference rounding in y"
    ),
    var_x = c(0, 1, 2, NA, 0.00019, 0.0002, 0.00035, 0.0003),
    var_y = c(0, 2, NA, 3, 0.0002, 0.00019, 0.0003, 0.00035),
    uuid = letters[1:8],
    review_check_var_x = c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
    review_comment_var_x = c(
      "Same results",
      "Different results",
      "Missing in var_y",
      "Missing in var_x",
      "Same results",
      "Same results",
      "Different results",
      "Different results"
    )
  )

  # cannot find x
  expect_error(
    review_one_variable(
      test_numeric %>%
        dplyr::select(!all_of(c("review_check_var_x", "review_comment_var_x"))),
      column_to_review = "VAR_X", column_to_compare_with = "var_y"
    ),
    "Cannot find VAR_X."
  )
  # cannot find y
  expect_error(
    review_one_variable(
      test_numeric %>%
        dplyr::select(!all_of(c("review_check_var_x", "review_comment_var_x"))),
      column_to_review = "var_x", column_to_compare_with = "VAR_Y"
    ),
    "Cannot find VAR_Y."
  )
  # cannot find uuid
  expect_error(
    review_one_variable(
      test_numeric %>%
        dplyr::select(!all_of(c("review_check_var_x", "review_comment_var_x"))),
      column_to_review = "var_x", column_to_compare_with = "var_y", uuid_column = "UUID"
    ),
    "Cannot find UUID."
  )

  # review_xxx already exist
  expect_error(
    review_one_variable(
      test_numeric %>%
        dplyr::select(!all_of(c("review_comment_var_x"))),
      column_to_review = "var_x", column_to_compare_with = "var_y"
    ),
    "review_check_var_x already exists."
  )

  expect_error(
    review_one_variable(
      test_numeric %>%
        dplyr::select(!all_of(c("review_check_var_x"))),
      column_to_review = "var_x", column_to_compare_with = "var_y"
    ),
    "review_comment_var_x already exists."
  )
})

test_that("review_variables returns correct numerical results", {
  test_numeric_2_var <- data.frame(
    test = c(
      "test equality",
      "test difference",
      "test Missing in y",
      "test Missing in x",
      "test equality rounding in x",
      "test equality rounding in y",
      "test difference rounding in x",
      "test difference rounding in y"
    ),
    stat_col_one.x = c(0, 1, 2, NA, 0.00019, 0.0002, 0.00035, 0.0003),
    stat_col_two.x = c(0, 1, 2, NA, 0.00019, 0.0002, 0.00035, 0.0003),
    stat_col_one.y = c(0, 2, NA, 3, 0.0002, 0.00019, 0.0003, 0.00035),
    stat_col_two.y = c(0, 2, NA, 3, 0.0002, 0.00019, 0.0003, 0.00035),
    uuid = letters[1:8]
  )

  review_check_columm <- c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE)
  review_comment_column_stat_col_one.x <- c(
    "Same results",
    "Different results",
    "Missing in stat_col_one.y",
    "Missing in stat_col_one.x",
    "Same results",
    "Same results",
    "Different results",
    "Different results"
  )

  review_comment_column_stat_col_two.x <- c(
    "Same results",
    "Different results",
    "Missing in stat_col_two.y",
    "Missing in stat_col_two.x",
    "Same results",
    "Same results",
    "Different results",
    "Different results"
  )

  expected_results_table <- test_numeric_2_var %>%
    cbind(
      review_check_stat_col_one.x = review_check_columm,
      review_comment_stat_col_one.x = review_comment_column_stat_col_one.x,
      review_check_stat_col_two.x = review_check_columm,
      review_comment_stat_col_two.x = review_comment_column_stat_col_two.x
    )

  expected_review_table <- data.frame(
    uuid = rep(letters[1:8],2),
    variable = c(rep("stat_col_one.x", 8), rep("stat_col_two.x", 8)),
    review_check = rep(review_check_columm, 2),
    review_comment = c(
      review_comment_column_stat_col_one.x,
      review_comment_column_stat_col_two.x
    )
  )

  expected_results <- list(
    dataset = expected_results_table,
    review_table = expected_review_table
  )

  actual_results <- review_variables(test_numeric_2_var,
    columns_to_review = c("stat_col_one.x", "stat_col_two.x"),
    columns_to_compare_with = c("stat_col_one.y", "stat_col_two.y")
  )

  expect_equal(actual_results, expected_results)
})

test_that("review_variables returns correct results for categorical and numerical values", {
  test_categorical_numerical <- data.frame(
    test = c(
      "test equality",
      "test difference",
      "test Missing in y",
      "test Missing in x"
    ),
    var_x = c("A", "B", "C", NA),
    var_y = c("A", "A", NA, "D"),
    stat_col_one.x = c(0, 1, 2, NA),
    stat_col_one.y = c(0, 1, 2, NA),
    uuid = letters[1:4]
  )
  expected_results_table <- cbind(test_categorical_numerical,
    review_check_var_x = c(TRUE, FALSE, FALSE, FALSE),
    review_comment_var_x = c(
      "Same results",
      "Different results",
      "Missing in var_y",
      "Missing in var_x"
    ),
    review_check_stat_col_one.x = rep(TRUE, 4),
    review_comment_stat_col_one.x = rep("Same results", 4)
  )
  expected_review_table <- data.frame(
    uuid = rep(letters[1:4],2),
    variable = c(rep("var_x", 4), rep("stat_col_one.x", 4)),
    review_check = c(TRUE, FALSE, FALSE, FALSE, rep(TRUE, 4)),
    review_comment = c(
      "Same results",
      "Different results",
      "Missing in var_y",
      "Missing in var_x",
      rep("Same results", 4)
    )
  )
  expected_results <- list(
    dataset = expected_results_table,
    review_table = expected_review_table
  )


  actual_results <- review_variables(test_categorical_numerical,
    columns_to_review = c("var_x", "stat_col_one.x"),
    columns_to_compare_with = c("var_y", "stat_col_one.y")
  )
  expect_equal(actual_results, expected_results)
})
