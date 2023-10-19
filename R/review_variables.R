#' Review 1 column comparing it to another one and spots differences
#'
#' @param dataset A dataset to be check.
#' @param column_to_review Name of the column to review.
#' @param column_to_compare_with Name of the column to compare with.
#' @param uuid_column uuid column in the dataset. Default is uuid.
#' @param prefix Prefix to be used for the review and comment column. Default is "review".
#' @param return_dataset Logical, if the result table should be returned. Default is "FALSE".
#'
#' @return The review table, or the review table added to the results.
#' @export
#'
#' @examples
#' test_numeric <- data.frame(
#'   test = c(
#'     "test equality",
#'     "test difference",
#'     "test Missing in y",
#'     "test Missing in x",
#'     "test equality rounding in x",
#'     "test equality rounding in y",
#'     "test difference rounding in x",
#'     "test difference rounding in y"
#'   ),
#'   var_x = c(0, 1, 2, NA, 0.00019, 0.0002, 0.00035, 0.0003),
#'   var_y = c(0, 2, NA, 3, 0.0002, 0.00019, 0.0003, 0.00035),
#'   uuid = letters[1:8]
#' )
#' review_one_variable(test_numeric,
#'   column_to_review = "var_x",
#'   column_to_compare_with = "var_y"
#' )
review_one_variable <- function(dataset,
                                column_to_review,
                                column_to_compare_with,
                                uuid_column = "uuid",
                                prefix = "review",
                                return_dataset = FALSE) {
  if (!column_to_review %in% names(dataset)) {
    msg <- glue::glue("Cannot find ", column_to_review, ".")
    stop(msg)
  }
  if (!column_to_compare_with %in% names(dataset)) {
    msg <- glue::glue("Cannot find ", column_to_compare_with, ".")
    stop(msg)
  }
  if (!uuid_column %in% names(dataset)) {
    msg <- glue::glue("Cannot find ", uuid_column, ".")
    stop(msg)
  }

  review_name <- paste0(prefix, "_check_", column_to_review)
  if (review_name %in% names(dataset)) {
    msg <- glue::glue(review_name, " already exists.")
    stop(msg)
  }

  comment_name <- paste0(prefix, "_comment_", column_to_review)
  if (comment_name %in% names(dataset)) {
    msg <- glue::glue(comment_name, " already exists.")
    stop(msg)
  }

  if (typeof(dataset[[column_to_review]]) %in% c("double", "integer")) {
    review_variable <- round(as.numeric(dataset[[column_to_review]]), 4) ==
      round(as.numeric(dataset[[column_to_compare_with]]), 4)
  } else if (typeof(dataset[[column_to_review]]) %in% c("character")) {
    review_variable <- dataset[[column_to_review]] ==
      dataset[[column_to_compare_with]]
  }

  review_variable[is.na(review_variable)] <- is.na(dataset[[column_to_review]][is.na(review_variable)]) &
    is.na(dataset[[column_to_compare_with]][is.na(review_variable)])

  comments_variable <- dplyr::case_when(
    review_variable ~ "Same results",
    is.na(dataset[[column_to_review]]) ~ glue::glue("Missing in ", column_to_review),
    is.na(dataset[[column_to_compare_with]]) ~ glue::glue("Missing in ", column_to_compare_with),
    TRUE ~ "Different results"
  )

  review_table <- data.frame(review_variable, comments_variable)

  names(review_table) <- c(review_name, comment_name)

  if (return_dataset) {
    dataset <- cbind(dataset, review_table)
    return(dataset)
  } else {
    review_table <- cbind(dataset[[uuid_column]], review_table) %>%
      `names<-`(c(uuid_column, names(review_table)))
    return(review_table)
  }
}

#' Review columns comparing it to another set of columns and spots differences
#'
#' review_variables is a wrapper around review_one_variable
#'
#' @param dataset A dataset to be check.
#' @param columns_to_review Vectors of columns to review (should be paired with
#' columns_to_compare_with).
#' @param columns_to_compare_with Vectors of columns to compare with  (should be paired with
#' columns_to_review).
#' @param uuid_column uuid column in the dataset. Default is uuid.
#' @param prefix Prefix to be used for the review and comment column. Default is "review"
#'
#' @return A list with two objects:
#'   - the result table the review and comment columns
#'   - the review table
#' @export
#'
#' @examples
#' test_numeric_2_var <- data.frame(
#'   test = c(
#'     "test equality",
#'     "test difference",
#'     "test Missing in y",
#'     "test Missing in x",
#'     "test equality rounding in x",
#'     "test equality rounding in y",
#'     "test difference rounding in x",
#'     "test difference rounding in y"
#'   ),
#'   stat_col_one.x = c(0, 1, 2, NA, 0.00019, 0.0002, 0.00035, 0.0003),
#'   stat_col_two.x = c(0, 1, 2, NA, 0.00019, 0.0002, 0.00035, 0.0003),
#'   stat_col_one.y = c(0, 2, NA, 3, 0.0002, 0.00019, 0.0003, 0.00035),
#'   stat_col_two.y = c(0, 2, NA, 3, 0.0002, 0.00019, 0.0003, 0.00035),
#'   uuid = letters[1:8]
#' )
#'
#' actual_results <- review_variables(test_numeric_2_var,
#'   columns_to_review = c("stat_col_one.x", "stat_col_two.x"),
#'   columns_to_compare_with = c("stat_col_one.y", "stat_col_two.y")
#' )
review_variables <- function(dataset,
                             columns_to_review,
                             columns_to_compare_with,
                             uuid_column = "uuid",
                             prefix = "review") {
  list_of_reviews <- purrr::map2(
    columns_to_review,
    columns_to_compare_with,
    ~ review_one_variable(dataset,
      column_to_review = .x,
      column_to_compare_with = .y,
      uuid_column = uuid_column,
      prefix = prefix
    )
  )
  review_table <- list_of_reviews %>%
    purrr::map2(.y = columns_to_review, ~ dplyr::mutate(.x, variable = .y)) %>%
    purrr::map(.f = ~ `names<-`(.x, c(
      uuid_column,
      paste0(prefix, "_check"),
      paste0(prefix, "_comment"),
      "variable"
    ))) %>%
    do.call(rbind, .) %>%
    dplyr::select(all_of(c(uuid_column, "variable", paste0(prefix, "_check"), paste0(prefix, "_comment"))))

  list_of_reviews <- purrr::reduce(list_of_reviews, dplyr::left_join, by = uuid_column)

  dataset <- dataset %>%
    dplyr::left_join(list_of_reviews, by = uuid_column)

  list_to_return <- list(
    dataset = dataset,
    review_table = review_table
  )
  return(list_to_return)
}
