#' Add the household hunger scale to the dataset
#'
#' @param .dataset Dataset
#' @param hhs_nofoodhh_1 The name of the column "In the past 4 weeks (30 days), was there ever no food to eat
#' of any kind in your house because of lack of resources to get food?". It has to be a string.
#' @param hhs_nofoodhh_1a The name of the column "How often did this happen in the past (4 weeks/30 days)?".
#' It has to be a string.
#' @param hhs_sleephungry_2 The name of the column "In the past 4 weeks (30 days), did you or any household
#' member go to sleep at night hungry because there was not enough food?". It has to be a string.
#' @param hhs_sleephungry_2a The name of the column "How often did this happen in the past (4 weeks/30 days)?".
#' It has to be a string.
#' @param hhs_alldaynight_3 The name of the column "In the past 4 weeks (30 days), did you or any household
#' member go a whole day and night without eating anything at all because there was not enough food?".
#' It has to be a string.
#' @param hhs_alldaynight_3a The name of the column "How often did this happen in the past (4 weeks/30 days)?".
#' It has to be a string.
#' @param yes_answer Value used for "Yes"
#' @param no_answer Value used for the "No"
#' @param rarely_answer Value used for "Rarely (1-2)"
#' @param sometimes_answer Value used for "Sometimes (3-10)"
#' @param often_answer Value used for "Often (10+ times)
#'
#' @return It returns the dataframe with 12 extras columns: recoded hhs questions, score for the 3 sets of questions
#' (from 0 to 2), the HHS score (from 0 to 6), the HHS category and the HHS IPC category
#'
#' @export
#'
#' @examples {
#'   input_data <- data.frame(
#'     fs_hhs_nofood_yn = c("no", "yes", "no", "no", "no"),
#'     fs_hhs_nofood_freq = c(NA_character_, "rarely_1_2", NA_character_, NA_character_, NA_character_),
#'     fs_hhs_sleephungry_yn = c("no", "no", "yes", "no", "no"),
#'     fs_hhs_sleephungry_freq = c(NA_character_, NA_character_, "often_10_times", NA_character_, NA_character_),
#'     fs_hhs_daynoteating_yn = c("no", "no", "yes", "yes", "yes"),
#'     fs_hhs_daynoteating_freq = c(NA_character_, NA_character_, "often_10_times", "rarely_1_2", "sometimes_3_10")
#'   )
#'
#'   add_hhs(
#'     .dataset = input_data,
#'     hhs_nofoodhh_1 = "fs_hhs_nofood_yn",
#'     hhs_nofoodhh_1a = "fs_hhs_nofood_freq",
#'     hhs_sleephungry_2 = "fs_hhs_sleephungry_yn",
#'     hhs_sleephungry_2a = "fs_hhs_sleephungry_freq",
#'     hhs_alldaynight_3 = "fs_hhs_daynoteating_yn",
#'     hhs_alldaynight_3a = "fs_hhs_daynoteating_freq",
#'     yes_answer = "yes",
#'     no_answer = "no",
#'     rarely_answer = "rarely_1_2",
#'     sometimes_answer = "sometimes_3_10",
#'     often_answer = "often_10_times"
#'   )
#' }
add_hhs <- function(.dataset,
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
                    often_answer = "often_10_times") {
  ## Throw an error if a dataset wasn't provided as a first argument
  if (!is.data.frame(.dataset)) {
    stop("First argument should be a dataset")
  }

  ## Throw an error if the dataset is empty
  if (nrow(.dataset) == 0) {
    stop("Dataset is empty")
  }

  ## Test if all columns are in the dataset
  tryCatch(
    {
      .dataset %>% dplyr::select(dplyr::all_of(c(
        hhs_nofoodhh_1,
        hhs_nofoodhh_1a,
        hhs_sleephungry_2,
        hhs_sleephungry_2a,
        hhs_alldaynight_3,
        hhs_alldaynight_3a
      )))
    },
    # if an error occurs, tell me the error
    error = function(e) {
      message("Missing hhs columns")
      # print(e)
    }
  )

  ## Throw an error in case wrong/unexpected values are found in hhs_nofoodhh_1
  if (!all(.dataset[[hhs_nofoodhh_1]] %in% c(yes_answer, no_answer, NA))) {
    stop(sprintf("Wrong values in %s: %s ", hhs_nofoodhh_1, paste0(unique(.dataset[[hhs_nofoodhh_1]][!.dataset[[hhs_nofoodhh_1]] %in% c(yes_answer, no_answer, NA)]), collapse = "/")))
  }

  ## Throw an error in case wrong/unexpected values are found in hhs_sleephungry_2
  if (!all(.dataset[[hhs_sleephungry_2]] %in% c(yes_answer, no_answer, NA))) {
    stop(sprintf("Wrong values in %s: %s ", hhs_sleephungry_2, paste0(unique(.dataset[[hhs_sleephungry_2]][!.dataset[[hhs_sleephungry_2]] %in% c(yes_answer, no_answer, NA)]), collapse = "/")))
  }

  ## Throw an error in case wrong/unexpected values are found in hhs_alldaynight_3
  if (!all(.dataset[[hhs_alldaynight_3]] %in% c(yes_answer, no_answer, NA))) {
    stop(sprintf("Wrong values in %s: %s ", hhs_alldaynight_3, paste0(unique(.dataset[[hhs_alldaynight_3]][!.dataset[[hhs_alldaynight_3]] %in% c(yes_answer, no_answer, NA)]), collapse = "/")))
  }

  ## Throw an error in case wrong/unexpected values are found in hhs_nofoodhh_1a
  if (!all(.dataset[[hhs_nofoodhh_1a]] %in% c(rarely_answer, sometimes_answer, often_answer, NA))) {
    stop(sprintf("Wrong values in %s: %s ", hhs_nofoodhh_1a, paste0(unique(.dataset[[hhs_nofoodhh_1a]][!.dataset[[hhs_nofoodhh_1a]] %in% c(rarely_answer, sometimes_answer, often_answer, NA)]), collapse = "/")))
  }

  ## Throw an error in case wrong/unexpected values are found in hhs_sleephungry_2a
  if (!all(.dataset[[hhs_sleephungry_2a]] %in% c(rarely_answer, sometimes_answer, often_answer, NA))) {
    stop(sprintf("Wrong values in %s: %s ", hhs_sleephungry_2a, paste0(unique(.dataset[[hhs_sleephungry_2a]][!.dataset[[hhs_sleephungry_2a]] %in% c(rarely_answer, sometimes_answer, often_answer, NA)]), collapse = "/")))
  }

  ## Throw an error in case wrong/unexpected values are found in hhs_alldaynight_3a
  if (!all(.dataset[[hhs_alldaynight_3a]] %in% c(rarely_answer, sometimes_answer, often_answer, NA))) {
    stop(sprintf("Wrong values in %s: %s ", hhs_alldaynight_3a, paste0(unique(.dataset[[hhs_alldaynight_3a]][!.dataset[[hhs_alldaynight_3a]] %in% c(rarely_answer, sometimes_answer, often_answer, NA)]), collapse = "/")))
  }


  ## Computing the hhs scores
  .dataset_with_calculation <- .dataset %>%
    dplyr::mutate_at(
      c(
        hhs_nofoodhh_1,
        hhs_sleephungry_2,
        hhs_alldaynight_3
      ),
      ~ dplyr::case_when(
        .x == yes_answer ~ 1,
        .x == no_answer ~ 0
      )
    ) %>%
    dplyr::mutate_at(
      c(
        hhs_nofoodhh_1a,
        hhs_sleephungry_2a,
        hhs_alldaynight_3a
      ),
      ~ dplyr::case_when(
        .x %in% c(rarely_answer, sometimes_answer) ~ 1,
        .x == often_answer ~ 2,
        TRUE ~ 0
      )
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      hhs_comp1 = !!rlang::sym(hhs_nofoodhh_1) * !!rlang::sym(hhs_nofoodhh_1a),
      hhs_comp2 = !!rlang::sym(hhs_sleephungry_2) * !!rlang::sym(hhs_sleephungry_2a),
      hhs_comp3 = !!rlang::sym(hhs_alldaynight_3) * !!rlang::sym(hhs_alldaynight_3a),
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      hhs_score = rowSums(.[grep("^hhs_comp\\d$", names(.))])
    ) %>%
    dplyr::mutate(
      hhs_cat_ipc = dplyr::case_when(
        hhs_score == 0 ~ "None",
        hhs_score == 1 ~ "Little",
        hhs_score <= 3 ~ "Moderate",
        hhs_score == 4 ~ "Severe",
        hhs_score <= 6 ~ "Very Severe"
      ),
      hhs_cat = dplyr::case_when(
        hhs_score <= 1 ~ "No or Little",
        hhs_score <= 3 ~ "Moderate",
        hhs_score <= 6 ~ "Severe",
        TRUE ~ NA_character_
      )
    )
  ## Selecting columns to export
  columns_to_export <- .dataset_with_calculation %>%
    dplyr::rename_at(c(
      hhs_nofoodhh_1,
      hhs_nofoodhh_1a,
      hhs_sleephungry_2,
      hhs_sleephungry_2a,
      hhs_alldaynight_3,
      hhs_alldaynight_3a
    ), ~ paste0(.x, "_recoded")) %>%
    dplyr::select(
      paste0(hhs_nofoodhh_1, "_recoded"),
      paste0(hhs_nofoodhh_1a, "_recoded"),
      paste0(hhs_sleephungry_2, "_recoded"),
      paste0(hhs_sleephungry_2a, "_recoded"),
      paste0(hhs_alldaynight_3, "_recoded"),
      paste0(hhs_alldaynight_3a, "_recoded"),
      hhs_comp1,
      hhs_comp2,
      hhs_comp3,
      hhs_score,
      hhs_cat_ipc,
      hhs_cat
    )

  .dataset <- .dataset %>% cbind(columns_to_export)


  return(.dataset)
}
