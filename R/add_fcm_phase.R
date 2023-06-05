#' Add the food consumption matrix to the dataset
#'
#' @param dataset A dataframe
#' @param fcs_column_name A string specifying the column name of the food consumption score in the dataset
#' @param rcsi_column_name A string specifying the column name of the reduced coping strategy index in the dataset
#' @param hhs_column_name A string specifying the column name of the household hunger scale in the dataset
#' @param fcs_categories_acceptable The name of the value "Acceptable" (by default) in the fcs categories
#' @param fcs_categories_poor The name of the value "Poor" (by default) in the fcs categories
#' @param fcs_categories_borderline The name of the value "Borderline" (by default) in the fcs categories
#' @param rcsi_categories_low The name of the value "No to Low" (by default) in the rcsi categories
#' @param rcsi_categories_medium The name of the value "Medium" (by default) in the rcsi categories
#' @param rcsi_categories_high The name of the value "High" (by default) in the rcsi categories
#' @param hhs_categories_none The name of the value "None" (by default) in the hhs categories
#' @param hhs_categories_little The name of the value "Little" (by default) in the hhs categories
#' @param hhs_categories_moderate The name of the value "Moderate" (by default) in the hhs categories
#' @param hhs_categories_severe The name of the value "Severe" (by default) in the hhs categories
#' @param hhs_categories_very_severe The name of the value "Very Severe" (by default) in the hhs categories
#'
#'
#' @return this function returns a dataframe with a column called fc_cell that includes values from 1 to 45
#' representing the Food Consumption Score Matrix and the fc_phase column that includes the different 5 phases
#' of food consumption
#' @export
#' @importFrom dplyr case_when mutate
#'
#'
#' @examples
#' test_data <- data.frame(
#'   fcs_cat = c("Acceptable", "Poor", "Borderline", "Acceptable"),
#'   rcsi_cat = c("No to Low", "Medium", "No to Low", "High"),
#'   hhs_cat = c("None", "Little", "Severe", "Very Severe")
#' )
#' add_fcm_phase(test_data,
#'   fcs_column_name = "fcs_cat",
#'   rcsi_column_name = "rcsi_cat",
#'   hhs_column_name = "hhs_cat",
#'   fcs_categories_acceptable = "Acceptable",
#'   fcs_categories_poor = "Poor",
#'   fcs_categories_borderline = "Borderline",
#'   rcsi_categories_low = "No to Low",
#'   rcsi_categories_medium = "Medium",
#'   rcsi_categories_high = "High",
#'   hhs_categories_none = "None",
#'   hhs_categories_little = "Little",
#'   hhs_categories_moderate = "Moderate",
#'   hhs_categories_severe = "Severe",
#'   hhs_categories_very_severe = "Very Severe"
#' )
#'
add_fcm_phase <- function(dataset,
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
                          hhs_categories_very_severe = "Very Severe") {
  fews_vars <- c(fcs_column_name, rcsi_column_name, hhs_column_name)

  ## Check if columns in dataset
  if (!all(fews_vars %in% names(dataset))) {
    warning("Please check if the fcs, rcsi, and hhs categories columns are avaialble in the data")
  }

  ## Check if fcs values are correct
  fcs_categories <- c(fcs_categories_acceptable, fcs_categories_poor, fcs_categories_borderline)

  if (!all(fcs_categories %in% dataset[[fcs_column_name]])) {
    warning("Please check if the fcs categories parameter passes is matching the values in your data")
  }

  ## Check if hhs values are correct
  hhs_categories <- c(hhs_categories_none, hhs_categories_little, hhs_categories_moderate, hhs_categories_severe, hhs_categories_very_severe)

  if (!all(hhs_categories %in% dataset[[hhs_column_name]])) {
    warning("Please check if the hhs categories parameter passes is matching the values in your data")
  }

  ## Check if rcsi values are correct
  rcsi_categories <- c(rcsi_categories_low, rcsi_categories_medium, rcsi_categories_high)

  if (!all(rcsi_categories %in% dataset[[rcsi_column_name]])) {
    warning("Please check if the rcsi categories parameter passes is matching the values in your data")
  }

  dataset <- dataset |>
    dplyr::mutate(fc_cell = dplyr::case_when(
      !!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_none ~ 1,
      !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_none ~ 11,
      !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_none ~ 6,
      !!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_none ~ 16,
      !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_none ~ 26,
      !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_none ~ 21,
      !!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_none ~ 31,
      !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_none ~ 41,
      !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_none ~ 36,
      !!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_little ~ 2,
      !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_little ~ 12,
      !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_little ~ 7,
      !!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_little ~ 17,
      !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_little ~ 27,
      !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_little ~ 22,
      !!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_little ~ 32,
      !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_little ~ 42,
      !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_little ~ 37,
      !!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_moderate ~ 3,
      !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_moderate ~ 13,
      !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_moderate ~ 8,
      !!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_moderate ~ 18,
      !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_moderate ~ 28,
      !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_moderate ~ 23,
      !!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_moderate ~ 33,
      !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_moderate ~ 43,
      !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_moderate ~ 38,
      !!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_severe ~ 4,
      !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_severe ~ 14,
      !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_severe ~ 9,
      !!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_severe ~ 19,
      !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_severe ~ 29,
      !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_severe ~ 24,
      !!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_severe ~ 34,
      !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_severe ~ 44,
      !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_severe ~ 39,
      !!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_very_severe ~ 5,
      !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_very_severe ~ 15,
      !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_low & !!rlang::sym(hhs_column_name) == hhs_categories_very_severe ~ 10,
      !!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_very_severe ~ 20,
      !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_very_severe ~ 30,
      !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_medium & !!rlang::sym(hhs_column_name) == hhs_categories_very_severe ~ 25,
      !!rlang::sym(fcs_column_name) == fcs_categories_acceptable & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_very_severe ~ 35,
      !!rlang::sym(fcs_column_name) == fcs_categories_poor & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_very_severe ~ 45,
      !!rlang::sym(fcs_column_name) == fcs_categories_borderline & !!rlang::sym(rcsi_column_name) == rcsi_categories_high & !!rlang::sym(hhs_column_name) == hhs_categories_very_severe ~ 40,
      TRUE ~ NA_real_
    ))

  # create the fc_phase columns
  dataset <- dataset |>
    dplyr::mutate(fc_phase = dplyr::case_when(
      fc_cell %in% c(1, 6) ~ "Phase 1 FC",
      fc_cell %in% c(2, 3, 7, 11, 12, 16, 17, 18, 21, 22, 26, 31, 32, 36) ~ "Phase 2 FC",
      fc_cell %in% c(4, 5, 8, 9, 13, 19, 20, 23, 24, 27, 28, 33, 34, 37, 38, 41, 42, 43) ~ "Phase 3 FC",
      fc_cell %in% c(10, 14, 15, 25, 29, 35, 39, 40, 44) ~ "Phase 4 FC",
      fc_cell %in% c(30, 45) ~ "Phase 5 FC",
      TRUE ~ NA_character_
    ))
}
