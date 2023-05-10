
#' Add the food consumption matrix to the dataset
#'
#'
#' @param dataset A dataframe
#' @param fews_vars A vector of 3 strings for the column names of fcs, hhs, and rcsi categories from the dataframe
#' it has to include all "fcs_cat", "rcsi_cat" and "hhs_cat"
#' @param fcs_categories A vector of 3 strings for the values included in the fcs_cat column
#' It have to be a character and include "Acceptable", "Poor" and "Borderline"
#' @param rcsi_categories A vector of 3 strings for the values included in the rcsi_cat column
#' It have to be a character and include "No to Low", "Medium" and "Severe"
#' @param hhs_categories A vector of 5 strings for the values included in the hhs_cat column
#' It have to be a character and include "None","Little","Moderate"," Severe","Very Severe"
#'
#' @return this function returns a dataframe with a column called fc_cell that includes values from 1 to 45
#' representing the Food Consumption Score Matrix and the fc_phase column that includes the different 5 phases
#' of food consumption
#' @export
#' @importFrom dplyr case_when mutate
add_fcm_phase <- function(dataset,
                          fews_vars = c("fcs_cat","rcsi_cat","hhs_cat"),
                          fcs_categories = c("Acceptable", "Poor", "Borderline"),
                          rcsi_categories = c("No to Low", "Medium", "Severe"),
                          hhs_categories = c("None","Little","Moderate","Severe","Very Severe")) {
  ## Check if columns in dataset
  if(!all(fews_vars %in% names(dataset))) {
      warning("Please check if the fcs, rcsi, and hhs categories columns are avaialble in the data")
  }

  ## Check if fcs values are correct
  fcs_col <- dataset$fcs_cat

  if(!all(fcs_categories %in% fcs_col)){
    warning("Please check if the fcs categories parameter passes is matching the values in your data")
  }

  ## Check if hhs values are correct
  hhs_col <- dataset$hhs_cat

  if(!all(hhs_categories %in% hhs_col)){
    warning("Please check if the hhs categories parameter passes is matching the values in your data")
  }

  ## Check if rcsi values are correct
  rcsi_col <- dataset$rcsi_cat

  if(!all(rcsi_categories %in% rcsi_col)){
    warning("Please check if the rcsi categories parameter passes is matching the values in your data")
  }

  dataset <- dataset %>%
    dplyr::mutate(fc_cell = dplyr::case_when(fcs_cat == "Acceptable" & rcsi_cat == "No to Low" & hhs_cat =="None" ~ 1,
                                             fcs_cat == "Poor" & rcsi_cat == "No to Low" & hhs_cat =="None" ~ 11,
                                             fcs_cat == "Borderline" & rcsi_cat == "No to Low" & hhs_cat =="None" ~ 6,
                                             fcs_cat == "Acceptable" & rcsi_cat == "Medium" & hhs_cat =="None" ~ 16,
                                             fcs_cat == "Poor" & rcsi_cat == "Medium" & hhs_cat =="None" ~ 26,
                                             fcs_cat == "Borderline" & rcsi_cat == "Medium" & hhs_cat =="None" ~ 21,
                                             fcs_cat == "Acceptable" & rcsi_cat == "Severe" & hhs_cat =="None" ~ 31,
                                             fcs_cat == "Poor" & rcsi_cat == "Severe" & hhs_cat =="None" ~ 41,
                                             fcs_cat == "Borderline" & rcsi_cat == "Severe" & hhs_cat =="None" ~ 36,
                                             fcs_cat == "Acceptable" & rcsi_cat == "No to Low" & hhs_cat =="Little" ~ 2,
                                             fcs_cat == "Poor" & rcsi_cat == "No to Low" & hhs_cat =="Little" ~ 12,
                                             fcs_cat == "Borderline" & rcsi_cat == "No to Low" & hhs_cat =="Little" ~ 7,
                                             fcs_cat == "Acceptable" & rcsi_cat == "Medium" & hhs_cat =="Little" ~ 17,
                                             fcs_cat == "Poor" & rcsi_cat == "Medium" & hhs_cat =="Little" ~ 27,
                                             fcs_cat == "Borderline" & rcsi_cat == "Medium" & hhs_cat =="Little" ~ 22,
                                             fcs_cat == "Acceptable" & rcsi_cat == "Severe" & hhs_cat =="Little" ~ 32,
                                             fcs_cat == "Poor" & rcsi_cat == "Severe" & hhs_cat =="Little" ~ 42,
                                             fcs_cat == "Borderline" & rcsi_cat == "Severe" & hhs_cat =="Little" ~ 37,
                                             fcs_cat == "Acceptable" & rcsi_cat == "No to Low" & hhs_cat =="Moderate" ~ 3,
                                             fcs_cat == "Poor" & rcsi_cat == "No to Low" & hhs_cat =="Moderate" ~ 13,
                                             fcs_cat == "Borderline" & rcsi_cat == "No to Low" & hhs_cat =="Moderate" ~ 8,
                                             fcs_cat == "Acceptable" & rcsi_cat == "Medium" & hhs_cat =="Very Severe" ~ 18,
                                             fcs_cat == "Poor" & rcsi_cat == "Medium" & hhs_cat =="Moderate" ~ 28,
                                             fcs_cat == "Borderline" & rcsi_cat == "Medium" & hhs_cat =="Moderate" ~ 23,
                                             fcs_cat == "Acceptable" & rcsi_cat == "Severe" & hhs_cat =="Moderate" ~ 33,
                                             fcs_cat == "Poor" & rcsi_cat == "Severe" & hhs_cat =="VModerate" ~ 43,
                                             fcs_cat == "Borderline" & rcsi_cat == "Severe" & hhs_cat =="Moderate" ~ 38,
                                             fcs_cat == "Acceptable" & rcsi_cat == "No to Low" & hhs_cat =="Severe" ~ 4,
                                             fcs_cat == "Poor" & rcsi_cat == "No to Low" & hhs_cat =="Severe" ~ 14,
                                             fcs_cat == "Borderline" & rcsi_cat == "No to Low" & hhs_cat =="Severe" ~ 9,
                                             fcs_cat == "Acceptable" & rcsi_cat == "Medium" & hhs_cat =="Severe" ~ 19,
                                             fcs_cat == "Poor" & rcsi_cat == "Medium" & hhs_cat =="Severe" ~ 29,
                                             fcs_cat == "Borderline" & rcsi_cat == "Medium" & hhs_cat =="Severe" ~ 24,
                                             fcs_cat == "Acceptable" & rcsi_cat == "Severe" & hhs_cat =="Severe" ~ 34,
                                             fcs_cat == "Poor" & rcsi_cat == "Severe" & hhs_cat =="Severe" ~ 44,
                                             fcs_cat == "Borderline" & rcsi_cat == "Severe" & hhs_cat =="Severe" ~ 39,
                                             fcs_cat == "Acceptable" & rcsi_cat == "No to Low" & hhs_cat =="Very Severe" ~ 5,
                                             fcs_cat == "Poor" & rcsi_cat == "No to Low" & hhs_cat =="Very Severe" ~ 15,
                                             fcs_cat == "Borderline" & rcsi_cat == "No to Low" & hhs_cat =="Very Severe" ~ 10,
                                             fcs_cat == "Acceptable" & rcsi_cat == "Medium" & hhs_cat =="Very Severe" ~ 20,
                                             fcs_cat == "Poor" & rcsi_cat == "Medium" & hhs_cat =="Very Severe" ~ 30,
                                             fcs_cat == "Borderline" & rcsi_cat == "Medium" & hhs_cat =="Very Severe" ~ 25,
                                             fcs_cat == "Acceptable" & rcsi_cat == "Severe" & hhs_cat =="Very Severe" ~ 35,
                                             fcs_cat == "Poor" & rcsi_cat == "Severe" & hhs_cat =="Very Severe" ~ 45,
                                             fcs_cat == "Borderline" & rcsi_cat == "Severe" & hhs_cat =="Very Severe" ~ 40,
                                             TRUE ~ NA_real_))

  # create the fc_phase columns
  dataset <- dataset %>%
    dplyr::mutate(fc_phase = dplyr::case_when(fc_cell %in% c(1,6) ~ "Phase 1 FC",
                                              fc_cell %in% c(2,3,7,11,12,16,17,18,21,22,26,31,32,36) ~ "Phase 2 FC",
                                              fc_cell %in% c(4,5,8,9,13,19,20,23,24,27,28,33,34,37,38,41,42,43) ~ "Phase 3 FC",
                                              fc_cell %in% c(10,14,15,25,29,35,39,40,44) ~ "Phase 4 FC",
                                              fc_cell %in% c(30,45) ~ "Phase 5 FC",
                                              TRUE ~ NA_character_))

  }

