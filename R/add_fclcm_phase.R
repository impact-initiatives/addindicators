
#' Calculating FEWSNET Food Consumption-Livelihood Coping Matrix
#'
#' @param dataset Dataset
#' @param fc_phase_var Column name containing food consumption phase.
#' @param fc_phase_1 The name of the value "Phase 1 FC" (by default) in the food consumption phase.
#' @param fc_phase_2 The name of the value "Phase 2 FC" (by default) in the food consumption phase.
#' @param fc_phase_3 The name of the value "Phase 3 FC" (by default) in the food consumption phase.
#' @param fc_phase_4 The name of the value "Phase 4 FC" (by default) in the food consumption phase.
#' @param fc_phase_5 The name of the value "Phase 5 FC" (by default) in the food consumption phase.
#' @param lcs_cat_var Column name containing livelihood coping category.
#' @param lcs_cat_none The name of the value "None" (by default) in the livelihood coping category.
#' @param lcs_cat_stress The name of the value "Stress" (by default) in the livelihood coping category.
#' @param lcs_cat_crisis The name of the value "Crisis" (by default) in the livelihood coping category.
#' @param lcs_cat_emergency The name of the value "Emergency" (by default) in the livelihood coping category.
#' @param fclcm_phase_var A character vector which will be the column name for FSLC phase.
#' @return Returns a dataframe with a additional column for FCLC phase.
#' @export
#'
#' @examples
#' test_df <- data.frame(lcs_cat_yes = c("None", "Stress"),
#' fc_phase = c("Phase 1 FC","Phase 2 FC"))
#' test_df |> add_fclcm_phase()

add_fclcm_phase <- function(dataset,
                            fc_phase_var = "fc_phase",
                            fc_phase_1 = "Phase 1 FC",
                            fc_phase_2 = "Phase 2 FC",
                            fc_phase_3 = "Phase 3 FC",
                            fc_phase_4 = "Phase 4 FC",
                            fc_phase_5 = "Phase 5 FC",
                            lcs_cat_var = "lcs_cat_yes", # need to cross check with saeaad/ yann
                            lcs_cat_none = "None" ,
                            lcs_cat_stress = "Stress",
                            lcs_cat_crisis = "Crisis",
                            lcs_cat_emergency = "Emergency",
                            fclcm_phase_var = "fclcm_phase"
) {

  if(sum(is.na(dataset[[fc_phase_var]])) > 0){ warning(glue::glue({{fc_phase_var}}, " has NA(s)"))}
  if(sum(is.na(dataset[[lcs_cat_var]])) > 0){ warning(glue::glue({{lcs_cat_var}}, " has NA(s)"))}

  if(fclcm_phase_var %in% names(dataset)){warning(glue::glue({{fclcm_phase_var}}, "is already existed in the dataset and will be replaced with new value"))}

  fc_phase_all_phase <- c(fc_phase_1,fc_phase_2,fc_phase_3,fc_phase_4,fc_phase_5)
  lcs_cat_all_phase <- c(lcs_cat_none,lcs_cat_stress,lcs_cat_crisis,lcs_cat_emergency)


  if(!fc_phase_var %in% names(dataset)){
    stop(glue::glue({{fc_phase_var}}, " column is not found in the dataset"))
  }

  if(!lcs_cat_var %in% names(dataset)){
    stop(glue::glue({{lcs_cat_var}}, " column is not found in the dataset"))
  }


  if(!all(unique(na.omit(dataset[[lcs_cat_var]])) %in% lcs_cat_all_phase)){
    print(unique(dataset[[lcs_cat_var]])[!unique(dataset[[lcs_cat_var]]) %in% lcs_cat_all_phase])
    stop("The above value(s) are not specified/possibile wrong entry")
  }

  if(!all(unique(na.omit(dataset[[fc_phase_var]])) %in% fc_phase_all_phase)){
    print(unique(dataset[[fc_phase_var]])[!unique(dataset[[fc_phase_var]]) %in% fc_phase_all_phase])
    stop("The above value(s) are not specified/possibile wrong entry")
  }


  if(!all(lcs_cat_all_phase %in% dataset[[lcs_cat_var]])){
    print(lcs_cat_all_phase[!lcs_cat_all_phase %in% dataset[[lcs_cat_var]]])
    warning("The above parameter(s) are not found in the dataset")
  }

  if(!all(fc_phase_all_phase %in% dataset[[fc_phase_var]])){
    print(fc_phase_all_phase[!fc_phase_all_phase %in% dataset[[fc_phase_var]]])
    warning("The above parameter(s) are not found in the dataset")
  }




  dataset %>% dplyr::mutate(
    !!rlang::sym(fclcm_phase_var) := dplyr::case_when( is.na(!!rlang::sym(fc_phase_var)) ~ NA_character_,
                                                       is.na(!!rlang::sym(lcs_cat_var)) ~ NA_character_,
                                                       !!rlang::sym(fc_phase_var) == "Phase 1 FC" & !!rlang::sym(lcs_cat_var) %in% c("None", "Stress")  ~ "Phase 1 FCLC",
                                                       (!!rlang::sym(fc_phase_var) == "Phase 1 FC" & !!rlang::sym(lcs_cat_var) == "Crisis") |
                                                         !!rlang::sym(fc_phase_var) == "Phase 2 FC" & !!rlang::sym(lcs_cat_var) %in% c("None", "Stress") ~ "Phase 2 FCLC",
                                                       (!!rlang::sym(fc_phase_var) == "Phase 1 FC" & !!rlang::sym(lcs_cat_var) == "Emergency") |
                                                         (!!rlang::sym(fc_phase_var) == "Phase 2 FC" & !!rlang::sym(lcs_cat_var) %in% c("Crisis", "Emergency")) |
                                                         (!!rlang::sym(fc_phase_var) == "Phase 3 FC" & !!rlang::sym(lcs_cat_var) %in% c("None", "Stress", "Crisis")) ~ "Phase 3 FCLC",
                                                       (!!rlang::sym(fc_phase_var) == "Phase 3 FC" & !!rlang::sym(lcs_cat_var) == "Emergency") | !!rlang::sym(fc_phase_var) == "Phase 4 FC"  ~ "Phase 4 FCLC",
                                                       !!rlang::sym(fc_phase_var) == "Phase 5 FC"  ~ "Phase 5 FCLC",
                                                       TRUE ~ NA_character_))




}







