# add_fclcm

# Function to combine the food consumption score (FCS) phase classification with the livelihoods coping strategies (LCSI) classification into a matrix

### Combining the FCS phase with the LCSI classification

dataset <- datatest
add_fclcm_phase <- function(dataset,
                            fc_phase,
                            lcsi_cat,
                            lcsi_cat_exhaust){

# extracting the columns for the fclcm_phase

  fclcm_vars <- c(fc_phase, lcsi_cat, lcsi_cat_exhaust)

# checks

  ## Check for the variables fc_phase and lcsi
  if (!all(fclcm_vars %in% names(dataset))) {
    warning("Please check if the food consumption phase and lcsi classification were created")
  }

  ## Check for missing values

  if (sapply(fclcm_vars, sum(is.na())) > 0) {
    warning("Please check for missing values in food consumption and lcsi")
  }

  ## Creating the combination of FCS and LCS

  df %>% dplyr::mutate(fclcm_phase = case_when(fc_phase == "Phase 1 FC" & lcsi_cat %in% c("None", "Stress")  ~ "Phase 1 FCLC",
                                                  (fc_phase == "Phase 1 FC" & lcsi_cat == "Crisis") |
                                                    fc_phase == "Phase 2 FC" & lcsi_cat %in% c("None", "Stress") ~ "Phase 2 FCLC",
                                                  (fc_phase == "Phase 1 FC" & lcsi_cat == "Emergency") |
                                                    (fc_phase == "Phase 2 FC" & lcsi_cat %in% c("Crisis", "Emergency")) |
                                                    (fc_phase == "Phase 3 FC" & lcsi_cat %in% c("None", "Stress", "Crisis")) ~ "Phase 3 FCLC",
                                                  (fc_phase == "Phase 3 FC" & lcsi_cat == "Emergency") |
                                                    fc_phase == "Phase 4 FC" & lcsi_cat %in% c("None", "Stress", "Crisis") ~ "Phase 4 FCLC",
                                                  fc_phase == "Phase 5 FC" & lcsi_cat_exhaust %in% c("Stress", "Crisis", "Emergency") ~ "Phase 5 FCLC", TRUE ~ NA_character_))


  prop.table(table(df$fclcm_phase))

}


