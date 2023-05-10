#
# add_fclcm_phase <- function(.dataset, lcs_cat_var = "lcs_cat_yes", fc_phase_var = "fc_phase") {
#
# }
#
# # Calculating FEWSNET Food Consumption-Livelihood Coping Matrix
#
# fews_lcs_vars <- c("fcs_cat", "rcsi_cat", "hhs_cat", "lcs_cat")
#
# if(length(setdiff(fews_lcs_vars, colnames(df)))==0) {
#
#   df <- df %>% dplyr::mutate(fclc_phase = ifelse(.data$fc_phase == "Phase 1 FC" & .data$lcs_cat_yes == "None", "Phase 1 - FCLC",
#                                                  ifelse(.data$fc_phase == "Phase 1 FC" & .data$lcs_cat_yes == "Stress", "Phase 1 - FCLC",
#                                                         ifelse(.data$fc_phase == "Phase 1 FC" & .data$lcs_cat_yes == "Crisis", "Phase 2 - FCLC",
#                                                                ifelse(.data$fc_phase == "Phase 1 FC" & .data$lcs_cat_yes == "Emergency", "Phase 3 - FCLC",
#                                                                       ifelse(.data$fc_phase == "Phase 2 FC" & .data$lcs_cat_yes == "None", "Phase 2 - FCLC",
#                                                                              ifelse(.data$fc_phase == "Phase 2 FC" & .data$lcs_cat_yes == "Stress", "Phase 2 - FCLC",
#                                                                                     ifelse(.data$fc_phase == "Phase 2 FC" & .data$lcs_cat_yes == "Crisis", "Phase 3 - FCLC",
#                                                                                            ifelse(.data$fc_phase == "Phase 2 FC" & .data$lcs_cat_yes == "Emergency", "Phase 3 - FCLC",
#                                                                                                   ifelse(.data$fc_phase == "Phase 3 FC" & .data$lcs_cat_yes == "None", "Phase 3 - FCLC",
#                                                                                                          ifelse(.data$fc_phase == "Phase 3 FC" & .data$lcs_cat_yes == "Stress", "Phase 3 - FCLC",
#                                                                                                                 ifelse(.data$fc_phase == "Phase 3 FC" & .data$lcs_cat_yes == "Crisis", "Phase 3 - FCLC",
#                                                                                                                        ifelse(.data$fc_phase == "Phase 3 FC" & .data$lcs_cat_yes == "Emergency", "Phase 4 - FCLC",
#                                                                                                                               ifelse(.data$fc_phase == "Phase 4 FC" & .data$lcs_cat_yes == "None", "Phase 4 - FCLC",
#                                                                                                                                      ifelse(.data$fc_phase == "Phase 4 FC" & .data$lcs_cat_yes == "Stress", "Phase 4 - FCLC",
#                                                                                                                                             ifelse(.data$fc_phase == "Phase 4 FC" & .data$lcs_cat_yes == "Crisis", "Phase 4 - FCLC",
#                                                                                                                                                    ifelse(.data$fc_phase == "Phase 4 FC" & .data$lcs_cat_yes == "Emergency", "Phase 4 - FCLC",
#                                                                                                                                                           ifelse(.data$fc_phase == "Phase 5 FC", "Phase 5 - FCLC", NA))))))))))))))))))
#
# }
