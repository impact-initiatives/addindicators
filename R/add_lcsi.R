# # Livelihood Coping Strategies
#
# lcs_vars <- c("lcs_emergency", "lcs_crisis", "lcs_stress",
#               "lcs_stress_yes", "lcs_crisis_yes", "lcs_emergency_yes",
#               "lcs_stress_exhaust", "lcs_crisis_exhaust", "lcs_emergency_exhaust")
#
# if(length(setdiff(lcs_vars, colnames(df)))==0) {
#
#   df[lcs_vars] <- lapply(df[lcs_vars], as.numeric)
#
#   df <- df %>%
#     dplyr::mutate(lcs_cat = ifelse(.data$lcs_emergency == 1, "Emergency", ifelse(.data$lcs_crisis == 1, "Crisis", ifelse(.data$lcs_stress == 1, "Stress", ifelse(.data$lcs_stress == 0 & .data$lcs_crisis == 0 & .data$lcs_emergency == 0, "None", NA)))),
#                   lcs_cat_yes = dplyr::case_when(.data$lcs_emergency_yes == 1 ~ "Emergency", .data$lcs_crisis_yes == 1 ~ "Crisis", .data$lcs_stress_yes == 1 ~ "Stress", .data$lcs_cat == "None" ~ "None", TRUE ~ NA_character_ ),
#                   lcs_cat_exhaust = dplyr::case_when(.data$lcs_emergency_exhaust == 1 ~ "Emergency",
#                                                      .data$lcs_crisis_exhaust == 1 ~ "Crisis",
#                                                      .data$lcs_stress_exhaust == 1 ~ "Stress",
#                                                      .data$lcs_emergency_exhaust != 1 & .data$lcs_crisis_exhaust != 1 & .data$lcs_stress_exhaust != 1  ~ "None",
#                                                      TRUE ~ NA_character_ ))
#
# }
