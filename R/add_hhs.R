# # Calculate Household Hunger Score
#
# hhs_vars <- c("hhs_nofoodhh_1", "hhs_nofoodhh_1a", "hhs_sleephungry_2", "hhs_sleephungry_2a", "hhs_alldaynight_3", "hhs_alldaynight_3a")
#
# if(length(setdiff(hhs_vars, colnames(df)))==0) {
#
#   df <- df %>%
#     dplyr::mutate(hhs_comp1 = ifelse(is.na(.data$hhs_nofoodhh_1), NA, ifelse(.data$hhs_nofoodhh_1 == "2", 0, ifelse(.data$hhs_nofoodhh_1 == "1" & (.data$hhs_nofoodhh_1a == "2" | .data$hhs_nofoodhh_1a == "1"), 1, ifelse(.data$hhs_nofoodhh_1 == "1" & .data$hhs_nofoodhh_1a == 3, 2, 0)))),
#                   hhs_comp2 = ifelse(is.na(.data$hhs_sleephungry_2), NA, ifelse(.data$hhs_sleephungry_2 == "2", 0, ifelse(.data$hhs_sleephungry_2 == "1" & (.data$hhs_sleephungry_2a == "2" | .data$hhs_sleephungry_2a == "1"), 1, ifelse(.data$hhs_sleephungry_2 == "1" & .data$hhs_sleephungry_2a == "3", 2, 0)))),
#                   hhs_comp3 = ifelse(is.na(.data$hhs_alldaynight_3), NA, ifelse(.data$hhs_alldaynight_3 == "2", 0, ifelse(.data$hhs_alldaynight_3 == "1" & (.data$hhs_alldaynight_3a == "2" | .data$hhs_alldaynight_3a == "1"), 1, ifelse(.data$hhs_alldaynight_3 == "1" & .data$hhs_alldaynight_3a == "3", 2, 0))))
#     ) %>% dplyr::rowwise() %>%
#     dplyr::mutate(hhs_score = sum(.data$hhs_comp1, .data$hhs_comp2, .data$hhs_comp3, na.rm = TRUE)) %>%
#     dplyr::ungroup() %>%
#     dplyr::mutate(hhs_cat_ipc = dplyr::case_when(.data$hhs_score == 0 ~ "None",
#                                                  .data$hhs_score == 1 ~ "Little",
#                                                  .data$hhs_score == 2 | .data$hhs_score == 3 ~ "Moderate",
#                                                  .data$hhs_score == 4 ~ "Severe",
#                                                  .data$hhs_score == 5 | .data$hhs_score == 6 ~ "Very Severe",
#                                                  TRUE ~ NA_character_),
#                   hhs_cat = dplyr::case_when(.data$hhs_score == 0 | .data$hhs_score == 1 ~ "No or Little",
#                                              .data$hhs_score == 2 | .data$hhs_score == 3 ~ "Moderate",
#                                              .data$hhs_score == 4 | .data$hhs_score == 5 | .data$hhs_score == 6 ~ "Severe",
#                                              TRUE ~ NA_character_))
#
# }
