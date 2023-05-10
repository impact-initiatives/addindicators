# # Calculate reduced Coping Strategies Index
#
# rcsi_vars <- c("rcsi_lesspreferred_1", "rcsi_borrowfood_2", "rcsi_limitportion_3", "rcsi_restrict_4", "rcsi_reducemeals5")
#
# if(length(setdiff(rcsi_vars, colnames(df)))==0) {
#
#   df <- df %>%
#     dplyr::mutate(rcsi_weight1 = .data$rcsi_lesspreferred_1*1,
#                   rcsi_weight2 = .data$rcsi_borrowfood_2*2,
#                   rcsi_weight3 = .data$rcsi_limitportion_3*1,
#                   rcsi_weight4 = .data$rcsi_restrict_4*3,
#                   rcsi_weight5 = .data$rcsi_reducemeals5*1) %>%
#     dplyr::rowwise() %>%
#     dplyr::mutate(rcsi_score = sum(.data$rcsi_weight1, .data$rcsi_weight2, .data$rcsi_weight3, .data$rcsi_weight4, .data$rcsi_weight5, na.rm = TRUE)) %>%
#     dplyr::ungroup() %>%
#     dplyr::mutate(rcsi_cat = ifelse(is.na(.data$rcsi_score), NA, ifelse(.data$rcsi_score <=3, "No to Low", ifelse(.data$rcsi_score>=4 & .data$rcsi_score<=18, "Medium", ifelse(.data$rcsi_score>18 & .data$rcsi_score <=1000, "High", NA)))),
#     )
#
# }
