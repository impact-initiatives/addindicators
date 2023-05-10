# # Calculate Food Consumption Scores
#
# fcs_vars <- c("fcs_cereal", "fcs_legumes", "fcs_dairy", "fcs_meat", "fcs_veg", "fcs_fruit", "fcs_oil", "fcs_sugar")
#
# if(length(setdiff(fcs_vars, colnames(df)))==0) {
#
#   df[fcs_vars] <- sapply(df[fcs_vars], as.numeric)
#
#   cutoffs <- c("1", "2")
#   print("Now calculating Food Consumption Score (FCS) indicator:")
#   a <- readline(cat(paste0("What thresholds do you want to use for Food Consumption Scores (FCS)? Please write '1' for normal cutoffs (21.5 / 35) or '2' for alternate cutoffs (28 / 42).")))
#   while(length(setdiff(a, cutoffs))==1) {
#     a <- readline(cat(paste0("Invalid input. Please select Please write '1' for normal cutoffs (21.5 / 35) or '2' for alternate cutoffs (28 / 42).", )))
#   }
#
#   cat("\014") #clears the console
#
#   df <- df %>%
#     dplyr::mutate(fcs_weight_cereal1 = ifelse(is.na(.data$fcs_cereal), NA, .data$fcs_cereal*2) ,
#                   fcs_weight_legume2 = ifelse(is.na(.data$fcs_legumes), NA, .data$fcs_legumes*3) ,
#                   fcs_weight_dairy3 = ifelse(is.na(.data$fcs_dairy), NA, .data$fcs_dairy*4) ,
#                   fcs_weight_meat4 = ifelse(is.na(.data$fcs_meat), NA, .data$fcs_meat*4),
#                   fcs_weight_veg5 = ifelse(is.na(.data$fcs_veg), NA, .data$fcs_veg*1),
#                   fcs_weight_fruit6 = ifelse(is.na(.data$fcs_fruit), NA, .data$fcs_fruit*1) ,
#                   fcs_weight_oil7 = ifelse(is.na(.data$fcs_oil), NA, .data$fcs_oil*0.5),
#                   fcs_weight_sugar8 = ifelse(is.na(.data$fcs_sugar), NA, .data$fcs_sugar*0.5)
#     ) %>%
#     dplyr::rowwise() %>%
#     dplyr::mutate(fcs_score = sum(.data$fcs_weight_cereal1, .data$fcs_weight_legume2, .data$fcs_weight_dairy3, .data$fcs_weight_meat4, .data$fcs_weight_veg5, .data$fcs_weight_fruit6, .data$fcs_weight_oil7, .data$fcs_weight_sugar8, na.rm = TRUE),
#     ) %>%
#     dplyr::ungroup()
#
#   if(a == "1") {
#
#     df <- df %>%
#       dplyr::mutate(fcs_score = ifelse(is.na(.data$fcs_weight_cereal1), NA, .data$fcs_score),
#                     fcs_cat = ifelse(is.na(.data$fcs_score), NA, ifelse(.data$fcs_score < 21.5, "Poor", ifelse(.data$fcs_score <=35, "Borderline", ifelse(.data$fcs_score>35 & .data$fcs_score < 200, "Acceptable", NA)))))
#
#   } else if(a == "2") {
#
#     df <- df %>%
#       dplyr::mutate(fcs_score = ifelse(is.na(.data$fcs_weight_cereal1), NA, .data$fcs_score),
#                     fcs_cat = ifelse(is.na(.data$fcs_score), NA, ifelse(.data$fcs_score <= 28, "Poor", ifelse(.data$fcs_score <=42, "Borderline", ifelse(.data$fcs_score>42 & .data$fcs_score < 200, "Acceptable", NA)))))
#
#   }
#
#
#
# }
