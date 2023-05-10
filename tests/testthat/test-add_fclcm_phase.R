#
# test_that("the indicators is calculated correctly") {
#   test_df <- expand.grid(lcs_cat_yes = c("None", "Stress", "Crisis", "Emergency", "Catastrophe"),
#                          fc_phase = c("Phase 1 FC","Phase 2 FC","Phase 3 FC","Phase 4 FC","Phase 5 FC")) %>%
#     as.data.frame()
#
#   expected_output <- test_df %>%
#     dplyr::mutate(fclc_phase = dplyr::case_when(lcs_cat_yes == "None" & fc_phase == "Phase 1 FS" ~ "Phase 1 - FCLC",
#                                                 lcs_cat_yes == "Stress" & fc_phase == "Phase 1 FS" ~ "Phase 1 - FCLC"))
#
#   expect_equal(add_fclcm_phase(test_df),
#                expexted_output)
# }
#
#
# test_that("if one of the variles are not present, the function stop") {
#   test_df <- expand.grid(lcs_cat_yes = c("None", "Stress", "Crisis", "Emergency", "Catastrophe"),
#                          fc_phase = c("Phase 1 FC","Phase 2 FC","Phase 3 FC","Phase 4 FC","Phase 5 FC")) %>%
#     as.data.frame()
#
#
#   expect_error(add_fclcm_phase(test_df, lcs_cat_var = "LCSI_category", fc_phase_var = "fc_phase"),
#                "Cannot identify one of the variable")
# }
#
