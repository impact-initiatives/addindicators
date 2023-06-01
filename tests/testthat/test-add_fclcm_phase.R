testthat::test_that("test error",{
  test_df <- data.frame(lcs_cat_yes = c("None", "Stress", "Crisis", "Emergency","None","Crisis","Stress"),
                        fc_phase = c("Phase 1 FC","Phase 2 FC","Phase 3 FC","Phase 3 FC","Phase 4 FC","Phase 4 FC","Phase 5 FC"))

  testthat::expect_error(add_fclcm_phase(test_df,fc_phase_var = "fcs"),"fcs column is not found in the dataset")
  testthat::expect_error(add_fclcm_phase(test_df,lcs_cat_var = "lsci"),"lsci column is not found in the dataset")

  ## CHECK ERRO::Entry exists in dataset but not found in the function parameter.
  test_df <- data.frame(lcs_cat_yes = c("None","Stress", "Crisis", "Emergency","None","Crisis","Stress"),
                        fc_phase = c("Phase 1 FC","Phase 2 FC","Phase 3 FC","Phase 3 FC","Phase 4 FC","Phase 4 FC","Phase 5 FC"))
  testthat::expect_error(add_fclcm_phase(test_df,fc_phase_1 = NULL))


  ## CHECK WARNING::Potential missing entry in the dataset
  test_df <- data.frame(lcs_cat_yes = c("Stress", "Crisis", "Emergency","None","Crisis","Stress"),
                        fc_phase = c("Phase 2 FC","Phase 3 FC","Phase 3 FC","Phase 4 FC","Phase 4 FC","Phase 5 FC"))
  testthat::expect_warning(add_fclcm_phase(test_df))

  testthat::expect_error(add_fclcm_phase(test_df,lcs_cat_none  = "phase__1"))

  ## additional entry

  test_df <- data.frame(lcs_cat_yes = c("Nonee", "Stress", "Crisis", "Emergency","None","Crisis","Stress"),
                        fc_phase = c("Phase 1 FC","Phase 2 FC","Phase 3 FC","Phase 3 FC","Phase 4 FC","Phase 4 FC","Phase 5 FC"))

  testthat::expect_error(add_fclcm_phase(test_df))


  test_df <- data.frame(lcs_cat_yes = c("None", "Stress", "Crisis", "Emergency","None","Crisis","Stress"),
                        fc_phase = c("Phase 11 FC","Phase 2 FC","Phase 3 FC","Phase 3 FC","Phase 4 FC","Phase 4 FC","Phase 5 FC"))
  testthat::expect_error(add_fclcm_phase(test_df))


})


testthat::test_that("test expect equal",{

    test_df <- expand.grid(lcs_cat_yes = c("None", "Stress", "Crisis", "Emergency","None","Crisis","Stress"),
                           fc_phase = c("Phase 1 FC","Phase 2 FC","Phase 3 FC","Phase 3 FC","Phase 4 FC","Phase 4 FC","Phase 5 FC"))

    expected_output <- test_df %>%
      dplyr::mutate(fclcm_phase = dplyr::case_when(lcs_cat_yes == "None" & fc_phase == "Phase 1 FC" ~ "Phase 1 FCLC",
                                                   lcs_cat_yes == "None" & fc_phase == "Phase 2 FC" ~ "Phase 2 FCLC",
                                                   lcs_cat_yes == "None" & fc_phase == "Phase 3 FC" ~ "Phase 3 FCLC",
                                                   lcs_cat_yes == "None" & fc_phase == "Phase 4 FC" ~ "Phase 4 FCLC",
                                                   lcs_cat_yes == "None" & fc_phase == "Phase 5 FC" ~ "Phase 5 FCLC",
                                                   lcs_cat_yes == "Stress" & fc_phase == "Phase 1 FC" ~ "Phase 1 FCLC",
                                                   lcs_cat_yes == "Stress" & fc_phase == "Phase 2 FC" ~ "Phase 2 FCLC",
                                                   lcs_cat_yes == "Stress" & fc_phase == "Phase 3 FC" ~ "Phase 3 FCLC",
                                                   lcs_cat_yes == "Stress" & fc_phase == "Phase 4 FC" ~ "Phase 2 FCLC",
                                                   lcs_cat_yes == "Stress" & fc_phase == "Phase 5 FC" ~ "Phase 5 FCLC",
                                                   lcs_cat_yes == "Crisis" & fc_phase == "Phase 1 FC" ~ "Phase 2 FCLC",
                                                   lcs_cat_yes == "Crisis" & fc_phase == "Phase 2 FC" ~ "Phase 3 FCLC",
                                                   lcs_cat_yes == "Crisis" & fc_phase == "Phase 3 FC" ~ "Phase 3 FCLC",
                                                   lcs_cat_yes == "Crisis" & fc_phase == "Phase 4 FC" ~ "Phase 4 FCLC",
                                                   lcs_cat_yes == "Crisis" & fc_phase == "Phase 5 FC" ~ "Phase 5 FCLC",
                                                   lcs_cat_yes == "Emergency" & fc_phase == "Phase 1 FC" ~ "Phase 3 FCLC",
                                                   lcs_cat_yes == "Emergency" & fc_phase == "Phase 2 FC" ~ "Phase 3 FCLC",
                                                   lcs_cat_yes == "Emergency" & fc_phase == "Phase 3 FC" ~ "Phase 4 FCLC",
                                                   lcs_cat_yes == "Emergency" & fc_phase == "Phase 4 FC" ~ "Phase 4 FCLC",
                                                   lcs_cat_yes == "Emergency" & fc_phase == "Phase 5 FC" ~ "Phase 5 FCLC",
                                                   T~NA_character_))

    actual_df <- test_df |> add_fclcm_phase()
    testthat::expect_equal(actual_df,expected_output)

    #with renamed columns
    renamed_test_df <- test_df |>
      dplyr::rename(LCS_cat_yes =lcs_cat_yes,
                    FC_phase = fc_phase)

    renamed_expected_output <- expected_output %>%
      dplyr::rename(LCS_cat_yes =lcs_cat_yes,
                    FC_phase = fc_phase,
                    FCLCM_phase = fclcm_phase)
    renamed_actual_df <- renamed_test_df |> add_fclcm_phase(fc_phase_var = "FC_phase",
                                                            lcs_cat_var = "LCS_cat_yes",
                                                            fclcm_phase_var = "FCLCM_phase")
    testthat::expect_equal(renamed_actual_df,renamed_expected_output)


    #with lower values
    lower_values_test_df <- test_df |>
      dplyr::mutate(lcs_cat_yes = tolower(lcs_cat_yes),
                    fc_phase = tolower(fc_phase))

    lower_values_expected_output <- expected_output %>%
      dplyr::mutate(lcs_cat_yes = tolower(lcs_cat_yes),
                    fc_phase = tolower(fc_phase))

    renamed_actual_df <- lower_values_test_df |> add_fclcm_phase(fc_phase_1 = "phase 1 fc",
                                                                 fc_phase_2 = "phase 2 fc",
                                                                 fc_phase_3 = "phase 3 fc",
                                                                 fc_phase_4 = "phase 4 fc",
                                                                 fc_phase_5 = "phase 5 fc",
                                                                 lcs_cat_none = "none" ,
                                                                 lcs_cat_stress = "stress",
                                                                 lcs_cat_crisis = "crisis",
                                                                 lcs_cat_emergency = "emergency")
    testthat::expect_equal(renamed_actual_df,lower_values_expected_output)

})


testthat::test_that("NA  check",{


  test_df <- data.frame(lcs_cat_yes = c("None", "Stress", "Crisis", "Emergency","None","Crisis","Stress",NA_character_),
                        fc_phase = c("Phase 1 FC","Phase 2 FC","Phase 3 FC","Phase 3 FC","Phase 4 FC","Phase 4 FC","Phase 5 FC","Phase 5 FC"))

  expected_output <- test_df %>%
    dplyr::mutate(fclcm_phase = dplyr::case_when(lcs_cat_yes == "None" & fc_phase == "Phase 1 FC" ~ "Phase 1 FCLC",
                                                 lcs_cat_yes == "Stress" & fc_phase == "Phase 2 FC" ~ "Phase 2 FCLC",
                                                 lcs_cat_yes == "Crisis" & fc_phase == "Phase 3 FC" ~ "Phase 3 FCLC",
                                                 lcs_cat_yes == "Emergency" & fc_phase == "Phase 3 FC" ~ "Phase 4 FCLC",
                                                 lcs_cat_yes == "None" & fc_phase == "Phase 4 FC" ~ "Phase 4 FCLC",
                                                 lcs_cat_yes == "Crisis" & fc_phase == "Phase 4 FC" ~ "Phase 4 FCLC",
                                                 lcs_cat_yes == "Stress" & fc_phase == "Phase 5 FC" ~ "Phase 5 FCLC",
                                                 T~NA_character_))

  testthat::expect_warning(add_fclcm_phase(test_df))
  actual_df <- test_df |> add_fclcm_phase()
  testthat::expect_equal(actual_df,expected_output)



  test_df <- data.frame(lcs_cat_yes = c("None", "Stress", "Crisis", "Emergency","None","Crisis","Stress"),
  fc_phase = c("Phase 1 FC","Phase 2 FC","Phase 3 FC","Phase 3 FC","Phase 4 FC","Phase 4 FC",NA_character_))

expected_output <- test_df %>%
  dplyr::mutate(fclcm_phase = dplyr::case_when(lcs_cat_yes == "None" & fc_phase == "Phase 1 FC" ~ "Phase 1 FCLC",
                                               lcs_cat_yes == "Stress" & fc_phase == "Phase 2 FC" ~ "Phase 2 FCLC",
                                               lcs_cat_yes == "Crisis" & fc_phase == "Phase 3 FC" ~ "Phase 3 FCLC",
                                               lcs_cat_yes == "Emergency" & fc_phase == "Phase 3 FC" ~ "Phase 4 FCLC",
                                               lcs_cat_yes == "None" & fc_phase == "Phase 4 FC" ~ "Phase 4 FCLC",
                                               lcs_cat_yes == "Crisis" & fc_phase == "Phase 4 FC" ~ "Phase 4 FCLC",
                                               lcs_cat_yes == "Stress" & fc_phase == "Phase 5 FC" ~ "Phase 5 FCLC",
                                               T~NA_character_))

testthat::expect_warning(test_df |> add_fclcm_phase())

actual_df <- test_df |> add_fclcm_phase()

testthat::expect_equal(actual_df,expected_output)

})




