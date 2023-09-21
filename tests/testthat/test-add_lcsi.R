testthat::test_that("testing add_lcsi", {
  input_data1 <- data.frame(
    stress1 = c("No", "No", "Exhausted", "Not Applicable", "No"),
    stress2 = c("No", "Yes", "Not Applicable", "No", "No"),
    stress3 = c("Not Applicable", "Not Applicable", "Yes", "No", "No"),
    stress4 = c("Not Applicable", "No", "Yes", "Yes", "No"),
    crisis1 = c("No", "Not Applicable", "Yes", "Exhausted", "No"),
    crisis2 = c("No", "No", "No", "No", "No"),
    crisis3 = c("No", "No", "Yes", "Not Applicable", "No"),
    emergency1 = c("No", "Not Applicable", "Not Applicable", "No", "No"),
    emergency2 = c("No", "Not Applicable", "Yes", "Not Applicable", "No"),
    emergency3 = c("Not Applicable", "No", "Not Applicable", "No", "Exhausted")
  )

  output_data1 <- data.frame(
    stress1 = c("No", "No", "Exhausted", "Not Applicable", "No"),
    stress2 = c("No", "Yes", "Not Applicable", "No", "No"),
    stress3 = c("Not Applicable", "Not Applicable", "Yes", "No", "No"),
    stress4 = c("Not Applicable", "No", "Yes", "Yes", "No"),
    crisis1 = c("No", "Not Applicable", "Yes", "Exhausted", "No"),
    crisis2 = c("No", "No", "No", "No", "No"),
    crisis3 = c("No", "No", "Yes", "Not Applicable", "No"),
    emergency1 = c("No", "Not Applicable", "Not Applicable", "No", "No"),
    emergency2 = c("No", "Not Applicable", "Yes", "Not Applicable", "No"),
    emergency3 = c("Not Applicable", "No", "Not Applicable", "No", "Exhausted"),
    lcsi_stress1 = c("no_had_no_need", "no_had_no_need", "no_exhausted", "not_applicable", "no_had_no_need"),
    lcsi_stress2 = c("no_had_no_need", "yes", "not_applicable", "no_had_no_need", "no_had_no_need"),
    lcsi_stress3 = c("not_applicable", "not_applicable", "yes", "no_had_no_need", "no_had_no_need"),
    lcsi_stress4 = c("not_applicable", "no_had_no_need", "yes", "yes", "no_had_no_need"),
    lcsi_crisis1 = c("no_had_no_need", "not_applicable", "yes", "no_exhausted", "no_had_no_need"),
    lcsi_crisis2 = c("no_had_no_need", "no_had_no_need", "no_had_no_need", "no_had_no_need", "no_had_no_need"),
    lcsi_crisis3 = c("no_had_no_need", "no_had_no_need", "yes", "not_applicable", "no_had_no_need"),
    lcsi_emergency1 = c("no_had_no_need", "not_applicable", "not_applicable", "no_had_no_need", "no_had_no_need"),
    lcsi_emergency2 = c("no_had_no_need", "not_applicable", "yes", "not_applicable", "no_had_no_need"),
    lcsi_emergency3 = c("not_applicable", "no_had_no_need", "not_applicable", "no_had_no_need", "no_exhausted"),
    lcsi_stress_yes = c("0", "1", "1", "1", "0"),
    lcsi_stress_exhaust = c("0", "0", "1", "0", "0"),
    lcsi_stress = c("0", "1", "1", "1", "0"),
    lcsi_crisis_yes = c("0", "0", "1", "0", "0"),
    lcsi_crisis_exhaust = c("0", "0", "0", "1", "0"),
    lcsi_crisis = c("0", "0", "1", "1", "0"),
    lcsi_emergency_yes = c("0", "0", "1", "0", "0"),
    lcsi_emergency_exhaust = c("0", "0", "0", "0", "1"),
    lcsi_emergency = c("0", "0", "1", "0", "1"),
    lcsi_cat_yes = c("None", "Stress", "Emergency", "Stress", "None"),
    lcsi_cat_exhaust = c("None", "None", "Stress", "Crisis", "Emergency"),
    lcsi_cat = c("None", "Stress", "Emergency", "Crisis", "Emergency")
  )

  input_data2 <- data.frame(
    stress1 = c("No", "No", "Exhausted", "Not Applicable", "No"),
    stress2 = c("No", "No", "Not Applicable", "No", "No"),
    stress3 = c("Not Applicable", "Not Applicable", "No", "No", "No"),
    stress4 = c("Not Applicable", "No", "No", "No", "No"),
    crisis1 = c("No", "Not Applicable", "No", "Exhausted", "No"),
    crisis2 = c("No", "No", "No", "No", "No"),
    crisis3 = c("No", "No", "No", "Not Applicable", "No"),
    emergency1 = c("No", "Not Applicable", "Not Applicable", "No", "No"),
    emergency2 = c("No", "Not Applicable", "No", "Not Applicable", "No"),
    emergency3 = c("Not Applicable", "No", "Not Applicable", "No", "Exhausted")
  )

  input_data3 <- data.frame(
    stress1 = c("WRONG", "No", "Exhausted", "Not Applicable", "No"),
    stress2 = c("No", "Yes", "Not Applicable", "No", "No"),
    stress3 = c("Not Applicable", "Not Applicable", "Yes", "No", "No"),
    stress4 = c("Not Applicable", "No", "Yes", "Yes", "No"),
    crisis1 = c("No", "Not Applicable", "Yes", "Exhausted", "No"),
    crisis2 = c("No", "No", "No", "No", "No"),
    crisis3 = c("No", "No", "Yes", "Not Applicable", "No"),
    emergency1 = c("No", "Not Applicable", "Not Applicable", "No", "No"),
    emergency2 = c("No", "Not Applicable", "Yes", "Not Applicable", "No"),
    emergency3 = c("Not Applicable", "No", "Not Applicable", "No", "Exhausted")
  )

  # Test 1 (happy path) - Correct classification with a given input

  expect_equal(
    add_lcsi(
      .dataset = input_data1,
      lcsi_stress_vars = c("stress1", "stress2", "stress3", "stress4"),
      lcsi_crisis_vars = c("crisis1", "crisis2", "crisis3"),
      lcsi_emergency_vars = c("emergency1", "emergency2", "emergency3"),
      yes_val = "Yes",
      no_val = "No",
      exhausted_val = "Exhausted",
      not_applicable_val = "Not Applicable"
    ),
    output_data1
  )

  # Test 2 - (happy path) - Correct number of columns and rows are returned.

  expect_equal(
    dim(add_lcsi(
      .dataset = input_data1,
      lcsi_stress_vars = c("stress1", "stress2", "stress3", "stress4"),
      lcsi_crisis_vars = c("crisis1", "crisis2", "crisis3"),
      lcsi_emergency_vars = c("emergency1", "emergency2", "emergency3"),
      yes_val = "Yes",
      no_val = "No",
      exhausted_val = "Exhausted",
      not_applicable_val = "Not Applicable"
    )),
    dim(output_data1)
  )

  # Test 3 (Sad path) - Error when inappropriate number of inputs are given

  testthat::expect_error(add_lcsi(
    .dataset = input_data1,
    lcsi_stress_vars = c("stress1", "stress2", "stress3", "stress4", "stress5"),
    lcsi_crisis_vars = c("crisis1", "crisis2", "crisis3"),
    lcsi_emergency_vars = c("emergency1", "emergency2", "emergency3"),
    yes_val = "Yes",
    no_val = "No",
    exhausted_val = "Exhausted",
    not_applicable_val = "Not Applicable"
  ))

  testthat::expect_error(add_lcsi(
    .dataset = input_data1,
    lcsi_stress_vars = c("stress1", "stress2", "stress3", "stress4"),
    lcsi_crisis_vars = c("crisis1", "crisis2", "crisis3", "crisis4"),
    lcsi_emergency_vars = c("emergency1", "emergency2", "emergency3"),
    yes_val = "Yes",
    no_val = "No",
    exhausted_val = "Exhausted",
    not_applicable_val = "Not Applicable"
  ))

  testthat::expect_error(add_lcsi(
    .dataset = input_data1,
    lcsi_stress_vars = c("stress1", "stress2", "stress3", "stress4"),
    lcsi_crisis_vars = c("crisis1", "crisis2", "crisis3"),
    lcsi_emergency_vars = c("emergency1", "emergency2", "emergency3", "emergency4"),
    yes_val = "Yes",
    no_val = "No",
    exhausted_val = "Exhausted",
    not_applicable_val = "Not Applicable"
  ))

  # Test 4 (Sad Path) - Warning if <4 unique values are seen in the values, and at least one expected value is not seen in the dataset
  # Test 4.1 - unobserved value in yes_val
  testthat::expect_warning(add_lcsi(
    .dataset = input_data2,
    lcsi_stress_vars = c("stress1", "stress2", "stress3", "stress4"),
    lcsi_crisis_vars = c("crisis1", "crisis2", "crisis3"),
    lcsi_emergency_vars = c("emergency1", "emergency2", "emergency3"),
    yes_val = "VALUE DOESNT EXIST",
    no_val = "No",
    exhausted_val = "Exhausted",
    not_applicable_val = "Not Applicable"
  ))
  # Test 4.2 - unobserved value in no_val
  testthat::expect_warning(add_lcsi(
    .dataset = input_data2,
    lcsi_stress_vars = c("stress1", "stress2", "stress3", "stress4"),
    lcsi_crisis_vars = c("crisis1", "crisis2", "crisis3"),
    lcsi_emergency_vars = c("emergency1", "emergency2", "emergency3"),
    yes_val = "Yes",
    no_val = "VALUE DOESNT EXIST",
    exhausted_val = "Exhausted",
    not_applicable_val = "Not Applicable"
  ))

  # Test 4.3 - unobserved value in exhausted_val
  testthat::expect_warning(add_lcsi(
    .dataset = input_data2,
    lcsi_stress_vars = c("stress1", "stress2", "stress3", "stress4"),
    lcsi_crisis_vars = c("crisis1", "crisis2", "crisis3"),
    lcsi_emergency_vars = c("emergency1", "emergency2", "emergency3"),
    yes_val = "Yes",
    no_val = "No",
    exhausted_val = "VALUE DOESNT EXIST",
    not_applicable_val = "Not Applicable"
  ))

  # Test 4.4 - unobserved value in not_applicable_val
  testthat::expect_warning(add_lcsi(
    .dataset = input_data2,
    lcsi_stress_vars = c("stress1", "stress2", "stress3", "stress4"),
    lcsi_crisis_vars = c("crisis1", "crisis2", "crisis3"),
    lcsi_emergency_vars = c("emergency1", "emergency2", "emergency3"),
    yes_val = "Yes",
    no_val = "No",
    exhausted_val = "Exhausted",
    not_applicable_val = "VALUE DOESNT EXIST"
  ))

  # Test 5 (Sad Path) - Error if there are 4 Unique values, and 1 of them is not of the 4 values g
  testthat::expect_error(add_lcsi(
    .dataset = input_data1,
    lcsi_stress_vars = c("stress1", "stress2", "stress3", "stress4"),
    lcsi_crisis_vars = c("crisis1", "crisis2", "crisis3"),
    lcsi_emergency_vars = c("emergency1", "emergency2", "emergency3"),
    yes_val = "Yes",
    no_val = "No",
    exhausted_val = "Exhausted",
    not_applicable_val = "VALUE DOESNT EXIST"
  ))

  # Test 6 (Sad Path) - Error if there are > 4 unique values seen in the lcs variables.

  testthat::expect_error(add_lcsi(
    .dataset = input_data3,
    lcsi_stress_vars = c("stress1", "stress2", "stress3", "stress4"),
    lcsi_crisis_vars = c("crisis1", "crisis2", "crisis3"),
    lcsi_emergency_vars = c("emergency1", "emergency2", "emergency3"),
    yes_val = "Yes",
    no_val = "No",
    exhausted_val = "Exhausted",
    not_applicable_val = "Not Applicable"
  ))
})

test_that("NA are handle correctly", {
  #NA will return NA
  test_data <- data.frame(
    uuid = letters[1:2],
    lcsi_option1 = c(NA, "yes"),
    lcsi_option2 = c("yes", "yes"),
    lcsi_option3 = c("yes", "yes"),
    lcsi_option4 = c("yes", "yes"),
    lcsi_option5 = c("yes", "yes"),
    lcsi_option6 = c("yes", "yes"),
    lcsi_option7 = c("yes", "yes"),
    lcsi_option8 = c("yes", "yes"),
    lcsi_option9 = c("yes", "yes"),
    lcsi_option10 = c("yes", "yes")
  )

  expected_output <- test_data %>%
    dplyr::mutate(lcsi_stress1 = lcsi_option1,
                  lcsi_stress2 = lcsi_option2,
                  lcsi_stress3 = lcsi_option3,
                  lcsi_stress4 = lcsi_option4,
                  lcsi_crisis1 = lcsi_option5,
                  lcsi_crisis2 = lcsi_option6,
                  lcsi_crisis3 = lcsi_option7,
                  lcsi_emergency1 = lcsi_option8,
                  lcsi_emergency2 = lcsi_option9,
                  lcsi_emergency3 = lcsi_option10,
                  lcsi_stress_yes = c(NA_character_, "1"),
                  lcsi_stress_exhaust = c(NA_character_, "0"),
                  lcsi_stress = c(NA_character_, "1"),
                  lcsi_crisis_yes = c(NA_character_, "1"),
                  lcsi_crisis_exhaust = c(NA_character_, "0"),
                  lcsi_crisis = c(NA_character_, "1"),
                  lcsi_emergency_yes = c(NA_character_, "1"),
                  lcsi_emergency_exhaust = c(NA_character_, "0"),
                  lcsi_emergency = c(NA_character_, "1"),
                  lcsi_cat_yes = c(NA_character_, "Emergency"),
                  lcsi_cat_exhaust = c(NA_character_, "None"),
                  lcsi_cat = c(NA_character_, "Emergency"))

  actual_output <- test_data %>% add_lcsi(
    lcsi_stress_vars = c("lcsi_option1", "lcsi_option2", "lcsi_option3", "lcsi_option4"),
    lcsi_crisis_vars = c("lcsi_option5", "lcsi_option6", "lcsi_option7"),
    lcsi_emergency_vars = c("lcsi_option8", "lcsi_option9", "lcsi_option10"),
    yes_val = "yes",
    no_val = "no",
    exhausted_val = "no_exhausted",
    not_applicable_val = "not_applicable"
  ) %>% suppressWarnings()

  expect_equal(actual_output, expected_output)


  #NA are ignored
  test_data <- data.frame(
    uuid = letters[1:5],
    lcsi_option1 = c(NA, "yes", "no_had_no_need", "no_exhausted", "not_applicable")
  )
  test_data <- test_data %>%
    dplyr::mutate(
      lcsi_option2 = c("yes", NA, "no_exhausted", "not_applicable", "yes"),
      lcsi_option3 = c("yes", "no_had_no_need", NA, "not_applicable", "yes"),
      lcsi_option4 = c("yes", "no_had_no_need", "no_exhausted", NA, "yes"),
      lcsi_option5 = c("yes", "no_had_no_need", "no_exhausted", "not_applicable", NA),
      lcsi_option6 = lcsi_option1,
      lcsi_option7 = lcsi_option1,
      lcsi_option8 = lcsi_option1,
      lcsi_option9 = lcsi_option1,
      lcsi_option10 = lcsi_option1
    )

  expected_data <- test_data %>%
    dplyr::mutate(lcsi_stress1 = lcsi_option1,
                  lcsi_stress2 = lcsi_option2,
                  lcsi_stress3 = lcsi_option3,
                  lcsi_stress4 = lcsi_option4,
                  lcsi_crisis1 = lcsi_option5,
                  lcsi_crisis2 = lcsi_option6,
                  lcsi_crisis3 = lcsi_option7,
                  lcsi_emergency1 = lcsi_option8,
                  lcsi_emergency2 = lcsi_option9,
                  lcsi_emergency3 = lcsi_option10,
                  lcsi_stress_yes = c("1", "1", "0", "0", "1"),
                  lcsi_stress_exhaust = c("0", "0", "1", "1", "0"),
                  lcsi_stress = c("1", "1", "1", "1", "1"),
                  lcsi_crisis_yes = c("1", "1", "0", "0", "0"),
                  lcsi_crisis_exhaust = c("0", "0", "1", "1", "0"),
                  lcsi_crisis = c("1", "1", "1", "1", "0"),
                  lcsi_emergency_yes = c("0", "1", "0", "0", "0"),
                  lcsi_emergency_exhaust = c("0", "0", "0", "1", "0"),
                  lcsi_emergency = c("0", "1", "0", "1", "0"),
                  lcsi_cat_yes = c("Crisis", "Emergency", "None", "None", "Stress"),
                  lcsi_cat_exhaust = c("None", "None", "Crisis", "Emergency", "None"),
                  lcsi_cat = c("Crisis", "Emergency", "Crisis", "Emergency", "Stress"))

  actual_output <- test_data %>% add_lcsi(
    lcsi_stress_vars = c("lcsi_option1", "lcsi_option2", "lcsi_option3", "lcsi_option4"),
    lcsi_crisis_vars = c("lcsi_option5", "lcsi_option6", "lcsi_option7"),
    lcsi_emergency_vars = c("lcsi_option8", "lcsi_option9", "lcsi_option10"),
    yes_val = "yes",
    no_val = "no_had_no_need",
    exhausted_val = "no_exhausted",
    not_applicable_val = "not_applicable",
    ignore_NA = TRUE
  )

  expect_equal(actual_output, expected_data)

})
