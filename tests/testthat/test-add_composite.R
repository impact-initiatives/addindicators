library(testthat)
library(dplyr)

# testing fcs-----------------------------------------------------------------

testthat::test_that("Check_fcs", {
  check <- evaluate_promise(check_fcs(
    dataset = food_consumption_df,
    uuid = "X_uuid",
    cereals = "cereals_grains_roots_tubers",
    pulses = "beans_legumes_pulses_nuts",
    dairy = "milk_dairy_products",
    meat = "meat_fish_eggs",
    vegetables = "vegetables",
    fruits = "fruite",
    oil = "oil_fat_butter",
    sugar = "sugar_sugary_food"
  ))



  df_issue_removed <- food_consumption_df %>% filter(!X_uuid %in% check$result$X_uuid) ### getting rid of potential issue

  res_check <- evaluate_promise(check_fcs(
    dataset = df_issue_removed,
    uuid = "X_uuid",
    cereals = "cereals_grains_roots_tubers",
    pulses = "beans_legumes_pulses_nuts",
    dairy = "milk_dairy_products",
    meat = "meat_fish_eggs",
    vegetables = "vegetables",
    fruits = "fruite",
    oil = "oil_fat_butter",
    sugar = "sugar_sugary_food"
  ))


  expect_equal(nrow(res_check$result), 0)
})

testthat::test_that("Check make_fcs", {
  check <- evaluate_promise(check_fcs(
    dataset = food_consumption_df,
    uuid = "X_uuid",
    cereals = "cereals_grains_roots_tubers",
    pulses = "beans_legumes_pulses_nuts",
    dairy = "milk_dairy_products",
    meat = "meat_fish_eggs",
    vegetables = "vegetables",
    fruits = "fruite",
    oil = "oil_fat_butter",
    sugar = "sugar_sugary_food"
  ))



  df_issue_removed <- food_consumption_df %>% filter(!X_uuid %in% check$result$X_uuid) ### getting rid of potential issue


  res <- evaluate_promise(make_fcs(
    dataset = df_issue_removed, var_name = "fcs_test",
    cereals = "cereals_grains_roots_tubers",
    pulses = "beans_legumes_pulses_nuts",
    dairy = "milk_dairy_products",
    meat = "meat_fish_eggs",
    vegetables = "vegetables",
    fruits = "fruite",
    oil = "oil_fat_butter",
    sugar = "sugar_sugary_food"
  ))


  res_unclean <- evaluate_promise(make_fcs(
    dataset = food_consumption_df, var_name = "fcs_test",
    cereals = "cereals_grains_roots_tubers",
    pulses = "beans_legumes_pulses_nuts",
    dairy = "milk_dairy_products",
    meat = "meat_fish_eggs",
    vegetables = "vegetables",
    fruits = "fruite",
    oil = "oil_fat_butter",
    sugar = "sugar_sugary_food"
  ))


  testthat::expect_identical(
    res_unclean$warnings,
    "Potential issue:: There are 105 observations where all the variables of food consumption score are the same."
  )

  testthat::expect_equal(food_consumption_df |> ncol() + 2, ncol(res$result))
})


# test HHS ----------------------------------------------------------------

testthat::test_that("add_hhs", {
  add_result <- evaluate_promise(
    add_hhs(
      dataset = food_consumption_df, new_colname = "hhs",
      hhs1a = "no_food_to_eat_because_of_lack_of_resources",
      hhs1b = "how_often_no_food_to_eat_because_of_lack_of_resources",
      hhs2a = "hh_memeber_sleep_hungry",
      hhs2b = "how_often_hh_memeber_sleep_hungry",
      hhs3a = "hh_member_go_a_whole_day_and_night_without_eating",
      hhs3b = "how_often_hh_member_go_a_whole_day_and_night_without_eating",
      frequency_choice = c("rarely_1_2_times", "sometimes_3_10_times", "often_10_plus_times")
    )
  )

  expect_true("hhs_cat" %in% names(add_result$result))
  expect_true("hhs_score" %in% names(add_result$result))
  expect_error(add_hhs(
    dataset = food_consumption_df, new_colname = "hhs",
    hhs1a = "no_food_to_eat_because_of_lack_of_resourc",
    hhs1b = "how_often_no_food_to_eat_because_of_lack_of_resources",
    hhs2a = "hh_memeber_sleep_hungry",
    hhs2b = "how_often_hh_memeber_sleep_hungry",
    hhs3a = "hh_member_go_a_whole_day_and_night_without_eating",
    hhs3b = "how_often_hh_member_go_a_whole_day_and_night_without_eating",
    frequency_choice = c("rarely_1_2_times", "sometimes_3_10_times", "often_10_plus_times")
  ))
})
