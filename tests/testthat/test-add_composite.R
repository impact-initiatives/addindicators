library(testthat)
library(dplyr)

# testing fcs-----------------------------------------------------------------



testthat::test_that("Check make_fcs", {

  to_remove <- c("e7da37c0-dd23-4d38-8cac-2e8e8a243b57", "f6b056c8-bda7-43d2-a723-582dc8001265",
                 "c8a00acf-1626-42f8-8c8f-592aa3c9e688", "911204db-f11d-451b-9cd4-44cb76635611",
                 "3f6ce5f7-5afe-4bd9-91ab-03c1b81a70ed", "cc002ebb-a3e5-4b6b-81ae-cbd9c9b047e8",
                 "11fa9057-aa4d-420d-aaa1-339527fc8aed", "3c9eb0b9-db04-4885-99b5-fbc044ee9b73",
                 "16afddb7-549f-457f-910e-455c18562239", "8bcdde7c-cc6c-4824-8b60-69ef602d5c91",
                 "2e821ae6-b745-4393-8495-2a8354dfcd97", "da845c42-31db-41f5-9878-ef796063cc20",
                 "4606c639-b992-4aed-9c1a-96700b640314", "ab71c041-8674-404a-b4af-a925a9237005",
                 "7d1fc4ca-c255-47cf-87b5-a7da554ab6d8", "71d5fc51-80f6-4175-a081-c0638d13e0c2",
                 "7c5cfb5f-85a6-4bdd-bf39-0df137888a7b", "69e9fb41-9e86-41e1-b367-58bf9102144d",
                 "3302b532-579c-48c7-9a0a-b86f93a465e2", "98ad30ba-4345-4db4-a84f-9599db2b3f87",
                 "7f07cdd0-93d7-4786-b7ec-4a5ed3a24277", "f63890e5-e9d8-481d-9b43-72e89cb939cc",
                 "1d3a4b90-50d2-4387-a008-37ac27557ff0", "f8ee3c88-8ca7-427e-8b42-2451a479b421",
                 "1b672a17-8af6-465c-8e6d-18a8f9fe6249", "e0f120bc-e1fc-43be-8974-c73df396e0a2",
                 "2205bcb1-fbe6-4fed-b31a-ef8a76ee21bf", "6223f56d-5b1c-4da1-a11d-3c75d0130628",
                 "74adbd31-c8dd-420f-982a-f89eba8f175b", "cd143b33-557f-4d9b-8cb0-cae9e730ffa6",
                 "9c087f89-197b-4263-a486-78e93c9578d3", "e8bb24ff-f43b-4321-8945-d76926cd3031",
                 "88f020b0-c8bb-444d-8298-d3e09dd0c66f", "c8ce6878-400b-48ad-800e-d04f6de2c242",
                 "af1e8d3f-b11b-4105-b377-78fc483aeb73", "88e1cf89-4cd5-4bb6-9fbe-628b65ed7743",
                 "3d6c766d-d623-4a7f-8d66-27a38d292853", "d3974907-2189-46de-a2cb-0d6ee0741f13",
                 "d3c6b260-1a18-4a80-afc0-9f00238100f1", "1a7f96e0-1a9a-460c-a4ba-746b3c691ec5",
                 "03b88f68-7273-4be3-b5a9-15c11af49649", "92b31c05-00d9-4677-acb7-d2b428fa5a12",
                 "46111832-5aef-4e05-b66c-f8f9605319a9", "087dbc9c-9d16-4517-9c03-b220627e8ec7",
                 "17beeb49-06fd-4b81-b51c-a67a1bef859a", "d3d2c94a-da24-4de5-9bd8-e08c66511ef4",
                 "88dcf69a-cee7-4975-804d-f2f6d81f892e", "c002a1c3-56ec-4535-9c3e-d19b9917686d",
                 "bfd5c01b-d226-49f0-a566-602c3183ad75", "6b5562af-c4d2-43bb-b0e3-9d36379b733e",
                 "088aa931-0205-4cb7-8d98-3e3302275a07", "9b496229-b1d2-4fcc-aa8e-175ad1edd822",
                 "dc077fc6-1010-4469-b512-b5a96fe3462f", "445ef8b2-1ece-4b54-a47f-77d190846c8c",
                 "24a41b39-7789-4c14-ae54-2f5833815343", "05bd9622-b58e-43b9-b648-f4d3285fe542",
                 "9e481398-bbc3-4933-aa59-a1154673d1bf", "cba69481-7812-402f-a254-4ce125adc242",
                 "66e7338f-a112-49bc-b741-93b9fcd33a1b", "f6d8fa0d-4f06-4f0e-8dfa-49ec48996a9c",
                 "de291262-1a03-4593-8884-1dc8b44f5905", "57e32f85-c979-4af2-9724-1c351760fa5b",
                 "dba0baab-6577-4945-a5f5-f964371a572e", "1c77157d-ec93-4faa-8697-5a8a401f31a3",
                 "0563c9f8-7904-45cd-b43a-146f590faa24", "2cf8b752-1651-4e9b-9c22-642647deecf5",
                 "ad22818c-63eb-49ec-a252-28d6120d2cff", "204e798a-e402-492b-bace-de0014f3cb43",
                 "c1ffb832-62df-43b4-bab5-cf513a26a29e", "41a7175a-33f9-4f51-a2e4-bdbda706cb69",
                 "098f8b29-3352-4980-ad38-00499544f7a3", "35176b8f-2758-4f60-8241-cc4816bb18cf",
                 "abbffe44-a4fe-43f1-a301-ea110d02c4bd", "993be6b1-58e3-4112-beb8-26aa3adc447a",
                 "291552e0-8829-4e4e-938b-3c061869e6a2", "a7cd1b08-0b4f-47d8-8854-ef811149d82d",
                 "b25225c6-78fe-4eb7-8e77-983e18ec2275", "af0bd06e-9fb3-4f6b-bbcc-ace1762003b2",
                 "df098e46-91c0-496f-a1d5-1fa51a8e95fa", "00fc2c74-df70-410a-91cd-2233e0908740",
                 "1d65d59e-589e-4deb-b6fd-c407327d1e53", "f26f42db-70c9-4e08-a778-b641d72336ad",
                 "0315c27a-3090-4d5d-8ee2-1e2eaf699b62", "39d131b2-34e9-4b97-bc06-26b35b6c2e6b",
                 "2669c6d6-df77-40d4-af94-a183969da47d", "47867974-b1df-4fb5-b7ad-7722ed9f78be",
                 "1cefe7ea-ada1-448e-beae-4b7b48d105fa", "f1a539f6-46a9-4ea4-957e-fdfe57f1c3a0",
                 "5ea0b2f8-5633-4680-84e5-46f15f6baa7d", "15d47989-10af-44b9-a866-57b852b45f0e",
                 "d1e31966-eaef-4e5a-ab5e-a2426c1464d4", "8870419f-59f9-4bad-8fc4-8c006d10fd64",
                 "74a37116-9641-4050-986c-98f4a02339a0", "05b83883-05e3-43c3-b8a8-f5786d346585",
                 "23265c93-8097-42e5-830b-68ccb9ff4935", "e388e8fc-8f72-457d-9721-36cbcaa8c6fa",
                 "3bdeda74-c157-4c77-b8ba-527751996ebf", "c4ef9ba0-fd47-4d63-b0fe-dfb475f3f12a",
                 "e5412777-966d-44e4-8263-284bf368f411", "ce363944-7005-4036-abf3-58e3d728e020",
                 "df917245-ab90-4a65-9101-0811f86958ce", "e464ca0e-dbb9-4e35-af3d-1fae8ee9a974",
                 "d192013f-8cb4-4594-bcd0-e5030d28d267", "02565481-23bb-4c3a-ab23-9ec97537f84f",
                 "26414463-83d9-4016-a0ad-90349b4a2751")
  df_issue_removed <- food_consumption_df %>% filter(!X_uuid %in% to_remove ) ### getting rid of potential issue


  res <- evaluate_promise(add_fcs(
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


  res_unclean <- evaluate_promise(add_fcs(
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
