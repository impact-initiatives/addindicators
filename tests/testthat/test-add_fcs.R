#test1 - ok!
test_that("calculations of fcs are correct", {
  df1 <- data.frame(cereal = c(1,2,3,2,5,6,7),
                    pulses = c(3,4,5,6,1,6,5),
                    vegetables = c(3,2,1,6,5,4,3),
                    fruits = c(1,4,6,2,2,2,4),
                    meat = c(5,4,3,2,7,4,5),
                    milk = c(1,2,6,7,3,4,2),
                    sugar = c(1,7,6,5,2,3,4),
                    oil = c(2,3,6,5,1,7,4)
  )
  result <- add_fcs(df1, fcs_cereal="cereal", fcs_legumes="pulses", fcs_veg="vegetables",
                    fcs_fruit = "fruits", fcs_meat = "meat", fcs_dairy="milk", fcs_sugar="sugar", fcs_oil="oil") %>%
    dplyr::select(-starts_with("fcs_weight"), -fcs_cat)
  expected_result <- data.frame(cereal = c(1,2,3,2,5,6,7),
                         pulses = c(3,4,5,6,1,6,5),
                         vegetables = c(3,2,1,6,5,4,3),
                         fruits = c(1,4,6,2,2,2,4),
                         meat = c(5,4,3,2,7,4,5),
                         milk = c(1,2,6,7,3,4,2),
                         sugar = c(1,7,6,5,2,3,4),
                         oil = c(2,3,6,5,1,7,4),
                         fcs_score = c(40.5, 51.0, 70.0, 71.0, 61.5, 73.0, 68.0))
  expect_equal(result, expected_result)
})



#test 2 - ok
test_that("doesn compute FCS when a value is NA", {
  df1 <- data.frame(cereal = c(NA,2,3,2,5,6,7),
                    pulses = c(3,4,5,6,1,6,5),
                    vegetables = c(3,2,1,6,5,4,3),
                    fruits = c(1,4,6,2,2,2,4),
                    meat = c(5,4,3,2,7,4,5),
                    milk = c(1,2,6,7,3,4,2),
                    sugar = c(1,7,6,5,2,3,4),
                    oil = c(2,3,6,5,1,7,4)
  )
  result <- add_fcs(df1, fcs_cereal="cereal", fcs_legumes="pulses", fcs_veg="vegetables",
                    fcs_fruit = "fruits", fcs_meat = "meat", fcs_dairy="milk", fcs_sugar="sugar", fcs_oil="oil") %>% #%>% select(-fcs_cat)
    dplyr::select(-starts_with("fcs_weight"), -fcs_cat)
  expected_result <- data.frame(cereal = c(NA,2,3,2,5,6,7),
                                pulses = c(3,4,5,6,1,6,5),
                                vegetables = c(3,2,1,6,5,4,3),
                                fruits = c(1,4,6,2,2,2,4),
                                meat = c(5,4,3,2,7,4,5),
                                milk = c(1,2,6,7,3,4,2),
                                sugar = c(1,7,6,5,2,3,4),
                                oil = c(2,3,6,5,1,7,4),
                                fcs_score = c(NA, 51.0, 70.0, 71.0, 61.5, 73.0, 68.0))
  expect_equal(result, expected_result)

})

#
# #test3 - if the inputed variables are characters, but can be made numeric
# test_that("calculations of fcs are correct", {
#   df1 <- data.frame(cereal = c(1,2,3,2,5,6,7),
#                     pulses = c(3,4,5,6,1,6,5),
#                     vegetables = c(3,2,1,6,5,4,3),
#                     fruits = c(1,4,6,2,2,2,4),
#                     meat = c(5,4,3,2,7,4,5),
#                     milk = c(1,2,6,7,3,4,2),
#                     sugar = c(1,7,6,5,2,3,4),
#                     oil = c(2,3,6,5,1,7,4)
#   )
#   df1<- sapply(df1, as.character) %>% as.data.frame()
#   result <- add_fcs(df1, fcs_cereal="cereal", fcs_legumes="pulses", fcs_veg="vegetables",
#                     fcs_fruit = "fruits", fcs_meat = "meat", fcs_dairy="milk", fcs_sugar="sugar", fcs_oil="oil") %>% #%>% select(-fcs_cat)
#     select(-starts_with("fcs_weight"))
#   expected_result <- data.frame(cereal = c(1,2,3,2,5,6,7),
#                                 pulses = c(3,4,5,6,1,6,5),
#                                 vegetables = c(3,2,1,6,5,4,3),
#                                 fruits = c(1,4,6,2,2,2,4),
#                                 meat = c(5,4,3,2,7,4,5),
#                                 milk = c(1,2,6,7,3,4,2),
#                                 sugar = c(1,7,6,5,2,3,4),
#                                 oil = c(2,3,6,5,1,7,4),
#                                 fcs_score = c(40.5, 51.0, 70.0, 71.0, 61.5, 73.0, 68.0))
#   expect_equal(result, expected_result)
# })

