testthat::test_that("test error", {

  ## test character input
  test_data <- data.frame(
    rCSILessQlty = c(1,2,3),
    rCSILessQlty = c(1,1,7),
    rCSIBorrow = c(0,0,3),
    rCSIMealSize = c(4,2,6),
    rCSIMealAdult = c(4,3,5),
    rCSIMealNb = c(2,5,NA_character_)
  )

  testthat::expect_error(add_rcsi(test_data))

  ## expect no error

  test_data <- data.frame(
    rCSILessQlty = c(1,2,3,1),
    rCSIBorrow = c(0,0,3,0),
    rCSIMealSize = c(4,2,6,1),
    rCSIMealAdult = c(4,3,5,0),
    rCSIMealNb = c(2,5,NA_integer_,1)
  )
  testthat::expect_no_error(add_rcsi(test_data))

  actual <- add_rcsi(test_data)

  expected <- structure(list(rCSILessQlty = c(1, 2, 3,1),
                             rCSIBorrow = c(0, 0, 3,0),
                             rCSIMealSize = c(4, 2, 6,1),
                             rCSIMealAdult = c(4, 3, 5,0),
                             rCSIMealNb = c(2, 5, NA,1),
                             rcsi_score = c(19, 18, NA,3),
                             rcsi_cat = c("High", "Medium", NA,"No to Low")),
                        class = "data.frame",
                        row.names = c(NA, -4L))

  testthat::expect_equal(actual,expected)


### check warning for existing column

  test_data <- data.frame(
    rCSILessQlty = c(1,2,3,1),
    rCSIBorrow = c(0,0,3,0),
    rCSIMealSize = c(4,2,6,1),
    rCSIMealAdult = c(4,3,5,0),
    rCSIMealNb = c(2,5,NA_integer_,1),
    rcsi_cat =NA_character_
  )

  testthat::expect_warning(add_rcsi(test_data))

  ### wrong entry

  test_data <- test_data |> dplyr::select(-rcsi_cat)
  testthat::expect_error( add_rcsi(test_data,rCSILessQlty = "a"))


  ## check no default value

  test_data <- data.frame(
    a = c(1,2,3,1),
    b = c(0,0,3,0),
    c = c(4,2,6,1),
    d = c(4,3,5,0),
    e = c(2,5,NA_integer_,1)
  )

  testthat::expect_no_error(
    test_data |> add_rcsi(rCSILessQlty = "a",rCSIBorrow = "b",rCSIMealSize = "c",rCSIMealAdult = "d",rCSIMealNb = "e")
)
  ### check non default new_column

  testthat::expect_equal(
    names(add_rcsi(test_data,rCSILessQlty = "a",rCSIBorrow = "b",rCSIMealSize = "c",
             rCSIMealAdult = "d",rCSIMealNb = "e",new_colname = "abcd")),
          c("a", "b", "c", "d", "e", "abcd_score", "abcd_cat")
  )


  expected <- structure(list(a = c(1, 2, 3, 1), b = c(0, 0, 3, 0), c = c(4, 2, 6, 1),
                             d = c(4, 3, 5, 0), e = c(2, 5, NA, 1), abcd_score = c(19, 18, NA, 3),
                             abcd_cat = c("High", "Medium", NA, "No to Low")), class = "data.frame",
                        row.names = c(NA,-4L))
  actual <- add_rcsi(test_data,rCSILessQlty = "a",rCSIBorrow = "b",rCSIMealSize = "c",
                     rCSIMealAdult = "d",rCSIMealNb = "e",new_colname = "abcd")

  testthat::expect_equal(actual,expected)


  ### check range
  test_data <- data.frame(
    rCSILessQlty = c(10,2,3),
    rCSIBorrow = c(0,0,3),
    rCSIMealSize = c(4,12,6),
    rCSIMealAdult = c(4,3,5),
    rCSIMealNb = c(2,5,NA_integer_)
  )

  testthat::expect_error(test_data |> add_rcsi())


})
