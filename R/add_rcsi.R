#' Add indicator for reduced Household CSI Score(rcsi)
#'
#' @param data dataset
#' @param rCSILessQlty Column representing question- During the last 7 days, were there days (and, if so, how many) when your household had to rely on less preferred and less expensive food to cope with a lack of food or money to buy it?
#' @param rCSIBorrow   Column representing question- During the last 7 days, were there days (and, if so, how many) when your household had to borrow food or rely on help from a relative or friend to cope with a lack of food or money to buy it?
#' @param rCSIMealSize Column representing question- During the last 7 days, were there days (and, if so, how many) when your household had to limit portion size of meals at meal times to cope with a lack of food or money to buy it?
#' @param rCSIMealAdult Column representing question- During the last 7 days, were there days (and, if so, how many) when your household had to restrict consumption by adults in order for small children to eat to cope with a lack of food or money to buy it?
#' @param rCSIMealNb Column representing question - During the last 7 days, were there days (and, if so, how many) when your household had to reduce number of meals eaten in a day to cope with a lack of food or money to buy it?
#' @param new_colname The prefix for the new columns. It has to be a string.
#' @return
#' A dataset with one additional column.
#' @export
#' @importFrom rlang :=
#' @examples
#' test_data <- data.frame(
#' rCSILessQlty = c(1,2,3,1),
#' rCSIBorrow = c(0,0,3,0),
#' rCSIMealSize = c(4,2,6,1),
#' rCSIMealAdult = c(4,3,5,0),
#' rCSIMealNb = c(2,5,NA_integer_,1)
#' )
#' add_rcsi(test_data)



add_rcsi <- function(data,
                     rCSILessQlty = "rCSILessQlty",
                     rCSIBorrow = "rCSIBorrow" ,
                     rCSIMealSize = "rCSIMealSize",
                     rCSIMealAdult = "rCSIMealAdult",
                     rCSIMealNb = "rCSIMealNb",
                     new_colname = "rcsi"
){

  score_name <- paste0(new_colname, "_score")
  cat_name <- paste0(new_colname, "_cat")
  score1 <- paste0(new_colname, "_1")
  score2 <- paste0(new_colname, "_2")
  score3 <- paste0(new_colname, "_3")
  score4 <- paste0(new_colname, "_4")
  score5 <- paste0(new_colname, "_5")

  rowsum_col <- c(score1,score2,score3,score4,score5)
  all_newly_created <- c(rowsum_col,score_name,cat_name)

  if(any(all_newly_created %in% names(data))) {
    print(all_newly_created[all_newly_created %in% names(data)])
    warning("The above(s) column are existing in the dataset. These columns will be replaced by the function. Please change `new_colname` parameter to keep them.")
  }

  all_names <- c(rCSILessQlty,rCSIBorrow,rCSIMealSize,rCSIMealAdult,rCSIMealNb)

  if(!all(all_names %in%  names(data))){
    message(all_names[!all_names %in% names(data)])
    stop("The above column(s) can not be found in the dataset.")
  }

  class <- sapply(data[all_names],is.numeric)

  if(!all(class==T)){
    message(paste(all_names[!class]," "),appendLF = T)
    stop("The avobe column(s) are not numeric.")

  }


  check_value <- data |> tidyr::pivot_longer(cols = dplyr::all_of(all_names),
                                             names_to = "question",values_to = "old_value") |>
    dplyr::filter(!old_value %in% 0:7 & !is.na(old_value))

  if(nrow(check_value) > 0) {
    message(paste(unique(check_value$question),collapse = ", "))
    stop("please check the above column(s) as they contain value(s) outside 0-7 range.")
  }


  data <- data |>
    dplyr::mutate( !!rlang::sym(score1) := !!rlang::sym(rCSILessQlty)*1,
                   !!rlang::sym(score2) := !!rlang::sym(rCSIBorrow)*2,
                   !!rlang::sym(score3) := !!rlang::sym(rCSIMealSize)*1,
                   !!rlang::sym(score4) := !!rlang::sym(rCSIMealAdult)*3,
                   !!rlang::sym(score5) := !!rlang::sym(rCSIMealNb)*1)

  data <- data |>
    dplyr::mutate( !!rlang::sym(score_name) :=  rowSums(data[rowsum_col])) |>
    dplyr::mutate( !!rlang::sym(cat_name) := dplyr::case_when(!!rlang::sym(score_name) <=3 ~ "No to Low",
                                                              !!rlang::sym(score_name) %in% 4:18 ~ "Medium",
                                                              !!rlang::sym(score_name) > 18 ~ "High",
                                                              T ~ NA_character_)
    )

  message(paste0("Variable name for rcsi score is ", score_name))
  message(paste0("Variable name for rcsi category is ", cat_name))

  data |> dplyr::select(-c(dplyr::all_of(score1),dplyr::all_of(score2),
                           dplyr::all_of(score3),dplyr::all_of(score4),dplyr::all_of(score5)))


}
