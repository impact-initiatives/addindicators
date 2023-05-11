#' add_fcs
#'
#' @param df the clean dataset
#' @param cutoffs either "normal 21.5-35", or "alternative 28-42". The default is set to normal
#' @param fcs_cereal the name of the variable that indicates the number of days cereals were consumed
#' @param fcs_legumes the name of the variable that indicates the number of days legumes were consumed
#' @param fcs_veg the name of the variable that indicates the number of days vegetables were consumed
#' @param fcs_fruit the name of the variable that indicates the number of days fruits were consumed
#' @param fcs_meat the name of the variable that indicates the number of days meat/fish were consumed
#' @param fcs_dairy the name of the variable that indicates the number of days dairy were consumed
#' @param fcs_sugar the name of the variable that indicates the number of days cereals was consumed
#' @param fcs_oil the name of the variable that indicates the number of days oild were consumed
#'
#' @return the dataset with fcs_score and fcs_cat computed, as well as  the 8 weighted food groups
#' @export
#'
#' @examples
#' df1 <- data.frame(cereal = c(1,2,3,2,5,6,7),
#' pulses = c(3,4,5,6,1,6,5),
#' vegetables = c(3,2,1,6,5,4,3),
#' fruits = c(1,4,6,2,2,2,4),
#' meat = c(5,4,3,2,7,4,5),
#' milk = c(1,2,6,7,3,4,2),
#' sugar = c(1,7,6,5,2,3,4),
#' oil = c(2,3,6,5,1,7,4))
#' add_fcs(df1, cutoffs="normal 21.5-35", fcs_cereal="cereal", fcs_legumes="pulses", fcs_veg="vegetables",
#'  fcs_fruit = "fruits", fcs_meat = "meat", fcs_dairy="milk", fcs_sugar="sugar", fcs_oil="oil")
#'
add_fcs <- function(df,
                    cutoffs = "normal 21.5-35",
                    fcs_cereal,
                    fcs_legumes,
                    fcs_veg,
                    fcs_fruit,
                    fcs_meat,
                    fcs_dairy,
                    fcs_sugar,
                    fcs_oil) {

  #TO DO if dataset inputted is a list, select the dataset

  if ("fcs_score" %in% names(df)) {
    warning("There is already a variable called fcs_score in your dataset, it will be overwritten")
    }
  if ("fcs_cat" %in% names(df)) {
    warning("There is already a variable called fcs_cat in your dataset, it will be overwritten")
  }
  fcs_vars <- c(fcs_cereal, fcs_legumes, fcs_dairy, fcs_meat, fcs_veg, fcs_fruit, fcs_oil, fcs_sugar)

    #make numeric.
    #TODO ADD if is.numeric then do all, else throw error + if not all vairables between 0 and 7 error
    df[fcs_vars] <- sapply(df[fcs_vars], as.numeric)


    #compute the FCS weighted variables
    df <- df %>%
      dplyr::mutate(fcs_weight_cereal1 = ifelse(is.na(df[[fcs_cereal]]), NA, df[[fcs_cereal]]*2) ,
                    fcs_weight_legume2 = ifelse(is.na(df[[fcs_legumes]]), NA, df[[fcs_legumes]]*3) ,
                    fcs_weight_dairy3 = ifelse(is.na(df[[fcs_dairy]]), NA, df[[fcs_dairy]]*4) ,
                    fcs_weight_meat4 = ifelse(is.na(df[[fcs_meat]]), NA, df[[fcs_meat]]*4),
                    fcs_weight_veg5 = ifelse(is.na(df[[fcs_veg]]), NA, df[[fcs_veg]]*1),
                    fcs_weight_fruit6 = ifelse(is.na(df[[fcs_fruit]]), NA, df[[fcs_fruit]]*1) ,
                    fcs_weight_oil7 = ifelse(is.na(df[[fcs_oil]]), NA, df[[fcs_oil]]*0.5),
                    fcs_weight_sugar8 = ifelse(is.na(df[[fcs_sugar]]), NA, df[[fcs_sugar]]*0.5)
      ) %>%

      #compute FCS, if any fcs_weight is NA, the FCS will be NA automatically
      dplyr::mutate(fcs_score = fcs_weight_cereal1 +fcs_weight_legume2 + fcs_weight_dairy3 + fcs_weight_meat4 +
                      fcs_weight_veg5 +fcs_weight_fruit6 + fcs_weight_oil7 + fcs_weight_sugar8)


    #compute FCS CATEGORIES, based on normal or alternative cutoff
    if(cutoffs == "normal 21.5-35") {
      df <- df %>%
        dplyr::mutate(
          fcs_cat = dplyr::case_when(is.na(fcs_score) ~ NA_character_,
                              fcs_score < 21.5 ~ "Poor",
                              fcs_score <=35 ~ "Borderline",
                              fcs_score>35 & fcs_score < 200 ~ "Acceptable",
                              TRUE ~ NA_character_))

    } else if(cutoffs == "alternative 28-42") {
      df <- df %>%
        dplyr::mutate(
          fcs_cat = dplyr::case_when(is.na(fcs_score) ~ NA_character_,
                              fcs_score <=28 ~ "Poor",
                              fcs_score <=42 ~ "Borderline",
                              fcs_score>42 & fcs_score < 200 ~ "Acceptable",
                              TRUE ~ NA_character_))
      }
    return(df)
}
