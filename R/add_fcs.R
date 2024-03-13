#' add_fcs
#'
#' @param df the clean dataset
#' @param cutoffs either "normal", or "alternative". The default is set to normal
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
#' df1 <- data.frame(
#'   fcs_cereal = c(1, 2, 3, 2, 5, 6, 7),
#'   fcs_legumes = c(3, 4, 5, 6, 1, 6, 5),
#'   fcs_veg = c(3, 2, 1, 6, 5, 4, 3),
#'   fcs_fruit = c(1, 4, 6, 2, 2, 2, 4),
#'   fcs_meat = c(5, 4, 3, 2, 7, 4, 5),
#'   fcs_dairy = c(1, 2, 6, 7, 3, 4, 2),
#'   fcs_sugar = c(1, 7, 6, 5, 2, 3, 4),
#'   fcs_oil = c(2, 3, 6, 5, 1, 7, 4)
#' )
#' add_fcs(.dataset = df1,
#'   cutoffs = "normal"
#' )
#'
add_fcs <- function(.dataset,
                    cutoffs = c("normal","alternative"),
                    fcs_cereal = "fcs_cereal",
                    fcs_legumes = "fcs_legumes",
                    fcs_veg = "fcs_veg",
                    fcs_fruit = "fcs_fruit",
                    fcs_meat = "fcs_meat",
                    fcs_dairy = "fcs_dairy",
                    fcs_sugar = "fcs_sugar",
                    fcs_oil = "fcs_oil") {

  ## Throw an error if a dataset wasn't provided as a first argument
  if (!is.data.frame(.dataset)) {
    stop("First argument should be a dataset")
  }

  if (nrow(.dataset) == 0) {
    stop("Dataset is empty")
  }
  if ("fcs_score" %in% names(.dataset)) {
    warning("There is already a variable called fcs_score in your dataset, it will be overwritten")
  }

  if ("fcs_cat" %in% names(.dataset)) {
    warning("There is already a variable called fcs_cat in your dataset, it will be overwritten")
  }

  fcs_vars <- c(fcs_cereal, fcs_legumes, fcs_dairy, fcs_meat,
                fcs_veg, fcs_fruit, fcs_oil, fcs_sugar)
  tryCatch({
    .dataset %>%
      dplyr::select(dplyr::all_of(c(fcs_vars)))
  }, error = function(e) {
    message("Missing fcs columns")
  })

  if (!all(.dataset[[fcs_cereal]] %in% c(0:7,NA))) {
    stop(sprintf("Wrong values in %s: %s ", fcs_cereal,
                 paste0(unique(.dataset[[fcs_cereal]][!.dataset[[fcs_cereal]] %in% c(0:7)]), collapse = "/")))
  }

  if (!all(.dataset[[fcs_legumes]] %in% c(0:7,NA))) {
    stop(sprintf("Wrong values in %s: %s ", fcs_legumes,
                 paste0(unique(.dataset[[fcs_legumes]][!.dataset[[fcs_legumes]] %in% c(0:7)]), collapse = "/")))
  }

  if (!all(.dataset[[fcs_dairy]] %in% c(0:7,NA))) {
    stop(sprintf("Wrong values in %s: %s ", fcs_dairy,
                 paste0(unique(.dataset[[fcs_dairy]][!.dataset[[fcs_dairy]] %in% c(0:7)]), collapse = "/")))
  }

  if (!all(.dataset[[fcs_meat]] %in% c(0:7,NA))) {
    stop(sprintf("Wrong values in %s: %s ", fcs_meat,
                 paste0(unique(.dataset[[fcs_meat]][!.dataset[[fcs_meat]] %in% c(0:7)]), collapse = "/")))
  }

  if (!all(.dataset[[fcs_veg]] %in% c(0:7,NA))) {
    stop(sprintf("Wrong values in %s: %s ", fcs_veg,
                 paste0(unique(.dataset[[fcs_veg]][!.dataset[[fcs_veg]] %in% c(0:7)]), collapse = "/")))
  }

  if (!all(.dataset[[fcs_fruit]] %in% c(0:7,NA))) {
    stop(sprintf("Wrong values in %s: %s ", fcs_fruit,
                 paste0(unique(.dataset[[fcs_fruit]][!.dataset[[fcs_fruit]] %in% c(0:7)]), collapse = "/")))
  }

  if (!all(.dataset[[fcs_oil]] %in% c(0:7,NA))) {
    stop(sprintf("Wrong values in %s: %s ", fcs_oil,
                 paste0(unique(.dataset[[fcs_oil]][!.dataset[[fcs_oil]] %in% c(0:7)]), collapse = "/")))
  }

  if (!all(.dataset[[fcs_sugar]] %in% c(0:7,NA))) {
    stop(sprintf("Wrong values in %s: %s ", fcs_sugar,
                 paste0(unique(.dataset[[fcs_sugar]][!.dataset[[fcs_sugar]] %in% c(0:7)]), collapse = "/")))
  }

  .dataset <- .dataset %>%
    dplyr::mutate_at(dplyr::vars(fcs_vars), as.numeric) %>%
    dplyr::mutate(fcs_weight_cereal1 = ifelse(is.na(!!rlang::sym(fcs_cereal)), NA, !!rlang::sym(fcs_cereal) * 2),
                  fcs_weight_legume2 = ifelse(is.na(!!rlang::sym(fcs_legumes)), NA, !!rlang::sym(fcs_legumes) * 3),
                  fcs_weight_dairy3 = ifelse(is.na(!!rlang::sym(fcs_dairy)), NA, !!rlang::sym(fcs_dairy) * 4),
                  fcs_weight_meat4 = ifelse(is.na(!!rlang::sym(fcs_meat)), NA, !!rlang::sym(fcs_meat) * 4),
                  fcs_weight_veg5 = ifelse(is.na(!!rlang::sym(fcs_veg)), NA, !!rlang::sym(fcs_veg) * 1),
                  fcs_weight_fruit6 = ifelse(is.na(!!rlang::sym(fcs_fruit)), NA, !!rlang::sym(fcs_fruit) * 1),
                  fcs_weight_oil7 = ifelse(is.na(!!rlang::sym(fcs_oil)), NA, !!rlang::sym(fcs_oil) * 0.5),
                  fcs_weight_sugar8 = ifelse(is.na(!!rlang::sym(fcs_sugar)), NA, !!rlang::sym(fcs_sugar) * 0.5)) %>%
    dplyr::mutate(fcs_score = rowSums(dplyr::across(c(fcs_weight_cereal1, fcs_weight_legume2, fcs_weight_dairy3, fcs_weight_meat4, fcs_weight_veg5,
                                               fcs_weight_fruit6, fcs_weight_oil7, fcs_weight_sugar8), .fns = as.numeric)))
  if (cutoffs == "normal") {
    .dataset <- .dataset %>% dplyr::mutate(fcs_cat = dplyr::case_when(fcs_score < 21.5 ~ "Poor",
                                                                      fcs_score <= 35 ~ "Borderline",
                                                                      fcs_score > 35 ~ "Acceptable",
                                                                      TRUE ~ NA))
  } else if (cutoffs == "alternative") {
    .dataset <- .dataset %>% dplyr::mutate(fcs_cat = dplyr::case_when(fcs_score <= 28 ~ "Poor",
                                                                      fcs_score <= 42 ~ "Borderline",
                                                                      fcs_score > 42 ~ "Acceptable",
                                                                      TRUE ~ NA))
  }
  return(.dataset)
}
