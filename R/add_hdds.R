#' add_hdds
#'
#' @param .dataset the clean dataset
#' @param fsl_hdds_cereals the name of the variable that indicates if cereals were consumed in the last 24 hours
#' @param fsl_hdds_tubers the name of the variable that indicates if roots or tubers were consumed in the last 24 hours
#' @param fsl_hdds_veg the name of the variable that indicates if vergetables were consumed in the last 24 hours
#' @param fsl_hdds_fruit the name of the variable that indicates if fruits were consumed in the last 24 hours
#' @param fsl_hdds_meat the name of the variable that indicates if meat were consumed in the last 24 hours
#' @param fsl_hdds_eggs the name of the variable that indicates if eggs were consumed in the last 24 hours
#' @param fsl_hdds_fish the name of the variable that indicates if fish were consumed in the last 24 hours
#' @param fsl_hdds_legumes the name of the variable that indicates if pulses or legumes were consumed in the last 24 hours
#' @param fsl_hdds_dairy the name of the variable that indicates if dairy were consumed in the last 24 hours
#' @param fsl_hdds_oil the name of the variable that indicates if oil were consumed in the last 24 hours
#' @param fsl_hdds_sugar the name of the variable that indicates if sugar were consumed in the last 24 hours
#' @param fsl_hdds_condiments the name of the variable that indicates if condiments were consumed in the last 24 hours
#' @param yes_val A character value in the dataset associated with "yes"
#' @param no_val A character value in the dataset associated with "no"
#'
#' @return the dataset with fsl_hdds_score and fsl_hdds_cat computed, as well as
#' all the hdds columns recoded to 1 and 0 for yes and no as fsl_hdds_x_recoded
#' @export
#'
#' @examples
#' df1 <- data.frame(fsl_hdds_cereals = c("yes", "yes", "yes", "no", "yes", "no"),
#'   fsl_hdds_tubers = c("yes", "yes", "yes", "no", "yes", "no"),
#'   fsl_hdds_veg = c("no", "yes", "yes", "no", "yes", "no"),
#'   fsl_hdds_fruit = c("yes", "yes", "yes", "no", "yes", "no"),
#'   fsl_hdds_meat = c("no", "no", "yes", "no", "yes", "no"),
#'   fsl_hdds_eggs = c("yes", "no", "yes", "no", "yes", "no"),
#'   fsl_hdds_fish = c("yes", "yes", "yes", "no", "yes", "no"),
#'   fsl_hdds_legumes = c("yes", "no", "yes", "no", "yes", "no"),
#'   fsl_hdds_dairy = c("no", "yes", "yes", "no", "yes", "no"),
#'   fsl_hdds_oil = c("yes", "yes", "yes", "no", "no", "no"),
#'   fsl_hdds_sugar = c("yes", "yes", "no", "no", "yes", "no"),
#'   fsl_hdds_condiments = c("no", "yes", "yes", "no", "yes", "no")
#' )
#' add_hdds(.dataset = df1
#' )


add_hdds <- function(.dataset,
                         fsl_hdds_cereals = "fsl_hdds_cereals",
                         fsl_hdds_tubers = "fsl_hdds_tubers",
                         fsl_hdds_veg = "fsl_hdds_veg",
                         fsl_hdds_fruit = "fsl_hdds_fruit",
                         fsl_hdds_meat = "fsl_hdds_meat",
                         fsl_hdds_eggs = "fsl_hdds_eggs",
                         fsl_hdds_fish = "fsl_hdds_fish",
                         fsl_hdds_legumes = "fsl_hdds_legumes",
                         fsl_hdds_dairy = "fsl_hdds_dairy",
                         fsl_hdds_oil = "fsl_hdds_oil",
                         fsl_hdds_sugar = "fsl_hdds_sugar",
                         fsl_hdds_condiments = "fsl_hdds_condiments",
                         yes_val = "yes",
                         no_val = "no") {

  ## Throw an error if a dataset wasn't provided as a first argument
  if (!is.data.frame(.dataset)) {
    stop("First argument should be a dataset")
  }

  if (nrow(.dataset) == 0) {
    stop("Dataset is empty")
  }

  if ("fsl_hdds_score" %in% names(.dataset)) {
    warning("There is already a variable called fsl_hdds_score in your dataset, it will be overwritten")
  }

  if ("fsl_hdds_cat" %in% names(.dataset)) {
    warning("There is already a variable called fsl_hdds_score in your dataset, it will be overwritten")
  }

  hdds_vars <- c(fsl_hdds_cereals,fsl_hdds_tubers,fsl_hdds_veg,fsl_hdds_fruit,
                 fsl_hdds_meat,fsl_hdds_eggs,fsl_hdds_fish,fsl_hdds_legumes,
                 fsl_hdds_dairy,fsl_hdds_oil,fsl_hdds_sugar,fsl_hdds_condiments)

  if(!all(hdds_vars %in% names(.dataset))) stop("Missing hdds columns")

  hdds_cat_values <- c(yes_val, no_val)

  if (!all(.dataset[[fsl_hdds_cereals]] %in% c(hdds_cat_values, NA))) {
    stop(sprintf("Wrong values in %s: %s ", fsl_hdds_cereals,
                 paste0(unique(.dataset[[fsl_hdds_cereals]][!.dataset[[fsl_hdds_cereals]] %in% c(hdds_cat_values, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[fsl_hdds_tubers]] %in% c(hdds_cat_values, NA))) {
    stop(sprintf("Wrong values in %s: %s ", fsl_hdds_tubers,
                 paste0(unique(.dataset[[fsl_hdds_tubers]][!.dataset[[fsl_hdds_tubers]] %in% c(hdds_cat_values, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[fsl_hdds_veg]] %in% c(hdds_cat_values, NA))) {
    stop(sprintf("Wrong values in %s: %s ", fsl_hdds_veg,
                 paste0(unique(.dataset[[fsl_hdds_veg]][!.dataset[[fsl_hdds_veg]] %in% c(hdds_cat_values, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[fsl_hdds_fruit]] %in% c(hdds_cat_values, NA))) {
    stop(sprintf("Wrong values in %s: %s ", fsl_hdds_fruit,
                 paste0(unique(.dataset[[fsl_hdds_fruit]][!.dataset[[fsl_hdds_fruit]] %in% c(hdds_cat_values, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[fsl_hdds_meat]] %in% c(hdds_cat_values, NA))) {
    stop(sprintf("Wrong values in %s: %s ", fsl_hdds_meat,
                 paste0(unique(.dataset[[fsl_hdds_meat]][!.dataset[[fsl_hdds_meat]] %in% c(hdds_cat_values, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[fsl_hdds_eggs]] %in% c(hdds_cat_values, NA))) {
    stop(sprintf("Wrong values in %s: %s ", fsl_hdds_eggs,
                 paste0(unique(.dataset[[fsl_hdds_eggs]][!.dataset[[fsl_hdds_eggs]] %in% c(hdds_cat_values, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[fsl_hdds_fish]] %in% c(hdds_cat_values, NA))) {
    stop(sprintf("Wrong values in %s: %s ", fsl_hdds_fish,
                 paste0(unique(.dataset[[fsl_hdds_fish]][!.dataset[[fsl_hdds_fish]] %in% c(hdds_cat_values, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[fsl_hdds_legumes]] %in% c(hdds_cat_values, NA))) {
    stop(sprintf("Wrong values in %s: %s ", fsl_hdds_legumes,
                 paste0(unique(.dataset[[fsl_hdds_legumes]][!.dataset[[fsl_hdds_legumes]] %in% c(hdds_cat_values, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[fsl_hdds_dairy]] %in% c(hdds_cat_values, NA))) {
    stop(sprintf("Wrong values in %s: %s ", fsl_hdds_dairy,
                 paste0(unique(.dataset[[fsl_hdds_dairy]][!.dataset[[fsl_hdds_dairy]] %in% c(hdds_cat_values, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[fsl_hdds_oil]] %in% c(hdds_cat_values, NA))) {
    stop(sprintf("Wrong values in %s: %s ", fsl_hdds_oil,
                 paste0(unique(.dataset[[fsl_hdds_oil]][!.dataset[[fsl_hdds_oil]] %in% c(hdds_cat_values, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[fsl_hdds_sugar]] %in% c(hdds_cat_values, NA))) {
    stop(sprintf("Wrong values in %s: %s ", fsl_hdds_sugar,
                 paste0(unique(.dataset[[fsl_hdds_sugar]][!.dataset[[fsl_hdds_sugar]] %in% c(hdds_cat_values, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[fsl_hdds_condiments]] %in% c(hdds_cat_values, NA))) {
    stop(sprintf("Wrong values in %s: %s ", fsl_hdds_condiments,
                 paste0(unique(.dataset[[fsl_hdds_condiments]][!.dataset[[fsl_hdds_condiments]] %in% c(hdds_cat_values, NA)]), collapse = "/")))
  }

  .dataset_with_calculation <- .dataset %>%
    dplyr::mutate_at(dplyr::vars(hdds_vars), ~ifelse(is.na(.),NA,
                                             ifelse(. == yes_val, 1,0))) %>%
    dplyr::mutate(fsl_hdds_score = rowSums(dplyr::across(c(hdds_vars), .fns = as.numeric)),
                  fsl_hdds_cat = dplyr::case_when(fsl_hdds_score <= 2 ~ "Low",
                                                  fsl_hdds_score <= 4 ~ "Medium",
                                                  fsl_hdds_score > 4 ~ "High",
                                                  TRUE ~ NA))

  columns_to_export <- .dataset_with_calculation %>%
    dplyr::rename_at(c(fsl_hdds_cereals,fsl_hdds_tubers,fsl_hdds_veg,fsl_hdds_fruit,
                       fsl_hdds_meat,fsl_hdds_eggs,fsl_hdds_fish,fsl_hdds_legumes,
                       fsl_hdds_dairy,fsl_hdds_oil,fsl_hdds_sugar,fsl_hdds_condiments), ~paste0(., "_recoded")) %>%
    dplyr::select(paste0(fsl_hdds_cereals, "_recoded"),
                  paste0(fsl_hdds_tubers, "_recoded"),
                  paste0(fsl_hdds_veg, "_recoded"),
                  paste0(fsl_hdds_fruit, "_recoded"),
                  paste0(fsl_hdds_meat, "_recoded"),
                  paste0(fsl_hdds_eggs, "_recoded"),
                  paste0(fsl_hdds_fish, "_recoded"),
                  paste0(fsl_hdds_legumes, "_recoded"),
                  paste0(fsl_hdds_dairy, "_recoded"),
                  paste0(fsl_hdds_oil, "_recoded"),
                  paste0(fsl_hdds_sugar, "_recoded"),
                  paste0(fsl_hdds_condiments, "_recoded"),
                  fsl_hdds_score, fsl_hdds_cat)
  .dataset <- .dataset %>%
    cbind(columns_to_export)

  return(.dataset)
}
