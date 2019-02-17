# Include other functions in package
#' @include Pipes.R
#' @include Error_Handling.R
NULL


#' Find percent of missing values
#'
#' Count the percent of missing values in a vector.
#' Counts the percent of NA values for non-character vectors,
#'   and the number of NA or "" values for characters.
#'
#' @param vec Vector to count missing values in
#'
#' @return A number containing the percent of missing values.
#' @export
#'
#' @examples
#' percent_missing_vals(c(NA))
#' percent_missing_vals(c(1, 2, 3, 4, 5))
#' percent_missing_vals(c(1, 2, NA, 4, 5))
#'
percent_missing_vals <- function(vec) {
  if (is.numeric(vec) | is.logical(vec) | lubridate::is.Date(vec) | is.factor(vec)) {
    numMissing <- sum(is.na(vec))
  } else if (is.character(vec)) {
    numMissing <- sum(vec == "", na.rm = TRUE) + sum(is.na(vec))
  } else {
    message("Unknown vector data type: ", class(vec))
    numMissing <- sum(is.na(vec))
  }
  return(numMissing / length(vec))
}


#' Replace missing values in a vector
#'
#' Replace missing values in a vector with either the mode, median,
#'   or mean of the vector, removing NA values in the calculation.
#'
#' @param vec Vector to replace missing values in
#' @param method The method to use to determine the imputed value
#' @param returnImputed Whether or not to return the values missing elements were imputed with
#'
#' @return A vector with missing values replaced as desired,
#'   or a list of that and the replacement value.
#' @export
#'
#' @examples
#' replace_missing(c(1, 1, 1, 2, NA))
#' replace_missing(c(1, 1, 1, 2, NA), method = "mode") # Same as above
#' replace_missing(c(1, NA, 1, 2), method = "median")
#' replace_missing(c(1, NA, 1, 2), method = "mean")
#' replace_missing(c(1, NA, 1, 2), method = "mean", returnImputed = TRUE)
#'
replace_missing <- function(vec, method = "mode", returnImputed = FALSE) {

  # Ensure the input is numeric
  stop_if(!is.numeric(vec), "Non-numeric vector passed.")

  # Find the value to replace missing values with based on the desired method
  if (method == "median") {
    imputedVal <- stats::median(vec, na.rm = TRUE)
  } else if (method == "mean") {
    imputedVal <- mean(vec, na.rm = TRUE)
  } else {
    if (method != "mode") message("Invalid method chosen to replace missing values. Mode will be used.")
    imputedVal <- vec %>% table(useNA = "no") %>% sort() %>% utils::tail(1) %>% names()
  }

  # Convert the imputed value to the appropriate type
  if (lubridate::is.Date(vec)) {
    imputedVal <- lubridate::as_date(imputedVal)
  } else {
    imputedVal <- as.vector(imputedVal, class(vec))
  }

  # Replace NA/missing values with the imputed value, and return the vector
  if (is.numeric(vec) | is.logical(vec) | lubridate::is.Date(vec)) {
    vec[is.na(vec)] <- imputedVal
  } else if (is.character(vec)) {
    vec[(vec == "") | is.na(vec)] <- imputedVal
  } else {
    message("Unknown vector data type: ", class(vec))
    vec[is.na(vec)] <- imputedVal
  }

  # Return the vector, and the imputed value as well if desired
  if (returnImputed) return(list(Vec = vec, ImputedVal = imputedVal))
  return(vec)

}
