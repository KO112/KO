# Include other functions in package
#' @include Pipes.R
#' @include Error_Handling.R
NULL

# Avoid "undefined variable" notes in package checking
globalVariables(c("Lorentz", "random", "cumPosFound"))


#' Weighted Gini
#'
#' Calculate the (normalized) weighted Gini index of predictions against the solutions.
#'
#' @param solutions Numerical vector of actual response values.
#' @param predictions Predictions to score against the solution.
#' @param weights Weights to assign to each prediction.
#'
#' @return (Normalized) weighted Gini index (numeric scalar).
#' @name gini_weighted
#' @export
#'
#' @examples
#' # Create a GLM for testing
#' \dontrun{
#'   data(dataCar, package = "insuranceData")
#'   dataCarGLM <- glm(numclaims ~ veh_value + veh_age + gender + agecat,
#'                     data = dataCar, family = poisson, offset = log(exposure), x = TRUE)
#'
#'   gini_weighted(dataCar$numclaims, predict(dataCarGLM, dataCar))
#' }
#'
gini_weighted <- function(solutions, predictions, weights = 1) {

  # Create a data frame of the solution, weights, predictions, arranging by the predictions
  data.frame(
      solutions = solutions,
      weights = weights,
      predictions = predictions
    ) %>%
    dplyr::arrange(dplyr::desc(predictions)) %>%

    # Calculate the
    dplyr::mutate(
      random = cumsum(weights / sum(weights)),
      cumPosFound = cumsum(solutions * weights),
      Lorentz = cumPosFound / sum(solutions * weights)
    ) %$%

    # Calculate the Gini index, and return the Gini
    return(sum(utils::tail(Lorentz, -1) * utils::head(random, -1)) -
             sum(utils::head(Lorentz, -1) * utils::tail(random, -1)))

}

#' @rdname gini_weighted
#' @export
#'
#' @examples
#' \dontrun{
#'   gini_weighted_normalized(dataCar$numclaims, predict(dataCarGLM, dataCar))
#' }
#'
gini_weighted_normalized <- function(solutions, predictions, weights = 1) {
  gini_weighted(solutions, predictions, weights) / gini_weighted(solutions, solutions, weights)
}
