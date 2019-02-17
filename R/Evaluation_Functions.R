# Include other functions in package
#' @include Pipes.R
#' @include Error_Handling.R
NULL

# Avoid "undefined variable" notes in package checking
globalVariables(c("Lorentz", "random", "cumPosFound"))


#' Weighted Gini
#'
#' Calculate the weighted Gini index of predictions against the solutions.
#'
#' @param solution Numerical vector of actual response.
#' @param predictions Predictions to score against the solution.
#' @param weights Weights to assign to each prediction.
#'
#' @return Weighted Gini index.
#'
gini_weighted <- function(solution, predictions, weights = 1) {

  # Create a data frame of the solution, weights, predictions, arranging by the predictions
  data.frame(
      solution = solution,
      weights = weights,
      predictions = predictions
    ) %>%
    dplyr::arrange(dplyr::desc(predictions)) %>%

    # Calculate the
    dplyr::mutate(
      random = cumsum(weights / sum(weights)),
      cumPosFound = cumsum(solution * weights),
      Lorentz = cumPosFound / sum(solution * weights)
    ) %$%

    # Calculate the Gini index, and return the Gini
    {sum(utils::tail(Lorentz, -1) * utils::head(random, -1)) -
        sum(utils::head(Lorentz, -1) * utils::tail(random, -1))} %>%
    return()

}

#' Normalized Weighted Gini
#'
#' Calculate the normalized weighted Gini index of predictions against the solutions.
#'
#' @param solution Numerical vector of actual response.
#' @param predictions Predictions to score against the solution.
#' @param weights Weights to assign to each prediction.
#'
#' @return Normalized weighted Gini index.
#'
gini_weighted_normalized <- function(solution, predictions, weights = 1) {
  gini_weighted(solution, predictions, weights) / gini_weighted(solution, solution, weights)
}
