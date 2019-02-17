# Include external operators
#' @include utils-pipe.R
NULL

# Avoid "undefined variable" notes in package checking
globalVariables(c("Lorentz", "random", "cumPosFound"))


#' Weighted Gini
#'
#' Calculate the weighted Gini index of predictions against the solutions.
#'
#' @param solution Numerical vector of actual response.
#' @param weights Weights to assign to each prediction.
#' @param predictions Predictions to score against the solution.
#'
#' @return Weighted Gini index.
#'
gini_weighted <- function(solution, weights = 1, predictions) {

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
#' @param weights Weights to assign to each prediction.
#' @param predictions Predictions to score against the solution.
#'
#' @return Normalized weighted Gini index.
#'
gini_weighted_normalized <- function(solution, weights = 1, predictions) {
  gini_weighted(solution, weights, predictions) / gini_weighted(solution, weights, solution)
}
