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
#' @param solutions Numerical vector of actual response.
#' @param predictions Predictions to score against the solution.
#' @param weights Weights to assign to each prediction.
#'
#' @return Weighted Gini index.
#' @export
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
    {sum(utils::tail(Lorentz, -1) * utils::head(random, -1)) -
        sum(utils::head(Lorentz, -1) * utils::tail(random, -1))} %>%
    return()

}

#' Normalized Weighted Gini
#'
#' Calculate the normalized weighted Gini index of predictions against the solutions.
#'
#' @param solutions Numerical vector of actual response.
#' @param predictions Predictions to score against the solution.
#' @param weights Weights to assign to each prediction.
#'
#' @return Normalized weighted Gini index.
#' @export
#'
gini_weighted_normalized <- function(solutions, predictions, weights = 1) {
  gini_weighted(solutions, predictions, weights) / gini_weighted(solutions, solutions, weights)
}



#' Calculate AIC & BIC
#'
#' Calculate the AIC and BIC for a quasi-Poisson GLM by calculating training a normal Poisson GLM.
#'
#' @param model Quasi-Poisson GLM to calculate the AIC/BIC for.
#'
#' @return A list of two elements, AIC and BIC.
#' @export
#'
#' @examples
#' # quasi_poisson_aic_bic(quasiPoissonGLM)
#'
quasi_poisson_aic_bic <- function(model) {

  # Train a Poisson GLM based on the passed model
  model_poi <- stats::glm(
      model$y ~ model$x,
      family = stats::poisson(link = "log"),
      data = model$data,
      offset = model$offset,
      subset = model$subset,
      na.action = "na.pass",
      x = TRUE
    )

  # Return the AIC and BIC of the model in a list
  return(list(AIC = stats::AIC(model_poi), BIC = stats::BIC(model_poi)))

}
