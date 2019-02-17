#' Weighted Gini
#'
#' Calculate the weighted Gini index of predictions against the solutions.
#'
#' @param solution Numerical vector of actual response.
#' @param weights Weights to assign to each prediction.
#' @param submission Predictions to score against the solution.
#'
#' @return Weighted Gini index.
#'
gini_weighted <- function(solution, weights = NULL, submission){
  if (is.null(weights)) weights = rep(1, length(solution))
  df = data.frame(solution = solution, weights = weights, submission = submission)
  df <- df[order(df$submission, decreasing = TRUE),]
  df$random = cumsum((df$weights/sum(df$weights)))
  totalPositive <- sum(df$solution * df$weights)
  df$cumPosFound <- cumsum(df$solution * df$weights)
  df$Lorentz <- df$cumPosFound / totalPositive
  n <- nrow(df)
  gini <- sum(df$Lorentz[-1]*df$random[-n]) - sum(df$Lorentz[-n]*df$random[-1])
  return(gini)
}

#' Normalized Weighted Gini
#'
#' Calculate the normalized weighted Gini index of predictions against the solutions.
#'
#' @param solution Numerical vector of actual response.
#' @param weights Weights to assign to each prediction.
#' @param submission Predictions to score against the solution.
#'
#' @return Normalized weighted Gini index.
#'
gini_weighted_normalized <- function(solution, weights = NULL, submission) {
  gini_weighted(solution, weights, submission) / gini_weighted(solution, weights, solution)
}
