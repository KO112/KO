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
gini_weighted <- function(solution, weights = 1, submission) {
  df <- data.frame(
      solution = solution,
      weights = weights,
      submission = submission
    ) %>%
    dplyr::arrange(dplyr::desc(submission)) %>%
    dplyr::mutate(
      random = cumsum(weights / sum(weights)),
      cumPosFound = cumsum(solution * weights),
      Lorentz <- cumPosFound / sum(solution * weights)
    )
  # df <- df[order(df$submission, decreasing = TRUE), ]
  # df$random = cumsum((df$weights / sum(df$weights)))
  # totalPositive <- sum(df$solution * df$weights)
  # df$cumPosFound <- cumsum(df$solution * df$weights)
  # df$Lorentz <- df$cumPosFound / totalPositive
  gini <- sum(df$Lorentz[-1] * df$random[-nrow(df)]) - sum(df$Lorentz[-nrow(df)] * df$random[-1])
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
gini_weighted_normalized <- function(solution, weights = 1, submission) {
  gini_weighted(solution, weights, submission) / gini_weighted(solution, weights, solution)
}
