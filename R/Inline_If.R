#' Inline If Statement
#'
#' Inline if statement that checks the condition,
#'   and returns either the \code{true} or \code{false} paramater.
#' Similar to \code{ifelse}, but returns the entire value of the parameters.
#'
#' @param condition Condition to check (logical)
#' @param true Value to return when the condition is true
#' @param false Value to return when the condition is false
#'
#' @return Either the \code{true} or \code{false} parameter.
#' @export
#'
#' @examples
#' iif(TRUE, "a", "b")
#' iif(10 < 20, TRUE, FALSE)
#' print(iif(TRUE, "true", "false"))
#'
#' # Compare iif to ifelse
#' iif(TRUE, 1:2, 3:4)
#' ifelse(TRUE, 1:2, 3:4)
#'
iif <- function(condition, true, false) {
  if (condition) return(true)
  return(false)
}
