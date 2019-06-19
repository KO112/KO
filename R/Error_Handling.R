#' Stop if condition is met (with message)
#'
#' If the condition is met, stop execution, and print an error message
#'
#' @param condition Condition that, if met, will stop and print an error message.
#' @param error_msg Message to be displayed if the stopping condition is met.
#'
#' @return Nothing.
#' @export
#'
#' @examples
#' # stop_if(TRUE, "I'm an error message") # Will stop execution and print the message
#' # stop_if(FALSE, "I'm an error message") # Will do nothing
#'
stop_if <- function(condition, error_msg) {
  if (isTRUE(condition)) stop(error_msg)
}
