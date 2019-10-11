# Avoid "undefined variable" notes in package checking
globalVariables(c(".KO_fn"))


#' Run a Function
#'
#' @examples
#' .KO_fn <- function() mean(1:10)
#' .KO_fn
#' 
run_fn <- function() {
  if (exists(".KO_fn")) .KO_fn() else rstudioapi::sendToConsole("# .KO_fn <- function() ")
}
