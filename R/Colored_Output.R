#' Print out Some Information
#' 
#' Used to print out some information during the execution of a function.
#' This function prints out the text in green (#00FF00) using the \code{crayon} package.
#' 
#' @param ... Text to print out (can also be named arguments to \code{message}/\code{cat},
#'   e.g. \code{sep}, which defaults to " ").
#' @param useMsg Whether to use \code{message} to print the text (uses \code{cat} otherwise).
#' @param printOut Whether to print the value out (might just want the colored string raw).
#' 
#' @return The colored string, invisibly.
#' @export
#' 
#' @examples
#' info("hello world")
#' 
info <- function(..., useMsg = TRUE, printOut = TRUE) {
  colStr <- crayon::make_style("#00FF00")(...)
  if (printOut) {
    if (useMsg) message(colStr) else cat(colStr, "\n")
  }
  return(invisible(colStr))
}
