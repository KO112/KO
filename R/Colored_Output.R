#' Print out Some Information
#' 
#' Used to print out some information during the execution of a function.
#' This function prints out the text in green (#00FF00) using the \code{crayon} package.
#' 
#' @param str The text to print out (character scalar/vector).
#' @param useMsg Whether to use \code{message} to print the text (uses \code{cat} otherwise).
#' 
#' @return The colored string, invisibly.
#' @export
#' 
#' @examples
#' info("hello world")
#' 
info <- function(..., useMsg = TRUE) {
  colStr <- crayon::make_style("#00FF00")(...)
  if (useMsg) message(colStr) else cat(colStr)
  return(invisible(colStr))
}
