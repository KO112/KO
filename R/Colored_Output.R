#' Print out some Information
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
#' @name colored_output
#' 
#' @examples
#' info("hello world")
#' 
info <- function(..., useMsg = TRUE, printOut = TRUE) {
  colStr <- crayon::make_style("#00FF00", colors = 256)(...)
  if (printOut) {
    if (useMsg) message(colStr) else cat(colStr, "\n")
  }
  return(invisible(colStr))
}



#' Print a (Simple) List with Color
#' 
#' Used to print out a simple list, with color, and aligned values.
#' 
#' @param vec Vector to print (preferably a list).
#' @param cols Colors to use (vector of three strings that \code{crayon::make_style} accepts).
#' 
#' @return The colored output lines, invisibly.
#' @export
#' @rdname colored_output
#' 
#' @examples
#' color_list(list(apple = "banana", cherry = "orange", peach = "pineapple"))
#' 
color_list <- function(vec, cols = c("#FF00FF", "#FF8800", "#00FF00"), printOut = TRUE) {
  
  # Coerce the vector to a list, get the names, & calculate the number of characters in each name
  vecList <- as.list(vec)
  vecNamesChars <- nchar(names(vecList))
  
  # Reset the number of colors available if need be
  # if (crayon::num_colors() != 256) crayon::num_colors(forget = TRUE)
  
  # Create the colored indexes, names, padding, and element vectors
  vecIndexes <- crayon::make_style(cols[1], colors = 256)(seq_along(vecList), ":", sep = "")
  vecNames <- crayon::make_style(cols[2], colors = 256)(names(vecList), ":", sep = "")
  vecPadding <- strrep(" ", max(vecNamesChars) - vecNamesChars)
  vecElems <- crayon::make_style(cols[3], colors = 256)(unlist(vecList))
  
  # Create the output lines, cat the out if desired, & return them insivibly
  outLines <- paste(vecIndexes, "\t", vecNames, vecPadding, "\t", vecElems, collapse = "\n")
  if (printOut) cat(outLines)
  return(invisible(outLines))
  
}
