#' Prepare the Rows of the Output
#'
#' @return The output rows vector.
#' @rdname vecPrint
#'
makeOutputRows <- function(vec, numCols, maxChars) {
  
  # Find the number of elements per column, & pad the vector with blanks
  perCol <- ceiling(length(vec) / numCols)
  vec <- c(vec, rep("", numCols * perCol - length(vec)))
  
  # Create the padded column matrix
  paddedColsMatrix <- apply(matrix(vec, ncol = numCols), 2, function(x) {
    trimmed <- paste0(substr(x, 1, maxChars), ifelse(nchar(x) > maxChars, "~", ""))
    padded <- paste0(trimmed, strrep(" ", max(nchar(trimmed)) - nchar(trimmed)))
    return(padded)
  })
  
  # Make the strings for each row, & return it
  rowStrs <- apply(paddedColsMatrix, 1, paste0, collapse = " | ")
  return(rowStrs)
  
}


#' Print out a Vector in Columns
#' 
#' Prints out a vector columnwise, using the full width available.
#' This is usually called for its side effects, but will also return the (unformatted) output rows vector.
#'
#' @param vec The (atomic) vector to print out.
#' @param maxLen The maximum length of a string to print out (integer scalar).
#' @param printOut Whether to print out the formatted output (logical scalar).
#'
#' @return The final output row vector.
#' @name vecPrint
#' @export
#'
#' @examples
#' 
vecPrint <- function(vec, maxLen, printOut = TRUE) {
  
  # Ensure that the input is atomic
  if (!is.atomic(vec)) stop("`vecPrint`: `vec` must be atomic, not of class `", class(vec), "`.")
  
  # Calculate maximum number of characters, & guess the number of columns we need
  maxChars <- min(maxLen, max(nchar(vec)))
  numCols <- floor(getOption("width") / (maxChars + 4))
  
  # Iterate to find the maximum number of columns that fit
  numColsIter <- numCols
  while (nchar(makeOutputRows(vec, numColsIter, maxChars)[1]) < getOption("width")) numColsIter <- numColsIter + 1
  numColsIter <- numColsIter - 1
  
  # Get the final output rows vector, & format it for printing
  finalOutputRows <- makeOutputRows(vec, numColsIter, maxChars)
  printVec <- crayon::make_style("#00FF00")(finalOutputRows)
  
  # Print out the output, if desired, & return the final output rows vector (invisibly)
  if (printOut) {
    maxPrintRows <- ceiling(getOption("max.print")) # / numColsIter)
    if (length(finalOutputRows) > maxPrintRows * 2) {
      cat(printVec[seq_len(maxPrintRows)], sep = "\n")
      message(" [ reached getOption('max.print') -- omitted ", length(finalOutputRows) - maxPrintRows, " entries ]")
    } else {
      cat(printVec, sep = "\n")
    }
  }
  return(invisible(finalOutputRows))
  
}
