# Include other functions in package
#' @include Pipes.R
NULL


#' \code{cat} Helper Function
#'
#' @param ... Arguments to be passed on to \code{sep}.
#'
#' @export
#'
#' @examples
#' cat(1:10)
#' cat0(1:10)
#' 
cat0 <- function(...) {
  cat(..., sep = "")
}


#' Print out some Information
#' 
#' Used to print out some information during the execution of a function.
#' This function prints out the text in green (#00FF00) using the \code{crayon} package.
#' 
#' @param ... Text to print out (can also be named arguments to \code{message}/\code{cat},
#'   e.g. \code{sep}, which defaults to " ").
#' @param useMsg Whether to use \code{message} to print the text (uses \code{cat} otherwise).
#' @param printOut Whether to print the value out (might just want the colored string raw).
#' @param sep Separator to use in the \code{cat} function (only used when \code{!useMsg}) (character scalar).
#' 
#' @return The colored string, invisibly.
#' @export
#' @name colored_output
#' 
#' @examples
#' info("hello world")
#' 
info <- function(..., useMsg = TRUE, printOut = TRUE, sep = "") {
  colStr <- crayon::make_style("#00FF00", colors = 256)(..., sep = sep)
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
#' color_list(list(apple = "banana", cherry = "orange", peach = "pineapple",
#'            plum = list(plum = "plum")))
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


#' Prepare the Rows of the Output
#' 
#' Used as a helper function in \code{vec_print} to make output row strings.
#'
#' @param numCols The number of columns to use in the output (integer scalar).
#' @param maxChars The maximum number of characters to print for each element of \code{vec} (integer scalar).
#'
#' @return The output rows vector.
#' @rdname vec_print
#'
make_output_rows <- function(vec, numCols, maxChars) {
  
  # If we have one column per element, just concatenate them
  if (numCols >= length(vec)) return(paste0(vec, collapse = " | "))
  
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
#' \code{order} can be one of three values: c("none" (default), "sort", "short"/"shortest")
#' \code{order = "none"} will print out the vector in the same order it was supplied.
#' \code{order = "sort"} will sort the vector before printing (using \code{sort}).
#' \code{order = "short"/"shortest"} will sort the vector according to length,
#'   which will ensure that the printing takes up as little space as possible.
#'
#' @param vec The (atomic) vector to print out.
#' @param indent Number of spaces to indent the output by (integer scalar),
#'   or a string to use as the indent (character scalar).
#' @param maxLen The maximum length of a string to print out (integer scalar).
#' @param printOut Whether to print out the formatted output (logical scalar).
#' @param maxWidth The mxximum width to print out per row (defaults to fill available space) (integer scalar).
#' @param order How to order the \code{vec} before printing (see details for more) (character scalar).
#' @param color The color of the output, which can be any valid input to \code{crayon::make_style}
#'   (e.g. 6/8 digit hex codes, RGB (as a 3 row, 1 column matrix), or an element from \code{colors()}).
#'
#' @return The final output row vector.
#' @name vec_print
#' @export
#'
#' @examples
#' 
#' set.seed(112)
#' fruits <- sample(c("apple", "banana", "cherry", "orange", "pineapple",
#'                    "really very long fruit name"), 100, replace = TRUE)
#' vec_print(fruits)
#' vec_print(fruits, maxLen = 20)
#' vec_print(fruits, maxWidth = 50)
#' vec_print(fruits, order = "sort")
#' vec_print(fruits, order = "shortest")
#' vp <- vec_print(fruits, printOut = FALSE)
#' vp
#' 
#' \dontrun{
#'   
#'   vec_print(lexicon::sw_fry_25)
#'   vec_print(rep(lexicon::sw_fry_25, 2))
#'   vec_print(lexicon::sw_fry_100)
#'   vec_print(lexicon::sw_fry_200)
#'   vec_print(lexicon::sw_fry_1000)
#'   
#'   vec_print(lexicon::sw_fry_200, maxLen = 5)
#'   vec_print(lexicon::sw_fry_200, maxLen = 20)
#'   vec_print(lexicon::sw_fry_200, maxWidth = 50)
#'   vec_print(lexicon::sw_fry_1000, order = "sort")
#'   vec_print(lexicon::sw_fry_1000, order = "shortest")
#'   
#'   vp <- vec_print(lexicon::sw_fry_1000, order = "shortest", printOut = FALSE)
#'   vp
#'   
#' }
#' 
vec_print <- function(vec, indent = 0, maxLen = 20, maxWidth = getOption("width") - indent, order = "none", printOut = TRUE, color = "#00FF00") {
  
  # Ensure that the input is atomic
  if (!is.atomic(vec)) stop("`vec_print`: `vec` must be atomic, not of class `", class(vec)[1], "`.")
  
  # Create the indent to use
  if (is.numeric(indent)) {
    indentStr <- strrep(" ", indent)
  } else if (is.character(indent) & length(indent) > 1) {
    warning()
    indentStr <- indent[1]
  } else if (!is.character(indent)) {
    stop("`indent` must be either numeric (to specify the number of spaces to indent with), ",
         "or a character scalar holding the string used as indentation.")
  }
  
  # Order the vector as desired
  order <- tolower(order)
  if (order == "sort") vec <- sort(vec)
  else if (order %in% c("short", "shortest")) vec <- vec[order(nchar(vec))]
  else if (order != "none") message("`vec_print`: \"", order, "\" is not a valid value for `order`. No sorting will be done.")
  
  # Calculate maximum number of characters, & guess the number of columns we need
  maxChars <- min(maxLen, max(nchar(vec)))
  numCols <- floor(maxWidth / (maxChars + 4))
  
  # Iterate to find the maximum number of columns that fit
  numColsIter <- numCols
  while (make_output_rows(vec, numColsIter, maxChars) %>% {(length(.) > 1) && (nchar(.[1]) <= maxWidth)}) numColsIter <- numColsIter + 1
  if (length(vec) > numColsIter) numColsIter <- numColsIter - 1
  
  # Get the final output rows vector, & format it for printing
  finalOutputRows <- make_output_rows(vec, numColsIter, maxChars) %>% gsub("( \\| )+$", "", .) %>% paste0(indentStr, .)
  printVec <- crayon::make_style(color, colors = 256)(finalOutputRows)
  
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
