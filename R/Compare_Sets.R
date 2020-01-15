#' Set Comparison
#' 
#' Compare two sets, printing out the differences if desired.
#'
#' @param set1 The first set to compare.
#' @param set2 The second set to compare.
#' @param compNames Whether to compare the names of the sets, or the sets themselves (logical scalar).
#' @param printOut Whether to print out the differences (logical scalar).
#'
#' @return A two-element list of the set differences.
#' @export
#'
#' @examples
#' set_comp(1:10, 6:15)
#' set_comp(mtcars[, -1], mtcars[, -2])
#' set_comp(c(a = 1, b = 2, c = 3), c(a = 1, b = 2, d = 4))
#' set_comp(c(a = 1, b = 2, c = 3), c(a = 1, b = 2, d = 4), compNames = FALSE)
#' set_comp(c(a = 1, b = 2, c = 3), c(a = 1, b = 2, d = 4), compNames = TRUE)
#' comp <- set_comp(c(a = 1, b = 2, c = 3), c(a = 1, b = 2, d = 4), printOut = FALSE)
#' 
set_comp <- function(set1, set2, compNames = FALSE, printOut = TRUE) {
  
  # Get the set expressions
  set1Expr <- gsub("[ ]+", " ", paste0(deparse(substitute(set1)), collapse = ""))
  set2Expr <- gsub("[ ]+", " ", paste0(deparse(substitute(set2)), collapse = ""))
  if (nchar(set1Expr) > 75) set1Expr <- paste0(substr(set1Expr, 1, 50), "...")
  if (nchar(set2Expr) > 75) set2Expr <- paste0(substr(set2Expr, 1, 50), "...")
  
  # Check that the classes are compatible
  if (!any(class(set1) %in% class(set2))) stop(
    "The classes of set 1 ([", paste0(class(set1), collapse = ", "),
    "]) and set 2 ([", paste0(class(set2), collapse = ", "), "]) are incompatible."
  )
  
  # If the user didn't supply `compNames`, check for names automatically
  if (missing(compNames)) {
    
    # If the sets aren't atomic vectors, & they have names, use the names
    if (!is.atomic(set1) & !is.atomic(set2) & !is.null(set1) & !is.null(set2)) compNames <- TRUE
    
  }
    
  # Compare the names of the sets, if desired
  if (compNames) {
    set1 <- names(set1)
    set2 <- names(set2)
  }
  
  # Calculate the set differences
  diff1 <- dplyr::setdiff(set1, set2)
  diff2 <- dplyr::setdiff(set2, set1)
  
  # Create styling functions
  headerText <- function(expr1, expr2, numElem, sep = "") crayon::make_style("#00FF00", colors = 256)(
    sep = sep, numElem, " elements in `", expr1, "` but not in `", expr2, "` (comparing ", crayon::bold(ifelse(compNames, "names", "values")), "):"
  )
  # headerText <- function(..., sep = "") crayon::underline(..., sep = sep)
  elementText <- function(..., sep = "") crayon::make_style("#00BBFF", colors = 256)(..., sep = sep)
  # elementText <- function(..., sep = "") paste0(..., sep = sep)
  
  # Print out the differences
  if (printOut) {
    cat(sep = "", headerText(set1Expr, set2Expr, length(diff1)), "\n\t", elementText("[", paste0(diff1, collapse = ", "), "]"), "\n")
    cat(sep = "", headerText(set2Expr, set1Expr, length(diff2)), "\n\t", elementText("[", paste0(diff2, collapse = ", "), "]"), "\n")
  }
  
  # Return the differences, invisibly
  return(invisible(list(diff1 = diff1, diff2 = diff2)))
  
}

# set_comp(1:10, 6:15)
# set_comp(mtcars[, -1], mtcars[, -2])
# set_comp(c(a = 1, b = 2, c = 3), c(a = 1, b = 2, d = 4))
# set_comp(c(a = 1, b = 2, c = 3), c(a = 1, b = 2, d = 4), compNames = FALSE)
# set_comp(c(a = 1, b = 2, c = 3), c(a = 1, b = 2, d = 4), compNames = TRUE)
# comp <- set_comp(c(a = 1, b = 2, c = 3), c(a = 1, b = 2, d = 4), printOut = FALSE)
