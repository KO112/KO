# Include other functions in package
#' @importFrom crayon bold
NULL


#' Set Comparison
#' 
#' Compare two sets.
#' 
#' @param set1 The first set to compare.
#' @param set2 The second set to compare.
#' @param compNames Whether to compare the names of the sets, or the sets themselves (logical scalar).
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
#' comp <- set_comp(c(a = 1, b = 2, c = 3), c(a = 1, b = 2, d = 4))
#' 
set_comp <- function(set1, set2, compNames = FALSE) {
  
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
  
  # Return the differences, invisibly
  comp <- dplyr::lst(
    set1 = sort(unique(set1)), set2 = sort(unique(set2)),
    set1Expr, set2Expr, compNames,
    num1 = length(set1), num2 = length(set2),
    setInt = intersect(set1, set2), setUnion = union(set1, set2),
    setDiff1 = setdiff(set1, set2), setDiff2 = setdiff(set2, set1)
  )
  class(comp) <- c("set_comp", class(comp))
  return(comp)
  
}


#' @param x A \code{set_comp} object.
#' @param ... Additional arguments passed to the print function (not used).
#' @param indent The indent argument to pass to the \code{KO::vec_print} function.
#' @param color The color argument to pass to the \code{KO::vec_print} function.
#' @param printSets Whether to print the original sets (logical scalar).
#' @param printInt Whether to print the intersection of the sets (logical scalar).
#' @param printUnion Whether to print the union of the sets (logical scalar).
#' @param printDiffs Whether to print the differences (logical scalar).
#'
#' @return The input, invisibly.
#' @rdname set_comp
#' @export
#' 
#' @examples
#' set_comp(1:10, 6:15)
#' print(set_comp(1:10, 6:15))
#' print(set_comp(1:10, 6:15), indent = 2, color = "#FFFF00")
#' print(set_comp(1:10, 6:15), printSets = TRUE, printInt = FALSE,
#'       printUnion = TRUE, printDiffs = FALSE)
#' 
print.set_comp <- function(x, ..., indent = 4, color = "#CCCCCC", printSets = FALSE, printInt = TRUE, printUnion = FALSE, printDiffs = TRUE) {
  
  # Create styling functions
  vec_print_fn <- function(x) vec_print(x, indent = indent, color = color, ...)
  expr <- function(..., sep = "") crayon::make_style("#00AAFF", colors = 256)(..., sep = sep)
  green <- function(..., sep = "") crayon::make_style("#00FF00", colors = 256)(..., sep = sep)
  cgreen <- function(..., sep = "") cat(green(..., sep = sep))
  diffHeaderText <- function(expr1, expr2, numElem, sep = "") {
    cgreen(sep = sep,
      bold(numElem), " elements are in ", bold("set 1"), " but not in ", bold("set 2"),
      " (comparing ", bold(ifelse(x$compNames, "names", "values")), "):\n"
    )
  }
  
  # Print out some set information
  cgreen(
    bold("Set 1"), " (", expr(x$set1Expr), ") has ", bold(x$num1), " unique elements, and ",
    bold("set 2"), " (", expr(x$set2Expr), ") has ", bold(x$num2), ".\n",
    "The ", bold("intersection"), " has ", bold(length(x$setInt)), " elements, ",
    "and the ", bold("union"), " has ", bold(length(x$setUnion)), " total elements.\n"
  )
  if (printSets) {
    cgreen(bold("Set 1:\n")); vec_print_fn(x$set1)
    cgreen(bold("Set 2:\n")); vec_print_fn(x$set2)
  }
  
  # If desired, print out the intersection & union
  if (printInt) {
    cgreen(bold(length(x$setInt)), " elements are in ", bold("both"), " sets:\n")
    vec_print_fn(x$setInt)
  }
  if (printUnion) {
    cgreen(bold(length(x$setUnion)), " elements are in the ", bold("union"), " of the sets:\n")
    vec_print_fn(x$setUnion)
  }
  
  # If desired, print out the differences
  if (printDiffs) {
    diffHeaderText(x$set1Expr, x$set2Expr, length(x$setDiff1)); vec_print_fn(x$setDiff1)
    diffHeaderText(x$set2Expr, x$set1Expr, length(x$setDiff2)); vec_print_fn(x$setDiff2)
  }
  
  # Return the input object, invisibly
  return(invisible(x))
  
}
