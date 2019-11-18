# Include other functions in package
#' @importFrom dplyr select
NULL


#' Select Columns of a Matrix
#' 
#' Select columns of a matrix by name, \code{dplyr} style.
#' Defines the \code{dplyr::select} method for matrixes.
#'
#' @param .data The matrix to select columns from.
#' @param ... The column names to select (can be quoted, but not necessary).
#' @param throwError whether to throw an error if invalid columns are selected
#'   (if \code{FALSE}), only a warning will be sent) (logical scalar).
#'
#' @return A matrix of the selected columns.
#' @export
#'
#' @examples
#' library(dplyr)
#' mat <- as.matrix(mtcars)
#' select(mat, mpg)
#' select(mat, mpg, cyl)
#' select(mat, mpg, cyl, "disp")
#' select(mat, "mpg", "cyl", "disp", "am")
#' try(select(mat, mpg2))
#' try(select(mat, mpg2, throwError = TRUE))
#' select(mat, mpg2, throwError = FALSE)
#' 
select.matrix <- function(.data, ..., throwError = TRUE) {
  
  # Turn the arguments into characters, get the column names from the data, & find valid/invalid column names
  args <- sapply(rlang::enquos(...), rlang::quo_name)
  colNames <- colnames(.data)
  validCols <- intersect(args, colNames)
  invalidCols <- setdiff(args, colNames)
  
  # If there are any invalid columns selected, display a message, & show a warning or throw an error
  if (length(invalidCols) > 0) {
    cat(crayon::make_style("#FFFF00", colors = 256)("`select.matrix`: Invalid columns selected:\n"))
    vec_print(invalidCols, color = "#FFFF00")
    (if (throwError) stop else warning)("`select.matrix`: Invalid columns selected.")
  }
  
  # Return the valid columns as a matrix
  return(.data[, validCols, drop = F])
  
}
