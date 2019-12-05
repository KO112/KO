#' Extract Multi-Valued Columns
#' 
#' Extract columns from a data frame that have more than one unique value.
#' 
#' @param df An object inheriting from a \code{data.frame} (e.g. \code{data.table}/\code{tibble}).
#' 
#' @return The columns of \code{df} that have more than one unique value.
#' @export
#' 
#' @examples
#' multi_val_cols(data.frame(a = c(1, 1, 1), b = c("a", "a", "a")))
#' multi_val_cols(data.frame(a = c(1, 1, 1), b = c(1, 2, 3),
#'                           c = c('a', 'a', 'a'), d = c('a', 'a', 'b')))
#' multi_val_cols(data.frame(a = c(1, 1, 3), b = c(1, 2, 3),
#'                           c = c('a', 'b', 'a'), d = c('a', 'a', 'b')))
#' 
multi_val_cols <- function(df) {
  
  # Check that the object inherits from a data.frame
  if (!is.data.frame(df)) stop(
    "`df` must inherit from a data.frame, but the class of the object passed is [",
    paste0(class(df), collapse = ", "), "]"
  )
  
  # Extract the columns with more than one unique value
  `[.data.frame`(df, purrr::map_lgl(df, ~ length(unique(.x)) > 1))
  
}
