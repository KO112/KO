# Include other functions in package
#' @include Pipes.R
NULL

# Avoid "undefined variable" notes in package checking
globalVariables(c("Quantiles"))


#' Count Distinct Values
#' 
#' Count the number of distinct values in a data frame/matrix/vector.
#' If a data frame or matrix is passed, counts the number of distinct values by column.
#' If a vector is passed, just counts the number of distinct values.
#' 
#' @param data A data frame/matrix/vector.
#' @param use_base Whether to use base \code{R} functions (i.e. \code{length/distinct})
#'   or \code{dplyr::n_distinct}, which seems to have some performance bugs.
#' 
#' @return Integer vector, named for data frames or matrixes with column names.
#' @export
#' 
#' @examples
#' count_distinct(mtcars)
#' count_distinct(as.matrix(mtcars))
#' count_distinct(mtcars$mpg)
#' 
count_distinct <- function(data, use_base = TRUE) {
  
  # Set the function to count distinct values with
  count_fn <- if (use_base) function(x) length(unique(x)) else dplyr::n_distinct
  
  # Take action based on the type of object passed, and print an error message for an invalid object
  if (inherits(data, "data.frame")) {
    return(sapply(data, count_fn))
  } else if (inherits(data, "matrix")) {
    return(apply(data, 2, count_fn))
  } else if (is.atomic(data)) {
    return(count_fn(data))
  } else {
    stop("Unknown data type passed. Unable to calculate counts.")
  }
  
}


#' Tabulate a Data Frame/Matrix
#' 
#' Calls the table function on each column in a data frame/matrix,
#'   but only for columns with up to a certain number of unique values.
#' 
#' @param data A data frame/matrix.
#' @param max_distinct Maximum number of unique elements to return ungrouped (integer).
#' @param group Whether or not to group columns with more than \code{max_distinct} (logical) (unimplemented).
#' @param useNA Value to pass to table to determine whether or not to use NA values
#'   (one of "no", "ifany", and "always").
#' @param dnn Value to be passed to the \code{table} function (dimension names of the resulting table).
#' 
#' @return List of tables.
#' @export
#' 
#' @examples
#' table_df(mtcars)
#' table_df(as.matrix(mtcars))
#' table_df(mtcars$mpg)
#' table_df(c(1, 2, 3, NA, NA))
#' table_df(c(1, 2, 3, NA, NA), useNA = "no")
#' table_df(c(1, 2, 3, NA, NA), useNA = "always")
#' 
table_df <- function(data, max_distinct = 250, group = TRUE, useNA = "ifany", dnn = NULL) {
  
  # Count the number of distinct values in the column
  num_distinct <- count_distinct(data)
  
  # Take action depending on the object passed
  if (inherits(data, "data.frame")) {
    retVal <- sapply(data, table, useNA = useNA, dnn = dnn)
  } else if (inherits(data, "matrix")) {
    retVal <- apply(data, 2, table, useNA = useNA, dnn = dnn)
  } else if (is.atomic(data)) {
    retVal <- table(data, useNA = useNA, dnn = dnn)
  } else {
    stop("Unknown data type passed. Unable to calculate counts.")
  }
  
  # Return the desired object
  return(retVal)
  
}


#' Statistical Mode
#' 
#' Ignoring NAs, find the most common element in a vector (statistical mode).
#' If multiple elements are tied for the mode,
#'   the last element in sorted order will be returned,
#'   unless all elements are specifically requested,
#'   in which case a vector of all modes will be returned.
#' 
#' @param vec Atomic vector.
#' @param return_all_modes Whether or not to return a vector of all modes (for ties) (logical scalar).
#' @param ignoreBlanks Whether or not to ignore blanks (i.e. NA's and empty strings ("")) (logical scalar).
#' 
#' @return Scalar of same type as \code{vec},
#'   or a vector of the same type if there are multiple modes,
#'   and \code{return_all_modes} is \code{TRUE}.
#' @export
#' 
#' @examples
#' mode_stat(c(1, 1, 1, 2, 2))
#' mode_stat(c("a", "b", "b", "b", "c"))
#' mode_stat(c("a", "b"))
#' mode_stat(c("a", "b"), return_all_modes = TRUE)
#' 
mode_stat <- function(vec, return_all_modes = FALSE, ignoreBlanks = FALSE) {
  
  # Remove blanks if desired, & tabulate the vector
  if (ignoreBlanks) vec <- vec[!is.na(vec) & (vec != "")]
  vec_table <- table(vec, useNA = "no")
  
  # Return either all the modes, or just the final one
  if (return_all_modes) {
    retVal <- vec_table %>% .[which(. == max(.))]
  } else {
    retVal <- vec_table %>% sort() %>% utils::tail(1)
  }
  
  # Get the names, convert them to the proper class, & return them
  return(names(retVal) %>% as.vector(mode = class(vec)))
  
}


#' Summarize a Data Frame/Matrix/Vector
#' 
#' Takes in a data frame/matrix/vector and calculates summary statistics based on the type.
#' If \code{get_table} is \code{TRUE}, returns a table of counts as well.
#' 
#' @param vec Atomic vector.
#' @param get_table Whether to tabulate the vector as well.
#' @param useNA Value to pass to table to determine whether or not to use NA values
#'   (one of "no", "ifany", and "always").
#' @param as_vec Whether to return the values as a named vector instead of a list (logical).
#' 
#' @return List of summary statistics.
#' @name summarize
#' @export
#' 
#' @examples
#' summarize_vec(mtcars$mpg)
#' summarize_vec(mtcars$mpg, as_vec = TRUE)
#' summarize_vec(mtcars$mpg, get_table = TRUE)
#' summarize_vec(rownames(mtcars))
#' summarize_vec(c(1, 1, 1, 2, 2, NA), get_table = TRUE)
#' summarize_vec(c(1, 1, 1, 2, 2, NA), get_table = TRUE, useNA = "no")
#' summarize_vec(sample(c(TRUE, FALSE), 10, replace = TRUE))
#' summarize_vec(sample(c(TRUE, FALSE), 10, replace = TRUE), as_vec = TRUE)
#' 
summarize_vec <- function(vec, as_vec = FALSE, get_table = FALSE, useNA = "ifany") {
  
  # Check that the argument is a vector
  stop_if(!is.vector(vec), "Only vectors can be summarized.")
  
  # Take actions based on the type of vector
  if (is.numeric(vec)) {
    
    # Calculate various numeric summary stats
    smy <- dplyr::lst(
      Mean = mean(vec, na.rm = TRUE)
      , Var = stats::var(vec, na.rm = TRUE)
      , Quantiles = stats::quantile(vec, na.rm = TRUE)
      , Min = unname(Quantiles[1])
      , Median = unname(Quantiles[3])
      , Max = unname(Quantiles[5])
      , IQR = unname(Quantiles[4] - Quantiles[2])
    )
    
    # Tabulate the vector if desired
    if (get_table) smy %<>% c(Table = list(table(vec, useNA = useNA)))
  
  } else if (is.character(vec)) {
    
    # Get the mode of the vector, and tabulate if desired
    smy <- list(
      Mode = mode_stat(vec)
    )
    if (get_table) smy %<>% c(Table = list(table(vec, useNA = useNA)))
    
  } else if (is.logical(vec)) {
    
    # Count how many elements are true, and how many are false
    smy <- list(
      True = sum(vec, na.rm = TRUE)
      , False = sum(!vec, na.rm = TRUE)
    )
    
  # } else if (is.list(vec)) {
  #   smy <- list(
  #
  #   )
    
  } else {
    smy <- summary(vec)
  }
  
  # Add the number of distinct and NA values
  smy %<>% c(
    Num_Distinct = dplyr::n_distinct(vec)
    , Num_Not_NA = sum(!is.na(vec))
    , Num_NA = sum(is.na(vec))
  )
  
  # Return the summary statistics calculated above, either as a named vector, or as a list
  if (as_vec) return(unlist(smy))
  return(smy)
  
}


#' @param data A data frame/matrix/vector.
#' 
#' @return List of lists of summary statistics.
#' @rdname summarize
#' @export
#' 
#' @examples
#' summarize_df(mtcars)
#' summarize_df(mtcars, as_vec = TRUE)
#' summarize_df(mtcars, get_table = TRUE)
#' 
summarize_df <- function(data, as_vec = FALSE, get_table = FALSE, useNA = "ifany") {
  
  # Summary of table, i.e. number of numeric/character columns, nrows, ncols, etc.
  
  # Take action depending on the object passed
  if (inherits(data, "data.frame")) {
    return(sapply(data, summarize_vec, as_vec = as_vec, get_table = get_table, useNA = useNA))
  } else if (inherits(data, "matrix")) {
    return(apply(data, 2, summarize_vec, as_vec = as_vec, get_table = get_table, useNA = useNA))
  } else if (is.atomic(data)) {
    return(summarize_vec(data, as_vec = as_vec, get_table = get_table, useNA = useNA))
  } else {
    stop("Unknown data type passed. Unable to summarize data.")
  }
  
}
