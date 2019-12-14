#' Clean Names
#' 
#' Clean names so that quotes (or backquotes) will not be necessary when subsetting the object.\cr
#' The names input will be cleaned by replacing all occurrences of:
#' \itemize{
#'   \item \code{$} with \code{"Dollar"} (or \code{"Dol"}, if \code{short == TRUE})
#'   \item \code{\%} with \code{"Percent"} (or \code{"Perc"}, if \code{short == TRUE})
#'   \item Any other non-word character (i.e. character not in \code{[a-z, A-Z, 0-9, _]}) with \code{"_"}
#' }
#'
#' @param vec A character vector of names to clean.
#' @param short Whether to shorten certain cleaned name replacements (logical scalar).
#'
#' @return A character vector of cleaned names.
#' @name clean_names
#' @export
#'
#' @examples
#' clean_names(c("apple", "banana", "ab.cd", "ef_gh", "ij/kl", "a_%_b", "a_$_b", "mn__op"))
#' clean_names(c("a_%_b", "a_$_b"), short = FALSE)
#' 
clean_names <- function(vec, short = FALSE) {
  
  # Clean the names, replacing all potentially problematic characters
  cleanNames <- stringr::str_replace_all(vec, c(
    "[%]" = ifelse(short, "Perc", "Percent")
    , "[$]" = ifelse(short, "Dol", "Dollar")
    , "\\W" = "_"
    , "_+" = "_"
  ))
  
  # If any names are duplictates, print a warning, & make the names unique
  if (any(duplicated(cleanNames))) {
    warning("Some duplicate names were encountered: ", paste0(cleanNames[duplicated(cleanNames)]))
    cleanNames <- make.unique(names = cleanNames, sep = "_")
  }
  
  # Return the cleaned names
  return(cleanNames)
  
}


#' Clean Column Names
#' 
#' Clean the column names of a \code{data.frame} or \code{data.table}.
#'
#' @param object The object to clean names for.
#' @param ... further arguments passed to or from other methods.
#'
#' @rdname clean_col_names
#' @export
#' 
clean_col_names <- function(object, ...) {
  UseMethod("clean_col_names")
}


#' @param short Whether to shorten certain cleaned name replacements (logical scalar).
#'
#' @return \code{clean_col_names.data.frame}: 
#' @rdname clean_col_names
#' @export
#' 
#' @examples
#' DF <- data.frame(
#'   "a[b.c]d" = 1:26, "e\\f;g1h" = letters,
#'   "i/j k_l+m-n.o(p)q_$_%_kl" = sample(c(TRUE, FALSE), 26, replace = TRUE),
#'   check.names = FALSE
#' )
#' colnames(DF)
#' print(methods("clean_col_names"))
#' DF <- clean_col_names(DF, TRUE)
#' colnames(DF)
#' 
clean_col_names.data.frame <- function(object, short = FALSE, ...) {
  
  # Get the cleaned names, & set them for the `data.frame`
  cleanNames <- clean_names(colnames(object), short)
  return(stats::setNames(object, cleanNames))
  
}


#' @return \code{clean_col_names.data.table}: 
#' @rdname clean_col_names
#' @export
#'
#' @examples
#' DT <- data.table::data.table(
#'   "a[b.c]d" = 1:26, "e\\f;g1h" = letters,
#'   "i/j k_l+m-n.o(p)q_$_%_kl" = sample(c(TRUE, FALSE), 26, replace = TRUE)
#' )
#' colnames(DT)
#' clean_col_names(DT, TRUE)
#' colnames(DT)
#' 
clean_col_names.data.table <- function(object, short = FALSE, ...) {
  
  # Get the cleaned names, & update them in the `data.table`
  cleanNames <- clean_names(colnames(object), short)
  data.table::setnames(object, colnames(object), cleanNames)
  
}
