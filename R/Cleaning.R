#' Clean Names
#' 
#' Clean a character vector of the names of an object so that quotes/backquotes
#'   will not be necessary when subsetting the object.
#' The names input will be cleaned by replacing all occurrences of:
#' \itemize{
#'   \item \code{#} with \code{"Number"} (or \code{"Num"}, if \code{short})
#'   \item \code{$} with \code{"Dollar"} (or \code{"Dol"}, if \code{short})
#'   \item \code{\%} with \code{"Percent"} (or \code{"Perc"}, if \code{short})
#'   \item Any other non-word character (i.e. character not in \code{[a-z, A-Z, 0-9, _]}) with \code{"_"}
#'   \item Multiple consecutive underscores with a single underscore
#'   \item Starting or ending underscores are removed
#' }
#' 
#' @param vec A character vector of names to clean.
#' @param short Whether to shorten certain cleaned name replacements (logical scalar).
#' @param number What to replace a \code{#} with.
#' @param dollar What to replace a \code{$} with.
#' @param percent What to replace a \code{\%} with.
#' 
#' @return A character vector of cleaned names.
#' @name clean_names
#' @export
#' 
#' @examples
#' 
#' clean_names(c(
#'   "apple", "banana", "ab.cd", "ef_gh", "ij/kl", "a_%_b", "a_$_b",
#'   "mn__op", "_A``~@!#-$-%^&*()-_=[]{};:,<.>/?'\"A_"
#' ))
#' clean_names(c("a_#_b", "a_$_b", "a_%_b"), short = FALSE)
#' 
#' c("A()B" = 1, "_C_-+=D_" = 2) %>% stats::setNames(clean_names(names(.)))
#' 
clean_names <- function(
  vec, short = FALSE,
  number = ifelse(short, "Num", "Number"),
  dollar = ifelse(short, "Dol", "Dollar"),
  percent = ifelse(short, "Perc", "Percent")
) {
  
  # Clean the names, replacing all potentially problematic characters
  cleanNames <- stringr::str_replace_all(vec, c(
    "[#]" = number
    , "[$]" = dollar
    , "[%]" = percent
    , "\\W" = "_"
    , "_+" = "_"
    , "^_+|_+$" = ""
  ))
  
  # If any names are duplictates, print a warning, & make the names unique
  dups <- duplicated(cleanNames)
  if (any(dups)) {
    warning("Some duplicate names were encountered: ", paste0(cleanNames[dups]))
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
#' @return \code{clean_col_names.data.frame}: the input object, with column names cleaned and modifed by reference.
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
  
  # Get the cleaned names, & update them in the object
  cleanNames <- clean_names(colnames(object), short)
  data.table::setnames(object, colnames(object), cleanNames)
  
}


#' @return \code{clean_col_names.matrix}: the input object, with column names cleaned and modifed.
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
clean_col_names.matrix <- function(object, short = FALSE, ...) {
  
  # Get the cleaned names, & update them in the object
  cleanNames <- clean_names(colnames(object), short)
  colnames(object) <- cleanNames
  return(object)
  
}
