# To-Do
#   Make class for single column summary, and a list of them
#   Override `[.dataDict` with special handler for table being NULL
#   Add lazy loading ability, maybe add attributes (logical vector of colum names?)?
#   For printing, change from tibble to something easier to read in bulk, maybe multiple columns


#' Create a Data Dictionary
#' 
#' Create a data dictionary object which holds information on the columns of a data.frame-like object.
#' 
#' Lazy table mode means that columns are not tabulated until requested.
#' When they are tabulated, they can be done one at a time, or all together.
#' This setting is useful since this can take a long time for large datasets.
#'
#' @param df The data.frame-like object to calculate information for.
#' @param table Whether or not to tabulate each column (logical scalar).
#' @param lazyTable Whether or not to enable lazy table mode (see below for more) (logical scalar).
#' @param verbose How verbose you want the function to be (higher prints more information) (integer scalar).
#' 
#' @return A \code{dataDict} object.
#' @export
#'
#' @examples
#' dd <- dataDict(mtcars)
#' dd <- dataDict(mtcars, table = TRUE)
#' dd <- dataDict(mtcars, table = TRUE, lazyTable = FALSE)
#' dd <- dataDict(mtcars, table = TRUE, lazyTable = FALSE, verbose = 0)
#' 
dataDict <- function(df, table = FALSE, lazyTable = TRUE, verbose = Inf) {
  
  # Ensure that we have a data.frame-like object
  if (!inherits(df, "data.frame")) stop("dataDict: 'df' must inherit from a 'data.frame'.")
  
  # Create the data dictionary object, & set the name of the original table
  dict <- new.env()
  dict$origDFName <- deparse(substitute(df))
  
  # Add various values to the dictionary
  evalq(envir = dict, expr = {
    
    # Set the name of the environment holding the data
    origDFEnv <- try(pryr::where(origDFName))
    
    # Set the dimensions of the original df object
    dims <- dim(df)
    nrow <- nrow(df)
    ncol <- ncol(df)
    
    # Set the column names & class of the original df object, & the classes of each column
    colNames <- colnames(df)
    dfClass <- class(df)
    classes <- sapply(df, class)
    
    # Get the number of unique elements
    numUnique <- sapply(df, function(x) length(unique(x)))
    
    # Tabulate the results, if desired, setting the 'table' attribute
    if (lazyTable) {
      if (verbose > 0) message("dataDict: Lazy table mode has been activated.")
      attr(dict, "table") <- "lazy"
      colTables <- NULL
    } else if (table) {
      attr(dict, "table") <- TRUE
      colTables <- sapply(df, table, useNA = "ifany", dnn = NULL, simplify = FALSE)
    } else {
      attr(dict, "table") <- FALSE
      colTables <- NULL
    }
    
  })
  
  # Set the dataDict class, & return it
  class(dict) <- c("dataDict") # , class(dict))
  return(dict)
  
}


#' Print a \code{dataDict} Object
#'
#' @param dict A \code{dataDict} object.
#'
#' @examples
#' dataDict(mtcars)
#' print(dataDict(mtcars))
#' 
print.dataDict <- function(dict) {
  # browser()
  # Print some information about the data that the dict is based off of
  dictEnv <- dict$origDFEnv
  cat(
    "The '", deparse(substitute(dict)), "' 'dataDict' object was based off of '", dict$origDFName,
    "' (a '", dict$dfClass[1], "') ", # "in the '", pryr::address(dictEnv), "' environment,\n",
    "which had ", dict$nrow, " rows, and ", dict$ncol, " columns.\n",
    sep = ""
  )
  
  # Print the column names, classes, and number of unique elements
  print(tibble::tibble(
    Column = dict$colNames
    , Class = dict$classes
    , NumUnique = dict$numUnique
  ))
  
}


#' Extract Elements from a \code{dataDict} Object
#'
#' @param dict A \code{dataDict} object.
#' @param elem The element to extract (character scalar).
#' @param col If \code{elem == "colTables"}, the column(s) to extract.
#'
#' @return The desired element extracted from the \code{dataDict} object.
#'
#' @examples
#' dd <- dataDict(mtcars)
#' dd["ncol"]
#' dd["numUnique"]
#' dd["colTables"]
#' dd["colTables", NA]
#' dd["colTables", "mpg"]
#' dd["colTables", c("mpg", "cyl")]
#' 
#' dd2 <- dataDict(mtcars, table = TRUE, lazyTable = FALSE)
#' dd2["colTables", NA]
#' dd2["colTables", "mpg"]
#' dd2["colTables", c("mpg", "cyl")]
#' 
`[.dataDict` <- function(dict, elem, col = NULL) {
  
  # Either throw an error if the element doesn't exist, or try to return the desired element
  if (!exists(elem, dict)) {
    
    # If the element doesn't exist, print a warning, & return nothing
    warning("`[.dataDict`: Element '", elem, "' does not exist in '", deparse(substitute(dict)), "'.")
    return(NULL)
    
  } else if (elem == "colTables") {
    
    # If the element requested is 'colTables', either return it, throw an error if it wasn't calculated, or calculate it
    if (isTRUE(attr(dict, "table"))) {
      
      # If col is NULL or NA, return all the tabulated columns, or just the desired ones,
      #   or throw an error if some column names aren't found
      if (is.null(col) || is.na(col)) {
        return(dict$colTables)
      } else if (all(col %in% names(dict$colTables))) {
        return(dict$colTables[col])
      } else {
        warning("`[.dataDict`: Columns not found in the dataDict: ",
                paste0(col[!(col %in% names(dict$colTables))], collapse = ", "))
        return(dict$colTables[col[col %in% names(dict$colTables)]])
      }
      
    } else if (isFALSE(attr(dict, "table"))) {
      
      # If the tables were disabled, print a warning, and return nothing
      warning("`[.dataDict`: The columns were not tabulated for this dataDict object.\n",
              "Please re-run with `dataDict(", dict$origDFName, ", table = TRUE)` or ",
              "`dataDict(", dict$origDFName, ", lazyTable = TRUE)` if you want to access this field.")
      return(NULL)
      
    } else {
      
      # If the original object that this dataDict was based off of still exists, get it, else throw an error
      if ((!is.null(dict$origDFEnv)) && exists(dict$origDFName, where = dict$origDFEnv)) {
        df <- get(dict$origDFName, dict$origDFEnv)
      } else {
        stop(
          "`[.dataDict`: The object that this 'dataDict' (", deparse(substitute(dict)), ") was based off (",
          dict$origDFName, ") no longer exists in its original environment (", dict$origDFEnv, ").\n",
          "Please update the reference using updateDD(", deparse(substitute(dict)), "df)."
        )
      }
      
      # Either throw an error if no column was specified, return all the tabulated columns, or calculate them & save the results
      if (is.null(col)) {
        
        # If no column is specified, print a warning, & return nothing
        warning("`[.dataDict`: Lazy table mode is active, so you must specify a column name (or 'NA' to get all the columns.")
        return(NULL)
        
      } else if (any(is.na(col))) {
        
        # Return all the tabulated columns, tabulating & saving them if need be
        if (length(dict$colTables) == dict$ncol) {
          return(dict$colTables)
        } else {
          return(dict$colTables <- sapply(df, table, useNA = "ifany", dnn = NULL, simplify = FALSE))
        }
        
      } else if (!all(col %in% dict$colNames)) {
        
        # If any of the columns don't exist in the data, throw an error
        # stop("`[.dataDict`: Column '", col, "' doesn't exist in the data.")
        warning("`[.dataDict`: Columns not found in the data: ",
                paste0(col[!(col %in% names(dict$colNames))], collapse = ", "))
        
      } else if (all(col %in% names(dict$colTables))) {
        
        # If the columns have already been tabulated, return them
        return(dict$colTables %>% .[names(.) %in% col])
        
      } else {
        
        # Else tabulate the desired columns, & save them
        colTable <- sapply(df[col], table, useNA = "ifany", dnn = NULL, simplify = FALSE)
        dict$colTables <- c(dict$colTables, colTable) # list(colTable) %>% setNames(col))
        return(colTable)
        
      }
      
    }
    
  } else (
    
    # Simply return the element
    return(get(x = elem, envir = dict))
    
  )
  
}


# Set `[[` to do the same as `[`
# `[[.dataDict` <- `[.dataDict`


#' Update a \code{dataDict} Object
#'
#' @param dict A \code{dataDict} object.
#' @param df The data.frame-like object to calculate information for.
#'
#' @export
#'
#' @examples
#' 
updateDD <- function(dict, df) {
  
}
