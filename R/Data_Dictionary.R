# To-Do
#   Make class for single column summary, and a list of them(?)
#   For printing, change from tibble to something easier to read in bulk, maybe multiple columns
#   Add colHash, maybe use constructor for both colTables/colHash


# Include other functions in package
#' @include Pipes.R
#' @include Error_Handling.R
#' @importFrom data.table :=
NULL

# Avoid "undefined variable" notes in package checking
globalVariables(c("dfEnv"))


#' Create a Data Dictionary
#' 
#' Create a data dictionary object which holds information on the columns of a data.frame-like object.
#' 
#' The \code{df} parameter should be the name of a data.frame-like object, as opposed to an expression.
#' E.g. call the function as \code{dataDict(mtcars)}, instead of \code{dataDict(as_tibble(mtcars))}.
#' This will allow for more functionality, and will help avoid errors, especially when lazy table mode is active.
#' If you want to pass an expression, simple ones should work, but do so at your own risk.
#' 
#' The \code{tableMode} parameter can have one of three values: c(TRUE, FALSE, "lazy").
#' Lazy table mode means that columns are not tabulated until requested.
#' When they are tabulated, they can be done one at a time (\code{tableMode = "lazy"}),
#'   or all together (\code{tableMode = true}).
#' This setting can be deactivated entirely (\code{tableMode = FALSE}), but this is not suggesteds.
#' This setting is useful since tabulation can take a long time for large datasets.
#' 
#' If \code{verbose > 0} (or \code{verbose != FALSE}), a message will be printed out when lazy table mode is active,
#'   as well as when \code{df} is passed as an expression.
#' If \code{verbose > 1}, a message will be printed out saying what object the \code{dataDict} is based on.
#' 
#' @param df The data.frame-like object to calculate information for (should be a name, not an expression).
#' @param tableMode What mode to use for tabulating each column (see \code{details} for more) (character/logical scalar).
#' @param verbose How verbose you want the function to be (higher prints more information) (integer scalar).
#' 
#' @return A \code{dataDict} object.
#' @export
#' 
#' @examples
#' dd1 <- dataDict(mtcars)
#' dd2 <- dataDict(mtcars, tableMode = "lazy")
#' dd3 <- dataDict(mtcars, tableMode = TRUE)
#' dd5 <- dataDict(mtcars, tableMode = FALSE)
#' dd4 <- dataDict(mtcars, tableMode = "lazy", verbose = 0)
#' dd4 <- dataDict(mtcars, tableMode = TRUE, verbose = 0)
#' dd4 <- dataDict(mtcars, tableMode = FALSE, verbose = 0)
#' 
#' # The line below works, since the expression is rather simple, but should be avoided
#' # It is better to declare use something like: `df <- as_tibble(mtcars); dd <- dataDict(df)`
#' dd <- dataDict(tibble::as_tibble(mtcars))
#' 
dataDict <- function(df, tableMode = "lazy", verbose = Inf) {
  
  # Ensure that we have a data.frame-like object
  if (!inherits(df, "data.frame")) stop("dataDict: `df`` must inherit from a `data.frame`.")
  
  # Create the data dictionary object, & set the call/verbosity attribute
  dict <- new.env()
  call <- match.call()
  dfCall <- substitute(df)
  
  # Set some attributes of the data dictionary object
  attributes(dict) <- dplyr::lst(
    
    # Set simple attributes (calculated outside)
    verbose, call, dfCall
    
    # Set the environment of the `df` object, & get the environment name/address
    , dfEnv = parent.frame(2)
    , dfEnvName = environmentName(dfEnv) %>% ifelse(. == "", pryr::address(dfEnv), .)
    
    # Get the dimensions/dimension names from the original df object
    , dims = dim(df)
    , dimNames = dimnames(df)
    
  )
  
  # Attempt to set the name of the original data
  if (length(dfCall) > 1) {
    message("`dataDict`: `df` has been passed as an expression (", deparse(dfCall), ").\n", strrep(" ", 12),
            "This may result in problems when using this `dataDict` object, but should be fine.")
    attr(dict, "dfName") <- dfCall[purrr::map_lgl(dfCall, ~ is.data.frame(eval(.x)))][[1]] %>% deparse()
  } else {
    attr(dict, "dfName") <- deparse(dfCall)
  }
  
  # If desired & relevant, print out a message about not changing the original object
  if ((tableMode == "lazy") && (verbose > 1)) message(
    "`dataDict`: This `dataDict` will be based off of the object named '", attr(dict, "dfName"), "'.\n", strrep(" ", 12),
    "To ensure that this `dataDict` will continue to work, do not change the name of the object,\n", strrep(" ", 14),
    "or use the `updateDD` function after the object changes."
  )
  
  # Add various values to the dictionary
  evalq(envir = dict, expr = {
    
    # Set the class of the original df object, & the classes of each column
    dfClass <- class(df)
    classes <- purrr::map_chr(df, class)
    
    # Get the number of unique elements, & the column tables object
    numUnique <- purrr::map_int(df, ~ length(unique(.x)))
    colTables <- columnTables(dict, df, tableMode)
    
  })
  
  # Set the dataDict class, & return it
  class(dict) <- c("dataDict", class(dict))
  return(dict)
  
}


#' Extract Elements from a \code{dataDict} Object
#' 
#' @param dict A \code{dataDict} object.
#' @param elem The element to extract (character scalar).
#' @param cols If \code{elem == "colTables"}, the column(s) to extract (character vector).
#' 
#' @return The desired element extracted from the \code{dataDict} object.
#' @export
#' 
#' @examples
#' dd <- dataDict(mtcars)
#' dd["numUnique"]
#' dd["colTables", "mpg"]
#' dd["colTables", NA]
#' dd["colTables"]
#' 
#' dd2 <- dataDict(mtcars, tableMode = TRUE)
#' dd2["colTables", "mpg"]
#' dd2["colTables", NA]
#' 
#' # The following lines work, but cause automated testing problems
#' # dd["colTables", c("mpg", "cyl")]
#' # dd2["colTables", c("mpg", "cyl")]
#' 
`[.dataDict` <- function(dict, elem, cols = NULL) {
  
  # If more than one element was requested, print a message, & take the first one
  if (length(elem) > 1) {
    warning("`[.dataDict`: `length(elem) > 1`, so only the first one will be used.")
    elem <- elem[1]
  }
  
  # Either throw an error if the element doesn't exist, or try to return the desired element
  if (!exists(elem, dict)) {
    
    # If the element doesn't exist, print a warning, & return nothing
    warning("`[.dataDict`: Element '", elem, "' does not exist in '", deparse(substitute(dict)), "'.")
    return(NULL)
    
  } else if (tolower(elem) == "coltables") {
    
    # Get the desired tables
    return(dict$colTables[cols])
    
  } else if (elem %in% ls(dict)) (
    
    # Return the desired element
    return(get(x = elem, envir = dict))
    
  ) else if (elem %in% colnames(dict)) {
    
    # Return data on the desired column
    return(list(
      class = dict$classes %>% .[names(.) == elem]
      , numUnique = dict$numUnique %>% .[names(.) == elem]
      , table = dict$colTables[elem]
    ))
    
  } else {
    
    # Throw an error if the element requested does not exist
    stop("`[.dataDict`: The requested element (", elem, ") does not exist in this `dataDict` object (",
         deparse(substitute(dict)), ".")
    
  }
  
}


#' Summarize a \code{dataDict} Object
#' 
#' @param object A \code{dataDict} object.
#' @param ... Currently not used.
#' 
#' @return A summary of \code{object}.
#' @export
#' 
#' @examples
#' summary(dataDict(mtcars))
#' 
summary.dataDict <- function(object, ...) {
  
  # Cretae a tibble of the column names, classes, number of unique elements,
  #   & whether or not the column has been tabulated
  return(tibble::tibble(
    Column = colnames(object)
    , Class = object$classes
    , NumUnique = object$numUnique
    , Tabulated = colnames(object) %in% names(object$colTables)
  ))
  
}


#' Print a \code{dataDict} Object
#' 
#' @param x A \code{dataDict} object.
#' @param ... Currently not used.
#' 
#' @return The summary of \code{x}, invisibly (i.e. \code{summary(x)}).
#' @export
#' 
#' @examples
#' dataDict(mtcars)
#' print(dataDict(mtcars))
#' dd <- dataDict(mtcars)
#' print(dd)
#' 
print.dataDict <- function(x, ...) {
  
  # Print some information about the data that the `dataDict` is based off of
  dictName <- deparse(substitute(x)) %>% {ifelse(. == "x", "", paste0("(", ., ") "))}
  cat(
    "This `dataDict` object ", dictName, "was based off of '", attr(x, "dfName"),
    "' (a '", x$dfClass[1], "') in the '", attr(x, "dfEnvName"), "' environment,\n",
    "  which had ", nrow(x), " rows and ", ncol(x), " columns when this `dataDict` was constructed.\n",
    "The `tableMode` mode is set to '", attr(x, "tableMode"), "', and the `verbose` level is '", attr(x, "verbose"), "'.\n",
    sep = ""
  )
  
  # Print the summary object
  return(print(summary(x)))
  
}


#' Dimensions of a \code{dataDict}
#' 
#' Generic function to retrieve the dimensions of the data.frame-like object
#'   that the \code{dataDict} is based on.
#' `ncol` and `nrow` will also work, since they call `dim` in their implementation.
#' 
#' @param x A \code{dataDict} object.
#' 
#' @return The dimensions of the original data.frame-like object.
#' @export
#' 
#' @examples
#' dd <- dataDict(mtcars)
#' dim(dd)
#' ncol(dd)
#' nrow(dd)
#' 
dim.dataDict <- function(x) {
  return(attr(x, "dims"))
}


#' Dimension Names of a \code{dataDict}
#' 
#' Generic function to retrieve the dimension names of the data.frame-like object
#'   that the \code{dataDict} is based on.
#' `colnames` and `rownames` will also work, since they call `dimnames` in their implementation.
#' 
#' @param x A \code{dataDict} object.
#' 
#' @return \code{dimnames}: The dimension names of the original data.frame-like object.
#' @export
#' @name dimensionNames
#' 
#' @examples
#' dd <- dataDict(mtcars)
#' dimnames(dd)
#' colnames(dd) # Same as names(dd)
#' rownames(dd)
#' names(dd) # Same as colnames(dd)
#' 
dimnames.dataDict <- function(x) {
  return(attr(x, "dimNames"))
}


#' @return \code{names}: The column names of the original data.frame-like object.
#' @export
#' @rdname dimensionNames
#' 
names.dataDict <- function(x) {
  return(colnames(x))
}


#' Update a \code{dataDict} Object
#' 
#' @param dict A \code{dataDict} object.
#' @param df The data.frame-like object to calculate information for.
#' 
#' @export
#' 
#' @examples
#' df <- mtcars
#' dd <- dataDict(df)
#' dd$colTables$mpg
#' df$mpg <- df$mpg * 2
#' updateDD(dd, df)
#' dd$colTables$mpg
#' 
updateDD <- function(dict, df) {
  # stop("`updateDD`: This function has not yet been implemented.")
  return(NULL)
}



#' Create a \code{columnTables} Object
#' 
#' Create an object that is used to store the results of tabulating columns in a data.frame-like object.
#' This is currently only called from with a \code{dataDict} object.
#' 
#' @param dict A \code{dataDict} object.
#' @param df The data.frame-like object to calculate information for (should be a name, not an expression).
#' @param tableMode What mode to use for tabulating each column (see \code{details} for more) (character/logical scalar).
#' 
#' @return A \code{columnTables} object.
#' 
columnTables <- function(dict, df, tableMode) {
  
  # Standardize the `tableMode` parameter
  tableMode <- switch(tolower(tableMode), lazy = "lazy", t = , true = TRUE, f = , false = FALSE)
  
  # Tabulate the results, if desired, & set the `tableMode` attribute
  attr(dict, "tableMode") <- tableMode
  if (tableMode == "lazy") {
    if (attr(dict, "verbose") > 0) message("`columnTables`: Lazy table mode active.")
    colTables <- vector(mode = "list", length = attr(dict, "dims")[[2]]) %>%
      stats::setNames(., attr(dict, "dimNames")[[2]]) %>% as.environment()
  } else if (isTRUE(tableMode)) {
    colTables <- purrr::map(df, table, useNA = "ifany", dnn = NULL) %>% as.environment()
  } else if (isFALSE(tableMode)) {
    colTables <- new.env()
  } else {
    if (attr(dict, "verbose") > 1)
      warning("`columnTables`: Invalid table mode (", tableMode, "). Lazy table mode will be used instead.")
    attr(dict, "tableMode") <- "lazy"
    colTables <- vector(mode = "list", length = attr(dict, "dims")[[2]]) %>%
      stats::setNames(., attr(dict, "dimNames")[[2]]) %>% as.environment()
  }
  
  # Create the columnTables object, holding the dataDict, & the list of the tables
  attr(colTables, "dict") <- dict
  
  # Set the dataDict class, & return it
  class(colTables) <- c("columnTables", class(colTables))
  return(colTables)
  
}


#' Extract Elements from a \code{columnTables} Object
#' 
#' @param colTables A \code{columnTables} object.
#' @param col The column to extract the table for (character scalar).
#' 
#' @return A \code{table} of the desired column.
#' @export
#' 
#' @examples
#' dd <- dataDict(mtcars)
#' dd$colTables$mpg
#' 
`$.columnTables` <- function(colTables, col = NULL) {
  colTables[col]
}


#' Extract Elements from a \code{columnTables} Object
#' 
#' @param colTables A \code{columnTables} object.
#' @param cols The columns to extract the tables for (character vector).
#' 
#' @return A list of \code{table}s of the desired columns.
#' @export
#' 
#' @examples
#' dd <- dataDict(mtcars)
#' dd$colTables["mpg"]
#' 
#' # The following line works, but causes automated checking/testing problems
#' # dd$colTables[c("mpg", "cyl", "am")]
#' 
`[.columnTables` <- function(colTables, cols = NULL) {
  
  # Get the `dict` object (for convenience), & deal with cols being NULL or NA
  dict <- attr(colTables, "dict")
  if (is.null(cols) || is.na(cols)) cols <- colnames(dict)
  
  # Determine which columns have already been tabulated, & which we need to calculate
  tabulatedCols <- names(colTables) # ls(colTables)[!sapply(colTables, is.null)]
  colsToCalc <- intersect(cols, colnames(dict)) %>% setdiff(tabulatedCols)
  
  # Determine which columns are valid/invalid
  invalidCols <- setdiff(cols, colnames(dict))
  validCols <- setdiff(cols, invalidCols)
  
  # If there are any invalid columns (i.e. ones not in the data), print a warning
  if (length(invalidCols) > 0)
    warning("`[.columnTables`: Some invalid columns were selected:\n\t", paste0(invalidCols, collapse = ", "))
  
  # If there are any columns we need to calculate, try to get them, else return the existing tables
  if (length(colsToCalc) > 0) {
    
    # If the original object that this dataDict was based off of still exists, get it, else throw an error
    if ((!is.null(attr(dict, "dfEnv"))) && exists(attr(dict, "dfName"), where = attr(dict, "dfEnv"))) {
      df <- eval(attr(dict, "dfCall"), attr(dict, "dfEnv"))
    } else {
      stop(
        "`[.dataDict`: The object that this `dataDict` was based off (",
        attr(dict, "dfName"), ") no longer exists in its original environment (", attr(dict, "dfEnvName"), ").\n",
        "Please update the reference using `updateDD(dict, df)`."
      )
    }
    
    # Calculate the new column tables, & save them to the tables environment
    purrr::walk(colsToCalc, ~ table(df[[.x]], useNA = "ifany", dnn = NULL) %>% assign(.x, ., colTables))
    
  }
  
  # Return the tables for the desired valid columns
  return(purrr::map(validCols, ~ get(.x, colTables)) %>% stats::setNames(., validCols))
  
}


#' Summarize a \code{dataDict} Object
#' 
#' @param object A \code{columnTables} object.
#' @param ... Currently not used.
#' 
#' @return A summary of \code{object}.
#' @export
#' 
#' @examples
#' dd <- dataDict(mtcars)
#' summary(dd$colTables)
#' 
summary.columnTables <- function(object, ...) {
  # stop("`summary.columnTables`: This function has not yet been implemented.")
  return(NULL)
}


#' Print a \code{dataDict} Object
#' 
#' @param x A \code{columnTables} object.
#' @param ... Currently not used.
#' 
#' @return A summary of \code{x}, invisibly (i.e. \code{summary(x)}).
#' @export
#' 
#' @examples
#' dd <- dataDict(mtcars)
#' dd$colTables
#' print(dd$colTables)
#' 
print.columnTables <- function(x, ...) {
  # stop("`print.columnTables`: This function has not yet been implemented.")
  return(NULL)
}


#' Get the Names of a \code{columnTables} Object
#' 
#' Get the names of the columns from a \code{columnTables} object that have
#'   been tabulated already (character vector).
#' 
#' @param x A \code{columnTables} object.
#' 
#' @return The names of the columns that have been tabulated already (character vector).
#' @export
#' 
#' @examples
#' dd <- dataDict(mtcars)
#' names(dd$colTables)
#' 
names.columnTables <- function(x) {
  # return(ls(x)[!sapply(x, is.null)])
  return(ls(x) %>% .[!sapply(., function(y) is.null(get(y, x)))])
}
