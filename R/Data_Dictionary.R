# To-Do
#   Make class for single column summary, and a list of them
#   Override `[.dataDict` with special handler for table being NULL
#   Add lazy loading ability, maybe add attributes (logical vector of colum names?)?
#   For printing, change from tibble to something easier to read in bulk, maybe multiple columns
#   Add colHash, maybe use constructor for both colTables/colHash


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
#' # It is better to declare use something like `df <- tibble(mtcars); dd <- dataDict(df)`
#' dd <- dataDict(tibble(mtcars))
#' 
dataDict <- function(df, tableMode = "lazy", verbose = Inf) {
  
  # Ensure that we have a data.frame-like object
  if (!inherits(df, "data.frame")) stop("dataDict: `df`` must inherit from a `data.frame`.")
  
  # Create the data dictionary object, & set the call/verbosity attribute
  dict <- new.env()
  dict$call <- match.call()
  attr(dict, "verbose") <- verbose
  
  # Add various values to the dictionary
  evalq(envir = dict, expr = {
    
    # Set the df call, & the environment holding it
    dfCall <- substitute(df, env = parent.frame(2))
    dfEnv <- parent.frame(3) # try(pryr::where(deparse(dfName)))
    dfEnvName <- environmentName(dfEnv) %>% ifelse(. == "", pryr::address(dfEnv), .)
    
    # Attempt to set the name of the original data
    if (length(dfCall) > 1) {
      message("`df` has been passed an an expression (", deparse(dfCall),
              "). This may result in some problems when using this `dataDict` object, but may be fine.")
      dfName <- dfCall[purrr::map_lgl(dfCall, ~ is.data.frame(eval(.x)))][[1]] %>% deparse()
    } else {
      dfName <- deparse(dfCall)
    }
    
    # If desired & relevant, print out a message about not changing the original object
    if ((tableMode != "lazy") && (verbose > 1)) message(
        "This `dataDict` will be based off of the object named '", dfName, "'.\n",
        "To ensure that this `dataDict` will continue to work, do not change the name of the object, ",
        "and use the `updateDD` function if the object changes."
      )
    
    # Get the dimensions/dimension names from the original df object
    dims <- dim(df)
    dimnames <- dimnames(df)
    
    # Set the class of the original df object, & the classes of each column
    dfClass <- class(df)
    classes <- purrr::map_chr(df, class)
    
    # Get the number of unique elements
    numUnique <- purrr::map_int(df, ~ length(unique(.x)))
    
    # Set the column tables element
    colTables <- columnTables(dict, df, tableMode)
    
  })
  
  # Set the dataDict class, & return it
  class(dict) <- "dataDict"
  return(dict)
  
}


#' Summarize a \code{dayaDict} Object
#'
#' @param dict A \code{dataDict} object.
#'
#' @return
#'
#' @examples
#' summary(dataDict(mtcars))
#' 
summary.dataDict <- function(dict) {
  
  # Cretae a tibble of the column names, classes, number of unique elements,
  #   & whether or not the column has been tabulated
  return(tibble::tibble(
    Column = colnames(dict)
    , Class = dict$classes
    , NumUnique = dict$numUnique
    , Tabulated = colnames(dict) %in% names(dict$colTables)
  ))
  
}


#' Print a \code{dataDict} Object
#'
#' @param dict A \code{dataDict} object.
#'
#' @examples
#' dataDict(mtcars)
#' print(dataDict(mtcars))
#' dd <- dataDict(mtcars)
#' print(dd)
#' 
print.dataDict <- function(dict) {
  
  # Print some information about the data that the dict is based off of
  dictName <- deparse(substitute(dict)) %>% {ifelse(. == "x", "", paste0("(", ., ") "))}
  cat(
    "This `dataDict` object ", dictName, "was based off of '", dict$dfName,
    "' (a '", dict$dfClass[1], "') in the '", dict$dfEnvName, "' environment,\n",
    "  which had ", nrow(dict), " rows and ", ncol(dict), " columns when this `dataDict` was constructed.\n",
    "The `tableMode` mode is set to '", attr(dict, "tableMode"), "', and the `verbose` level is '", attr(dict, "verbose"), "'.\n",
    sep = ""
  )
  
  # Print the summary object
  print(summary(dict))
  
}


# #' Title
# #'
# #' @param dict 
# #' @param elem 
# #'
# #' @return
# #' @export
# #'
# #' @examples
# `$.dataDict` <- function(dict, elem) {
#   `[.dataDict`(dict, elem)
# }


#' Extract Elements from a \code{dataDict} Object
#'
#' @param dict A \code{dataDict} object.
#' @param elem The element to extract (character scalar).
#' @param cols If \code{elem == "colTables"}, the column(s) to extract (character vector).
#'
#' @return The desired element extracted from the \code{dataDict} object.
#'
#' @examples
#' dd <- dataDict(mtcars)
#' dd["numUnique"]
#' dd["colTables"]
#' dd["colTables", "mpg"]
#' dd["colTables", c("mpg", "cyl")]
#' dd["colTables", NA]
#' 
#' dd2 <- dataDict(mtcars, table = TRUE)
#' dd2["colTables", "mpg"]
#' dd2["colTables", c("mpg", "cyl")]
#' dd2["colTables", NA]
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
    
  } else if (elem == "colTables") {
    
    # Get the desired tables
    # return(getTables(dict, cols))
    return(dict$colTables[cols])
    
  } else if (elem %in% ls(dict)) (
    
    # Return the desired element
    return(get(x = elem, envir = dict))
    
  ) else if (elem %in% colNames(dict)) {
    
    # Return data on the desired column
    return(list(
      class = dict$classes %>% .[names(.) == elem]
      , numUnique = dict$numUnique %>% .[names(.) == elem]
      , table = dict$colTables[elem]
    ))
    
  } else {
    
    # Throw an error if the element requested does not exist
    stop("The requested element (", elem, ") does not exist in this `dataDict` object (",
         deparse(substitute(dict)), ".")
    
  }
  
}


# Set `[[` to do the same as `[`(?)
# `[[.dataDict` <- `[.dataDict`


#' Dimensions of a \code{dataDict}
#' 
#' Generic function to retrieve the dimensions of the data.frame-like object
#'   that the \code{dataDict} is based on.
#' `ncol` and `nrow` will also work, since they call `dim` in their implementation.
#'
#' @param dict A \code{dataDict} object.
#'
#' @return The dimensions of the original data.frame-like object.
#'
#' @examples
#' dd <- dataDict(mtcars)
#' dim(dd)
#' ncol(dd)
#' nrow(dd)
#' 
dim.dataDict <- function(dict) {
  return(dict$dims)
}


#' Dimension Names of a \code{dataDict}
#' 
#' Generic function to retrieve the dimension names of the data.frame-like object
#'   that the \code{dataDict} is based on.
#' `colnames` and `rownames` will also work, since they call `dimnames` in their implementation.
#'
#' @param dict A \code{dataDict} object.
#'
#' @return \code{dimnames}: The dimension names of the original data.frame-like object.
#' @name dimensionNames
#'
#' @examples
#' dd <- dataDict(mtcars)
#' dimnames(dd)
#' colnames(dd) # Same as names(dd)
#' rownames(dd)
#' names(dd) # Same as colnames(dd)
#' 
dimnames.dataDict <- function(dict) {
  return(dict$dimnames)
}


#' @return \code{names}: The column names of the original data.frame-like object.
#' @rdname dimensionNames
names.dataDict <- function(dict) {
  return(colnames(dict))
}


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
  stop("`updateDD`: This function has not yet been implemented.")
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
#' @return
#'
#' @examples
#' 
columnTables <- function(dict, df, tableMode) {
  
  # Standardize the `tableMode` parameter
  tableMode <- switch(tolower(tableMode), lazy = "lazy", t = , true = TRUE, f = , false = FALSE)
  
  # Tabulate the results, if desired, & set the `tableMode` attribute
  attr(dict, "tableMode") <- tableMode
  if (tableMode == "lazy") {
    if (attr(dict, "verbose") > 0) message("`dataDict`: Lazy table mode active.")
    colTables <- vector(mode = "list", length = dict$dims[[2]]) %>% setNames(dict$dimnames[[2]]) %>% as.environment()
  } else if (isTRUE(tableMode)) {
    colTables <- purrr::map(df, table, useNA = "ifany", dnn = NULL) %>% as.environment()
  } else if (isFALSE(tableMode)) {
    colTables <- NULL
  } else {
    if (attr(dict, "verbose") > 1) warning("`dataDict`: Invalid table mode (", tableMode,
                                           "). Lazy table mode will be used instead.")
    attr(dict, "tableMode") <- "lazy"
    colTables <- vector(mode = "list", length = dict$dims[[2]]) %>% setNames(dict$dimnames[[2]]) %>% as.environment()
  }
  
  # Create the columnTables object, holding the dataDict, & the list of the tables
  # colTables <- list(dict = dict, colTables = colTables)
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
#'
#' @examples
#' dd <- dataDict(mtcars)
#' dd$colTables$mpg
#' 
`$.columnTables` <- function(colTables, col) {
  colTables[col]
}


#' Extract Elements from a \code{columnTables} Object
#'
#' @param colTables A \code{columnTables} object.
#' @param cols The columns to extract the tables for (character vector).
#'
#' @return A list of \code{table}s of the desired columns.
#'
#' @examples
#' dd <- dataDict(mtcars)
#' dd$colTables$mpg
#' 
`[.columnTables` <- function(colTables, cols) {
  
  # Get the `colTables` & `dict` objects (for convenience)
  # actualTables <- colTables # [["colTables"]]
  dict <- attr(colTables, "dict")
  
  browser()
  # Retrieve existing column tables, & set the ones that we need to calculate/aren't in the data
  # existingTables <- cols[cols %in% names(colTables)] %>% sapply(get, colTables, simplify = FALSE)
  # otherCols <- cols[!(cols %in% names(colTables))]
  tabulatedCols <- names(colTables)[!sapply(colTables, is.null)]
  otherCols <- names(colTables)[sapply(colTables, is.null)]
  colsToCalc <- intersect(cols, colnames(dict))
  invalidCols <- setdiff(cols, colnames(dict))
  # colsToCalc <- intersect(otherCols, colnames(dict)) %>% intersect(cols)
  # invalidCols <- setdiff(otherCols, colnames(dict)) %>% intersect(cols)
  
  # If there are any invlaid columns (i.e. ones not in the data)
  if (length(invalidCols) > 0) warning(
      "`[.columnTables`: Some invalid columns were selected:\n",
      paste0(invalidCols, sep = ", ")
    )
  
  # If there are any columns we need to calculate, try to get them, else return the existing tables
  if (length(colsToCalc) > 0) {
    
    # If the original object that this dataDict was based off of still exists, get it, else throw an error
    if ((!is.null(dict$dfEnv)) && exists(dict$dfName, where = dict$dfEnv)) {
      df <- eval(dict$dfCall, dict$dfEnv)
    } else {
      stop(
        "`[.dataDict`: The object that this `dataDict` was based off (",
        dict$dfName, ") no longer exists in its original environment (", dict$dfEnvName, ").\n",
        "Please update the reference using `updateDD(dict, df)`."
      )
    }
    
    # Calculate the new column tables, save them to the tables environment, & return them in the desired order
    newTables <- purrr::map(`[.data.frame`(df, colsToCalc), table, useNA = "ifany", dnn = NULL)
    purrr::walk2(newTables, names(newTables), ~ assign(.y, .x, colTables))
    # for (i in seq_along(newTables)) newTables[[i]] %>% assign(names(.), ., colTables)
      # colTables[[names(newTables[[i]])]] <- newTables[[i]]
    # tabulatedCols <- c(tabulatedCols, names(newTables))
    # return(c(existingTables, newTables)[cols])
    
  # } else {
  #   
  #   # If the requested tables have all already been calculated, return the existing tables
  #   return(existingTables)
    
  }
  
  # Return the desired columns
  return(purrr::map(cols, ~ get(.x, colTables)) %>% setNames(cols))
  
}


#' Get the Names of a \code{columnTables} Object
#' 
#' Get the names of the columns from a \code{columnTables} object that have
#'   been tabulated already (character vector).
#' 
#' @param colTables A \code{columnTables} object.
#'
#' @return The names of the columns that have been tabulated already (character vector).
#'
#' @examples
#' dd <- dataDict(mtcars)
#' names(dd$colTables)
#' 
names.colTables <- function(colTables) {
  # return(names(colTables[["colTables"]]))
  return(names(colTables)[!sapply(colTables, is.null)])
}


# #' Extract Tabuated Columns from a \code{dataDict}
# #'
# #' @param elem The element to extract (character scalar).
# #' @param cols The columns to extract the tables for (character vector).
# #'
# #' @return A named list of tables, corresponding to the requested columns.
# #'
# #' @examples
# #' dd <- dataDict(mtcars)
# #' getTables(dd)
# #' getTables(dd, cols = NA)
# #' getTables(dd, cols = NA)
# #' getTables(dd, cols = "mpg")
# #' getTables(dd, cols = c("mpg", "cyl"))
# #' 
# getTables <- function(dict, cols = NULL) {
#   
#   # Either throw an error if tables were disabled, return them, or calculate them
#   if (isFALSE(attr(dict, "table"))) {
#     
#     # If the tables were disabled, print a warning, & return nothing
#     warning("`[.dataDict`: The columns were not tabulated for this `dataDict` object.\n",
#             "Please re-run with `dataDict(", deparse(dict$dfCall), ", table = TRUE)` or ",
#             "`dataDict(", deparse(dict$dfCall), ", table = 'lazy')` if you want to access this field.")
#     return(NULL)
#     
#   } else if (isTRUE(attr(dict, "table"))) {
#     
#     # If `cols` is NULL or NA, return all the tabulated columns, or just the desired ones,
#     #   printing a warning message if some column names aren't found
#     if (is.null(cols) || is.na(cols)) {
#       return(dict$colTables)
#     } else if (all(cols %in% names(dict$colTables))) {
#       return(dict$colTables[cols])
#     } else {
#       warning("`[.dataDict`: Columns not found in the `dataDict`: ",
#               paste0(cols[!(cols %in% names(dict$colTables))], collapse = ", "))
#       return(dict$colTables[cols[cols %in% names(dict$colTables)]])
#     }
#     
#   } else {
#     
#     # If the original object that this dataDict was based off of still exists, get it, else throw an error
#     if ((!is.null(dict$dfEnv)) && exists(dict$dfName, where = dict$dfEnv)) {
#       df <- eval(dict$dfCall, dict$dfEnv)
#     } else {
#       stop(
#         "`[.dataDict`: The object that this `dataDict` (", deparse(substitute(dict)), ") was based off (",
#         dict$dfName, ") no longer exists in its original environment (", dict$dfEnvName, ").\n",
#         "Please update the reference using updateDD(", deparse(substitute(dict)), "df)."
#       )
#     }
#     
#     # Either throw an error if no column was specified, return all the tabulated columns,
#     #   or calculate them & save the results
#     if (is.null(cols)) {
#       
#       # If no column is specified, print a warning, & return nothing
#       warning("`[.dataDict`: Lazy table mode is active, so you must specify a column name ",
#               "(or use `cols = NA` to get all the columns.")
#       return(NULL)
#       
#     } else if (any(is.na(cols))) {
#       
#       # If any of the names were not NA, print a message
#       if (!all(is.na(cols))) message("Some of the names in `cols` were NA, so data was returned for all columns.")
#       
#       # Return all the tabulated columns, tabulating & saving them if need be
#       if (length(dict$colTables) == ncol(dict)) {
#         return(dict$colTables)
#       } else {
#         return(dict$colTables <- sapply(df, table, useNA = "ifany", dnn = NULL, simplify = FALSE))
#       }
#       
#     } else if (!all(cols %in% dict$colNames)) {
#       
#       # If any of the columns don't exist in the data, throw an error
#       warning("`[.dataDict`: Columns not found in the data: ",
#               paste0(cols[!(cols %in% names(dict$colNames))], collapse = ", "))
#       
#       # } else if (all(cols %in% names(dict$colTables))) {
#       #   
#       #   # If the columns have already been tabulated, return them
#       #   return(dict$colTables %>% .[names(.) %in% cols])
#       
#     } else {
#       
#       # Else retrieve the columns that have already been calculated, & calculate/save the others
#       colTable <- dict$colTables %>% .[names(.) %in% cols]
#       
#       # Else tabulate the desired columns, & save them
#       # colTable <- sapply(df[cols], table, useNA = "ifany", dnn = NULL, simplify = FALSE)
#       # dict$colTables <- c(dict$colTables, colTable) # list(colTable) %>% setNames(cols))
#       # return(colTable)
#       
#     }
#     
#   }
#   
# }
