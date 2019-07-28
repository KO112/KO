# To-Do
#   Make class for single column summary, and a list of them
#   Override `[.dataDict` with special handler for table being NULL
#   Add lazy loading ability, maybe add attributes (logical vector of colum names?)?


dataDict <- function(df, table = FALSE, lazyTable = TRUE, verbose = Inf) {
  
  # Ensure that we have a data.frame-like object
  if (!inherits(df, "data.frame")) stop("dataDict: 'df' must inherit from a 'data.frame'.")
  
  # Create the data dictionary object
  dict <- new.env()
  evalq(envir = dict, expr = {
    
    # Set the name of the original table, and its environment
    origDFName <- deparse(substitute(dict))
    origDFEnv <- pryr::where(origDFName)
    
    # Set the dimensions of the original df object
    dims <- dim(df)
    nrow <- nrow(df)
    ncol <- ncol(df)
    
    # Set the column names & class of the original df object, and the classes of each column
    colNames <- colnames(df)
    class <- class(df)
    classes <- sapply(df, class)
    
    # Get the number of unique elements
    numUnique <- sapply(df, function(x) unique(x) %>% length())
    
    # Tabulate the results, if desired, setting the 'table' attribute
    if (lazyTable) {
      if (verbose > 0) message("dataDict: Lazy table mode has been activated.")
      attr(dict, "table") <- "lazy"
      colTables <- NULL
    } else if (table) {
      attr(dict, "table") <- TRUE
      colTables <- sapply(df, table, useNA = "ifany", simplify = FALSE)
    } else {
      attr(dict, "table") <- FALSE
      colTables <- NULL
    }
    
  })
  
  # Set the dataDict class, and return it
  class(dict) <- c("dataDict") # , class(dict))
  return(dict)
  
}


print.dataDict <- function(dict) {
  
}

`[.dataDict` <- function(dict, elem, col = NULL) {
  
  # Either throw an error if the element doesn't exist, or try to return the desired element
  if (!exists(elem, dict)) {
    
    # If the element doesn't exist, print a message
    stop("`[.dataDict`: Element '", elem, "' does not exist in '", deparse(substitute(dict)), "'.")
    
  } else if (elem == "colTables") {
    
    # If the element requested is 'colTables', either return it, throw an error if it wasn't calculated, or calculate it
    if (isTRUE(attr(dict, "table"))) {
      return(get(x = elem, envir = dict))
    } else if (isFALSE(attr(dict, "table"))) {
      stop("`[.dataDict`: The columns were not tabulated for this dataDict object.\n",
           "Please re-run with `dataDict(", dict$origDFName, ", table = TRUE)` or ",
           "`dataDict(", dict$origDFName, ", lazyTable = TRUE)` if you want to access this field.")
    } else {
      
      # Check that the original object that this dataDict was based off of still exists, & get it if it does
      if (!exists(dict$origDFName, where = dict$origDFEnv)) {
        stop(
          "`[.dataDict`: The object that this 'dataDict' (", deparse(substitute(dict)), ") was based off (",
          dict$origDFName, ") no longer exists in its original environment (", dict$origDFEnv, ").\n",
          "Please update the reference using updateDF(", deparse(substitute(dict)), "df)."
        )
      } else {
        df <- get(dict$origDFName, dict$origDFEnv)
      }
      
      # If no column is specified, throw an error, else tabulate all the columns, or just one at a time
      if (is.null(col)) {
        stop("`[.dataDict`: Lazy table mode is active, so you must specify a column name (or 'NA' to get all the columns.")
      } else if (is.na(col)) {
        return(dict$colTables <- sapply(df, table, useNA = "ifany"))
      } else {
        if (!exists(col, where = df)) stop("`[.dataDict`: Column '", col, "' doesn't exist in the data.")
        colTable <- table(df[[col]], useNA = "ifany")
        dict$colTables <- c(dict$colTables, list(colTable))
        return(colTable)
      }
      
    }
    
  } else (
    
    # Simply return the element
    return(get(x = elem, envir = dict))
    
  )
  
}

`[[.dataDict` <- `[.dataDict`
