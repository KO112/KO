#' Compare Data-Frames
#' 
#' Compare two data-frame (like) objects.
#' The function will compare the objects by column names, and print information about differences.
#'
#' @param df1 The first object inheriting from a data.frame.
#' @param df2 The second object inheriting from a data.frame.
#' @param printColDiffs Whether or not to print messages showing
#'   the differences between columns (logical scalar).
#' @param tol The acceptable tolerance for equality between numeric columns (numeric scalar).
#' @param trim Whether or not to trim whitespace from character columns (logical scalar).
#'
#' @return A list of column comparison data for each non-identical column.
#' @export
#'
#' @examples
#' 
#' # Returns an empty list, since the objects are identical
#' compare_dfs(mtcars, mtcars)
#' 
#' # Prints a message about the difference in column names
#' compare_dfs(mtcars, mtcars[, -1])
#' 
#' # Prints a message about the difference in values, & returns a tibble showing the differences
#' compare_dfs(mtcars, dplyr::mutate(mtcars, mpg = mpg * 2))
#' 
#' # Same as the above, but doesn't print the message about the differences
#' compare_dfs(mtcars, dplyr::mutate(mtcars, mpg = mpg * 2), printColDiffs = FALSE)
#' 
compare_dfs <- function(df1, df2, printColDiffs = TRUE, tol = 1e-10, trim = TRUE) {
  
  # Ensure we have two data-frame (like) objects
  KO::stop_if(!inherits(df1, "data.frame"), "'df1' must inherit from a 'data.frame'.")
  KO::stop_if(!inherits(df2, "data.frame"), "'df2' must inherit from a 'data.frame'.")
  
  # Create diagnostic variables
  df1_nrow <- nrow(df1); df2_nrow <- nrow(df2)
  df1_ncol <- ncol(df1); df2_ncol <- ncol(df2)
  df1_colnames <- colnames(df1) %>% sort(); df2_colnames <- colnames(df2) %>% sort()
  sameNames <- intersect(df1_colnames, df2_colnames) %>% sort()
  df1_not_df2_colnames <- setdiff(df1_colnames, df2_colnames) %>% sort()
  df2_not_df1_colnames <- setdiff(df2_colnames, df1_colnames) %>% sort()
  
  # Ensure we have the same number of rows in each object
  KO::stop_if(df1_nrow != df2_nrow, sprintf("The number of rows in 'df1' (%s) does not match the number of rows in 'df2' (%s).", df1_nrow, df2_nrow))
  
  # Check column names, & print differences
  KO::stop_if(length(sameNames) == 0, "There are no columns with common names.")
  if (length(df1_not_df2_colnames) > 0)
    message("Column names in 'df1' but not in 'df2': ", paste0(df1_not_df2_colnames, collapse = ", "))
  if (length(df2_not_df1_colnames) > 0)
    message("Column names in 'df2' but not in 'df1': ", paste0(df2_not_df1_colnames, collapse = ", "))
  
  # Compare columns with common names
  compareCols <- mapply(identical, `[.data.frame`(df1, sameNames), `[.data.frame`(df2, sameNames))
  diffCols <- names(compareCols[!compareCols])
  
  # Print information on non-identical columns
  colComps <- sapply(diffCols, function(colName) {
    
    # Get the columns, & trim their whitespace if they are characters
    df1_col <- df1[[colName]]; df2_col <- df2[[colName]]
    if (is.character(df1_col)) df1_col <- trimws(df1_col)
    if (is.character(df2_col)) df2_col <- trimws(df2_col)
    
    # Get some information about the columns
    df1_class <- class(df1_col); df2_class <- class(df2_col)
    df1_na <- is.na(df1_col); df2_na <- is.na(df2_col)
    df1_na_count <- sum(df1_na); df2_na_count <- sum(df2_na)
    colComp <- df1_col == df2_col
    
    # If one of the columns is a date, convert them both to characters
    if (xor(lubridate::is.Date(df1_col), lubridate::is.Date(df2_col))) {
      df1_col <- trimws(as.character(df1_col)); df2_col <- trimws(as.character(df2_col))
    }
    
    # Check if the columns are equal
    if (isTRUE(all.equal(df1_col, df2_col, check.attributes = FALSE))) {
      message("The columns of ", colName, " are not equal, but they are identical.")
      return(NULL)
    }
    
    # Get the indices of non-matching values, & the corresponding values
    nmInds <- which((df1_col != df2_col) | xor(df1_na, df2_na))
    df1_values <- df1_col[nmInds]; df2_values <- df2_col[nmInds]
    
    # Tabulate the non-matching values?
    compTable <- table(colComp, useNA = "ifany")
    # df1_table <- table(df1_values, useNA = "ifany")
    # df2_table <- table(df2_values, useNA = "ifany")
    
    # If desired, print column differences
    if (printColDiffs) {
      
      # Compare classes
      if (df1_class != df2_class) message("Column '", colName, "' has different classes: (df1: ", df1_class, ", df2: ", df2_class, ")")
      
      # Print information on the number of NA's
      if ((df1_na_count == 0) && (df2_na_count == 0)) {
        message("Column '", colName, "' has 0 NA's.")
      } else {
        message("Column '", colName, "' has ", df1_na_count, " NA's in 'df1', and ", df2_na_count, " in 'df2', ",
                ifelse(all(df1_na == df2_na), "all", sum(df1_na & df2_na)), " of which are in the same position.")
      }
      
      # Print the comparisons table
      names(dimnames(compTable)) <- NULL
      print(compTable)
      # print(df1_table)
      # print(df2_table)
      
    }
    
    # Compare numeric columns (for rounding errors)
    if (is.double(df1_col) && is.double(df2_col)) {
      nmIndsExclNA <- which(df1_col != df2_col)
      diffs <- abs(df1_col[nmIndsExclNA] - df2_col[nmIndsExclNA])
      if (suppressWarnings(max(diffs, na.rm = TRUE)) <= tol) {
        if (printColDiffs) message("All differences for column '", colName, "' are <= ", tol, ".")
        return(NULL)
      } else if (printColDiffs) {
        message("The maximum difference for column '", colName, "' was ", max(diffs),
                ", and ", sum(diffs > tol), " exceed the desired tolerance (", tol, ").")
      }
    }
    
    # Return a tibble holding the desired values
    diffTbl <- tibble::tibble(nmInds, df1_values, df2_values) %>%
      stats::setNames(., c("Index", paste0(colName, "_df1"), paste0(colName, "_df2")))
    if (nrow(diffTbl) == 0) {
      message("Column ", colName, " returns 0 rows.")
      return(NULL)
    } else {
      return(diffTbl)
    }
    
  }, simplify = FALSE)
  
  # Return the column comparisons, removing NULL values
  return(colComps[!sapply(colComps, is.null)])
  
}


# d1 <- ggplot2::diamonds
# d2 <- ggplot2::diamonds %>% mutate(x = ifelse(color == "E", x * 2, x), y = ifelse(color == "F", NA, y))
# # microbenchmark::microbenchmark(times = 10000, unit = "ms", compare_dfs(d1, d2))
# a <- compare_dfs(d1, d2) # %>% print()

# old_KO <- readRDS("~/Projects/20181213_Monitoring/retention/data/ji_ho_retention_predict_pif_data_old_KO.RDS")
# new <- readRDS("~/Projects/20181213_Monitoring/retention/data/ji_ho_retention_predict_pif_data_new.RDS")
# new_KO <- readRDS("~/Projects/20181213_Monitoring/retention/data/ji_ho_retention_predict_pif_data_new_KO.RDS")

# comp <- compare_dfs(old, new) %>% print()
