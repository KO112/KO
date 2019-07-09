# Compare two data-frame (like) objects
compare_dfs <- function(df1, df2, printColDiffs = TRUE, tol = 1e-10) {
  
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
  
  # Check column names, and print differences
  KO::stop_if(length(sameNames) == 0, "There are no columns with common names.")
  if (length(df1_not_df2_colnames) > 0)
    message("Column names in 'df1' but not in 'df2': ", paste0(df1_not_df2_colnames, collapse = ", "))
  if (length(df2_not_df1_colnames) > 0)
    message("Column names in 'df2' but not in 'df1': ", paste0(df2_not_df1_colnames, collapse = ", "))
  
  # Compare columns with common names
  compareCols <- mapply(identical, df1[, sameNames], df2[, sameNames])
  diffCols <- names(compareCols[!compareCols])
  
  # Print information on non-identical columns
  colComps <- sapply(diffCols, function(x) {
    
    # Get the columns, and some information about them
    df1_col <- df1[[x]]; df2_col <- df2[[x]]
    df1_na <- is.na(df1_col); df2_na <- is.na(df2_col)
    df1_na_count <- sum(df1_na); df2_na_count <- sum(df2_na)
    colComp <- df1_col == df2_col
    
    # Get the indices of non-matching values, and the corresponding values
    nmInds <- which((df1_col != df2_col) | xor(df1_na, df2_na))
    df1_values <- df1_col[nmInds]; df2_values <- df2_col[nmInds]
    
    # Tabulate the non-matching values?
    compTable <- table(colComp, useNA = "ifany")
    # df1_table <- table(df1_values, useNA = "ifany")
    # df2_table <- table(df2_values, useNA = "ifany")
    
    # Print information on the number of NA's.
    if (printColDiffs) {
      
      if ((df1_na_count == 0) && (df2_na_count == 0)) {
        message("Column '", x, "' has 0 NA's.")
      } else {
        message("Column '", x, "' has ", df1_na_count, " NA's in 'df1', and ", df2_na_count, " in 'df2', ",
                ifelse(all(df1_na == df2_na), "all", sum(df1_na & df2_na)), " of which are in the same position.")
      }
      
      # Print the comparisons table
      print(compTable)
      # print(df1_table)
      # print(df2_table)
      
    }
    
    # Compare numeric columns (for rounding errors)
    if (is.double(df1_col) && is.double(df2_col)) { # && (df1_na_count == 0) && (df2_na_count == 0)) {
      nmIndsExclNA <- which(df1_col != df2_col)
      diffs <- abs(df1_col[nmIndsExclNA] - df2_col[nmIndsExclNA])
      if (max(diffs) <= tol) {
        if (printColDiffs) message("All differences for column '", x, "' are <= ", tol, ".")
        return(NULL)
        # return(paste0("All less than ",  tol, "."))
      } else if (printColDiffs) {
        message("The maximum difference for column '", x, "' was ", max(diffs),
                ", and ", sum(diffs > tol), " exceed the desired tolerance (", tol, ").")
      }
    }
    
    # Return a tibble holding the desired values
    tibble::tibble(nmInds, df1_values, df2_values) %>%
      stats::setNames(c("Index", paste0(x, "_df1"), paste0(x, "_df2"))) %>%
      return()
    
  }, simplify = FALSE)
  
  # Return the column comparisons, removing NULL values
  return(colComps[!sapply(colComps, is.null)])
  
}


# d1 <- ggplot2::diamonds
# d2 <- ggplot2::diamonds %>% mutate(x = ifelse(color == "E", x * 2, x), y = ifelse(color == "F", NA, y))
# # microbenchmark::microbenchmark(times = 10000, unit = "ms", compare_dfs(d1, d2))
# a <- compare_dfs(d1, d2) # %>% print()

