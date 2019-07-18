context("Compare Data-Frames")


# Test compare_df
test_that("compare_df", {
  
  # Returns an empty list, since the objects are identical
  expect_equal(compare_dfs(mtcars, mtcars),
               list() %>% `attr<-`("names", character()))
  
  # Prints a message about the difference in column names
  expect_message(compare_dfs(mtcars, mtcars[, -1]),
                 "Column names in 'df1' but not in 'df2': mpg")
  
  # Prints a message about the difference in values, & returns a tibble showing the differences
  # expect_equal(compare_dfs(mtcars, dplyr::mutate(mtcars, mpg = mpg * 2)),
  #              )
  # 
  # # Same as the above, but doesn't print the message about the differences
  # expect_equal(compare_dfs(mtcars, dplyr::mutate(mtcars, mpg = mpg * 2), printColDiffs = FALSE),
  #              )
  
})
