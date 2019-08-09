context("Select Matrix")


# Set the matrix for testing
mat <- as.matrix(mtcars)


# Test basic selection
test_that("select.matrix basic selection", {
  
  expect_equal(dplyr::select(mat, mpg),
               mat[, "mpg", drop = FALSE])
  expect_equal(dplyr::select(mat, mpg, cyl),
               mat[, c("mpg", "cyl"), drop = FALSE])
  expect_equal(dplyr::select(mat, mpg, cyl, "disp"),
               mat[, c("mpg", "cyl", "disp"), drop = FALSE])
  expect_equal(dplyr::select(mat, "mpg", "cyl", "disp", "am"),
               mat[, c("mpg", "cyl", "disp", "am"), drop = FALSE])
  
})
  
# Test messages/warnings/errors
test_that("select.matrix messages/warnings/errors", {
  
  expect_output(
    expect_error(dplyr::select(mat, mpg2),
                 "`select.matrix`: Invalid columns selected.")
    , "`select.matrix`: Invalid columns selected:\nmpg2"
  )
  expect_output(
    expect_error(dplyr::select(mat, mpg2, throwError = TRUE),
                 "`select.matrix`: Invalid columns selected.")
    , "`select.matrix`: Invalid columns selected:\nmpg2"
  )
  expect_output(
    expect_warning(dplyr::select(mat, mpg2, throwError = FALSE),
                   "`select.matrix`: Invalid columns selected.")
    , "`select.matrix`: Invalid columns selected:\nmpg2"
    
  )
  
})
