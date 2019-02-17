context("Missing Values")


# Test percent_missing_vals output
test_that("percent_missing_vals", {
  expect_equal(percent_missing_vals(c(NA)),
               1)
  expect_equal(percent_missing_vals(c(1, 2, 3, 4, 5)),
               0)
  expect_equal(percent_missing_vals(c(1, 2, NA, 4, 5)),
               0.2)
})


# Test replace_missing output
test_that("replace_missing", {
  expect_equal(replace_missing(c(1, 1, 1, 2, NA)),
               c(1, 1, 1, 2, 1))
  expect_equal(replace_missing(c(1, 1, 1, 2, NA), method = "mode"),
               c(1, 1, 1, 2, 1))
  expect_equal(replace_missing(c(1, NA, 1, 2), method = "median"),
               c(1, 1, 1, 2))
  expect_equal(replace_missing(c(1, NA, 1, 2), method = "mean"),
               c(1, 4/3, 1, 2))
  expect_equal(replace_missing(c(1, NA, 1, 2), method = "mean", returnImputed = TRUE),
               list(Vec = c(1, 4/3, 1, 2), ImputedVal = 4/3))
})
