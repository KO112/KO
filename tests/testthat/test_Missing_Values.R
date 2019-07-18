context("Missing Values")


# Test percent_missing output
test_that("percent_missing output", {
  expect_equal(percent_missing(c(NA)),
               1)
  expect_equal(percent_missing(c(1, 2, 3, 4, 5)),
               0)
  expect_equal(percent_missing(c(1, 2, NA, 4, 5)),
               0.2)
  expect_equal(percent_missing(c("a", "b", NA)),
               1/3)
  expect_equal(percent_missing(factor(c(1, 2, NA))),
               1/3)
})


# Test num_missing output
test_that("num_missing output", {
  expect_equal(num_missing(c(NA)),
               1)
  expect_equal(num_missing(c(1, 2, 3, 4, 5)),
               0)
  expect_equal(num_missing(c(1, 2, NA, 4, 5)),
               1)
  expect_equal(num_missing(c("a", "b", NA, NA)),
               2)
  expect_equal(num_missing(factor(c(NA, NA, NA))),
               3)
})


# Test replace_missing output
test_that("replace_missing output", {
  expect_equal(replace_missing(c(1, 1, 1, 2, NA)),
               c(1, 1, 1, 2, 1))
  expect_equal(replace_missing(c("a", "a", "b", NA)),
               c("a", "a", "b", "a"))
  expect_equal(replace_missing(factor(c("a", "a", "b", NA))),
               factor(c("a", "a", "b", "a")))
  expect_equal(replace_missing(factor(c(1, 2, 2, NA))),
               factor(c(1, 2, 2, 2)))
  expect_equal(replace_missing(c(1, 1, 1, 2, NA), method = "mode"),
               c(1, 1, 1, 2, 1))
  expect_equal(replace_missing(c(1, NA, 1, 2), method = "median"),
               c(1, 1, 1, 2))
  expect_equal(replace_missing(c(1, NA, 1, 2), method = "mean"),
               c(1, 4/3, 1, 2))
  expect_equal(replace_missing(c(1, NA, 1, 2), method = "mean", return_imputed = TRUE),
               list(Vec = c(1, 4/3, 1, 2), ImputedVal = 4/3))
  expect_equal(replace_missing(c(1, NA, 1, 2), with = 5),
               c(1, 5, 1, 2))
  expect_warning(replace_missing(c(1, NA, 1, 2), method = "mode", with = 5),
                 "Both the 'method' and 'with' fields were specified inside the 'replace_missing' function. The 'method' field will be ignored.")
})
