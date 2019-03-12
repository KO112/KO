context("Bounding and Rounding")


# Test bound
test_that("bound output", {
  expect_equal(bound(1:10, 2, 8),
               c(2, 2, 3, 4, 5, 6, 7, 8, 8, 8))
  expect_equal(bound(1:5, upper = 3),
               c(1, 2, 3, 3, 3))
  expect_equal(bound(1:5, lower = 3),
               c(3, 3, 3, 4, 5))
  expect_equal(bound(1:5),
               1:5)
})


# Test divide_round_bound
test_that("divide_round_bound output", {
  expect_equal(divide_round_bound(1:10, 2),
               c(0, 1, 2, 2, 2, 3, 4, 4, 4, 5))
  expect_equal(divide_round_bound(1:10, 2, 2, 4),
               c(2, 2, 2, 2, 2, 3, 4, 4, 4, 4))
})
