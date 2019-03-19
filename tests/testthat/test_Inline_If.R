context("Inline If")


# Test iif
test_that("iif", {
  expect_equal(iif(TRUE, "a", "b"),
               "a")
  expect_equal(iif(10 < 20, TRUE, FALSE),
               TRUE)
  expect_equal(iif(TRUE, 1:2, 3:4),
               1:2)
})
