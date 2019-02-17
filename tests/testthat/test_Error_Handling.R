context("Error Handling")


# Test stop_if
test_that("stop_if", {
  expect_error(stop_if(TRUE, "message"),
               "message")
  expect_silent(stop_if(FALSE, "message"))
  expect_silent(stop_if(NA, "message"))
})
