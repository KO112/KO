context("Printing")

# Test vecPrint
test_that("vecPrint", {
  expect_equal(vecPrint(), )
  expect_message(vecPrint(), )
})
