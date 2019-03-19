context("Summarize")


# Test count_distinct
test_that("count_distinct", {
  expect_equal(count_distinct(mtcars),
               setNames(c(25, 3, 27, 22, 22, 29, 30, 2, 2, 3, 6), names(mtcars)))
  expect_equal(count_distinct(as.matrix(mtcars)),
               setNames(c(25, 3, 27, 22, 22, 29, 30, 2, 2, 3, 6), names(mtcars)))
  expect_equal(count_distinct(mtcars$mpg),
               25)
  expect_equal(count_distinct(c(1, 1, 1, NULL, NA)),
               2)
  expect_equal(count_distinct(c("a", "b", "c", "a", "A", NA)),
               5)
})


# Test table_df
test_that("table_df", {
  expect_identical(table_df(mtcars),
                   sapply(mtcars, table, useNA = "ifany"))
  expect_identical(table_df(mtcars),
                   sapply(mtcars, table, useNA = "ifany"))
  expect_identical(table_df(mtcars$am),
                   table(mtcars$am))

})


# Test mode_stat
# test_that("mode_stat", {
#   expect_equal(mode_stat(),
#                )
# })


# Test summarize_vec
# test_that("summarize_vec", {
#   expect_equal(summarize_vec(),
#                )
# })


# Test summarize_df
# test_that("summarize_df", {
#   expect_equal(summarize_df(),
#                )
# })
