context("Compare ggplots")


# Create plots to compare with
plot1 <- ggplot2::qplot(mpg, wt, data = mtcars, colour = cyl)
plot2 <- ggplot2::qplot(mpg, wt, data = mtcars, colour = cyl)
plot3 <- ggplot2::qplot(mpg, wt, data = mtcars, colour = carb)


# Test compare_ggplots
test_that("compare_ggplots", {
  
  expect_true(compare_ggplots(plot1, plot2))
  expect_true(compare_ggplots(plot1, plot2, useTempFiles = FALSE))
  expect_false(compare_ggplots(plot1, plot3))
  expect_false(compare_ggplots(plot1, plot3, useTempFiles = FALSE))
  
})
