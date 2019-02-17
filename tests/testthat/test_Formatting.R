context("Format Functions")


# Test format_percent output values
test_that("format_percent output", {
  expect_equal(format_percent(0.12345), "12.34%")
  expect_equal(format_percent(pi), "314.16%")
  expect_equal(format_percent(0.1), "10.00%")
  expect_equal(format_percent(0.12345, accuracy = 0.1), "12.3%")
  expect_equal(format_percent(c(0.12345, 0.54321), vecNames = c("a", "b")),
               setNames(c("12.34%", "54.32%"), c("a", "b")))
})

# Test format_percent messages/errors
test_that("format_percent other", {
  expect_message(
    expect_equal(format_percent(NA_real_), "NA%"),
    "NA values were found, and will be printed as \"NA%\""
  )
  expect_silent(expect_equal(format_percent(NA_real_, silent = TRUE), "NA%"))
  expect_error(format_percent(NA), "Non-numeric vector passed.")
})


# Test latex_table output
test_that("latex_table output", {
  expect_equal(latex_table(1),
               "\n\\begin{tabular}{|r|}\n\\hline\nx\\\\\n\\hline\n1\\\\\n\\hline\n\\end{tabular}")
  expect_equal(latex_table(1:2),
               "\n\\begin{tabular}{|r|}\n\\hline\nx\\\\\n\\hline\n1\\\\\n\\hline\n2\\\\\n\\hline\n\\end{tabular}")
  expect_equal(latex_table(head(mtcars)),
               "\n\\begin{tabular}{|l|r|r|r|r|r|r|r|r|r|r|r|}\n\\hline\n  & mpg & cyl & disp & hp & drat & wt & qsec & vs & am & gear & carb\\\\\n\\hline\nMazda RX4 & 21.0 & 6 & 160 & 110 & 3.90 & 2.620 & 16.46 & 0 & 1 & 4 & 4\\\\\n\\hline\nMazda RX4 Wag & 21.0 & 6 & 160 & 110 & 3.90 & 2.875 & 17.02 & 0 & 1 & 4 & 4\\\\\n\\hline\nDatsun 710 & 22.8 & 4 & 108 & 93 & 3.85 & 2.320 & 18.61 & 1 & 1 & 4 & 1\\\\\n\\hline\nHornet 4 Drive & 21.4 & 6 & 258 & 110 & 3.08 & 3.215 & 19.44 & 1 & 0 & 3 & 1\\\\\n\\hline\nHornet Sportabout & 18.7 & 8 & 360 & 175 & 3.15 & 3.440 & 17.02 & 0 & 0 & 3 & 2\\\\\n\\hline\nValiant & 18.1 & 6 & 225 & 105 & 2.76 & 3.460 & 20.22 & 1 & 0 & 3 & 1\\\\\n\\hline\n\\end{tabular}")
  expect_equal(latex_table(head(cars)),
               "\n\\begin{tabular}{|r|r|}\n\\hline\nspeed & dist\\\\\n\\hline\n4 & 2\\\\\n\\hline\n4 & 10\\\\\n\\hline\n7 & 4\\\\\n\\hline\n7 & 22\\\\\n\\hline\n8 & 16\\\\\n\\hline\n9 & 10\\\\\n\\hline\n\\end{tabular}")
})