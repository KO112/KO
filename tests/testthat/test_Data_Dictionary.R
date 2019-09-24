context("Data Dictionary")


# Set up testing objects
colCounts <- as.integer(c(25, 3, 27, 22, 22, 29, 30, 2, 2, 3, 6))
summaryTbl <- tibble::tibble(
  Column = colnames(mtcars)
  , Class = "numeric"
  , NumUnique = colCounts
  , Tabulated = FALSE
)


# Set expected messages
lazyTableStr <- "`columnTables`: Lazy table mode active."
expressionMsg <- c(
    "`dataDict`: `df` has been passed as an expression (tibble::as_tibble(mtcars)).",
    "            This may result in problems when using this `dataDict` object, but should be fine."
  ) %>%
  paste0(collapse = "\n") %>%
  info(printOut = FALSE)
keepNameMsg <- c(
    "`dataDict`: This `dataDict` will be based off of the object named 'mtcars'.",
    "            To ensure that this `dataDict` will continue to work, do not change the name of the object,",
    "            You can alternatively use the `updateDD` function to update this `dataDict` after the object changes."
  ) %>%
    paste0(collapse = "\n") %>%
    info(printOut = FALSE)

# Set the expected output
outputStr <- paste0(c(
    "This `dataDict` object (dd) was based off of 'mtcars' (a 'data.frame') in the 'R_GlobalEnv' environment,"
    , "  which had 32 rows and 11 columns when this `dataDict` was constructed."
    , "The `tableMode` mode is set to 'lazy', and the `verbose` level is 'Inf'."
    , "# A tibble: 11 x 4"
    , "   Column Class   NumUnique Tabulated"
    , "   <chr>  <chr>       <int> <lgl>    "
    , " 1 mpg    numeric        25 FALSE    "
    , " 2 cyl    numeric         3 FALSE    "
    , " 3 disp   numeric        27 FALSE    "
    , " 4 hp     numeric        22 FALSE    "
    , " 5 drat   numeric        22 FALSE    "
    , " 6 wt     numeric        29 FALSE    "
    , " 7 qsec   numeric        30 FALSE    "
    , " 8 vs     numeric         2 FALSE    "
    , " 9 am     numeric         2 FALSE    "
    , "10 gear   numeric         3 FALSE    "
    , "11 carb   numeric         6 FALSE    "
  ), collapse = "\n")


# Test for messages/lack thereof
test_that("dataDict messages", {
  
  expect_silent({dataDict(mtcars, verbose = 0)})
  expect_message({dd <- dataDict(mtcars, verbose = 1)}, lazyTableStr)
  expect_message({dd <- dataDict(mtcars)}, lazyTableStr)
  expect_message({dd <- dataDict(tibble::as_tibble(mtcars))}, expressionMsg, fixed = TRUE)
  expect_message({dd <- dataDict(mtcars)}, keepNameMsg, fixed = TRUE)
  
})


# Check the output of the printing, the printing return value, & the summary
dd <- dataDict(mtcars)
test_that("dataDict output", {
  
  expect_output(print(dd), outputStr, fixed = TRUE)
  invisible(capture.output(expect_equal(print(dd), summaryTbl)))
  expect_identical(summary(dd), summaryTbl)
  
})


# Test basic values
test_that("dataDict basic values", {
  
  expect_equal(dd$classes, setNames(rep("numeric", 11), names(mtcars)))
  expect_equal(dd$dfClass, "data.frame")
  expect_equal(dd$numUnique, setNames(colCounts, names(mtcars)))
  
  expect_equal(dd[["classes"]], setNames(rep("numeric", 11), names(mtcars)))
  expect_equal(dd[["dfClass"]], "data.frame")
  expect_equal(dd[["numUnique"]], setNames(colCounts, names(mtcars)))
  
  expect_equal(dd["classes"], setNames(rep("numeric", 11), names(mtcars)))
  expect_equal(dd["dfClass"], "data.frame")
  expect_equal(dd["numUnique"], setNames(colCounts, names(mtcars)))
  
})


# Test generic functions
test_that("dataDict generic functions", {
  
  expect_equal(dim(dd), c(32, 11))
  expect_equal(ncol(dd), 11)
  expect_equal(nrow(dd), 32)
  expect_equal(names(dd), colnames(mtcars))
  expect_equal(colnames(dd), colnames(mtcars))
  expect_equal(rownames(dd), rownames(mtcars))
  
})

# Test attributes
test_that("dataDict attributes", {
  
  expect_equal(attr(dd, "verbose"), Inf)
  expect_equal(attr(dd, "dfName"), "mtcars")
  expect_equal(attr(dd, "tableMode"), "lazy")
  
})


# Test tabulation
test_that("dataDict tabulation", {
  
  expect_equal(dd$colTables$am, table(mtcars$am, useNA = "ifany", dnn = NULL))
  expect_equal(dd$colTables$disp, table(mtcars$disp, useNA = "ifany", dnn = NULL))
  expect_equal(dd$colTables$mpg, table(mtcars$mpg, useNA = "ifany", dnn = NULL))
  
  expect_equal(dd$colTables["am"], list(am = table(mtcars$am, useNA = "ifany", dnn = NULL)))
  expect_equal(dd$colTables["disp"], list(disp = table(mtcars$disp, useNA = "ifany", dnn = NULL)))
  expect_equal(dd$colTables["mpg"], list(mpg = table(mtcars$mpg, useNA = "ifany", dnn = NULL)))
  
  expect_equal(dd$colTables[["am"]], table(mtcars$am, useNA = "ifany", dnn = NULL))
  expect_equal(dd$colTables[["disp"]], table(mtcars$disp, useNA = "ifany", dnn = NULL))
  expect_equal(dd$colTables[["mpg"]], table(mtcars$mpg, useNA = "ifany", dnn = NULL))
  
  expect_equal(dd["colTables", "am"], list(am = table(mtcars$am, useNA = "ifany", dnn = NULL)))
  expect_equal(dd["colTables", "disp"], list(disp = table(mtcars$disp, useNA = "ifany", dnn = NULL)))
  expect_equal(dd["colTables", "mpg"], list(mpg = table(mtcars$mpg, useNA = "ifany", dnn = NULL)))
  
})


# Test tabulated names
test_that("dataDict tabulated names", {
  
  expect_equal(names(dd$colTables), c("am", "disp", "mpg"))
  
})


# Test multiple tabulation
# dd <- dataDict(mtcars)
# test_that("dataDict multiple tabulation", {
# 
#   expect_identical(
#     dd["colTables", c("am", "disp", "mpg")],
#     list(
#       am = table(mtcars$am, useNA = "ifany", dnn = NULL)
#       , disp = table(mtcars$disp, useNA = "ifany", dnn = NULL)
#       , mpg = table(mtcars$mpg, useNA = "ifany", dnn = NULL)
#     )
#   )
# 
# })
