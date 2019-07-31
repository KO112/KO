context("Data Dictionary")


# Set up testing objects
colCounts <- as.integer(c(25, 3, 27, 22, 22, 29, 30, 2, 2, 3, 6))
summaryTbl <- tibble::tibble(
  Column = colnames(mtcars)
  , Class = "numeric"
  , NumUnique = colCounts
  , Tabulated = FALSE
)


# Test dataDict
if (interactive()) {
  
  test_that("dataDict", {
    
    # Test for messages/lack thereof
    expect_silent({dataDict(mtcars, verbose = 0)})
    expect_message({dd <- dataDict(mtcars, verbose = 1)},
                   "`columnTables`: Lazy table mode active.")
    expect_message({dd <- dataDict(mtcars)},
                   "`columnTables`: Lazy table mode active.")
    expect_message(
      {dd <- dataDict(tibble::as_tibble(mtcars))},
      paste0(c(
        "`dataDict`: `df` has been passed as an expression (tibble::as_tibble(mtcars)).",
        "            This may result in problems when using this `dataDict` object, but should be fine."
      ), collapse = "\n"), fixed = TRUE
    )
    expect_message(
      {dd <- dataDict(mtcars)},
      paste0(c(
        "`dataDict`: This `dataDict` will be based off of the object named 'mtcars'.",
        "            To ensure that this `dataDict` will continue to work, do not change the name of the object,",
        "              or use the `updateDD` function after the object changes."
      ), collapse = "\n"), fixed = TRUE
    )
    
    # Check the output of the printing
    expect_output(print(dd), "This `dataDict` object (dd) was based off of 'mtcars' (a 'data.frame') in the 'R_GlobalEnv' environment,\n  which had 32 rows and 11 columns when this `dataDict` was constructed.\nThe `tableMode` mode is set to 'lazy', and the `verbose` level is 'Inf'.\n# A tibble: 11 x 4\n   Column Class   NumUnique Tabulated\n   <chr>  <chr>       <int> <lgl>    \n 1 mpg    numeric        25 FALSE    \n 2 cyl    numeric         3 FALSE    \n 3 disp   numeric        27 FALSE    \n 4 hp     numeric        22 FALSE    \n 5 drat   numeric        22 FALSE    \n 6 wt     numeric        29 FALSE    \n 7 qsec   numeric        30 FALSE    \n 8 vs     numeric         2 FALSE    \n 9 am     numeric         2 FALSE    \n10 gear   numeric         3 FALSE    \n11 carb   numeric         6 FALSE    ", fixed = TRUE)
    
    # Test the summary
    expect_identical(summary(dd), summaryTbl)
    
    # Test the printing return
    invisible(capture.output(expect_equal(print(dd), summaryTbl)))
    
    # Test basic values
    expect_equal(dd$classes, setNames(rep("numeric", 11), names(mtcars)))
    expect_equal(dd$dfClass, "data.frame")
    expect_equal(dd$numUnique, setNames(colCounts, names(mtcars)))
    
    expect_equal(dd[["classes"]], setNames(rep("numeric", 11), names(mtcars)))
    expect_equal(dd[["dfClass"]], "data.frame")
    expect_equal(dd[["numUnique"]], setNames(colCounts, names(mtcars)))
    
    expect_equal(dd["classes"], setNames(rep("numeric", 11), names(mtcars)))
    expect_equal(dd["dfClass"], "data.frame")
    expect_equal(dd["numUnique"], setNames(colCounts, names(mtcars)))
    
    # Test generic functions
    expect_equal(dim(dd), c(32, 11))
    expect_equal(ncol(dd), 11)
    expect_equal(nrow(dd), 32)
    expect_equal(names(dd), colnames(mtcars))
    expect_equal(colnames(dd), colnames(mtcars))
    expect_equal(rownames(dd), rownames(mtcars))
    
    # Test attributes
    expect_equal(attr(dd, "verbose"), Inf)
    expect_equal(attr(dd, "dfName"), "mtcars")
    expect_equal(attr(dd, "tableMode"), "lazy")
    
    # Test tabulation
    expect_equal(dd$colTables$am, list(am = table(mtcars$am, useNA = "ifany", dnn = NULL)))
    expect_equal(dd$colTables$disp, list(disp = table(mtcars$disp, useNA = "ifany", dnn = NULL)))
    expect_equal(dd$colTables$mpg, list(mpg = table(mtcars$mpg, useNA = "ifany", dnn = NULL)))
    
    expect_equal(dd$colTables["am"], list(am = table(mtcars$am, useNA = "ifany", dnn = NULL)))
    expect_equal(dd$colTables["disp"], list(disp = table(mtcars$disp, useNA = "ifany", dnn = NULL)))
    expect_equal(dd$colTables["mpg"], list(mpg = table(mtcars$mpg, useNA = "ifany", dnn = NULL)))
    
    expect_equal(dd$colTables[["am"]], table(mtcars$am, useNA = "ifany", dnn = NULL))
    expect_equal(dd$colTables[["disp"]], table(mtcars$disp, useNA = "ifany", dnn = NULL))
    expect_equal(dd$colTables[["mpg"]], table(mtcars$mpg, useNA = "ifany", dnn = NULL))
    
    expect_equal(dd["colTables", "am"], list(am = table(mtcars$am, useNA = "ifany", dnn = NULL)))
    expect_equal(dd["colTables", "disp"], list(disp = table(mtcars$disp, useNA = "ifany", dnn = NULL)))
    expect_equal(dd["colTables", "mpg"], list(mpg = table(mtcars$mpg, useNA = "ifany", dnn = NULL)))
    
    expect_equal(
      dd["colTables", c("am", "disp", "mpg")],
      list(
        am = table(mtcars$am, useNA = "ifany", dnn = NULL)
        , disp = table(mtcars$disp, useNA = "ifany", dnn = NULL)
        , mpg = table(mtcars$mpg, useNA = "ifany", dnn = NULL)
      )
    )
    
    # Test tabulated names
    # expect_equal(names(dd$colTables), c("am", "disp", "mpg"))
    
  })

}
