context("Binned Data and Plots")


# Create datasets to test
diamonds <- data.table::data.table(ggplot2::diamonds)
binnedData1 <- binned_one_way_data(diamonds[, carat], diamonds[, .(x = x + 0.5, y, z)], diamonds[, price])
binnedData2 <- binned_one_way_data(diamonds[, carat], diamonds[, .(x = x + 0.5, y, z)], diamonds[, price], scaleWeight = FALSE)
binnedData3 <- binned_one_way_data(diamonds[, carat], diamonds[, .(x = x + 0.5, y, z)], diamonds[, price], type = "equal")
binnedData4 <- binned_one_way_data(diamonds[, carat], diamonds[, .(x = x + 0.5, y, z)], diamonds[, price], bins = 5)
binnedData5 <- binned_one_way_data(diamonds[, carat], diamonds[, .(x = x + 0.5, y, z)], diamonds[, price],
                                  scaleWeight = FALSE, type = "equal", bins = 7)

binnedPlot1 <- binned_one_way_plot(diamonds[, carat], diamonds[, .(x = x + 0.5, y, z)], diamonds[, price])
binnedPlot2 <- binned_one_way_plot(diamonds[, carat], diamonds[, .(x = x + 0.5, y, z)], diamonds[, price], scaleWeight = FALSE)
binnedPlot3 <- binned_one_way_plot(diamonds[, carat], diamonds[, .(x = x + 0.5, y, z)], diamonds[, price], type = "equal")
binnedPlot4 <- binned_one_way_plot(diamonds[, carat], diamonds[, .(x = x + 0.5, y, z)], diamonds[, price], bins = 5)
binnedPlot5 <- binned_one_way_plot(diamonds[, carat], diamonds[, .(x = x + 0.5, y, z)], diamonds[, price],
                                  scaleWeight = FALSE, type = "equal", bins = 7)


# Read in the expected data and plots
expectedData1 <- readRDS("./Test Data/binnedData1.RDS")
expectedData2 <- readRDS("./Test Data/binnedData2.RDS")
expectedData3 <- readRDS("./Test Data/binnedData3.RDS")
expectedData4 <- readRDS("./Test Data/binnedData4.RDS")
expectedData5 <- readRDS("./Test Data/binnedData5.RDS")

expectedPlot1 <- readRDS("./Test Data/binnedPlot1.RDS")
expectedPlot2 <- readRDS("./Test Data/binnedPlot2.RDS")
expectedPlot3 <- readRDS("./Test Data/binnedPlot3.RDS")
expectedPlot4 <- readRDS("./Test Data/binnedPlot4.RDS")
expectedPlot5 <- readRDS("./Test Data/binnedPlot5.RDS")


# 



# Test binned_one_way_data
test_that("binned_one_way_data", {
  
  expect_equal(binnedData1, expectedData1)
  expect_equal(binnedData2, expectedData2)
  expect_equal(binnedData3, expectedData3)
  expect_equal(binnedData4, expectedData4)
  expect_equal(binnedData5, expectedData5)
  
  expect_identical(binnedPlot1, expectedPlot1)
  expect_identical(binnedPlot2, expectedPlot2)
  expect_identical(binnedPlot3, expectedPlot3)
  expect_identical(binnedPlot4, expectedPlot4)
  expect_identical(binnedPlot5, expectedPlot5)
  
})


# Test binned_one_way_plot
test_that("binned_one_way_plot", {
  
})
