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

# expectedData1 <- data.table::fread(file = "./Test Data/binnedData1.csv", stringsAsFactors = TRUE)
# expectedData2 <- data.table::fread(file = "./Test Data/binnedData2.csv", stringsAsFactors = TRUE)
# expectedData3 <- data.table::fread(file = "./Test Data/binnedData3.csv", stringsAsFactors = TRUE)
# expectedData4 <- data.table::fread(file = "./Test Data/binnedData4.csv", stringsAsFactors = TRUE)
# expectedData5 <- data.table::fread(file = "./Test Data/binnedData5.csv", stringsAsFactors = TRUE)
# expectedData1 <- data.table::data.table(
#   Bins__ = as.factor(c("[0.20,0.54)", "[0.54,0.79)", "[0.79,1.00)", "[1.00,1.03)", "[1.03,1.15)",
#                        "[1.15,1.29)", "[1.29,1.51)", "[1.51,1.65)", "[1.65,2.02)", "[2.02,5.01]"))
#   , x = c(5.22787638240818, 6.13503911438935, 6.64114286670354, 6.90713086498048, 7.08900738074751,
#           7.34574434919767, 7.70014266980421, 7.90616515315557, 8.37607126176521, 8.82991503325015)
#   , y = c(4.73722441246743, 5.64477886846374, 6.14600716370178, 6.40564403068177, 6.59129652765576,
#           6.84004821095511, 7.19297805883652, 7.40134931681738, 7.88768701295146, 8.3093196993509)
#   , z = c(2.92136810622095, 3.47521445606317, 3.80809115962681, 3.96577063800716, 4.05299331643584,
#           4.2176463332137, 4.44242286506286, 4.56534188651218, 4.86576151751169, 5.11063028009022)
#   , Weight__ = c(0.0955734426688804, 0.104406252357429, 0.0684647094687725, 0.119999080586417, 0.108467275379363,
#                  0.102951515117832, 0.0773757664197737, 0.121681526363442, 0.0985069254201201, 0.10257350621797)
# )


# Test binned_one_way_data
test_that("binned_one_way_data", {
  
  expect_identical(binnedData1, expectedData1)
  expect_identical(binnedData2, expectedData2)
  expect_identical(binnedData3, expectedData3)
  expect_identical(binnedData4, expectedData4)
  expect_identical(binnedData5, expectedData5)
  
  expect_identical(binnedPlot1, expectedPlot1)
  expect_identical(binnedPlot2, expectedPlot2)
  expect_identical(binnedPlot3, expectedPlot3)
  expect_identical(binnedPlot4, expectedPlot4)
  expect_identical(binnedPlot5, expectedPlot5)
  
  # # Test the binnedData1
  # expect_equal(binnedData1[, Bins__], expectedData1[, Bins__])
  # expect_equal(binnedData1[, x], expectedData1[, x])
  # expect_equal(binnedData1[, y], expectedData1[, y])
  # expect_equal(binnedData1[, z], expectedData1[, z])
  # expect_equal(binnedData1[, Weight__], expectedData1[, Weight__])
  # 
  # # Test the binnedData2
  # expect_equal(binnedData2[, Bins__], expectedData2[, Bins__])
  # expect_equal(binnedData2[, x], expectedData2[, x])
  # expect_equal(binnedData2[, y], expectedData2[, y])
  # expect_equal(binnedData2[, z], expectedData2[, z])
  # expect_equal(binnedData2[, Weight__], expectedData2[, Weight__])
  # 
  # # Test the binnedData3
  # expect_equal(binnedData3[, Bins__], expectedData3[, Bins__])
  # expect_equal(binnedData3[, x], expectedData3[, x])
  # expect_equal(binnedData3[, y], expectedData3[, y])
  # expect_equal(binnedData3[, z], expectedData3[, z])
  # expect_equal(binnedData3[, Weight__], expectedData3[, Weight__])
  # 
  # # Test the binnedData4
  # expect_equal(binnedData4[, Bins__], expectedData4[, Bins__])
  # expect_equal(binnedData4[, x], expectedData4[, x])
  # expect_equal(binnedData4[, y], expectedData4[, y])
  # expect_equal(binnedData4[, z], expectedData4[, z])
  # expect_equal(binnedData4[, Weight__], expectedData4[, Weight__])
  # 
  # # Test the binnedData5
  # expect_equal(binnedData5[, Bins__], expectedData5[, Bins__])
  # expect_equal(binnedData5[, x], expectedData5[, x])
  # expect_equal(binnedData5[, y], expectedData5[, y])
  # expect_equal(binnedData5[, z], expectedData5[, z])
  # expect_equal(binnedData5[, Weight__], expectedData5[, Weight__])
  
})


# Test binned_one_way_plot
test_that("binned_one_way_plot", {
  
})
