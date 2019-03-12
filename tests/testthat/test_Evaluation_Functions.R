context("Evaluation Metrics")


# Create a sample GLM and predictions from mtcars
mtcarsGLM <- glm(mpg ~ ., data = mtcars, x = TRUE)
preds <- predict(mtcarsGLM, mtcars[-1])


# Create some test data for a quasipoisson GLM
testData <- data.frame(
  pred1 = c("a", "a", "b", "b", "b", "a", "c", "c", "b", "a", "b"),
  pred2 = c(2, 3, 10, 12, 20, 5, 2, 15, 25, 11, 10),
  resp = c(0, 0, 1, 1, 2, 0, 0, 1, 3, 1, 1)
)

# Create a quasipoisson GLM and make predictions from it
# testGLM_P <- glm(resp ~ ., data = testData, family = stats::poisson(link = "log"), x = TRUE)
testGLM_QP <- glm(resp ~ ., data = testData, family = stats::quasipoisson(link = "log"), x = TRUE)
testPreds <- predict(testGLM_QP, testData)


# Test gini_weighted output
test_that("gini_weighted output", {
  expect_equal(gini_weighted(1:10, 1:10),
               -0.3)
  expect_equal(gini_weighted(1:10, 10:1),
               0.3)
  expect_equal(gini_weighted(c(1,5,2,4,3), c(1,4,2,5,3), c(1, 1, 2, 1, 1)),
               -0.245098039215686)
  expect_equal(gini_weighted(mtcars$mpg, preds),
               -0.155618097682376)
  expect_equal(gini_weighted(testData$resp, testPreds),
               -0.50909090909091)
})


# Test gini_weighted_normalized output
test_that("gini_weighted_normalized output", {
  expect_equal(gini_weighted_normalized(1:10, 1:10),
               1)
  expect_equal(gini_weighted_normalized(1:10, 10:1),
               -1)
  expect_equal(gini_weighted_normalized(c(1,5,2,4,3), c(1,4,2,5,3)),
               0.9)
  expect_equal(gini_weighted_normalized(c(1,5,2,4,3), c(1,4,2,5,3), c(1,1,2,1,1)),
               0.925925925925926)
  expect_equal(gini_weighted_normalized(mtcars$mpg, preds),
               0.949746358539261)
  expect_equal(gini_weighted_normalized(testData$resp, testPreds),
               1)
})
