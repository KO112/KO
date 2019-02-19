context("Evaluation Metrics")


# Test gini_weighted output
test_that("gini_weighted output", {
  expect_equal(gini_weighted(1:10, 1:10),
               -0.3)
  expect_equal(gini_weighted(1:10, 10:1),
               0.3)
  expect_equal(gini_weighted(c(1,5,2,4,3), c(1,4,2,5,3), c(1, 1, 2, 1, 1)),
               -0.245098039215686)
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
})


# Test quasi_poisson_aic_bic output
test_that("quasi_poisson_aic_bic output", {
  expect_equal(T, T)
})



# WeightedGini <- function(solution, weights = NULL, submission) {
#   if (is.null(weights)) {
#     weights = rep(1, length(solution))
#   }
#   df = data.frame(solution = solution, weights = weights, submission = submission)
#   df <- df[order(df$submission, decreasing = TRUE),]
#   df$random = cumsum((df$weights/sum(df$weights)))
#   totalPositive <- sum(df$solution * df$weights)
#   df$cumPosFound <- cumsum(df$solution * df$weights)
#   df$Lorentz <- df$cumPosFound / totalPositive
#   n <- nrow(df)
#   gini <- sum(df$Lorentz[-1]*df$random[-n]) - sum(df$Lorentz[-n]*df$random[-1])
#   return(gini)
# }
#
# NormalizedWeightedGini <- function(solution, weights = NULL, submission) {
#   WeightedGini(solution, weights, submission) / WeightedGini(solution, weights, solution)
# }
