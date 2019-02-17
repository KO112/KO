context("Trimming")


# Create a sample GLM and GLM summary
mtcarsGLM <- glm(mpg ~ ., data = mtcars)
mtcarsGLMSummary <- summary(mtcarsGLM)


# Test trim_glm size reduction
test_that("trim_glm size reduction", {
  expect(object.size(mtcarsGLM) > object.size(trim_glm(mtcarsGLM)), "Size reduction failed")
})

# Test trim_glm_summary size reduction
test_that("trim_glm_summary size reduction", {
  expect(object.size(mtcarsGLMSummary) > object.size(trim_glm_summary(mtcarsGLMSummary)), "Size reduction failed")
})
