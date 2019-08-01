#' Remove excess parts of a GLM
#' 
#' Remove parts of a GLM not needed for prediction to trim down size.
#' 
#' @param model A GLM object.
#' 
#' @return A trimmed-down GLM, with only the parts needed for predicton.
#' @export
#' 
#' @examples
#' mtcarsGLM <- glm(mpg ~ ., data = mtcars)
#' object.size(mtcarsGLM)
#' object.size(trim_glm(mtcarsGLM))
#' 
trim_glm <- function(model) {

  # Assert that the model object is a GLM
  stop_if(!inherits(model, "glm"), "Passed object is not a GLM.")

  # Set the vector of parts of the GLM to keep in the final output
  keep <- c(
    "coefficients"
    , "rank"
    , "qr"
    , "family"
    , "call"
    , "formula"
    , "terms"
    , "deviance"
    , "null.deviance"
    , "aic"
    , "xlevels"
  )

  # Remove everything that we don't specifically want to keep
  sapply(names(model), function(part) {
    if (!(part %in% keep))
      model[[part]] <<- NULL
  })

  # Trim the family object (maybe leave in)
  model$family$variance <- NULL
  model$family$dev.resids <- NULL
  model$family$aic <- NULL
  model$family$validmu <- NULL
  model$family$simulate <- NULL

  # Trim the qr object and remove environment pointers
  model$qr$qr <- NULL
  attr(model$terms, ".Environment") <- NULL
  attr(model$formula, ".Environment") <- NULL

  # Return the tirmmed model
  return(model)

}


#' Remove excess parts of a GLM summary
#' 
#' Remove parts of a GLM summary to shrink the size.
#' 
#' @param model_smy Summary of a GLM object.
#' 
#' @return A trimmed-down GLM summary.
#' @export
#' 
#' @examples
#' mtcarsGLMSummary <- summary(glm(mpg ~ ., data = mtcars))
#' object.size(mtcarsGLMSummary)
#' object.size(trim_glm_summary(mtcarsGLMSummary))
#' 
trim_glm_summary <- function(model_smy) {
  stop_if(!inherits(model_smy, "summary.glm"), "Passed object is not a GLM model summary.")
  model_smy$deviance.resid <- NULL
  attr(model_smy$terms, ".Environment") <- NULL
  attr(model_smy$formula, ".Environment") <- NULL
  return(model_smy)
}
