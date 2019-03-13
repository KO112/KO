#' Create a function to define AIC within a quasi Poisson family
#'
#' @param ... Input values to families
#'
#' @return Quasi-Poisson family with AIC/BIC defined.
#' @export
#'
#' @examples
#' \dontrun{
#'   data(dataCar, package = "insuranceData")
#'   dataCarGLM <- glm(numclaims ~ veh_value + veh_age + gender + agecat,
#'                     data = dataCar, family = quasi_poisson, offset = log(exposure))
#'   dataCarGLMSummary <- summary(dataCarGLM)
#' }
#'
quasi_poisson <- function(...) {
  res <- stats::quasipoisson(...)
  res$aic <- stats::poisson(...)$aic
  return(res)
}


#' Calculate AIC & BIC
#'
#' Calculate the AIC and BIC for a quasi-Poisson GLM by training a normal Poisson GLM.
#'
#' @param model Quasi-Poisson GLM to calculate the AIC/BIC for
#'
#' @return A list of two elements, AIC and BIC.
#' @export
#'
#' @examples
#' \dontrun{
#'   data(dataCar, package = "insuranceData")
#'   dataCarGLM <- glm(numclaims ~ veh_value + veh_age + gender + agecat,
#'                     data = dataCar, family = quasi_poisson, offset = log(exposure))
#'   quasi_poisson_aic_bic(dataCarGLM)
#' }
#'
quasi_poisson_aic_bic <- function(model) {

  # Train a Poisson GLM based on the passed model
  model_poi <- stats::glm(
    model$y ~ model$x,
    family = stats::poisson(link = "log"),
    data = model$data,
    offset = model$offset,
    subset = model$subset,
    na.action = "na.pass",
    x = TRUE
  )

  # Return the AIC and BIC of the model in a list
  return(list(AIC = stats::AIC(model_poi), BIC = stats::BIC(model_poi)))

}
