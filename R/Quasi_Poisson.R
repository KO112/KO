#' Create a function to define AIC within a quasi Poisson family
#'
#' @param ... Input values to families
#'
#' @return Quasi Poisson family with AIC/BIC defined
#' @export
#'
#' @examples
#' quasi_poisson()
#'
quasi_poisson <- function(...) {
  res <- stats::quasipoisson(...)
  res$aic <- stats::poisson(...)$aic
  return(res)
}
