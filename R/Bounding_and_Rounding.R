# Include other functions in package
#' @include Pipes.R
NULL


#' Bound or Divide/Round/Bound
#'
#' Bound a number by two other numbers, or divide, round, and bound
#'
#' @param num Numeric vector to bound.
#' @param lower Lower bound to floor the result at.
#' @param upper Upper bound to cap the result at.
#'
#' @return \code{bound}: bounded numeric vector.
#' @name bounding
#' @export
#'
#' @examples
#' bound(1:10, 2, 8)
#' bound(runif(10), 0.25, 0.75)
#' bound(1:5, upper = 3)
#' bound(1:5, lower = 3)
#'
bound <- function(num, lower = -Inf, upper = Inf) {
  return(num %>% pmax(lower) %>% pmin(upper))
}


#' @param div Numeric vector to divide \code{num} by.
#' @param type Type of rounding to be performed (one of "floor", "ceiling", or "round").
#'
#' @return \code{divide_round_bound}: divided, rounded, and bounded integer vector.
#' @rdname bounding
#' @export
#'
#' @examples
#' divide_round_bound(1:10, 2, 2, 4)
#' divide_round_bound(runif(10), 0.01, 20, 80)
#' divide_round_bound(1:100, 10, type = "ceiling")
#' divide_round_bound(1:100, 10, type = "floor")
#' divide_round_bound(1:100, 10, type = "round")
#' divide_round_bound(1:100, 10)
#'
divide_round_bound <- function(num, div = 1, lower = -Inf, upper = Inf, type = "round") {
  return(
    switch(tolower(type), floor = floor(num / div), ceiling = ceiling(num / div), round = , round(num / div)) %>%
      bound(lower, upper) %>%
      as.integer()
  )
}
