# Include external operators
#' @include utils-pipe.R
NULL

# Avoid "undefined variable" notes in package checking
globalVariables(c("."))


#' Format a numerical vector as percents
#'
#' @param x Object to format as a percent
#' @param accuracy The number to round to the nearest multiple of
#'   (e.g 3.14159 will round to 314.16% by default).
#'   The absolutely value of the value passed will be used.
#'   The rounding will be done after conversion to a percent.
#' @param vecNames Vector used to name the output
#'
#' @return Character vector of items formatted as percent.
#' @export
#'
#' @examples
#' format_percent(0.12345)
#' format_percent(pi)
#' format_percent(0.1)
#'
format_percent <- function(x, accuracy = 0.01, vecNames = names(x)) {
  scales::percent(x = x, accuracy = abs(accuracy)) %>%
    stats::setNames(vecNames) %>%
    return()
}


#' Format a data frame in LaTeX code
#'
#' Format a data frame to print out as a LaTeX table with side borders.
#'
#' @param data Data frame to be formatted into a LaTeX table
#'
#' @return String to use used for printing a table in LaTeX.
#' @export
#'
#' @examples
#' latex_table(mtcars)
#'
latex_table <- function(data) {
  knitr::kable(x = data, format = "latex") %>%
    gsub("\\{([rcl\\|]+)\\}", "\\{|\\1|\\}", .) %>%
    as.character() %>%
    return()
}
