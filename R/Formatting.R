# Include other functions in package
#' @include Pipes.R
#' @include Error_Handling.R
NULL

# Avoid "undefined variable" notes in package checking
globalVariables(c("."))


#' Format a numerical vector as percents
#' 
#' Format a numerical vector as a character vector holding the values formatted as percents.
#' Will throw an error for non-numeric vetcors, and  print a message if NA values are found.
#' 
#' @param vec Vector to format as a percent.
#' @param accuracy The number to round to the nearest multiple of
#'   (e.g 3.14159 will round to 314.16\% by default).
#'   The absolute value of this parameter will be used.
#'   The rounding will be done after conversion to a percent.
#' @param vec_names Vector used to name the output.
#' @param silent If \code{TRUE}, will not print out a message if NA values are found.
#' 
#' @return Character vector of items formatted as percent.
#' @export
#' 
#' @examples
#' format_percent(0.12345)
#' format_percent(pi)
#' format_percent(0.1)
#' format_percent(0.12345, accuracy = 0.1)
#' format_percent(c(0.12345, 0.54321), vec_names = c("a", "b"))
#' 
format_percent <- function(vec, accuracy = 0.01, vec_names = names(vec), silent = FALSE) {
  stop_if(!is.numeric(vec), "Non-numeric vector passed.")
  if (any(is.na(vec)) & !silent) message("NA values were found, and will be printed as \"NA%\"")
  return(
    scales::percent(x = vec, accuracy = abs(accuracy)) %>%
      stats::setNames(., vec_names) %>%
      ifelse(is.na(.), "NA%", .)
  )
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
  return(knitr::kable(x = data, format = "latex") %>%
           gsub("\\{([rcl\\|]+)\\}", "\\{|\\1|\\}", .) %>%
           as.character())
}


#' Convert a vector to a padded character column
#' 
#' @param vec Vector to pad values of.
#' @param header Optional name of vector to add to top of return value.
#' @param padding Character to pad output vector with.
#' @param alignment Alignment of text in output (one of 'R', 'L', 'C').
#' 
#' @return A character vector padded with spaces to give uniform width.
#' @export
#' 
#' @examples
#' pad_vector(c(1, 2, 300))
#' pad_vector(c(1, 2), header = "Header")
#' pad_vector(c(1, 2, 300), padding = "_")
#' pad_vector(c(1, 200, 3), alignment = "L")
#' pad_vector(c(100, 2, 30), alignment = "C")
#' 
pad_vector <- function(vec, header = NA, padding = " ", alignment = "R") {

  # If a header was passed, add it as the first element, and convert alignment to upper case
  if (!is.na(header)) vec <- c(header, vec)
  alignment <- toupper(alignment) %>% substr(1, 1)

  # Convert the vector to character, count the number of characters in each element,
  #   find the maximum width, and create a vector holding the padding
  width <- as.character(vec) %>% nchar()
  maxWidth <- max(width)
  paddedVec <- strrep(padding, maxWidth - width)

  # Pad the vector, aligning the values to either the left, center, or right
  if (alignment == "L") {
    paddedVec %<>% paste0(vec, .)
  } else if (alignment == "C") {
    paddedVec <- strrep(padding, maxWidth - width) %>% paste0(vec)
    paddedVec <- paste0(strrep(padding, floor((maxWidth - width) / 2)),
                        vec,
                        strrep(padding, ceiling((maxWidth - width) / 2)))
  } else {
    if (alignment != "R") message("Invalid alignment option chosen. Defaulting to \"R\".")
    paddedVec %<>% paste0(., vec)
  }

  # If a header was passed, add a row of dashes between it and the rest of the vector
  if (!is.na(header)) return(paddedVec %>% {c(.[1], strrep("-", maxWidth), .[-1])})
  return(paddedVec)

}


#' Returns a data frame in plain text with padded (equal width) columns
#' 
#' @param data A data frame to convert to plain text.
#' @param sep Characters to bs used as column seperators.
#' @param cat_out Whether or not to print the value to standard output.
#' 
#' @return If cat_out, nothing, else a string containing the table in plain text form.
#' @export
#' 
#' @examples
#' plain_text_table(head(mtcars))
#' plain_text_table(head(cars))
#' plain_text_table(data.frame(a = 1:2, b = 3:4))
#' plain_text_table(head(mtcars), sep = "+")
#' plain_text_table(head(mtcars), cat_out = FALSE)
#' 
plain_text_table <- function(data, sep = " | ", cat_out = TRUE) {
  plainTextTbl <- mapply(pad_vector, vec = data, header = colnames(data)) %>%
    apply(1, paste0, collapse = sep) %>%
    paste0(collapse = "\n")
  if (cat_out) cat(plainTextTbl)
  else return(plainTextTbl)
}


#' P-Value Stars
#' 
#' Calculate the significance stars for p-values.
#' 
#' @param p_values A numeric vector of p-values.
#' @param alignment Which side to align the significance stars to
#'   (padding spaces will be added to the other side).
#' 
#' @return A character vector of the p-value significance stars.
#' @export
#' 
#' @examples
#' pvalue_stars(c(-Inf, 0, 0.0005, 0.001, 0.005, 0.01, 0.025, 0.05, 0.1, 0.15, 1, Inf, NA))
#' pvalue_stars(c(-Inf, 0, 0.001, 0.01, 0.025, 0.05, 0.1, 0.15, 1, Inf, NA), alignment = "L")
#' 
pvalue_stars <- function(p_values, alignment = "R") {
  return(
    dplyr::case_when(
      p_values <= 0.001 ~ "***",
      p_values <= 0.01 ~ "**",
      p_values <= 0.05 ~ "*",
      p_values <= 0.1 ~ ".",
      TRUE ~ "---"
    ) %>%
      pad_vector(vec = ., alignment = alignment)
  )
}
