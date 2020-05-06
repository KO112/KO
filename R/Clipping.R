# Allow convenient use of functions from other packages
#' @include Pipes.R
NULL


#' Transform a Vector to Code
#'
#' @param vec The vector to transform (atomic vector).
#' @param width The approximate desired line width (integer scalar).
#' @param indent The number of spaces to use as an indent (integer scalar).
#' @param tol The tolerance for going above thw width (integer scalar).
#'
#' @return A character scalar of the vector transformed to code.
#' @export
#'
#' @examples
#' 
#' set.seed(112)
#' s <- replicate(100, sample(letters, floor(runif(1, 8, 13)), replace = TRUE))
#' s <- sapply(s, paste0, collapse = "")
#' v <- vec_to_code(s)
#' cat(v)
#' purrr::map_int(strsplit(v, "\n")[[1]], nchar)
#' 
vec_to_code <- function(vec, width = 100, indent = 2, tol = 5) {
  
  # Ensure the input is atomic, & quote character vectors
  if (!is.atomic(vec)) stop("The input vector must be atomic.")
  if (is.character(vec)) vec <- paste0('"', vec, '", ')
  
  # Initialize the current line & line width
  curLine <- 1L
  curLineWidth <- 0
  
  # Find the split points
  splits <- purrr::map_int(nchar(vec), ~ {
    
    # Update the current line width
    curLineWidth <<- curLineWidth + .x
    
    # Determine whether to add the element to the current line, or the next one
    if ((curLineWidth >= width - indent) && (curLineWidth <= width - indent + tol)) {
      curLine <<- curLine + 1L
      curLineWidth <<- 0
      return(curLine - 1L)
    } else if (curLineWidth > width - indent) {
      curLine <<- curLine + 1L
      curLineWidth <<- .x
    }
    
    # Return the current line
    return(curLine)
    
  })
  
  # Split the input vector, join each line together, & concatenate the lines
  return(
    split(vec, splits) %>%
      purrr::map_chr(~ paste0(.x, collapse = "") %>% trimws() %>% paste0(strrep(" ", indent), .)) %>%
      paste0(collapse = "\n") %>%
      gsub(",$", "", .)
  )
  
}
