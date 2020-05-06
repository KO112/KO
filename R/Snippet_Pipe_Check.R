# Allow convenient use of functions from other packages
#' @include Pipes.R
NULL


#' Snippet Pipe Check
#' 
#' Decides whether or not to insert a pipe as well during snippet insertion.
#' 
#' @param afterStr Text to insert after the pipe (character scalar).
#' @param elseStr Text to insert if no pipe is needed (character scalar).
#' @param pipeStr The pipe to insert if needed (character scalar).
#' 
#' @return The deleted code (character scalar).
#' 
#' @examples
#' # Set up a snippet as:
#' # snippet mu
#' # `r KO:::snippet_pipe_check();`mutate(${0})
#' 
#' # Then type "mtcars mu", & run the add-in function
#' # Or type "mtcars %>% mu", & run the add-in function
#' # The output for both will be "mtcars %>% mutate()"
#' 
snippet_pipe_check <- function(afterStr = "", elseStr = "", pipeStr = "%>% ") {
  
  # If no "else" string was provided, set it based off the "after" string
  if (missing(elseStr)) {
    if (grepl("()", afterStr, fixed = TRUE)) {
      elseStr <- sub("()", "(${0})", afterStr, fixed = TRUE)
    } else if (grepl("\\($", afterStr)) {
      elseStr <- sub("\\($", "(${0}, ", afterStr)
    }
  }
  
  # Get the context of the call, as well as the contents/selection/position of the context
  context <- rstudioapi::getActiveDocumentContext()
  contents <- context$contents
  selection <- rstudioapi::primary_selection(context$selection)
  selStart <- selection$range$start
  
  # Extract the code before the cursor as a string
  beforeStr <- substring(contents[selStart["row"]], 1, selStart["column"] - 1)
  
  # If the cursor is touching a word on the border, just return the "else" string
  if (grepl("^(\\w)+$", beforeStr)) return(elseStr)
  
  # Set a list of characters that we don't want a pipe after
  noPipeAfter <- c(
    "~", "!", "@", "#", "$", "%", "^", "&", "*", "\\(", "\\-", "=", "+",
    "\\[", "\\{", "\\\\", "\\|", ";", ":", ",", "<", ">", "/", "?"
  )
  
  # Determine whether the current expressions needs a pipe, & return a pipe if one is needed
  # The "\\w+$" at the end is needed to match the snippet being evaluated
  needsPipe <- stringi::stri_extract_last_regex(
      beforeStr, paste0("^[ ]+\\w+$|[", paste0(noPipeAfter, collapse = ""), "]+\\s*\\w+$")
    ) %>% is.na()
  if (needsPipe) return(paste0(pipeStr, afterStr)) else return(elseStr)
  
}
