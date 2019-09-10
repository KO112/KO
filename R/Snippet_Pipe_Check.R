# Include other functions in package
#' @include Pipes.R
NULL


#' Snippet Pipe Check
#' 
#' Decides whether or not to insert a pipe as well during snippet insertion.
#' 
#' @param pipeStr The pipe to insert if needed (character scalar).
#' @param afterStr Text to insert after the pipe (character scalar).
#' @param elseStr Text to insert if no pipe is needed (character scalar).
#'
#' @return The deleted code (character scalar).
#' 
#' @examples
#' 
#' 
snippet_pipe_check <- function(pipeStr = "%>% ", afterStr = "", elseStr = "") {
  
  # Get the context of the call, as well as the contents/selection/position of the context
  context <- rstudioapi::getActiveDocumentContext()
  # context <- rstudioapi::getConsoleEditorContext()
  contents <- context$contents
  selection <- rstudioapi::primary_selection(context$selection)
  selStart <- selection$range$start
  
  # Extract the code before the cursor as a string
  beforeStr <- substring(contents[selStart["row"]], 1, selStart["column"])
  
  # If the cursor is touching a word on the border, just return the after string
  if (grepl("^(\\w)+$", beforeStr)) return(afterStr)
  
  # Determine whether the current expressions has just been piped, & return a pipe if one is needed
  isPiped <- stringi::stri_extract_last_regex(
      beforeStr, paste0("(", gsub("^[ ]*|[ ]*$", "", pipeStr), ")+[ ]*\\w+$")
    ) %>% .[[1]] %>% is.na()
  if (isPiped) return(paste0(pipeStr, afterStr)) else return(elseStr)
  
  
}
