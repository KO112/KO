# Include other functions in package
#' @include Pipes.R
NULL


#' Pipe Backspace
#' 
#' An add-in function that deletes the code between the cursor and the last pipe.
#' 
#' @return The deleted code, invisibly (character scalar).
#' 
#' @examples
#' 
pipe_backspace <- function() {
  
  # Get the context of the call, as well as the contents/selection/position of the context
  context <- rstudioapi::getActiveDocumentContext()
  # context <- rstudioapi::getConsoleEditorContext()
  contents <- context$contents
  selection <- rstudioapi::primary_selection(context$selection)
  selStart <- selection$range$start
  selEnd <- selection$range$end
  
  # If the cursor is at the start of the line, clear the selection, & exit early
  if (selStart["column"] == 1) {
    rstudioapi::insertText(location = selection$range, text = "", id = context$id)
    return(NULL)
  }
  
  # Find the end position of the previous pipe
  pipePos <- substring(contents[selStart["row"]], 1, selStart["column"]) %>%
    stringi::stri_locate_last_regex("%>%")
  pipeEndPos <- pipePos[1, "end"] + 2
  if (is.na(pipeEndPos)) pipeEndPos <- 0
  if (abs(pipeEndPos - selStart["column"]) <= 1) pipeEndPos <- pipePos[1, "start"]
  
  # Set the output range
  outputRange <- rstudioapi::document_range(
    rstudioapi::document_position(selStart["row"], pipeEndPos),
    rstudioapi::document_position(selEnd["row"], selEnd["column"])
  )
  
  # Delete & return the desired code
  deletedCode <- substring(contents[selStart["row"]], pipeEndPos, selEnd["column"])
  rstudioapi::insertText(location = outputRange, text = "", id = context$id)
  return(invisible(deletedCode))
  
}
