#' Pipe Backspace
#' 
#' An add-in function that deletes the code between the cursor and the last pipe.
#' 
#' @return The deleted code (character scalar).
#' @export
#' 
#' @examples
#' # Type "apple, banana, cherry", put your cursor anywhere in a word (including on the edges),
#' #   & run the add-in function
#' 
pipe_backspace <- function() {
  
  # Get the context of the call, as well as the contents/selection/position of the context
  # context <- rstudioapi::getActiveDocumentContext()
  context <- rstudioapi::getConsoleEditorContext()
  contents <- context$contents
  selection <- context$selection %>% rstudioapi::primary_selection()
  selStart <- selection$range$start
  selEnd <- selection$range$end
  
  # Find the previous pipe
  
  # Delete the desired code
  
  # Return the deleted code
  return(deletedCode)
  
}
