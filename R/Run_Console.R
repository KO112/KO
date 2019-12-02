#' Run Code in the Console
#' 
#' Run the code currently in the console, without clearing it or moving the selection.
#'
#' @return The code in the console, invisibly (character scalar).
#'
#' @examples
#' # Enter any code in the console, & run the add-in function
#' 
run_console <- function() {
  
  # Get the context of the call, run the code, reset the selection, & return the run code
  context <- rstudioapi::getConsoleEditorContext()
  rstudioapi::sendToConsole(context$contents)
  rstudioapi::setSelectionRanges(rstudioapi::primary_selection(context$selection)$range)
  return(invisible(context$contents))
  
}
