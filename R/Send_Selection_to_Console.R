#' Send Code to the Console
#' 
#' Insert the currently highlighted code into the console.
#'
#' @return The inserted text, invisibly (character scalar).
#'
#' @examples
#' # Highlight some code in the source pane, & run the add-in function
#' 
send_selection_to_console <- function() {
  
  # Get the context of the call, as well as the selection
  context <- rstudioapi::getActiveDocumentContext()
  selection <- rstudioapi::primary_selection(context$selection)
  
  # Get the console context, & find the start of the console selection
  console <- rstudioapi::getConsoleEditorContext()
  insertPos <- console$selection[[1]]$range$start
  
  # Set the output range to the start of the console selection
  outputRange <- rstudioapi::document_range(
    rstudioapi::document_position(insertPos["row"], insertPos["column"]),
    rstudioapi::document_position(insertPos["row"], insertPos["column"])
  )
  
  # Insert the highlighted text, & return it invisibly
  rstudioapi::insertText(location = outputRange, text = selection$text, id = console$id)
  return(invisible(selection$text))
  
}
