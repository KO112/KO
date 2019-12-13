# Include other functions in package
#' @include Pipes.R
NULL

# Avoid "undefined variable" notes in package checking
globalVariables(c(".KO_clipboard"))


#' Send Value to Source Editor
#' 
#' Send the value of the currently selected expression to the source editor.
#' This is particularly valuable in RStudio Server,
#'   where the clipboard is not available programatically.
#' 
#' @return The value of the currently selected expression, invisibly.
#' @name server-clipboard
#' 
#' @examples
#' # Select an expression that can be evaluated to a value, & run this add-in
#' 
send_value_to_source_editor <- function() {
  
  # Get the context of the call, as well as the selection
  context <- rstudioapi::getActiveDocumentContext()
  selection <- rstudioapi::primary_selection(context$selection)
  
  # Evaluate the selected expression
  val <- eval(parse(text = selection$text))
  if (is.null(val)) return(invisible(NULL))
  
  # Get the editor context, & find the end of the editor selection
  editor <- rstudioapi::getSourceEditorContext()
  editorSelection <- rstudioapi::primary_selection(editor$selection)
  insertPos <- editorSelection$range$end
  
  # Insert the selected value, & return it invisibly
  rstudioapi::insertText(insertPos, as.character(val), editor$id)
  rstudioapi::setSelectionRanges(editorSelection$range, editor$id)
  return(invisible(val))
  
}


#' Save Value to Clipboard Variable
#' 
#' Save the value of the currently selected expression to a variable.
#' This is particularly valuable in RStudio Server,
#'   where the clipboard is not available programatically.
#' 
#' @return The value saved to the clipboard variable, invisibly.
#' 
#' @examples
#' # Select an expression that can be evaluated to a value, & run this add-in
#' 
save_value_to_clipboard_variable <- function() {
  
  # Get the context of the call, as well as the selection
  context <- rstudioapi::getActiveDocumentContext()
  selection <- rstudioapi::primary_selection(context$selection)
  
  # Evaluate the selected expression
  val <- eval(parse(text = selection$text))
  if (is.null(val)) return(invisible(NULL))
  
  # Save the value, & return it invisibly
  globalEnv <- .GlobalEnv
  assign(".KO_clipboard", val, globalEnv)
  return(invisible(val))
  
}


#' Paste Value from Clipboard Variable
#' 
#' Paste the value of the clipboard variable to the active context.
#' This is particularly valuable in RStudio Server,
#'   where the clipboard is not available programatically.
#' 
#' @return The value read from the clipboard variable, invisibly.
#' @rdname server-clipboard
#' 
#' @examples
#' # After running `save_value_to_clipboard_variable` on a valid expression, run this add-in
#' 
paste_value_from_clipboard_variable <- function() {
  
  # Exit early if the package variable does not exist
  if (!exists(".KO_clipboard") || is.null(.KO_clipboard)) return(invisible(NULL))
  
  # Get the context of the call, as well as the selection
  context <- rstudioapi::getActiveDocumentContext()
  selection <- rstudioapi::primary_selection(context$selection)
  insertPos <- selection$range$end
  
  # Insert the selected value, & return it invisibly
  rstudioapi::insertText(insertPos, as.character(.KO_clipboard), context$id)
  rstudioapi::setSelectionRanges(selection$range, context$id)
  return(invisible(.KO_clipboard))
  
}
