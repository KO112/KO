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
  rstudioapi::insertText(insertPos, as.character(val) %>% paste0(collapse = "\n"), editor$id)
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
  # insertPos <- selection$range$end
  
  # Insert the selected value, & return it invisibly
  # rstudioapi::insertText(insertPos, as.character(.KO_clipboard), context$id)
  rstudioapi::insertText(selection$range, as.character(.KO_clipboard) %>% paste0(collapse = "\n"), context$id)
  # rstudioapi::setSelectionRanges(selection$range, context$id)
  return(invisible(.KO_clipboard))
  
}


#' Clean & Reformat Code
#'
#' @param code The code to be cleaned (character vector).
#'
#' @return The cleaned code (character vector).
#'
#' @examples
#' KO:::clean_code("apple=5;banana<-1;cherry==100;1+2;10+20;1-2;1*2;1/2;1^2;1|2;1&2")
#' 
clean_code <- function(code) {
  
  # Set a list of operators, & create an expression out of them
  ops <- c("==", "!=", "~", "%[^%]*%", "\\^", "\\+", "-", "\\*", "/", "=", "<-", "\\|", "&", "<", ">", "<=", ">=")
  opsExp <- paste0(ops, collapse = "|")
  
  # Clean the code
  cleanCode <- code %>%
    
    # Clean whitespace around operators
    gsub(paste0("([^=!<>:])(", opsExp, ")([^=!<>:])"), "\\1 \\2 \\3", .) %>%
    gsub(paste0("[ \t]+(", opsExp, ")[ \t]+"), " \\1 ", .) %>%
    
    # Fix pipes at the start or end of the line
    gsub("^%[ \t]*([^ %]*)[ \t]*%", "%\\1% ", .) %>%
    gsub("%[ \t]*([^ %]*)[ \t]*%$", " %\\1%", .) %>%
    
    # Ensure spaces after commas/semicolons
    gsub("([,;])", "\\1 ", .) %>%
    gsub("([,;])[ \t]+", "\\1 ", .) %>%
    
    # Ensure spaces around comment tags
    gsub("(#+)", "\\1 ", .) %>%
    gsub("(#+)[ \t]+", "\\1 ", .) %>%
    gsub("([^ \t#])(#+)[ \t]+", "\\1 \\2 ", .) %>%
    
    # Ensure spaces before braces
    gsub("\\)\\{", ") {", .) %>%
    
    # Clean spaces after control flow constructs (if/else/for/while/repeat)
    gsub("(if|else|for|while|repeat)([^ a-zA-Z0-9_])[ \t]*", "\\1 \\2", .) %>%
    
    # Remove spaces after `ifelse` (introduced in the step above)
    gsub("ifelse[ \t]+", "ifelse", .) %>%
    
    # Remove whitespace at the end of lines
    gsub("[ \t]+$", "", .)
  
  # Return the cleaned code
  return(cleanCode)
  
}


#' Clean the Current Code Selection
#' 
#' If no code is selected, the current line will be cleaned.
#'
#' @return A list of the old/original code, and the new/cleaned code.
#' @export
#'
#' @examples
#' # Highlight a line of code with spacing that would cause RStudio's style diagnostics to complain,
#' #   and run the add-in function
#' 
clean_selection <- function() {
  
  # Get the context of the call, as well as the contents/selection/position of the context
  context <- rstudioapi::getActiveDocumentContext()
  contents <- context$contents
  selection <- rstudioapi::primary_selection(context$selection)
  selStart <- selection$range$start
  selEnd <- selection$range$end
  
  # If no text is selected, clean the entire line, else clean the selection
  if (all(selStart == selEnd)) {
    oldCode <- contents[selStart["row"]]
    outputRange <- rstudioapi::document_range(
      rstudioapi::document_position(selStart["row"], 1),
      rstudioapi::document_position(selStart["row"], Inf)
    )
  } else {
    oldCode <- selection$text
    outputRange <- selection$range
  }
  cleanCode <- clean_code(oldCode)
  
  # Insert the cleaned code, & return the old & cleaned code
  rstudioapi::insertText(location = outputRange, text = cleanCode, id = context$id)
  return(invisible(list(oldCode = oldCode, cleanCode = cleanCode)))
  
}
