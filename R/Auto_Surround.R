#' Auto-Surround
#' 
#' An add-in function used to help auto-surround text with give characters.
#' 
#' @param char The character to auto-surround with (character scalar).
#'
#' @return The output range, invisibly.
#' @export
#' 
#' @examples
#' # Type "apple, banana, cherry", put your cursor anywhere in a word (including on the edges),
#' #   & run the add-in function
#' 
auto_surround <- function(char) {
  
  # Set the ending character
  if (missing(char)) char <- rstudioapi::showPrompt("Enter Character", "Enter a character to wrap the current text with.", "\"")
  if (is.null(char)) return(NULL)
  endChar <- switch(char, "(" = ")", "[" = "]", "{" = "}", char)
  
  # Get the context of the call, as well as the contents/selection/position of the context
  # context <- rstudioapi::getActiveDocumentContext()
  context <- rstudioapi::getConsoleEditorContext()
  contents <- context$contents
  selection <- context$selection %>% rstudioapi::primary_selection()
  selStart <- selection$range$start
  selEnd <- selection$range$end
  
  # Get the strings to the left and right of the selection
  beforeStr <- substring(contents[selStart["row"]], 1, selStart["column"] - 1)
  afterStr <- substring(contents[selEnd["row"]], selEnd["column"])
  
  # Set the number of characters to the left of the selection
  if (!grepl("\\w", substring(beforeStr, nchar(beforeStr)))) {
    beforeLen <- 0
  } else {
    beforeLen <- stringi::stri_extract_last_regex(beforeStr, "\\w+") %>% .[[1]] %>% nchar()
  }
  
  # Set the number of characters to the right of the selection
  if (!grepl("\\w", substring(afterStr, 1, 1))) {
    afterLen <- 0
  } else {
    afterLen <- stringi::stri_extract_first_regex(afterStr, "\\w+") %>% .[[1]] %>% nchar()
  }
  
  # Set the output range
  outputRange <- rstudioapi::document_range(
    rstudioapi::document_position(selStart["row"], selStart["column"] - beforeLen),
    rstudioapi::document_position(selEnd["row"], selEnd["column"] + afterLen)
  )
  
  # Insert the wrapping characters
  rstudioapi::insertText(location = outputRange$end, text = endChar, id = context$id)
  rstudioapi::insertText(location = outputRange$start, text = char, id = context$id)
  
  # Return the output range, invisibly
  return(invisible(outputRange))
  
}


# auto_surround_single_quote <- function() auto_surround("'")
# auto_surround_double_quote <- function() auto_surround("\"")
# auto_surround_parentheses <- function() auto_surround("(")
# auto_surround_bracket <- function() auto_surround("[")
# auto_surround_brace <- function() auto_surround("{")
