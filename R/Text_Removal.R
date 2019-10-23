# Include other functions in package
#' @include Pipes.R
NULL


#' Remove to Text
#' 
#' An add-in helper function that deletes the code between the cursor and the desired string.
#' 
#' @param backTo The regex pattern to remove text to (character scalar).
#' @param elseBackTo If \code{backTo} is not found before this regex pattern, only remove up to these (character scalar).
#' @param backward Whether to remove text before the cursor (default) or text after it (boolean scalar).
#'
#' @return The deleted code, invisibly (character scalar).
#' @name remove_to_text
#' 
remove_to_text <- function(backTo, elseBackTo = "\\(|\\)|\\[|\\]|~|=", backward = TRUE) {
  
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
  
  # Find the code to remove, either before or after the cursor
  if (backward) {
    
    # Get the text before the cursor, & find the position of the desired strings
    preCode <- substring(contents[selStart["row"]], 1, selStart["column"] - 1)
    codePos <- stringi::stri_locate_last_regex(preCode, paste0(" *(", backTo, ")"))
    codeElsePos <- stringi::stri_locate_last_regex(preCode, paste0(" *(", elseBackTo, ")"))
    
    # If `elseBackTo` is null, just use `codePos`
    if (!is.null(elseBackTo)) {
      
      # If the cursor is right after a string matching `elseBackTo`, exclude that string & redo the above
      if ((!any(is.na(codeElsePos))) && (codeElsePos[1, "start"] == selStart["column"] - 1)) {
        preCode <- substring(contents[selStart["row"]], 1, codeElsePos[1, "start"] - 1)
        codePos <- stringi::stri_locate_last_regex(preCode, paste0(" *(", backTo, ")"))
        codeElsePos <- stringi::stri_locate_last_regex(preCode, paste0(" *(", elseBackTo, ")"))
      }
      
      # Set the start position of the code to remove
      if (any(is.na(codePos))) {
        codeStartPos <- codeElsePos[1, "start"] + 1
      } else if (any(is.na(codeElsePos))) {
        codeStartPos <- codePos[1, "start"]
      } else {
        codeStartPos <- ifelse(codePos[1, "start"] > codeElsePos[1, "start"], codePos[1, "start"], codeElsePos[1, "start"] + 1)
      }
      
    } else {
      
      # Set the start position of the code to remove
      codeStartPos <- codePos[1, "start"]
      
    }
    
    # Clear to the start of the line if no matches were found
    if (is.na(codeStartPos)) codeStartPos <- 0
    
    # Set the output range
    outputRange <- rstudioapi::document_range(
      rstudioapi::document_position(selStart["row"], codeStartPos),
      rstudioapi::document_position(selEnd["row"], selEnd["column"])
    )
    
    # Retrieve the code to be deleted
    deletedCode <- substring(contents[selStart["row"]], codeStartPos, selEnd["column"])
    
  } else {
    
    # Must implement later
    rstudioapi::sendToConsole("warning(\"`KO:::remove_to_text`: remove forward not implemented yet.\")")
    
  }
  
  # Delete & return the desired code
  rstudioapi::insertText(location = outputRange, text = "", id = context$id)
  return(invisible(deletedCode))
  
}


#' Pipe Backspace
#' 
#' An add-in function that deletes the code between the cursor and the last pipe.
#' 
#' @rdname remove_to_text
#' 
#' @examples
#' # Type "mtcars %>% mutate(a = 1) %>% filter(cyl = 2)", & run the add-in function
#' 
pipe_backspace <- function() {
  remove_to_text("%>%", NULL)
}


#' Comma Backspace
#' 
#' An add-in function that deletes the code between the cursor and the last comma.
#' 
#' @rdname remove_to_text
#' 
#' @examples
#' # Type "c(apple, banana, cherry)", & run the add-in function
#' 
comma_backspace <- function() {
  remove_to_text(",")
}
