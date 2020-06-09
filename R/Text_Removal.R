# Allow convenient use of functions from other packages
#' @include Pipes.R
NULL




#' Find the Last Position
#' 
#' Find the last position of the desired patterns.
#'
#' @param txt The text to search in (character scalar).
#'
#' @return The last position of the desired patterns.
#'
#' @examples
#' KO:::find_last_pos("mtcars %>% select(mpg, disp, cyl)")
#' 
find_last_pos <- function(txt) {
  
  # Set some search patterns
  noSpaceAfterPattern <- "[]\\[(){}`@$:\"'/\\ ]"
  spaceAfterPattern <- "[!#%&*+,-;<=>?^|~]"
  
  # Calculate the last position of the patterns
  lastPos1 <- stringi::stri_locate_last_regex(txt, sprintf("[ ]*\\w*%s+[ ]*$|[ ]*\\w*%s+[ ]*|[ ]*%%>%%[ ]*$", noSpaceAfterPattern, spaceAfterPattern))
  lastPos2 <- stringi::stri_locate_last_regex(txt, sprintf("(?<=\\W)\\w*$", noSpaceAfterPattern, spaceAfterPattern))
  
  # Deal with bad matches
  greater <- function(x, y) ifelse(is.na(x), 0, x) > ifelse(is.na(x), 0, y)
  if (greater(lastPos1[1, "start"], lastPos1[1, "end"])) lastPos1[1, "start"] <- 0
  if (greater(lastPos2[1, "start"], lastPos2[1, "end"])) lastPos2[1, "start"] <- 0
  
  # If no matches were found, clear the whole line, else take the final match
  if (is.na(lastPos1[1, "start"]) && is.na(lastPos2[1, "start"])) {
    lastPos <- 0
  } else {
    lastPos <- max(lastPos1[1, "start"], lastPos2[1, "start"], na.rm = TRUE) - 1
  }
  
  # Return the last position
  return(lastPos)
  
}




#' Remove to Text
#' 
#' An add-in helper function that deletes the code between the cursor and the desired string.
#' 
#' @param backTo The regex pattern to remove text to (character scalar).
#' @param backward Whether to remove text before the cursor (default) or text after it (boolean scalar).
#'
#' @return The deleted code, invisibly (character scalar).
#' @name remove_to_text
#' 
remove_to_text <- function(backTo, backward = TRUE) {
  
  
  # Get the context of the call, as well as the contents/selection/position of the context
  context <- rstudioapi::getActiveDocumentContext()
  contents <- context$contents
  selection <- rstudioapi::primary_selection(context$selection)
  selStart <- selection$range$start
  selEnd <- selection$range$end
  contentRow <- contents[selStart["row"]]
  
  
  # If the cursor is at the start/end of the line, clear the selection, & exit early
  if ((backward && (selStart["column"] == 1)) || (!backward && (selStart[["column"]] == nchar(contentRow)))) {
    rstudioapi::insertText(location = selection$range, text = "", id = context$id)
    return(invisible(NULL))
  }
  
  
  # Find the code to remove, either before or after the cursor
  # if (backward) {
    
    # Get the text before the cursor
    preCode <- if (backward) {
      substring(contentRow, 1, selStart["column"] - 1)
    } else {
      # backTo <- stringi::stri_reverse(backTo)
      stringi::stri_reverse(substring(contentRow, selStart["column"]))
    }
    
    # Set the end position of the pattern to remove back to
    if (is.null(backTo)) {
      endPos <- find_last_pos(preCode) + 1
    } else {
      endPos <- stringi::stri_locate_last_regex(preCode, paste0("[ ]*(", backTo, ")[ ]*"))[1, "start"]
    }
    
    # Clear to the start of the line if no matches were found
    if (is.na(endPos)) endPos <- 0
    if (!backward) endPos <- nchar(contentRow) - endPos + 2
    
    # Set the output range
    outputRange <- rstudioapi::document_range(
      rstudioapi::document_position(selStart["row"], if (backward) endPos else selEnd["column"]),
      rstudioapi::document_position(selEnd["row"], if (backward) selEnd["column"] else endPos)
    )
    
    # Retrieve the code to be deleted
    deletedCode <- substring(contentRow, endPos, selEnd["column"])
    
  # } else {
  #   
  #   # Must implement later
  #   rstudioapi::sendToConsole("warning(\"`KO:::remove_to_text`: remove forward not implemented yet.\")")
  #   
  # }
  
  
  # Delete & return the desired code
  rstudioapi::insertText(location = outputRange, text = "", id = context$id)
  return(invisible(deletedCode))
  
  
}




#' \code{pipe_backspace} (\code{pipe_delete}): an add-in function that deletes the code between the cursor and the last (next) pipe.
#' 
#' @rdname remove_to_text
#' @examples
#' # Type "mtcars %>% mutate(a = 1) %>% filter(cyl = 2)", & run the add-in function
#' 
pipe_backspace <- function() {
  remove_to_text(backTo = "%[^ ]+%")
}


#' @rdname remove_to_text
pipe_delete <- function() {
  remove_to_text(backTo = "%[^ ]+%", backward = FALSE)
}




#' \code{comma_backspace} (\code{comma_delete}): an add-in function that deletes the code between the cursor and the last (next) comma.
#' 
#' @rdname remove_to_text
#' @examples
#' # Type "c('apple', 'banana', cherry')", & run the add-in function
#' 
comma_backspace <- function() {
  remove_to_text(backTo = ",")
}


#' @rdname remove_to_text
comma_delete <- function() {
  remove_to_text(backTo = ",", backward = FALSE)
}




#' \code{smart_backspace} (\code{smart_delete}): an add-in function that deletes the code between the cursor and the last (next) group of characters.
#' 
#' @rdname remove_to_text
#' @examples
#' # Type "c(apple, banana, cherry)", & run the add-in function
#' 
smart_backspace <- function() {
  remove_to_text(backTo = NULL)
}


#' @rdname remove_to_text
smart_delete <- function() {
  remove_to_text(backTo = NULL, backward = FALSE)
}
