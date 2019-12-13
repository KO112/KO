# Include other functions in package
#' @include Pipes.R
NULL

# Avoid "undefined variable" notes in package checking
globalVariables(c(".KO_fn"))


#' Run a Function
#' 
#' Run a user-set function in the background at the press of a button.
#' If the function has not been set, this function will print out some code to help the user get started.
#' Once the function has been set, running this add-in will execute the set function.
#'
#' @examples
#' # To make use of this add-in, run it (preferably by assigning a shortcut),
#' #   and it will print run a commented out line which will allow you to set a function.
#' # Once the `.KO_fn` object exists, running this add-in will execute the set function.
#' 
#' .KO_fn <- function() mean(1:10)
#' .KO_fn
#' .KO_fn()
#' 
run_fn <- function() {
  
  # If the user has set the function, run it, else print out some code to help the user set one
  if (exists(".KO_fn")) {
    
    .KO_fn()
    
  } else {
    
    # Get the source editor content, extract the selection & the code at/above the cursor,
    #  find the last function declaration in that, & determine whether to execute the result directly
    editor <- rstudioapi::getSourceEditorContext()
    selection <- rstudioapi::primary_selection(editor$selection)
    code <- utils::head(editor$contents, selection$range$start[["row"]])
    lastFn <- grep("^\\w+[ ]*<-[ ]*function()", code, ignore.case = T, value = T) %>% utils::tail(1)
    execute <- rstudioapi::getConsoleEditorContext()$contents != ""
    
    # Send the starter code to the console, executing as a comment if there's anything in the console
    rstudioapi::sendToConsole(
      code = sprintf(
        "%s.KO_fn <- function() { source(\"%s\"); %s(); }"
        , ifelse(execute, "# ", "")
        , editor$path
        , gsub("(\\w+).*", "\\1", lastFn)
      )
      , focus = FALSE
      , execute = execute
    )
    
  }
}


#' Run Code in the Console
#' 
#' Run the code currently in the console, without clearing it or moving the selection.
#'
#' @return The code in the console, invisibly (character scalar).
#'
#' @examples
#' # Enter any code in the console, & run the add-in function
#' 
run_chunk <- function() {
  
  # Get the context of the call, & exit if the editor is not active
  editor <- rstudioapi::getSourceEditorContext()
  context <- rstudioapi::getActiveDocumentContext()
  console <- rstudioapi::getConsoleEditorContext()
  if (editor$id != context$id) return(invisible(NULL)) else rm(context)
  
  # Get the documents contents, & the primary selection
  contents <- editor$contents
  selection <- rstudioapi::primary_selection(editor$selection)
  
  # Get the selected line number & contents
  rowNum <- selection$range$start[["row"]]
  rowText <- contents[rowNum]
  
  # Find the bounds (first & last row) of the current chunk
  firstRow <- utils::head(contents, rowNum) %>% grep("^```\\{r", .) %>% utils::tail(1)
  lastRow <- utils::tail(contents, -firstRow) %>% grep("^```$", .) %>% utils::head(1) + firstRow
  
  # Exit if there was a problem finding the chunk bounds, or of the bounds are invalid
  if ((length(c(firstRow, lastRow)) != 2) || (firstRow >= lastRow)) return(invisible(NULL))
  
  # Get the chunk code
  code <- contents[seq(firstRow + 1, lastRow - 1)]
  rstudioapi::sendToConsole(code, focus = FALSE)
  rstudioapi::setSelectionRanges(rstudioapi::primary_selection(console$selection)$range, id = console$id)
  return(invisible(code))
  
}


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
  console <- rstudioapi::getConsoleEditorContext()
  rstudioapi::sendToConsole(console$contents, focus = FALSE)
  rstudioapi::setSelectionRanges(rstudioapi::primary_selection(console$selection)$range, id = console$id)
  return(invisible(console$contents))
  
}


#' Run Selected Code
#' 
#' Run the code currently selected, without clearing the console or moving the selection.
#'
#' @return The selected code, invisibly (character scalar).
#'
#' @examples
#' # Select any code, & run the add-in function
#' 
run_selection <- function() {
  
  # Get the context of the call, the console, & get the selection
  context <- rstudioapi::getActiveDocumentContext()
  console <- rstudioapi::getConsoleEditorContext()
  selection <- rstudioapi::primary_selection(context$selection)
  
  # Run the selected text, reset the console selection, & return it invisibly
  rstudioapi::sendToConsole(selection$text, focus = FALSE)
  rstudioapi::setSelectionRanges(rstudioapi::primary_selection(console$selection)$range, id = console$id)
  return(invisible(selection$text))
  
}


#' Send Code to the Console
#' 
#' Insert the currently selected code into the console.
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
  
  # Get the console context, & find the end of the console selection
  console <- rstudioapi::getConsoleEditorContext()
  insertPos <- rstudioapi::primary_selection(console$selection)$range$end
  
  # Set the output range to the end of the console selection
  outputRange <- rstudioapi::document_range(
    rstudioapi::document_position(insertPos["row"], insertPos["column"]),
    rstudioapi::document_position(insertPos["row"], insertPos["column"])
  )
  
  # Insert the selected text, & return it invisibly
  rstudioapi::insertText(location = outputRange, text = selection$text, id = console$id)
  return(invisible(selection$text))
  
}
