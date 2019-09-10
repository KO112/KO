# Include other functions in package
#' @include Pipes.R
NULL


#' Snippet Pipe Check
#' 
#' An add-in function that decides whether or not to insert a pipe as well during snippet insertion.
#' 
#' @return The deleted code (character scalar).
#' 
#' @examples
#' # Type "1:10 %>% mean()", put your cursor anywhere after the pipe, & run the add-in function
#' 
snippet_pipe_check <- function() {
  
  # Get the context of the call, as well as the contents/selection/position of the context
  context <- rstudioapi::getActiveDocumentContext()
  # context <- rstudioapi::getConsoleEditorContext()
  contents <- context$contents
  selection <- rstudioapi::primary_selection(context$selection)
  selStart <- selection$range$start
  
  # Determine whether the current expressions has just been piped, & return a pipe if one is needed
  isPiped <- substring(contents[selStart["row"]], 1, selStart["column"]) %>%
    stringi::stri_extract_last_regex("(%>%)+[ ]*\\w+$") %>% .[[1]] %>% is.na()
  if (isPiped) return("%>% ") else return("")
  
}
