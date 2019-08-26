#' Auto-Complete
#' 
#' An add-in function used to help auto-complete variables in the global environment.
#'
#' @export
#'
#' @examples
#' apple <- 10
#' # Type "ap" and run the add-in function
#' 
auto_complete_var <- function() {
  
  # Get the context of the call, as well as the contents/selection/position of the context
  # context <- rstudioapi::getActiveDocumentContext()
  context <- rstudioapi::getConsoleEditorContext()
  contents <- context$contents
  selection <- context$selection %>% rstudioapi::primary_selection()
  start <- selection$range$start
  # end <- selection$range$end
  
  # Set the variables search text, & the output range
  if (selection$text != "") {
    varText <- selection$text
    outputRange <- selection$range
  } else {
    varText <- substr(contents[start["row"]], 1, start["column"]) %>%
      stringi::stri_extract_all_regex("\\w+$") %>% .[[1]]
    outputRange <- rstudioapi::document_position(start["row"], start["column"] - nchar(varText)) %>%
      rstudioapi::document_range(start)
  }
  
  # Create a regex pattern for matching, search for a match, get unique elements, & print them to the console
  matchText <- paste0(strsplit(varText, "")[[1]], collapse = ".*")
  purrr::map(c(environment(), .GlobalEnv), ~ grep(matchText, ls(.x), ignore.case = TRUE, value = TRUE)) %>%
    unlist() %>% unique() %>% sort() %>%
    ifelse(length(.) <= 1, ., paste0("${", seq_along(.), ":", ., "}", collapse = ", ")) %>%
    rstudioapi::insertText(outputRange, ., context$id)
  
  # rstudioapi::setSelectionRanges(list(c(1, 1, 1, 5), c(1, 6, 1, 10)), context$id)
}
