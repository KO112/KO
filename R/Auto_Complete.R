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
  
  # context <- rstudioapi::getActiveDocumentContext()
  context <- rstudioapi::getConsoleEditorContext()
  contents <- context$contents
  selection <- context$selection %>% rstudioapi::primary_selection()
  
  start <- selection$range$start
  end <- selection$range$end
  
  # rstudioapi::getConsoleEditorContext() %$% rstudioapi::sendToConsole(paste0(contents, "#"), execute = FALSE)
  # rstudioapi::insertText(selection$range, "banana\napple", id = "#console")
  # rstudioapi::modifyRange(selection$range, "banana\napple", id = "#console")
  # selection$text
  
  # banana
  # insertText("banana", id = "#console")
  # insertText(c(1, 1, 1, 6), "apple", id = "#console")
  # modifyRange(c(1, 1, 1, 100), "apple", id = "#console")
  
  # if (context$id == "#console") {
    
    # if (start[[1]] == end[[2]])
    
  # } else if (selection$text == "") {
  if (selection$text == "") {
      varText <- contents[start[1]] %>%
        substr(1, start[2]) %>%
        stringi::stri_extract_all_regex("\\w+$") %>%
        .[[1]]
  } else {
    varText <- selection$text
  }
  
  paste0(strsplit(varText, "")[[1]], collapse = ".*") %>%
    grep(ls(envir = .GlobalEnv), ignore.case = T, value = T) %>%
    ifelse(length(.) == 1, ., paste0("${", seq_along(.), ":", ., "}", collapse = ", ")) %>%
    rstudioapi::sendToConsole(code = ., execute = FALSE)
  
  # paste0(c(letters, LETTERS, 0:9, "_-+=[]{}\\|;:'\",<.>/?)", "\\w+$"), collapse = "") %>%
  #   stringi::stri_extract_all_regex("\\w") %>% {sort(.[[1]])}
  
}


