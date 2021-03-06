# Allow convenient use of functions from other packages
#' @include Pipes.R
NULL


#' Auto-Complete
#' 
#' An add-in function used to help auto-complete variables in the global environment.
#' 
#' @return The variable name that is inserted, invisibly (character scalar).
#' 
#' @examples
#' apple <- 10
#' # Type "ap" & run the add-in function
#' 
#' var1 <- 1; var2 <- 2; var3 <- 3
#' # Type "var" & run the add-in function
#' 
#' # source('~/GitHub/KO/R/Auto_Complete.R'); auto_complete_var()
#' 
auto_complete_var <- function() {
  
  # Get the context of the call, as well as the contents/selection/position of the context
  context <- rstudioapi::getActiveDocumentContext()
  contents <- context$contents
  selection <- rstudioapi::primary_selection(context$selection)
  selStart <- selection$range$start
  
  # Set the variables search text, & the output range
  if (selection$text != "") {
    varText <- selection$text
    outputRange <- selection$range
  } else {
    varText <- substring(contents[selStart["row"]], 1, selStart["column"]) %>%
      stringi::stri_extract_last_regex("\\w+") %>% .[[1]]
    outputRange <- rstudioapi::document_position(selStart["row"], selStart["column"] - nchar(varText)) %>%
      rstudioapi::document_range(selStart)
  }
  
  # Create a regex pattern for matching, search for a match, get unique elements, & print them to the console
  matchText <- paste0(strsplit(varText, "")[[1]], collapse = ".*")
  varMatches <- c(parent.frame(2), .GlobalEnv) %>%
    purrr::map(~ grep(matchText, ls(.x), ignore.case = TRUE, value = TRUE)) %>%
    unlist() %>% unique() %>% sort()
  
  # If there is more than one match, create a drop-down list for the user to choose from (on Windows), else a text prompt
  if (length(varMatches) > 1) {
    closestMatch <- which.min(stringdist::stringdist(varText, varMatches))
    if (Sys.info()["sysname"] == "Windows") {
      # varMatches <- tkDropDown(varMatches, default = pmax(0, closestMatch - 1))
      # varMatches <- varMatches[menu(varMatches, graphics = TRUE, title = "Select a Variable")]
      varMatches <- utils::select.list(varMatches, preselect = varMatches[closestMatch], title = "Select a Variable", graphics = TRUE)
    } else {
      response <- paste0(seq_along(varMatches), ". ", varMatches, " ", strrep(intToUtf8(8192), 50), collapse = " ") %>%
        rstudioapi::showPrompt("Select a Variable", ., closestMatch)
      varMatches <- varMatches[as.integer(response)]
    }
  }
  
  # If only one element was selected, send it to the console, & return the selected element invisibly
  if (length(varMatches) == 1) rstudioapi::insertText(outputRange, varMatches, context$id)
  return(invisible(varMatches))
  
}


#' Create a TCL/TK Drop-Down List
#'
#' @param varList Variable names to choose from (character vector)/
#' @param title Title of the pop-up (character scalar).
#' @param default Default selection for the drop-down list (integer scalar).
#'
#' @return The selected variable name.
#' @export
#'
#' @examples
#' \dontrun{
#'   tkDropDown(letters)
#'   tkDropDown(letters, "Select a Variable")
#' }
#' 
tkDropDown <- function(varList, title = "Select a Word", default = 0L) {
  
  # Check that we have version 8.5 or later, set the list object, & set the variable list
  have_ttk <- as.character(tcltk::tcl("info", "tclversion")) >= "8.5"
  listObj <- tcltk::tclVar()
  tcltk::tclObj(listObj) <- varList
  
  # Hide the temporary widget, storing the original modal setting, & create the top-level widget
  oldMode <- tcltk::tclServiceMode(FALSE)
  dlg <- tcltk::tktoplevel()
  
  # Set the title label
  if (title != "") {
    tcltk::tkwm.title(dlg, title)
    lab <- if (have_ttk) tcltk::ttklabel(dlg, text = title, foreground = "black")
           else tcltk::tklabel(dlg, text = title, fg = "black")
    tcltk::tkpack(lab, side = "top")
  }
  
  # Initialize the selected variable ()
  selectedVar <- character(0)
  
  # Runs when the form is cancelled
  onCancel <- function() {
    tcltk::tkgrab.release(dlg)
    tcltk::tkdestroy(dlg)
  }
  
  # Runs when the form is submitted
  onOK <- function() {
    res <- 1L + as.integer(tcltk::tkcurselection(listBox))
    selectedVar <<- varList[res]
    onCancel()
  }
  
  # Moves the selection in the list box
  moveSelection <- function(index, moveView = TRUE) {
    tcltk::tkselection.clear(listBox, tcltk::tkcurselection(listBox))
    tcltk::tkselection.set(listBox, index)
    tcltk::tkactivate(listBox, index)
    if (moveView) tcltk::tkyview(listBox, index)
  }
  
  # Get the height of the top-level widget
  scht <- as.numeric(tcltk::tclvalue(tcltk::tkwinfo("screenheight", dlg))) - 200
  ht <- min(length(varList), scht %/% 20)
  
  # Set the selection mode (follows cursor moved by arrow keys), create a temporary list box, & get its height
  selectionMode <- "browse"
  listBox <- tcltk::tklistbox(dlg, height = ht, listvariable = listObj, setgrid = 1, selectmode = selectionMode)
  tmp <- tcltk::tcl("font", "metrics", tcltk::tkcget(listBox, font = NULL))
  tmp <- as.numeric(sub(".*linespace ([0-9]+) .*", "\\1", tcltk::tclvalue(tmp))) + 3
  ht <- min(length(varList), scht %/% tmp)
  
  # Destroy the temporary list box
  tcltk::tkdestroy(listBox)
  
  # Create the list box, adding scroll bars if necessary
  if (ht < length(varList)) {
    
    # Create a scroll bar object
    scr <- if (have_ttk) tcltk::ttkscrollbar(dlg, command = function(...) tcltk::tkyview(listBox, ...))
      else tcltk::tkscrollbar(dlg, repeatinterval = 5, command = function(...) tcltk::tkyview(listBox, ...))
    
    # Create the list box, adding the scroll bar
    listBox <- tcltk::tklistbox(dlg, height = ht, width = 0, listvariable = listObj, bg = "white", selectmode = selectionMode,
                         setgrid = 1, yscrollcommand = function(...) tcltk::tkset(scr, ...))
    tcltk::tkpack(listBox, side = "left", fill = "both", expand = TRUE)
    tcltk::tkpack(scr, side = "right", fill = "y")
    
  } else {
    
    # Create the list box
    listBox <- tcltk::tklistbox(dlg, height = ht, width = 0, listvariable = listObj, bg = "white", selectmode = selectionMode)
    tcltk::tkpack(listBox, side = "left", fill = "both")
    
  }
  
  # Set key bindings for submitting
  tcltk::tkbind(listBox, "<Double-ButtonPress-1>", onOK)
  tcltk::tkbind(dlg, "<Return>", onOK)
  tcltk::tkbind(dlg, "<space>", onOK)
  
  # Set key bindings for cancelling
  tcltk::tkbind(dlg, "<Destroy>", onCancel)
  tcltk::tkbind(dlg, "<Escape>", onCancel)
  tcltk::tkbind(dlg, "<q>", onCancel)
  
  # Set key bindings for changing the selection
  purrr::walk(0:9, ~ tcltk::tkbind(dlg, .x, function() moveSelection(.x))) # , moveView = FALSE)))
  tcltk::tkbind(dlg, "<Home>", function() moveSelection(0L))
  tcltk::tkbind(dlg, "<End>", function() moveSelection(length(varList) - 1L))
  
  # Select the first element, focus the list box, show it modally
  tcltk::tkselection.set(listBox, default) # , 0L)
  tcltk::tkfocus(listBox)
  tcltk::tclServiceMode(TRUE) # oldMode)
  
  # Set paths to look for the AHK script at, & check which ones exist
  ahkPaths <- c(
    "C:/Users/tae8766/Documents/GitHub/General/AutoHotKey/Scripts/Activate Window.exe",
    "C:/Users/KO/Google Drive/Programming/Repos/General/AutoHotKey/Scripts/Activate Window.exe"
  )
  ahkPathsCheck <- file.exists(ahkPaths)
  
  # If any of the AHK paths are valid, run the script to activate/reposition the popup
  if (any(ahkPathsCheck))
    system(paste0("\"", ahkPaths[ahkPathsCheck][1],"\" \"", gsub("\\", "\\\\", title, fixed = TRUE), "\""))
  
  # Wait for the user to take actions
  tcltk::tkwait.window(dlg)
  
  # Return the selection
  return(invisible(selectedVar))
  
}
