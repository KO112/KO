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
  varMatches <- purrr::map(c(parent.frame(2), .GlobalEnv), ~ grep(matchText, ls(.x), ignore.case = TRUE, value = TRUE)) %>%
    unlist() %>% unique() %>% sort()
  
  # If there is more than one match, create a drop-down list for the user to choose from
  if (length(varMatches) > 1) varMatches <- tkDropDown(varMatches)
  
  # If only one element was selected, send it to the console, & return it, else throw an error
  if (length(varMatches) == 1) {
    rstudioapi::insertText(outputRange, varMatches, context$id)
    return(invisible(varMatches))
  } else {
    stop("An invalid selection was made. This should be impossible.",
         length(varMatches), " elements were selected.")
  }
  
}
# auto_complete_var()



# 
tkDropDown <- function(opts, title = "Select a Word") {
  
  # Check that we have version 8.5 or later, & set the tkbutton object
  have_ttk <- as.character(tcl("info", "tclversion")) >= "8.5"
  # if (!have_ttk) ttkbutton <- tkbutton
  
  # Set the tcl object, adding the desired options
  lvar <- tclVar()
  tclObj(lvar) <- opts
  
  # Hide the temporary widget, storing the original modal setting, & create the top-level widget
  oldMode <- tclServiceMode(FALSE)
  dlg <- tktoplevel()
  
  # Set the title, remove the icon, get the set, & focus the widget
  tkwm.title(dlg, title)
  # tkwm.deiconify(dlg)
  # tkgrab.set(dlg)
  # tkfocus(dlg)
  
  # Set the title label
  if (title != "") {
    lab <- if (have_ttk) ttklabel(dlg, text = title, foreground = "blue")
           else tklabel(dlg, text = title, fg = "blue")
    tkpack(lab, side = "top")
  }
  
  # Runs when the form is submitted
  onOK <- function() {
    res <- 1L + as.integer(tkcurselection(listBox))
    selectedVar <<- opts[res]
    tkgrab.release(dlg)
    tkdestroy(dlg)
  }
  
  # Runs when the form is cancelled
  onCancel <- function() {
    tkgrab.release(dlg)
    tkdestroy(dlg)
  }
  
  # 
  # buttons <- tkframe(dlg)
  # tkpack(buttons, side = "bottom")
  # OK <- ttkbutton(buttons, text = gettext("OK"), width = 6, command = onOK)
  # Cancel <- ttkbutton(buttons, text = gettext("Cancel"), command = onCancel)
  # tkpack(OK, Cancel, side = "left", fill = "x", padx = "2m")
  
  # 
  scht <- as.numeric(tclvalue(tkwinfo("screenheight", dlg))) - 200L
  ht <- min(length(opts), scht %/% 20)
  
  # 
  selectionMode <- "browse" # "single"
  listBox <- tklistbox(dlg, height = ht, listvariable = lvar, bg = "white", setgrid = 1, selectmode = selectionMode)
  tmp <- tcl("font", "metrics", tkcget(listBox, font = NULL))
  tmp <- as.numeric(sub(".*linespace ([0-9]+) .*", "\\1", tclvalue(tmp))) + 3
  ht <- min(length(opts), scht %/% tmp)
  
  # Destroy the temporary list box
  tkdestroy(listBox)
  
  # 
  if (ht < length(opts)) {
    
    # 
    scr <- if (have_ttk) ttkscrollbar(dlg, command = function(...) tkyview(listBox, ...))
      else tkscrollbar(dlg, repeatinterval = 5, command = function(...) tkyview(listBox, ...))
    
    # 
    listBox <- tklistbox(dlg, height = ht, width = 0, listvariable = lvar, bg = "white", selectmode = selectionMode,
                         setgrid = 1, yscrollcommand = function(...) tkset(scr, ...))
    tkpack(listBox, side = "left", fill = "both", expand = TRUE)
    tkpack(scr, side = "right", fill = "y")
    
  } else {
    
    # 
    listBox <- tklistbox(dlg, height = ht, width = 0, listvariable = lvar, bg = "white", selectmode = selectionMode)
    tkpack(listBox, side = "left", fill = "both")
    
  }
  
  # Select the first element
  tkselection.set(listBox, 0L)
  selectedVar <- character(0)
  
  # Set key bindings
  tkbind(listBox, "<Double-ButtonPress-1>", onOK)
  tkbind(dlg, "<Return>", onOK)
  tkbind(dlg, "<space>", onOK)
  tkbind(dlg, "<Destroy>", onCancel)
  tkbind(dlg, "<Escape>", onCancel)
  # tkbind(dlg, "<Up>", onUp)
  # tkbind(dlg, "<Down>", onDown)
  
  # Activate the list box, show it modally, activate it, & wait for the user to take an action
  tkfocus(listBox)
  tclServiceMode(TRUE) # oldMode)
  # system(paste0("WScript \"C:/Users/tae8766/Documents/GitHub/General/AutoHotKey/Scripts/Activate Window.vbs\" \"", title, "\""))
  system(paste0("\"C:/Users/tae8766/Documents/GitHub/General/AutoHotKey/Scripts/Activate Window.exe\" \"", title, "\""))
  tkwait.window(dlg)
  
  # Return the selection
  return(selectedVar)
  
}
