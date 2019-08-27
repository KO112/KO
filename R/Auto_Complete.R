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
  
  # ifelse(length(.) <= 1, ., paste0("${", seq_along(.), ":", ., "}", collapse = ", ")) %>%
  #   rstudioapi::insertText(outputRange, ., context$id)
  
  # rstudioapi::setSelectionRanges(list(c(1, 1, 1, 5), c(1, 6, 1, 10)), context$id)
  
}
# auto_complete_var()




# 
tkDropDown <- function(opts, title = "Select a Word") {
  
  # Check that we have version 8.5 or later, & set the tkbutton object
  have_ttk <- as.character(tcl("info", "tclversion")) >= "8.5"
  if (!have_ttk) ttkbutton <- tkbutton
  
  # Set the tcl object, adding the desired options
  lvar <- tclVar()
  tclObj(lvar) <- opts
  
  # Set the service mode, & cretae the top-level widget
  oldmode <- tclServiceMode(FALSE)
  dlg <- tktoplevel()
  
  # Set the title, remove the icon, get the set, & focus the widget
  tkwm.title(dlg, title)
  tkwm.deiconify(dlg)
  tkgrab.set(dlg)
  tkfocus(dlg)
  
  # 
  if (!is.null(title) && nzchar(title)) {
    lab <- if (have_ttk) ttklabel(dlg, text = title, foreground = "blue")
    else tklabel(dlg, text = title, fg = "blue")
    tkpack(lab, side = "top")
  }
  
  # Runs when the form is submitted
  onOK <- function() {
    res <- 1L + as.integer(tkcurselection(box))
    # res2 <<- tknearest(box)
    ans.select_list <<- opts[res]
    tkgrab.release(dlg)
    tkdestroy(dlg)
  }
  
  # Runs when the form is cancelled
  onCancel <- function() {
    tkgrab.release(dlg)
    tkdestroy(dlg)
  }
  
  # 
  buttons <- tkframe(dlg)
  tkpack(buttons, side = "bottom")
  OK <- ttkbutton(buttons, text = gettext("OK"), width = 6, command = onOK)
  Cancel <- ttkbutton(buttons, text = gettext("Cancel"), command = onCancel)
  tkpack(OK, Cancel, side = "left", fill = "x", padx = "2m")
  
  # 
  scht <- as.numeric(tclvalue(tkwinfo("screenheight", dlg))) - 200L
  ht <- min(length(opts), scht %/% 20)
  
  # 
  s_mode <- if (multiple) "multiple" else "single"
  box <- tklistbox(dlg, height = ht, listvariable = lvar, bg = "white", setgrid = 1, selectmode = s_mode)
  tmp <- tcl("font", "metrics", tkcget(box, font = NULL))
  tmp <- as.numeric(sub(".*linespace ([0-9]+) .*", "\\1", tclvalue(tmp))) + 3
  ht <- min(length(opts), scht %/% tmp)
  
  # 
  tkdestroy(box)
  
  # 
  if (ht < length(opts)) {
    
    # 
    scr <- if (have_ttk) ttkscrollbar(dlg, command = function(...) tkyview(box, ...))
      else tkscrollbar(dlg, repeatinterval = 5, command = function(...) tkyview(box, ...))
    
    # 
    box <- tklistbox(dlg, height = ht, width = 0, listvariable = lvar, bg = "white", setgrid = 1,
                     selectmode = s_mode, yscrollcommand = function(...) tkset(scr, ...))
    tkpack(box, side = "left", fill = "both", expand = TRUE)
    tkpack(scr, side = "right", fill = "y")
    
  } else {
    
    # 
    box <- tklistbox(dlg, height = ht, width = 0, listvariable = lvar, bg = "white", selectmode = s_mode)
    tkpack(box, side = "left", fill = "both")
    
  }
  
  # 
  tkselection.set(box, 1L)
  tkyview(box, 1L)
  ans.select_list <- character()
  
  # 
  tkbind(dlg, "<Destroy>", onCancel)
  tkbind(box, "<Double-ButtonPress-1>", onOK)
  tkbind(dlg, "<Return>", onOK)
  # tkbind(dlg, "<Esc>", onCancel)
  
  # 
  tkfocus(box)
  tclServiceMode(oldmode)
  # system(paste0("WScript \"C:/Users/tae8766/Documents/GitHub/General/AutoHotKey/Scripts/Activate Window.vbs\" \"", title, "\""))
  # browser()
  system(paste0("\"C:/Users/tae8766/Documents/GitHub/General/AutoHotKey/Scripts/Activate Window.exe\" \"", title, "\""))
  tkwait.window(dlg)
  
  # 
  Sys.sleep(0.1)
  if (!multiple && !length(ans.select_list)) ans.select_list <- ""
  return(ans.select_list)
  
}
