#' Send a Toast Notification
#' 
#' Send a toast notification on Windows.
#'
#' @param msg,title,img The notification message/title/image path (character scalars).
#'
#' @export
#'
#' @examples
#' notify("Hello", "World")
#' 
notify <- function(msg, title = "", img = "C:/Program Files/RStudio/resources/tutorial_resources/rstudio-logo.png") {
  if (Sys.info()[["sysname"]] == "Windows") {
    shell(glue::glue("C:/toaster/toast.exe -w -t \"{title}\" -m \"{msg}\" -p \"{img}\""), wait = FALSE)
  } else {
    warning("`KO::notify` only works on Windows installations.", immediate. = TRUE)
  }
}
