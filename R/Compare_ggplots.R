#' Compare two ggplot objects
#'
#' @param plot1 First ggplot object to compare.
#' @param plot2 Second ggplot object to compare.
#' @param useTempFiles Whether or not to use temporary files in the comparison.
#'   Using temp files is faster, but some may wish to compare purely in memory.
#'
#' @return A boolean scalar showing whether the two plots are visually identical or not.
#' @export
#'
#' @examples
#' plot1 <- ggplot2::qplot(mpg, wt, data = mtcars, colour = cyl)
#' plot2 <- ggplot2::qplot(mpg, wt, data = mtcars, colour = cyl)
#' plot3 <- ggplot2::qplot(mpg, wt, data = mtcars, colour = carb)
#' compare_ggplots(plot1, plot2)
#' compare_ggplots(plot1, plot2, useTempFiles = FALSE)
#' compare_ggplots(plot1, plot3)
#' compare_ggplots(plot1, plot3, useTempFiles = FALSE)
compare_ggplots <- function(plot1, plot2, useTempFiles = TRUE) {
  
  if (!useTempFiles && requireNamespace("magick")) {
    
    # Render each image as an object, and check that they are identical
    fig1 <- magick::image_graph(width = 200, height = 200); print(plot1); grDevices::dev.off()
    fig2 <- magick::image_graph(width = 200, height = 200); print(plot2); grDevices::dev.off()
    return(identical(fig1[[1]], fig2[[1]]))
    
  } else {
    
    # Create temp file names, save the plots, compare those files, and remove the temp files
    file1 <- tempfile(pattern = "ggplot_", fileext = ".png")
    file2 <- tempfile(pattern = "ggplot_", fileext = ".png")
    suppressMessages(ggplot2::ggsave(filename = file1, plot = plot1))
    suppressMessages(ggplot2::ggsave(filename = file2, plot = plot2))
    are_equal <- identical(digest::digest(file = file1), digest::digest(file = file2))
    file.remove(c(file1, file2))
    return(are_equal)
    
  }
  
}
