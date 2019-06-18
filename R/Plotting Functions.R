#' Title
#'
#' @param x
#' @param yData
#' @param weight
#' @param type
#' @param bins
#'
#' @return
#' @export
#'
#' @examples
binned_one_way_data <- function(x, yData, weight = 1, type = "quantile", bins = 10) {

  # If x isn't numeric, or if there are as many distinct x values as bins requested, just use x
  if (!is.numeric(x) || (dplyr::n_distinct(x) <= bins)) {

    dataBins <- x

  } else if (is.numeric(x)) {

    # Calculate the bins of the data
    dataBins <- switch(
      type
      , r = , raw = x
      , e = , equal = Hmisc::cut2(x, cuts = pretty(x, bins), oneval = F)
      # , q = , quantile = Hmisc::cut2(round(x, 3), cuts = unique(Hmisc::wtd.quantile(x, weights = weight, probs = seq_len(bins - 1) / bins)))
      , q = , quantile = Hmisc::cut2(x, cuts = unique(Hmisc::wtd.quantile(x, weights = weight, probs = seq_len(bins - 1) / bins)))
      , m = , min = , minimum = Hmisc::cut2(x, m = bins)
    )

  }

  # Summarize the data, & return the binned data
  binnedData <- data.table::data.table(x = dataBins, weight = weight) %>% cbind(yData)
  sdCols <- setdiff(names(binnedData), c("x", "weight"))
  binnedData <- binnedData[, c(lapply(.SD, weighted.mean, w = weight), weight = sum(weight)), by = "x", .SDcols = sdCols]
  return(binnedData)

}


binned_one_way_plot <- function(x, yData, weight = 1, type = "equal", bins = 10, fontSize = 10,
                                showWeights = TRUE, overlap = TRUE, # plotly = FALSE,
                                xlab = "Bins", ylab = "Response", wlab = "Weight", title = "One-Way Plot") {

  # Calculate the binned one way data, & create an index
  binnedData <- binned_one_way_data(x = x, yData = yData, weight = weight, type = type, bins = bins)
  binnedData[, index = .I]

  # Plot the weights (done first since the data is melted/gathered below)
  weightPlot <- ggplot(binnedData) +
    aes(x = index, y = weight) +
    geom_bar(color = "black", stat = "identity", width = 1) +
    labs(x = xlab, y = wlab) # +
    # scale_x_continuous(limits = c(0.5, max(data$index) + 0.5), breaks = data$index, labels = data$x) +
    # theme(
    #   axis.text.x = element_text(angle = 45, hjust = 1)
    #   , plot.title = element_text(hjust = 0.5)
    #   , text = element_text(size = fontSize)
    # )

  # Plot the y data
  binnedData <- binnedData[, melt.data.table(data, id.vars = c("x", "weight", "index"))]
  dataPlot <- ggplot(binnedData) +
    aes(x = index, y = value, color = variable) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(x = NULL, y = ylab, title = title) +
    scale_x_continuous(limits = c(0.5, max(data$index) + 0.5)) # +
    # theme(
    #   axis.line.x = element_blank()
    #   , axis.text.x = element_blank()
    #   , axis.ticks.x = element_blank()
    #   , legend.position = "top"
    #   , plot.title = element_text(hjust = 0.5)
    #   , text = element_text(size = fontSize)
    # )

  # Alight the plots better (see http://www.exegetic.biz/blog/2015/05/r-recipe-aligning-axes-in-ggplot2)
  p1 <- ggplot_gtable(ggplot_build(p1))
  p2 <- ggplot_gtable(ggplot_build(p2))
  maxWidth <- unit.pmax(p1$widths[2:3], p2$widths[2:3])
  p1$widths[2:3] <- maxWidth
  p2$widths[2:3] <- maxWidth

  # Return the plots (merging into one if desired)
  if (showWeights) return(gridExtra::arrangeGrob(dataPlot, weightPlot))
  return(dataPlot)


}