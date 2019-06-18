#' Title
#'
#'
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
binned_one_way_data <- function(x, yData, weight = rep(1, nrow(yData)), type = "quantile", bins = 10) {

  # If x isn't numeric, or if there are as many distinct x values as bins requested, just use x
  if (!is.numeric(x) || (dplyr::n_distinct(x) <= bins)) {

    dataBins <- x

  } else if (is.numeric(x)) {

    # Calculate the bins of the data
    dataBins <- switch(
      type
      , r = , raw = x
      , e = , equal =
      , p = , pretty = Hmisc::cut2(x, cuts = pretty(x, bins), oneval = FALSE)
      # , q = , quantile = Hmisc::cut2(round(x, 3), cuts = unique(Hmisc::wtd.quantile(x, weights = weight, probs = seq_len(bins - 1) / bins)))
      , q = , quantile = Hmisc::cut2(x, cuts = unique(Hmisc::wtd.quantile(x, weights = weight, probs = seq_len(bins - 1) / bins)))
      , m = , min = , minimum = Hmisc::cut2(x, m = bins)
    )

  }

  # Remove non-numeric data from yData
  yData <- dplyr::select_if(yData, is.numeric)

  # Summarize the data, & return the binned data
  binnedData <- data.table::data.table(binnedCol__ = dataBins, weight = weight) %>% cbind(yData)
  sdCols <- setdiff(names(binnedData), c("binnedCol__", "weight"))
  binnedData <- binnedData[, c(lapply(.SD, weighted.mean, w = weight), weight = sum(weight)), keyby = "binnedCol__", .SDcols = sdCols]
  return(binnedData)

}


binned_one_way_plot <- function(x, yData, weight = rep(1, nrow(yData)), type = "equal", bins = 10, fontSize = 10,
                                showWeights = TRUE, overlap = TRUE, # plotly = FALSE,
                                xlab = "Bins", ylab = "Response", wlab = "Weight", title = "One-Way Plot") {

  # Calculate the binned one way data, & create an index
  binnedData <- binned_one_way_data(x = x, yData = yData, weight = weight, type = type, bins = bins)
  binnedData[, index := .I]

  # Plot the weights (done first since the data is melted/gathered below)
  weightPlot <- ggplot2::ggplot(binnedData) +
    ggplot2::aes(x = index, y = weight) +
    ggplot2::geom_bar(color = "black", stat = "identity", width = 1) +
    ggplot2::labs(x = xlab, y = wlab) +
    ggplot2::scale_x_continuous(limits = c(0.5, max(binnedData$index) + 0.5), breaks = binnedData$index, labels = binnedData$binnedCol__) +
    ggplot2::theme(
      text = ggplot2::element_text(size = fontSize)
      , axis.text.x = ggplot2::element_text(angle = 22.5, hjust = 1)
    )

  # Plot the y data
  binnedData <- binnedData[, data.table::melt.data.table(binnedData, id.vars = c("binnedCol__", "weight", "index"))]
  dataPlot <- ggplot2::ggplot(binnedData) +
    ggplot2::aes(x = index, y = value, color = variable) +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(x = NULL, y = ylab, title = title) +
    ggplot2::scale_x_continuous(limits = c(0.5, max(binnedData$index) + 0.5)) +
    ggplot2::theme(
      legend.position = "top"
      , text = ggplot2::element_text(size = fontSize)
      , plot.title = ggplot2::element_text(hjust = 0.5)
      , axis.text.x = ggplot2::element_blank()
      , axis.ticks.x = ggplot2::element_blank()
    )

  # Align the plots better (see https://github.com/tidyverse/ggplot2/wiki/align-two-plots-on-a-page)
  dataPlot <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(dataPlot))
  weightPlot <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(weightPlot))
  combinedPlot <- rbind(dataPlot, weightPlot, size = "first")
  combinedPlot$widths <- grid::unit.pmax(dataPlot$widths, weightPlot$widths)

  # Return the plots (merging into one if desired)
  if (showWeights) return(gridExtra::arrangeGrob(dataPlot, weightPlot))
  if (showWeights) return(combinedPlot)
  return(dataPlot)

}
