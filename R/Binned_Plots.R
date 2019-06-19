# Include other functions in package
#' @include Pipes.R
NULL


#' Binned One-Way Data
#'
#' Calculate the average value of
#'
#' @param x The vector to perform binning by.
#'   If numeric, it will be binned according to \code{type}.
#'   If non-numeric, it will be binned according to all the unique values.
#' @param yData A data.frame-like object (or numeric vector) whose numerical columns will be used
#'   to calculate average values by bin.
#'   Must have the same number of rows (or length) as the length of x.
#' @param weight An (optional) vector of weights for weighted binning,
#'   and for taking weighted averages.
#'   Must be the same length as x.
#' @param scaleWeight If \code{TRUE}, the weights will be scaled from 0 to 1 (i.e. as a percent).
#' @param type The type of binning desired.
#'   Valid options are:
#'     \code{r == raw}, which will create a bin for each distinct value in \code{x}.
#'     \code{e == equal}, which will do the same as \code{p == pretty}
#'     \code{p == pretty}, which will cut the range of \code{x} into equally sized bins,
#'       with the endpoints of each bin being chosen to be nice round numbers.
#'     \code{q == quantile}, which will cut \code{x} into bins according to weighted quantiles.
#'     \code{m == min == minimum}, which will try to ensure there are a minimum of
#'       \code{bins} observations in each bin.
#' @param bins The number of bins to create (or, if \code{type} is in \code{m == min == minimum},
#'   the desired minimum number of observations per bin).
#'
#' @return
#' @export
#'
#' @examples
binned_one_way_data <- function(x, yData, weight = rep(1, length(x)), scaleWeight = TRUE, type = "quantile", bins = 10) {
  
  # Convert yData to a data.table (if necessary), and check that the lengths are all the same
  if (is.vector(yData)) yData <- data.table::data.table(y = yData)
  stop_if(!is.atomic(x), paste0("The vector to bin by (x) must be atomic. It is instead of class: ", class(x)))
  stop_if(length(x) != length(weight), "The weights are not the same length as the vector to bin by (x).")
  stop_if(length(x) != nrow(yData), "The data does not have the same number of rows as the length of the vector to bin by (x).")
  
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
      , q = , quantile = Hmisc::cut2(x, cuts = unique(Hmisc::wtd.quantile(x, weights = weight, probs = seq_len(bins - 1) / bins)))
      , m = , min = , minimum = Hmisc::cut2(x, m = bins)
    )
    
  }
  
  # Remove non-numeric data from yData
  yData <- dplyr::select_if(yData, is.numeric)
  
  # Summarize the data by bins
  binnedData <- data.table::data.table(bins__ = dataBins, weight = weight) %>% cbind(yData)
  sdCols <- setdiff(names(binnedData), c("bins__", "weight"))
  binnedData <- binnedData[, c(lapply(.SD, weighted.mean, w = weight), weight = sum(weight)), keyby = "bins__", .SDcols = sdCols]
  
  # Scale the weight if desired, and return the binned data (the extra brackets mean it will print implicitly)
  if (scaleWeight) binnedData[, weight := weight / sum(weight)]
  return(binnedData[])
  
}


#' Title
#'
#' @param x The vector to perform binning by.
#'   If numeric, it will be binned according to \code{type}.
#'   If non-numeric, it will be binned according to all the unique values.
#' @param yData A data.frame-like object (or numeric vector) whose numerical columns will be used
#'   to calculate average values by bin.
#'   Must have the same number of rows (or length) as the length of x.
#' @param weight An (optional) vector of weights for weighted binning,
#'   and for taking weighted averages.
#'   Must be the same length as x.
#' @param scaleWeight If \code{TRUE}, the weights will be scaled from 0 to 1 (i.e. as a percent).
#' @param type The type of binning desired.
#'   Valid options are:
#'     \code{r == raw}, which will create a bin for each distinct value in \code{x}.
#'     \code{e == equal}, which will do the same as \code{p == pretty}
#'     \code{p == pretty}, which will cut the range of \code{x} into equally sized bins,
#'       with the endpoints of each bin being chosen to be nice round numbers.
#'     \code{q == quantile}, which will cut \code{x} into bins according to weighted quantiles.
#'     \code{m == min == minimum}, which will try to ensure there are a minimum of
#'       \code{bins} observations in each bin.
#' @param bins The number of bins to create (or, if \code{type} is in \code{m == min == minimum},
#'   the desired minimum number of observations per bin).
#' @param fontSize 
#' @param showWeights 
#' @param overlap If \code{TRUE}, the data and weights plots will be overlapped
#'   (not used if \code{showWeights} is \code{FALSE}).
#' @param xlab The x-axis label of the plot.
#' @param ylab The y-axis label of the data section of the plot.
#' @param wlab The y-axis label of the weight section of the plot.
#' @param title The title of the plot.
#'
#' @return
#' @export
#'
#' @examples
binned_one_way_plot <- function(x, yData, weight = rep(1, length(x)), scaleWeight = TRUE, type = "quantile", bins = 10,
                                fontSize = 10, showWeights = TRUE, overlap = TRUE, # plotly = FALSE,
                                xlab = "Bins", ylab = "Response", wlab = "Weight", title = "One-Way Plot") {
  
  # Calculate the binned one way data, create an index, & melt the table
  binnedData <- binned_one_way_data(x = x, yData = yData, weight = weight, scaleWeight = scaleWeight, type = type, bins = bins)
  binnedData[, index := .I]
  meltedBinnedData <- binnedData[, data.table::melt.data.table(binnedData, id.vars = c("bins__", "weight", "index"))]
  
  # Create the y data plot
  dataPlot <- ggplot2::ggplot(meltedBinnedData) +
    ggplot2::aes(x = index, y = value, color = variable) +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(x = xlab, y = ylab, title = title) +
    ggplot2::scale_x_continuous(limits = c(0.5, max(meltedBinnedData$index) + 0.5), breaks = meltedBinnedData$index, labels = meltedBinnedData$bins__) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::theme(
      legend.position = "top"
      , text = ggplot2::element_text(size = fontSize)
      , plot.title = ggplot2::element_text(hjust = 0.5)
      , axis.text.x = ggplot2::element_text(angle = 22.5, hjust = 1)
    )
  
  # If the weights should be shown, merge and align them, else return just the plotted data
  if (showWeights) {
    
    # Remove x-axis label, text and ticks
    dataPlot <- dataPlot +
      ggplot2::labs(x = NULL) +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())
    
    # Plot the weights
    weightPlot <- ggplot2::ggplot(binnedData) +
      ggplot2::aes(x = index, y = weight) +
      ggplot2::geom_bar(color = "black", stat = "identity", width = 1) +
      ggplot2::labs(x = xlab, y = wlab) +
      ggplot2::scale_x_continuous(limits = c(0.5, max(binnedData$index) + 0.5), breaks = binnedData$index, labels = binnedData$bins__) +
      ggplot2::scale_y_continuous(labels = if (scaleWeight) scales::percent else scales::comma) +
      ggplot2::theme(
        text = ggplot2::element_text(size = fontSize)
        , axis.text.x = ggplot2::element_text(angle = 22.5, hjust = 1)
      )
    
    # Align the plots nicely (see https://github.com/tidyverse/ggplot2/wiki/align-two-plots-on-a-page)
    dataPlot <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(dataPlot))
    weightPlot <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(weightPlot))
    combinedPlot <- rbind(dataPlot, weightPlot, size = "first")
    combinedPlot$widths <- grid::unit.pmax(dataPlot$widths, weightPlot$widths)
    return(combinedPlot)
    
  } else {
    
    return(dataPlot)
    # return(plotly::ggplotly(dataPlot))
    
  }
  
}

# https://rpubs.com/MarkusLoew/226759
# d <- data.table::data.table(ggplot2::diamonds)
# a <- binned_one_way_data(d[, carat], d[, .(x = x + 0.5, y, z)], d[, price])[, index := .I]
# b <- a[, data.table::melt.data.table(a, id.vars = c("bins__", "weight", "index"))]
#
# ggplot() +
#   geom_bar(color = "black", stat = "identity", width = 1, data = a, mapping = aes(x = index, y = weight * 75)) +
#   geom_line(size = 1, data = b, mapping = aes(x = index, y = value, color = variable)) +
#   geom_point(size = 2, data = b, mapping = aes(x = index, y = value, color = variable)) +
#   scale_x_continuous(limits = c(0.5, max(b$index) + 0.5), breaks = a$index, labels = a$bins__) +
#   scale_y_continuous(limits = range(a$x), labels = scales::comma)
