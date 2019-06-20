# Include other functions in package
#' @include Pipes.R
#' @include Error_Handling.R
#' @importFrom data.table :=
NULL

# Avoid "undefined variable" notes in package checking
globalVariables(c(".SD", ".I", "Weight__", "Index__", "Value__", "Variable__"))


#' Binned One-Way Data/Plot
#'
#' Calculate the (weighted) average value of a binned set of data, and plot it.
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
#'     \code{e == equal}, which will do the same as \code{p == pretty}.
#'     \code{p == pretty}, which will cut the range of \code{x} into equally sized bins,
#'       with the endpoints of each bin being chosen to be nice round numbers.
#'     \code{q == quantile}, which will cut \code{x} into bins according to weighted quantiles.
#'     \code{m == min == minimum}, which will try to ensure there are a minimum of
#'       \code{bins} observations in each bin.
#' @param bins The number of bins to create (or, if \code{type} is in \code{m == min == minimum},
#'   the desired minimum number of observations per bin).
#'
#' @return \code{binned_one_way_data}: a \code{data.table} holding the binned data.
#' @name binned
#' @export
#'
#' @examples
#' # 
binned_one_way_data <- function(x, yData, weight = rep(1, length(x)), scaleWeight = TRUE, type = "quantile", bins = 10) {
  
  # Convert yData to a data.table (if necessary), & check that the lengths are all the same
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
  binnedData <- data.table::data.table(Bins__ = dataBins, Weight__ = weight) %>% cbind(yData)
  sdCols <- setdiff(names(binnedData), c("Bins__", "Weight__"))
  binnedData <- binnedData[, c(lapply(.SD, stats::weighted.mean, w = Weight__), Weight__ = sum(Weight__)), keyby = "Bins__", .SDcols = sdCols]
  
  # Scale the weight if desired, & return the binned data (the extra brackets mean it will print implicitly)
  if (scaleWeight) binnedData[, Weight__ := Weight__ / sum(Weight__)]
  return(binnedData[])
  
}


#' @param fontSize Size of the font to use in the plot.
#' @param showWeights If \code{TRUE}, the weights plot will be shown as well.
#' @param plotly Will return a \code{plotly} object instead of a \code{ggplot} one.
#' @param xlab The x-axis label of the plot.
#' @param ylab The y-axis label of the data section of the plot.
#' @param wlab The y-axis label of the weight section of the plot.
#' @param title The title of the plot.
#'
#' @return \code{binned_one_way_plot}: a \code{ggplot} object, or a \code{plotly} object if \code{plotly} is \code{TRUE}.
#' @rdname binned
#' @export
#'
#' @examples
#' # 
binned_one_way_plot <- function(x, yData, weight = rep(1, length(x)), scaleWeight = TRUE, type = "quantile", bins = 10,
                                fontSize = 10, showWeights = TRUE, plotly = FALSE,
                                xlab = "Bins", ylab = "Response", wlab = "Weight", title = "One-Way Plot") {
  
  # Calculate the binned one way data, create an index, & melt the table
  binnedData <- binned_one_way_data(x = x, yData = yData, weight = weight,
                                    scaleWeight = scaleWeight, type = type, bins = bins)
  binnedData[, Index__ := .I]
  meltedBinnedData <- data.table::melt.data.table(binnedData, id.vars = c("Bins__", "Weight__", "Index__"),
                                                  variable.name = "Variable__", value.name = "Value__")
  
  # Return a plotly object if desired, else a ggplot one
  if (plotly) {
    
    # Create the plot object, using the bins along the x-axis
    dataPlot <- plotly::plot_ly(x = ~ as.factor(Bins__)) %>%
      
      # Add lines with points for each data column
      plotly::add_trace(
        data = meltedBinnedData, y = ~ Value__, color = ~ Variable__
        , text = ~ paste0(Bins__, "\n", round(Value__, 3)), hoverinfo = "text"
        , colors = c("#FF3333", "#33FF33", "#4488FF"), mode = "lines+markers", type = "scatter"
      ) %>%
      
      # Center the legend above the plot, & name the axes
      plotly::layout(
        legend = list(orientation = "h", xanchor = "center", x = 0.5, y = 10)
        , title = list(text = title, yanchor = "top", y = 1)
        # , annotations = list(xanchor = "center", x = 0.5, y = 10)
        # , annotations = list(text = "One-Way Plot", showarrow = FALSE, xref = "paper",
                             # yref = "paper", yanchor = "top", y = 1)
        , xaxis = list(title = xlab)
        , yaxis = list(title = ylab)
      )
        
    # If the weights should be shown, merge & align them, else return just the plotted data
    if (showWeights) {
      
      # Add the weights in the background
      dataPlot <- plotly::add_bars(
          p = dataPlot, name = "Weight", data = binnedData, y = ~ Weight__, color = I("#666666")
          , text = ~ paste0(Bins__, "\n", round(Weight__, 3)), hoverinfo = "text", yaxis = "y2"
          , marker = list(line = list(color = "#FF0000", width = 1))
        ) %>%
        
        # Ensure the lines are on top of the bars, & move the weight axis to the right side
        plotly::layout(
          yaxis = list(overlaying = "y2")
          , yaxis2 = list(side = "right", title = wlab, showgrid = FALSE)
        )
      
    }
      
  } else {
    
    # Create the y data ggplot, with lines & points for each data column
    dataPlot <- ggplot2::ggplot(meltedBinnedData, mapping = ggplot2::aes(x = Index__, y = Value__, color = Variable__)) +
      ggplot2::geom_line(size = 1) + ggplot2::geom_point(size = 2) +
      
      # Set the x-axis limits, use bin names as labels, & format the y-axis labels to use commas
      ggplot2::scale_x_continuous(limits = c(0.5, max(meltedBinnedData$Index__) + 0.5),
                                  breaks = meltedBinnedData$Index__, labels = meltedBinnedData$Bins__) +
      ggplot2::scale_y_continuous(labels = scales::comma) +
      
      # Set the title & axis labels, put the legend on top, size the text, center the title, & tilt the x-axis labels
      ggplot2::labs(title = title, x = xlab, y = ylab, color = "Variable") +
      ggplot2::theme(
        legend.position = "top"
        , text = ggplot2::element_text(size = fontSize)
        , plot.title = ggplot2::element_text(hjust = 0.5)
        , axis.text.x = ggplot2::element_text(angle = 22.5, hjust = 1)
      )
    
    # If the weights should be shown, merge & align the plots, else return just the plotted data
    if (showWeights) {
      
      # Remove x-axis label, text, & ticks
      dataPlot <- dataPlot +
        ggplot2::labs(x = NULL) +
        ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())
      
      # Create the weights ggplot, using bars
      weightPlot <- ggplot2::ggplot(binnedData, mapping = ggplot2::aes(x = Index__, y = Weight__)) +
        ggplot2::geom_bar(stat = "identity", color = "#FF0000", size = 1) +
        
        # Set the x-axis limits, use bin names as labels, & format the y-axis labels to use commas or percents
        ggplot2::scale_x_continuous(limits = c(0.5, max(binnedData$Index__) + 0.5),
                                    breaks = binnedData$Index__, labels = binnedData$Bins__) +
        ggplot2::scale_y_continuous(labels = if (scaleWeight) scales::percent else scales::comma) +
        
        # Set the x-axis & y-axis labels, set the font size, & angle the text
        ggplot2::labs(x = xlab, y = wlab) +
        ggplot2::theme(
          text = ggplot2::element_text(size = fontSize)
          , axis.text.x = ggplot2::element_text(angle = 22.5, hjust = 1)
        )
      
      # Stack the data & weight plots vertically, align their axes, & set the heights
      dataPlot <- cowplot::plot_grid(dataPlot, weightPlot, nrow = 2, align = "v", rel_heights = c(1.5, 1))
      
    }
    
  }
  
  # Return the plotted data
  return(dataPlot)
  
}

# https://rpubs.com/MarkusLoew/226759
# d <- data.table::data.table(ggplot2::diamonds)
# a <- binned_one_way_data(d[, carat], d[, .(x = x + 0.5, y, z)], d[, price])[, Index__ := .I][]
# b <- data.table::melt.data.table(a, id.vars = c("Bins__", "Weight__", "Index__"), variable.name = "Variable__", value.name = "Value__")
# binned_one_way_plot(d[, carat], d[, .(x = x + 0.5, y, z)], d[, price])
# binned_one_way_plot(d[, carat], d[, .(x = x + 0.5, y, z)], d[, price], type = "equal", bins = 20)
# binned_one_way_plot(d[, carat], d[, .(x = x + 0.5, y, z)], d[, price], plotly = F, showWeights = T)
# binned_one_way_plot(d[, carat], d[, .(x = x + 0.5, y, z)], d[, price], plotly = F, showWeights = F)
# binned_one_way_plot(d[, carat], d[, .(x = x + 0.5, y, z)], d[, price], plotly = T, showWeights = T)
# binned_one_way_plot(d[, carat], d[, .(x = x + 0.5, y, z)], d[, price], plotly = T, showWeights = F)
#
# ggplot() +
#   geom_bar(color = "black", stat = "identity", width = 1, data = a, mapping = aes(x = Index__, y = Weight__ * 75)) +
#   geom_line(size = 1, data = b, mapping = aes(x = Index__, y = Value__, color = Variable__)) +
#   geom_point(size = 2, data = b, mapping = aes(x = Index__, y = Value__, color = Variable__)) +
#   scale_x_continuous(limits = c(0.5, max(b$Index__) + 0.5), breaks = a$Index__, labels = a$Bins__) +
#   scale_y_continuous(limits = range(a$x), labels = scales::comma)

# plotly::plot_ly(
#   data = binnedData, x = ~ as.factor(Bins__), y = ~ Weight__, type = "bar", name = "Weight",
#   marker = list(color = "#DDDDDD"), hoverinfo = "text", text = ~ paste(Weight__)
# ) %>% plotly::add_trace(
#   data = meltedBinnedData, x = ~ as.factor(Bins__), y = ~ Value__, type = "scatter", name = "Num of Obs",
#   mode = "markers+lines", marker = list(color = "#FF4400"), line = list(color = "#FF4400"),
#   yaxis = "y2", hoverinfo = "text", text = ~ paste(round(Value__, 2))
# ) %>% plotly::layout(
#   yaxis  = list(side = "right", title = "Num of Obs", showgrid = FALSE, zeroline = FALSE),
#   yaxis2 = list(side = "left", overlaying = "y", title = "Average Response", showgrid = FALSE, zeroline = FALSE),
#   legend = list(x = 0.3, y = 1.1, orientation = "h"), xaxis  = list(title = "")
# )

# plot_ly(x = ~ as.factor(Bins__)) %>%
#   add_trace(data = b, y = ~ Value__, color = ~ Variable__, colors = c("#FF3333", "#33FF33", "#4488FF"),
#             text = ~ round(Value__, 3), mode = "lines+markers", type = "scatter") %>%
#   add_bars(name = "Weight", data = a, y = ~ Weight__, color = I("#444444"),
#            text = ~ round(Weight__, 3), yaxis = "y2") %>%
#   layout(
#     legend = list(orientation = "h", xanchor = "center", x = 0.5, y = 10),
#     yaxis = list(side = "left", title = "Value", overlaying = "y2"),
#     yaxis2 = list(side = "right", title = "Weight")
#   )
