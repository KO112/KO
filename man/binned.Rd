% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Binned_Data_and_Plots.R
\name{binned}
\alias{binned}
\alias{binned_one_way_data}
\alias{binned_one_way_plot}
\title{Binned One-Way Data/Plot}
\usage{
binned_one_way_data(
  x,
  yData,
  weight = rep(1, length(x)),
  scaleWeight = TRUE,
  type = "quantile",
  bins = 10
)

binned_one_way_plot(
  x,
  yData,
  weight = rep(1, length(x)),
  scaleWeight = TRUE,
  type = "quantile",
  bins = 10,
  fontSize = 10,
  showWeights = TRUE,
  plotly = FALSE,
  bgColor = "#CCDDFF",
  xlab = "Bins",
  ylab = "Response",
  wlab = "Weight",
  title = paste("One-Way Plot", if (!missing(xlab)) paste0("of ", xlab), if
    (!missing(ylab)) paste0("by ", ylab))
)
}
\arguments{
\item{x}{The vector to perform binning by.
If numeric, it will be binned according to \code{type}.
If non-numeric, it will be binned according to all the unique values.}

\item{yData}{A data.frame-like object (or numeric vector) whose numerical columns will be used
to calculate average values by bin.
Must have the same number of rows (or length) as the length of x.}

\item{weight}{An (optional) vector of weights for weighted binning,
and for taking weighted averages.
Must be the same length as x.}

\item{scaleWeight}{If \code{TRUE}, the weights will be scaled from 0 to 1 (i.e. as a percent).}

\item{type}{The type of binning desired.
Valid options are:
  \code{r == raw}, which will create a bin for each distinct value in \code{x}.
  \code{e == equal}, which will do the same as \code{p == pretty}.
  \code{p == pretty}, which will cut the range of \code{x} into equally sized bins,
    with the endpoints of each bin being chosen to be nice round numbers.
  \code{q == quantile}, which will cut \code{x} into bins according to weighted quantiles.
  \code{m == min == minimum}, which will try to ensure there are a minimum of
    \code{bins} observations in each bin.}

\item{bins}{The number of bins to create (or, if \code{type} is in \code{m == min == minimum},
the desired minimum number of observations per bin).}

\item{fontSize}{Size of the font to use in the plot.}

\item{showWeights}{If \code{TRUE}, the weights plot will be shown as well.}

\item{plotly}{Will return a \code{plotly} object instead of a \code{ggplot} one.}

\item{bgColor}{Color to use for the background of the plot.}

\item{xlab}{The x-axis label of the plot.}

\item{ylab}{The y-axis label of the data section of the plot.}

\item{wlab}{The y-axis label of the weight section of the plot.}

\item{title}{The title of the plot.}
}
\value{
\code{binned_one_way_data}: a \code{data.table} holding the binned data.

\code{binned_one_way_plot}: a \code{ggplot} object, or a \code{plotly} object if \code{plotly} is \code{TRUE}.
}
\description{
Calculate the (weighted) average value of a binned set of data, and plot it.
}
\examples{
diamonds <- data.table::data.table(ggplot2::diamonds)
binnedData1 <- binned_one_way_data(diamonds[, carat], diamonds[, .(x = x + 0.5, y, z)],
                                   diamonds[, price])
binnedData2 <- binned_one_way_data(diamonds[, carat], diamonds[, .(x = x + 0.5, y, z)],
                                   diamonds[, price], scaleWeight = FALSE)
binnedData3 <- binned_one_way_data(diamonds[, carat], diamonds[, .(x = x + 0.5, y, z)],
                                   diamonds[, price], type = "equal")
binnedData4 <- binned_one_way_data(diamonds[, carat], diamonds[, .(x = x + 0.5, y, z)], 
                                   diamonds[, price], bins = 5)
binnedPlot1 <- binned_one_way_plot(diamonds[, carat], diamonds[, .(x = x + 0.5, y, z)],
                                   diamonds[, price])
binnedPlot2 <- binned_one_way_plot(diamonds[, carat], diamonds[, .(x = x + 0.5, y, z)],
                                   diamonds[, price], scaleWeight = FALSE)
binnedPlot3 <- binned_one_way_plot(diamonds[, carat], diamonds[, .(x = x + 0.5, y, z)],
                                   diamonds[, price], type = "equal")
binnedPlot4 <- binned_one_way_plot(diamonds[, carat], diamonds[, .(x = x + 0.5, y, z)],
                                   diamonds[, price], bins = 5)
}
