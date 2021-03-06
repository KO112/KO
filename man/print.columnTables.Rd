% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Data_Dictionary.R
\name{print.columnTables}
\alias{print.columnTables}
\title{Print a \code{dataDict} Object}
\usage{
\method{print}{columnTables}(x, ...)
}
\arguments{
\item{x}{A \code{columnTables} object.}

\item{...}{Currently not used.}
}
\value{
A summary of \code{x}, invisibly (i.e. \code{summary(x)}).
}
\description{
Print a \code{dataDict} Object
}
\examples{
dd <- dataDict(mtcars)
dd$colTables
print(dd$colTables)

}
