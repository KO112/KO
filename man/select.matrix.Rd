% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Select_Matrix.R
\name{select.matrix}
\alias{select.matrix}
\title{Select Columns of a Matrix}
\usage{
\method{select}{matrix}(.data, ..., throwError = TRUE)
}
\arguments{
\item{.data}{The matrix to select columns from.}

\item{...}{The column names to select (can be quoted, but not necessary).}

\item{throwError}{whether to throw an error if invalid columns are selected
(if \code{FALSE}), only a warning will be sent) (logical scalar).}
}
\value{
A matrix of the selected columns.
}
\description{
Select columns of a matrix by name, \code{dplyr} style.
Defines the \code{dplyr::select} method for matrixes.
}
\examples{
library(dplyr)
mat <- as.matrix(mtcars)
select(mat, mpg)
select(mat, mpg, cyl)
select(mat, mpg, cyl, "disp")
select(mat, "mpg", "cyl", "disp", "am")
try(select(mat, mpg2))
try(select(mat, mpg2, throwError = TRUE))
select(mat, mpg2, throwError = FALSE)

}
