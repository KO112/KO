% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Error_Handling.R
\name{stop_if}
\alias{stop_if}
\title{Stop if condition is met (with message)}
\usage{
stop_if(condition, error_msg)
}
\arguments{
\item{condition}{Condition that, if met, will stop and print an error message.}

\item{error_msg}{Message to be displayed if the stopping condition is met.}
}
\value{
Nothing.
}
\description{
If the condition is met, stop execution, and print an error message
}
\examples{
# stop_if(TRUE, "I'm an error message") # Will stop execution and print the message
# stop_if(FALSE, "I'm an error message") # Will do nothing

}
