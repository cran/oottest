% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vuong_statistic.R
\name{vuong_matrix}
\alias{vuong_matrix}
\title{Getting a vuong matrix}
\usage{
vuong_matrix(data, theories)
}
\arguments{
\item{data}{matrix rows: choices, columns: treatments}

\item{theories}{list of matrices each in the same format as above}
}
\value{
Table of z-scores for all comparisons
}
\description{
Getting a vuong matrix
}
\examples{
vuong_matrix(data_two_action_games, predictions_two_action_games)
}
