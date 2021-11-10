% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pal.R
\name{pal_na}
\alias{pal_na}
\title{NA palette.}
\usage{
pal_na(pal = "#7F7F7FFF")
}
\arguments{
\item{pal}{The hex code or name of the NA colour. Defaults to "#7F7F7FFF".}
}
\value{
A character vector.
}
\description{
A function to retreive a hex code for a colour to use for NA values.
}
\examples{
scales::show_col(pal_na()) 
}