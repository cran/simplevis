% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/theme.R
\name{theme_map}
\alias{theme_map}
\title{Theme for maps.}
\usage{
theme_map(font_family = "", font_size_title = 11,
  font_size_body = 10)
}
\arguments{
\item{font_family}{Font family to use. Defaults to "".}

\item{font_size_title}{Font size for the title text. Defaults to 11.}

\item{font_size_body}{Font size for all text other than the title. Defaults to 10.}
}
\value{
A ggplot theme.
}
\description{
Theme for maps.
}
\examples{
library(ggplot2)

ggplot() +
  theme_map("Courier", 9, 7) +
  ggtitle("This is a title of a font family and size")
}
