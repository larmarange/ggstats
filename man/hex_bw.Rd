% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hex_bw.R
\name{hex_bw}
\alias{hex_bw}
\alias{hex_bw_threshold}
\title{Identify a suitable font color (black or white) given a background HEX color}
\source{
Adapted from \code{saros}
}
\usage{
hex_bw(hex_code)

hex_bw_threshold(hex_code, values, threshold)
}
\arguments{
\item{hex_code}{Background color in hex-format.}

\item{values}{Values to be compared.}

\item{threshold}{Threshold.}
}
\value{
Either black or white, in hex-format
}
\description{
\code{hex_bw_threshold()} is a variation of \code{hex_bw()}. For \code{values} below
\code{threshold}, black (\code{"#000000"}) will always be returned, regardless of
\code{hex_code}.
}
\examples{
hex_bw("#0dadfd")

library(ggplot2)
ggplot(diamonds) +
  aes(x = cut, fill = color, label = after_stat(count)) +
  geom_bar() +
  geom_text(
    mapping = aes(color = after_scale(hex_bw(.data$fill))),
    position = position_stack(.5),
    stat = "count",
    size = 2
  )
}
