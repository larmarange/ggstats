% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hex_bw.R
\docType{data}
\name{hex_bw}
\alias{hex_bw}
\alias{hex_bw_threshold}
\alias{auto_contrast}
\title{Identify a suitable font color (black or white) given a background HEX color}
\format{
An object of class \code{uneval} of length 1.
}
\source{
Adapted from \code{saros} for \code{hex_code()} and from
\url{https://github.com/teunbrand/ggplot_tricks?tab=readme-ov-file#text-contrast}
for \code{auto_contrast}.
}
\usage{
hex_bw(hex_code)

hex_bw_threshold(hex_code, values, threshold)

auto_contrast
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
You could use \code{auto_contrast} as a shortcut of
\code{aes(colour = after_scale(hex_bw(.data$fill)))}. You should use \verb{!!!} to
inject it within \code{\link[ggplot2:aes]{ggplot2::aes()}} (see examples).

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

ggplot(diamonds) +
  aes(x = cut, fill = color, label = after_stat(count)) +
  geom_bar() +
  geom_text(
    mapping = auto_contrast,
    position = position_stack(.5),
    stat = "count",
    size = 2
  )

ggplot(diamonds) +
  aes(x = cut, fill = color, label = after_stat(count), !!!auto_contrast) +
  geom_bar() +
  geom_text(
    mapping = auto_contrast,
    position = position_stack(.5),
    stat = "count",
    size = 2
  )
}
\keyword{datasets}
