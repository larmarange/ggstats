% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat_prop.R
\name{geom_prop_bar}
\alias{geom_prop_bar}
\alias{geom_prop_text}
\alias{geom_prop_connector}
\title{Convenient geometries for proportion bar plots}
\usage{
geom_prop_bar(
  mapping = NULL,
  data = NULL,
  position = "stack",
  ...,
  width = 0.9,
  complete = NULL,
  default_by = "x"
)

geom_prop_text(
  mapping = ggplot2::aes(!!!auto_contrast),
  data = NULL,
  position = ggplot2::position_stack(0.5),
  ...,
  complete = NULL,
  default_by = "x"
)

geom_prop_connector(
  mapping = NULL,
  data = NULL,
  position = "stack",
  ...,
  width = 0.9,
  complete = "fill",
  default_by = "x"
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}}. If specified and
\code{inherit.aes = TRUE} (the default), it is combined with the default mapping
at the top level of the plot. You must supply \code{mapping} if there is no plot
mapping.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{position}{A position adjustment to use on the data for this layer. This
can be used in various ways, including to prevent overplotting and
improving the display. The \code{position} argument accepts the following:
\itemize{
\item The result of calling a position function, such as \code{position_jitter()}.
This method allows for passing extra arguments to the position.
\item A string naming the position adjustment. To give the position as a
string, strip the function name of the \code{position_} prefix. For example,
to use \code{position_jitter()}, give the position as \code{"jitter"}.
\item For more information and other ways to specify the position, see the
\link[ggplot2:layer_positions]{layer position} documentation.
}}

\item{...}{Additional parameters passed to \code{\link[ggplot2:geom_bar]{ggplot2::geom_bar()}},
\code{\link[ggplot2:geom_text]{ggplot2::geom_text()}} or \code{\link[=geom_bar_connector]{geom_bar_connector()}}.}

\item{width}{Bar width (\code{0.9} by default).}

\item{complete}{Name (character) of an aesthetic for those statistics should
be completed for unobserved values (see example).}

\item{default_by}{If the \strong{by} aesthetic is not available, name of another
aesthetic that will be used to determine the denominators (e.g. \code{"fill"}),
or \code{NULL} or \code{"total"} to compute proportions of the total. To be noted,
\code{default_by = "x"} works both for vertical and horizontal bars.}
}
\description{
\code{geom_prop_bar()}, \code{geom_prop_text()} and \code{geom_prop_connector()} are
variations of \code{\link[ggplot2:geom_bar]{ggplot2::geom_bar()}}, \code{\link[ggplot2:geom_text]{ggplot2::geom_text()}} and
\code{\link[=geom_bar_connector]{geom_bar_connector()}} using \code{\link[=stat_prop]{stat_prop()}}, with custom default aesthetics:
\code{after_stat(prop)} for \strong{x} or \strong{y}, and
\code{scales::percent(after_stat(prop))} for \strong{label}.
}
\examples{
library(ggplot2)
d <- as.data.frame(Titanic)
ggplot(d) +
  aes(x = Class, fill = Survived, weight = Freq) +
  geom_prop_bar() +
  geom_prop_text() +
  geom_prop_connector()

ggplot(d) +
  aes(y = Class, fill = Survived, weight = Freq) +
  geom_prop_bar(width = .5) +
  geom_prop_text() +
  geom_prop_connector(width = .5, linetype = "dotted")

ggplot(d) +
  aes(
    x = Class,
    fill = Survived,
    weight = Freq,
    y = after_stat(count),
    label = after_stat(count)
  ) +
  geom_prop_bar() +
  geom_prop_text() +
  geom_prop_connector()
}
\seealso{
\code{\link[=geom_bar_connector]{geom_bar_connector()}}
}
