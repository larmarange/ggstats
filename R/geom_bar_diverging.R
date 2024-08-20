#' Geometries for diverging bar plots
#'
#' These geometries are similar to [`ggplot2::geom_bar()`] but provides
#' different set of default values.
#'
#' - `geom_bar_diverging()` is designed for stacked diverging bar plots, using
#'   [`position_diverging()`].
#' - `geom_bar_diverging()` is designed for Likert-type items. Using
#'   `position_likert()` (each bar sums to 100%).
#' - `geom_bar_pyramid()` is adapted for population pyramid plots when a factor
#'   with two levels is mapped to the **fill** aesthetics. The proportions are,
#'   here by default, computed separately for each value of the **fill**
#'   aesthetics.
#'
#' @param mapping Optional set of aesthetic mappings.
#' @param data The data to be displayed in this layers.
#' @param stat The statistical transformation to use on the data for this layer.
#' @param position A position adjustment to use on the data for this layer.
#' @param ... Other arguments passed on to [`ggplot2::geom_bar()`]
#' @param complete An aesthetic for those unobserved values should be completed,
#' see [`stat_prop()`]. Passed only if `stat = "prop"`.
#' @param complete Name of an aesthetic determining denominators by default,
#' see [`stat_prop()`]. Passed only if `stat = "prop"`.
#' @inheritParams position_likert
#' @param default_aes Default aesthetics.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(diamonds) +
#'   aes(y = clarity, fill = cut) +
#'   geom_bar_diverging()
#'
#' ggplot(diamonds) +
#'   aes(y = clarity, fill = cut) +
#'   geom_bar_diverging(cutoff = 1)
#'
#' ggplot(diamonds) +
#'   aes(y = clarity, fill = cut) +
#'   geom_bar_likert()
#'
#' d <- Titanic |> as.data.frame()
#'
#' ggplot(d) +
#'   aes(y = Class, fill = Sex, weight = Freq) +
#'   geom_bar_diverging()
#'
#' ggplot(d) +
#'   aes(y = Class, fill = Sex, weight = Freq) +
#'   geom_bar_pyramid()
geom_bar_diverging <- function(mapping = NULL,
                               data = NULL,
                               stat = "prop",
                               position = position_diverging(
                                 reverse = reverse,
                                 exclude_fill_values = exclude_fill_values,
                                 cutoff = cutoff
                               ),
                               ...,
                               complete = "fill",
                               default_by = "total",
                               vjust = 1,
                               reverse = FALSE,
                               exclude_fill_values = NULL,
                               cutoff = NULL) {

  args <- list(...)
  if (stat %in% c("prop", "prop2")) {
    args$complete <- complete
    args$default_by <- default_by
  }

  args$mapping <- mapping
  args$data <- data
  args$stat <- stat
  args$position <- position
  do.call(ggplot2::geom_bar, args)
}

geom_bar_likert <- function(mapping = NULL,
                            data = NULL,
                            stat = "prop",
                            position = position_likert(
                              reverse = reverse,
                              exclude_fill_values = exclude_fill_values,
                              cutoff = cutoff
                            ),
                            ...,
                            complete = "fill",
                            default_by = "x",
                            vjust = 1,
                            reverse = FALSE,
                            exclude_fill_values = NULL,
                            cutoff = NULL) {

  args <- c(as.list(environment()), list(...))
  do.call(geom_bar_diverging, args)
}

geom_bar_pyramid <- function(mapping = NULL,
                             data = NULL,
                             stat = "prop2",
                             position = position_diverging(
                               reverse = reverse,
                               exclude_fill_values = exclude_fill_values,
                               cutoff = cutoff
                             ),
                             ...,
                             complete = "fill",
                             default_by = "fill",
                             vjust = 1,
                             reverse = FALSE,
                             exclude_fill_values = NULL,
                             cutoff = NULL) {

  args <- c(as.list(environment()), list(...))
  do.call(geom_bar_diverging, args)
}
