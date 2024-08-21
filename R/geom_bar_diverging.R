#' Geometries for diverging bar plots
#'
#' These geometries are similar to [`ggplot2::geom_bar()`] but provides
#' different set of default values.
#'
#' - `geom_bar_diverging()` is designed for stacked diverging bar plots, using
#'   [`position_diverging()`].
#' - `geom_bar_likert()` is designed for Likert-type items. Using
#'   `position_likert()` (each bar sums to 100%).
#' - `geom_bar_pyramid()` is similar to `geom_bar_diverging()` but uses
#'   proportions of the total instead of counts.
#'
#' To add labels on the bar plots, simply use `geom_text_diverging()`,
#' `geom_text_likert()`, or `geom_text_pyramid()`.
#'
#' @param mapping Optional set of aesthetic mappings.
#' @param data The data to be displayed in this layers.
#' @param stat The statistical transformation to use on the data for this layer.
#' @param position A position adjustment to use on the data for this layer.
#' @param ... Other arguments passed on to [`ggplot2::geom_bar()`]
#' @param complete An aesthetic for those unobserved values should be completed,
#' see [`stat_prop()`]. Passed only if `stat = "prop"`.
#' @param default_by Name of an aesthetic determining denominators by default,
#' see [`stat_prop()`]. Passed only if `stat = "prop"`.
#' @param height Statistic used, by default, to determine the height/width,
#' see [`stat_prop()`]. Passed only if `stat = "prop"`.
#' @param labels Statistic used, by default, to determine the labels,
#' see [`stat_prop()`]. Passed only if `stat = "prop"`.
#' @param labeller Labeller function to format labels,
#' see [`stat_prop()`]. Passed only if `stat = "prop"`.
#' @inheritParams position_likert
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(diamonds) +
#'   aes(x = clarity, fill = cut) +
#'   geom_bar_diverging()
#'
#' ggplot(diamonds) +
#'   aes(x = clarity, fill = cut) +
#'   geom_bar_diverging(cutoff = 4)
#'
#' ggplot(diamonds) +
#'   aes(y = clarity, fill = cut) +
#'   geom_bar_likert() +
#'   geom_text_likert(aes(color = after_scale(hex_bw(.data$fill))))
#'
#' d <- Titanic |> as.data.frame()
#'
#' ggplot(d) +
#'   aes(y = Class, fill = Sex, weight = Freq) +
#'   geom_bar_diverging() +
#'   geom_text_diverging()
#'
#' ggplot(d) +
#'   aes(y = Class, fill = Sex, weight = Freq) +
#'   geom_bar_pyramid() +
#'   geom_text_pyramid()
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
                               height = "count",
                               reverse = FALSE,
                               exclude_fill_values = NULL,
                               cutoff = NULL) {

  args <- list(...)
  if (stat == "prop") {
    args$complete <- complete
    args$default_by <- default_by
    args$height <- height
  }

  args$mapping <- mapping
  args$data <- data
  args$stat <- stat
  args$position <- position
  do.call(ggplot2::geom_bar, args)
}

#' @rdname geom_bar_diverging
#' @export
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
                            height = "prop",
                            reverse = FALSE,
                            exclude_fill_values = NULL,
                            cutoff = NULL) {

  args <- c(as.list(environment()), list(...))
  do.call(geom_bar_diverging, args)
}

#' @rdname geom_bar_diverging
#' @export
geom_bar_pyramid <- function(mapping = NULL,
                             data = NULL,
                             stat = "prop",
                             position = position_diverging(
                               reverse = reverse,
                               exclude_fill_values = exclude_fill_values,
                               cutoff = cutoff
                             ),
                             ...,
                             complete = NULL,
                             default_by = "total",
                             height = "prop",
                             reverse = FALSE,
                             exclude_fill_values = NULL,
                             cutoff = NULL) {

  args <- c(as.list(environment()), list(...))
  do.call(geom_bar_diverging, args)
}

#' @rdname geom_bar_diverging
#' @export
geom_text_diverging <- function(mapping = NULL,
                                data = NULL,
                                stat = "prop",
                                position = position_diverging(
                                  vjust = 0.5,
                                  reverse = reverse,
                                  exclude_fill_values = exclude_fill_values,
                                  cutoff = cutoff
                                ),
                                ...,
                                complete = "fill",
                                default_by = "total",
                                height = "count",
                                labels = "count",
                                labeller = label_number_abs(),
                                reverse = FALSE,
                                exclude_fill_values = NULL,
                                cutoff = NULL) {

  args <- list(...)
  if (stat == "prop") {
    args$complete <- complete
    args$default_by <- default_by
    args$height <- height
    args$labels <- labels
    args$labeller <- labeller
  }

  args$mapping <- mapping
  args$data <- data
  args$stat <- stat
  args$position <- position
  do.call(ggplot2::geom_text, args)
}

#' @rdname geom_bar_diverging
#' @export
geom_text_likert <- function(mapping = NULL,
                             data = NULL,
                             stat = "prop",
                             position = position_likert(
                               vjust = 0.5,
                               reverse = reverse,
                               exclude_fill_values = exclude_fill_values,
                               cutoff = cutoff
                             ),
                             ...,
                             complete = "fill",
                             default_by = "x",
                             height = "prop",
                             labels = "prop",
                             labeller = label_percent_abs(accuracy = 1),
                             reverse = FALSE,
                             exclude_fill_values = NULL,
                             cutoff = NULL) {

  args <- c(as.list(environment()), list(...))
  do.call(geom_text_diverging, args)
}

#' @rdname geom_bar_diverging
#' @export
geom_text_pyramid <- function(mapping = NULL,
                              data = NULL,
                              stat = "prop",
                              position = position_diverging(
                                vjust = 0.5,
                                reverse = reverse,
                                exclude_fill_values = exclude_fill_values,
                                cutoff = cutoff
                              ),
                              ...,
                              complete = NULL,
                              default_by = "total",
                              height = "prop",
                              labels = "prop",
                              labeller = label_percent_abs(accuracy = 1),
                              reverse = FALSE,
                              exclude_fill_values = NULL,
                              cutoff = NULL) {

  args <- c(as.list(environment()), list(...))
  do.call(geom_text_diverging, args)
}
