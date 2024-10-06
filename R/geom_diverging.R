#' Geometries for diverging bar plots
#'
#' These geometries are variations of [ggplot2::geom_bar()] and
#' [ggplot2::geom_text()] but provides different set of default values.
#'
#' - `geom_diverging()` is designed for stacked diverging bar plots, using
#'   [position_diverging()].
#' - `geom_likert()` is designed for Likert-type items. Using
#'   [position_likert()] (each bar sums to 100%).
#' - `geom_pyramid()` is similar to `geom_diverging()` but uses
#'   proportions of the total instead of counts.
#'
#' To add labels on the bar plots, simply use `geom_diverging_text()`,
#' `geom_likert_text()`, or `geom_pyramid_text()`.
#'
#' All these geometries relies on [stat_prop()].
#'
#' @param mapping Optional set of aesthetic mappings.
#' @param data The data to be displayed in this layers.
#' @param position A position adjustment to use on the data for this layer.
#' @param ... Other arguments passed on to [`ggplot2::geom_bar()`]
#' @param complete An aesthetic for those unobserved values should be completed,
#' see [`stat_prop()`].
#' @param default_by Name of an aesthetic determining denominators by default,
#' see [`stat_prop()`].
#' @inheritParams position_likert
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(diamonds) +
#'   aes(x = clarity, fill = cut) +
#'   geom_diverging()
#'
#' ggplot(diamonds) +
#'   aes(x = clarity, fill = cut) +
#'   geom_diverging(position = position_diverging(cutoff = 4))
#'
#' ggplot(diamonds) +
#'   aes(y = clarity, fill = cut) +
#'   geom_likert() +
#'   geom_likert_text()
#'
#' ggplot(diamonds) +
#'   aes(y = clarity, fill = cut) +
#'   geom_likert() +
#'   geom_likert_text(
#'     aes(
#'       label = label_percent_abs(accuracy = 1, hide_below = .10)(
#'         after_stat(prop)
#'       ),
#'       colour = after_scale(hex_bw(.data$fill))
#'     )
#'   )
#'
#' d <- Titanic |> as.data.frame()
#'
#' ggplot(d) +
#'   aes(y = Class, fill = Sex, weight = Freq) +
#'   geom_diverging() +
#'   geom_diverging_text()
#'
#' ggplot(d) +
#'   aes(y = Class, fill = Sex, weight = Freq) +
#'   geom_pyramid() +
#'   geom_pyramid_text()
geom_diverging <- function(mapping = NULL,
                           data = NULL,
                           position = "diverging",
                           ...,
                           complete = "fill",
                           default_by = "total") {
  ggplot2::geom_bar(
    mapping = mapping,
    data = data,
    position = position,
    complete = complete,
    default_by = default_by,
    stat = StatProp
  )
}

#' @rdname geom_diverging
#' @export
geom_likert <- function(mapping = NULL,
                        data = NULL,
                        position = "likert",
                        ...,
                        complete = "fill",
                        default_by = "x") {
  new_default_aes <- ggplot2::aes(
    x = after_stat(prop),
    y = after_stat(prop)
  )
  CustomStat <- StatProp
  CustomStat$default_aes <- ggplot2::aes(
    !!!utils::modifyList(StatProp$default_aes, new_default_aes)
  )
  ggplot2::geom_bar(
    mapping = mapping,
    data = data,
    position = position,
    complete = complete,
    default_by = default_by,
    stat = CustomStat
  )
}

#' @rdname geom_diverging
#' @export
geom_pyramid <- function(mapping = NULL,
                         data = NULL,
                         position = "diverging",
                         ...,
                         complete = NULL,
                         default_by = "total") {
  new_default_aes <- ggplot2::aes(
    x = after_stat(prop),
    y = after_stat(prop)
  )
  CustomStat <- StatProp
  CustomStat$default_aes <- ggplot2::aes(
    !!!utils::modifyList(StatProp$default_aes, new_default_aes)
  )
  ggplot2::geom_bar(
    mapping = mapping,
    data = data,
    position = position,
    complete = complete,
    default_by = default_by,
    stat = CustomStat
  )
}

#' @rdname geom_diverging
#' @export
geom_diverging_text <- function(mapping = NULL,
                                data = NULL,
                                position = position_diverging(0.5),
                                ...,
                                complete = "fill",
                                default_by = "total") {
  new_default_aes <- ggplot2::aes(
    label = after_stat(count),
    colour = after_scale(hex_bw(.data$fill))
  )
  CustomStat <- StatProp
  CustomStat$default_aes <- ggplot2::aes(
    !!!utils::modifyList(StatProp$default_aes, new_default_aes)
  )
  ggplot2::geom_text(
    mapping = mapping,
    data = data,
    position = position,
    complete = complete,
    default_by = default_by,
    stat = CustomStat
  )
}

#' @rdname geom_diverging
#' @export
geom_likert_text <- function(mapping = NULL,
                             data = NULL,
                             position = position_likert(0.5),
                             ...,
                             complete = "fill",
                             default_by = "x") {
  new_default_aes <- ggplot2::aes(
    x = after_stat(prop),
    y = after_stat(prop),
    label = scales::percent(after_stat(prop), accuracy = 1),
    colour = after_scale(hex_bw(.data$fill))
  )
  CustomStat <- StatProp
  CustomStat$default_aes <- ggplot2::aes(
    !!!utils::modifyList(StatProp$default_aes, new_default_aes)
  )
  ggplot2::geom_text(
    mapping = mapping,
    data = data,
    position = position,
    complete = complete,
    default_by = default_by,
    stat = CustomStat
  )
}

#' @rdname geom_diverging
#' @export
geom_pyramid_text <- function(mapping = NULL,
                              data = NULL,
                              position = position_diverging(0.5),
                              ...,
                              complete = NULL,
                              default_by = "total") {
  new_default_aes <- ggplot2::aes(
    x = after_stat(prop),
    y = after_stat(prop),
    label = scales::percent(after_stat(prop), accuracy = 1),
    colour = after_scale(hex_bw(.data$fill))
  )
  CustomStat <- StatProp
  CustomStat$default_aes <- ggplot2::aes(
    !!!utils::modifyList(StatProp$default_aes, new_default_aes)
  )
  ggplot2::geom_text(
    mapping = mapping,
    data = data,
    position = position,
    complete = complete,
    default_by = default_by,
    stat = CustomStat
  )
}
