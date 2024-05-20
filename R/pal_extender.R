#' Extend a discrete colour palette
#'
#' If the palette returns less colours than requested, the list of colours
#' will be expanded using [scales::pal_gradient_n()]. To be used with a
#' sequential or diverging palette. Not relevant for qualitative palettes.
#'
#' @param pal A palette function, such as returned by [scales::brewer_pal],
#' taking a number of colours as entry and returning a list of colours.
#' @return A palette function.
#' @export
#' @examples
#' pal <- scales::pal_brewer(palette = "PiYG")
#' scales::show_col(pal(16))
#' scales::show_col(pal_extender(pal)(16))
pal_extender <- function(pal = scales::brewer_pal(palette = "BrBG")) {
  function(n) {
    cols <- suppressWarnings(
      stats::na.omit(pal(n))
    )
    if (length(cols) <= n) {
      cols <- scales::pal_gradient_n(cols)(seq(0, 1, length.out = n))
    }
    cols
  }
}

#' @rdname pal_extender
#' @param name The name of the scale. Used as the axis or legend title.
#' If `waiver()`, the default, the name of the scale is taken from the first
#' mapping used for that aesthetic. If `NULL`, the legend title will be omitted.
#' @param ... Other arguments passed on to `discrete_scale()` to control name,
#' limits, breaks, labels and so forth.
#' @param aesthetics Character string or vector of character strings listing
#' the name(s) of the aesthetic(s) that this scale works with. This can be
#' useful, for example, to apply colour settings to the colour and fill
#' aesthetics at the same time, via `aesthetics = c("colour", "fill")`.
#' @export
scale_fill_extended <- function(name = waiver(), ...,
                                pal = scales::brewer_pal(palette = "BrBG"),
                                aesthetics = "fill") {
  ggplot2::discrete_scale(
    aesthetics,
    name = name,
    palette = pal_extender(pal = pal),
    ...
  )
}

#' @rdname pal_extender
#' @export
scale_colour_extended <- function(name = waiver(), ...,
                                  pal = scales::brewer_pal(palette = "BrBG"),
                                  aesthetics = "colour") {
  ggplot2::discrete_scale(
    aesthetics,
    name = name,
    palette = pal_extender(pal = pal),
    ...
  )
}
