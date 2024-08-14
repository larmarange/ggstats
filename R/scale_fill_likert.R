#' Colour scale for Likert-type plots
#'
#' This scale is similar to other diverging discrete colour scales, but allows
#' to change the "center" of the scale using `cutoff` argument, as used by
#' [position_likert()].
#'
#' @param name The name of the scale. Used as the axis or legend title.
#' If `waiver()`, the default, the name of the scale is taken from the first
#' mapping used for that aesthetic. If `NULL`, the legend title will be omitted.
#' @param ... Other arguments passed on to `discrete_scale()` to control name,
#' limits, breaks, labels and so forth.
#' @param pal A palette function taking a number of colours as entry and
#' returning a list of colours (see examples), ideally a diverging palette
#' @param cutoff Number of categories displayed negatively (see
#' [position_likert()]) and therefore changing the center of the colour scale
#' (see examples).
#' @param aesthetics Character string or vector of character strings listing
#' the name(s) of the aesthetic(s) that this scale works with. This can be
#' useful, for example, to apply colour settings to the colour and fill
#' aesthetics at the same time, via `aesthetics = c("colour", "fill")`.
#' @examples
#' library(ggplot2)
#' ggplot(diamonds) +
#'   aes(y = clarity, fill = cut) +
#'   geom_bar(position = "likert") +
#'   scale_x_continuous(label = label_percent_abs()) +
#'   xlab("proportion")
#'
#' ggplot(diamonds) +
#'   aes(y = clarity, fill = cut) +
#'   geom_bar(position = "likert") +
#'   scale_x_continuous(label = label_percent_abs()) +
#'   xlab("proportion") +
#'   scale_fill_likert()
#'
#'  ggplot(diamonds) +
#'   aes(y = clarity, fill = cut) +
#'   geom_bar(position = position_likert(cutoff = 1)) +
#'   scale_x_continuous(label = label_percent_abs()) +
#'   xlab("proportion") +
#'   scale_fill_likert(cutoff = 1)
#' @export
scale_fill_likert <- function(name = waiver(), ...,
                              pal = scales::brewer_pal(palette = "BrBG"),
                              cutoff = NULL,
                              aesthetics = "fill") {
  ggplot2::discrete_scale(
    aesthetics,
    name = name,
    palette = likert_pal(pal = pal, cutoff = cutoff),
    ...
  )
}

#' @rdname scale_fill_likert
#' @export
likert_pal <- function(pal = scales::brewer_pal(palette = "BrBG"),
                       cutoff = NULL) {
  function(n) {
    if (is.null(cutoff)) cutoff <- n / 2
    if (cutoff < 0)
      cli::cli_abort("{.arg cutoff} should be positive.")
    if (cutoff > n)
      cli::cli_abort(
        "{.arg cutoff} higher than the number of requested colours."
      )
    left <- floor(cutoff)
    center <- cutoff %% 1 > 0
    right <- n - ceiling(cutoff)
    nc <- 2 * max(left, right) + center # needed colors
    cols <- pal_extender(pal = pal)(nc)
    if (left <= right) {
      cols[(nc - n + 1):nc]
    } else {
      cols[1:n]
    }
  }
}
