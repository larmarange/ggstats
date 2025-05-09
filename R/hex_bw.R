#' Identify a suitable font color (black or white) given a background HEX color
#'
#' You could use `auto_contrast` as a shortcut of
#' `aes(colour = after_scale(hex_bw(.data$fill)))`. You should use `!!!` to
#' inject it within [ggplot2::aes()] (see examples).
#'
#' @param hex_code Background color in hex-format.
#' @return Either black or white, in hex-format
#' @source Adapted from `saros` for `hex_code()` and from
#' <https://github.com/teunbrand/ggplot_tricks?tab=readme-ov-file#text-contrast>
#' for `auto_contrast`.
#' @export
#' @examples
#' hex_bw("#0dadfd")
#'
#' library(ggplot2)
#' ggplot(diamonds) +
#'   aes(x = cut, fill = color, label = after_stat(count)) +
#'   geom_bar() +
#'   geom_text(
#'     mapping = aes(color = after_scale(hex_bw(.data$fill))),
#'     position = position_stack(.5),
#'     stat = "count",
#'     size = 2
#'   )
#'
#' ggplot(diamonds) +
#'   aes(x = cut, fill = color, label = after_stat(count)) +
#'   geom_bar() +
#'   geom_text(
#'     mapping = auto_contrast,
#'     position = position_stack(.5),
#'     stat = "count",
#'     size = 2
#'   )
#'
#' ggplot(diamonds) +
#'   aes(x = cut, fill = color, label = after_stat(count), !!!auto_contrast) +
#'   geom_bar() +
#'   geom_text(
#'     mapping = auto_contrast,
#'     position = position_stack(.5),
#'     stat = "count",
#'     size = 2
#'   )
hex_bw <- function(hex_code) {
  rgb_conv <-
    lapply(
      grDevices::col2rgb(hex_code),
      FUN = function(.x) {
        ifelse(
          .x / 255 <= 0.04045,
          .x * 12.92 / 255,
          ((.x / 255 + 0.055) / 1.055)^2.4
        )
      }
    ) |>
    unlist() |>
    matrix(ncol = length(hex_code), byrow = FALSE) |>
    sweep(MARGIN = 1, STATS = c(0.2126, 0.7152, 0.0722), FUN = `*`) |>
    apply(MARGIN = 2, FUN = sum)

  bw <- ifelse(
    rgb_conv > 0.2, # 0.179 in the original code
    "#000000",
    "#ffffff"
  )

  bw[is.na(hex_code)] <- "#ffffff"
  bw
}

#' @rdname hex_bw
#' @description
#' `hex_bw_threshold()` is a variation of `hex_bw()`. For `values` below
#' `threshold`, black (`"#000000"`) will always be returned, regardless of
#' `hex_code`.
#' @export
#' @param values Values to be compared.
#' @param threshold Threshold.
hex_bw_threshold <- function(hex_code, values, threshold) {
  x <- hex_bw(hex_code)
  x[values < threshold] <- "#000000"
  x
}

#' @rdname hex_bw
#' @export
auto_contrast <- ggplot2::aes(colour = after_scale(hex_bw(.data$fill)))
