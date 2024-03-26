#' Identify a suitable font color (black or white) given a background HEX color
#'
#' @param hex_code Background color in hex-format.
#' @return Either black or white, in hex-format
#' @source Adapted from `saros`
#' @export
#' @examples
#' hex_bw("#0dadfd")
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
    ) %>%
    unlist() %>%
    matrix(ncol = length(hex_code), byrow = FALSE) %>%
    sweep(MARGIN = 1, STATS = c(0.2126, 0.7152, 0.0722), FUN = `*`) %>%
    apply(MARGIN = 2, FUN = sum)

  bw <- ifelse(
    rgb_conv > 0.2, # 0.179 in the original code
    "#000000",
    "#ffffff"
  )

  bw[is.na(hex_code)] <- "#ffffff"
  bw
}
