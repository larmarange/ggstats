#' Weighted Sum
#'
#' @param x a numeric vector of values
#' @param w a numeric vector of weights
#' @param na.rm a logical indicating whether to ignore `NA` values
#' @returns A numeric vector.
#' @export
#' @examples
#' x <- 1:20
#' w <- runif(20)
#' weighted.sum(x, w)
weighted.sum <- function(x, w, na.rm = TRUE) {
  sum(x * w, na.rm = na.rm)
}
