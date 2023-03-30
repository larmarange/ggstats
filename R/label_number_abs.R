#' Label absolute values
#'
#' @param ... arguments passed to [scales::label_number()] or
#' [scales::label_percent()]
#' @param hide_below if provided, values below `hide_below` will be masked
#' (i.e. an empty string `""` will be returned)
#' @seealso [scales::label_number()], [scales::label_percent()]
#' @export
#' @examples
#' x <- c(-0.2, -.05, 0, .07, .25, .66)
#'
#' scales::label_number()(x)
#' label_number_abs()(x)
#'
#' scales::label_percent()(x)
#' label_percent_abs()(x)
#' label_percent_abs(hide_below = .1)(x)
label_number_abs <- function(..., hide_below = NULL) {
  function(x) {
    res <- scales::label_number(...)(abs(x))
    if (!is.null(hide_below))
      res[abs(x) < hide_below] <- ""
    res
  }
}

#' @rdname label_number_abs
#' @export
label_percent_abs <- function(..., hide_below = NULL) {
  function(x) {
    res <- scales::label_percent(...)(abs(x))
    if (!is.null(hide_below))
      res[abs(x) < hide_below] <- ""
    res
  }
}

