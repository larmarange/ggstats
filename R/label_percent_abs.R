#' Label absolute values
#'
#' @param ... arguments passed to [scales::label_number()] or
#' [scales::label_percent()]
#' @seealso [scales::label_number()], [scales::label_percent()]
#' @export
#' @examples
#' x <- c(-0.2, -.05, 0, .25, .66)
#'
#' scales::label_number()(x)
#' label_number_abs()(x)
#'
#' scales::label_percent()(x)
#' label_percent_abs()(x)
#'
label_number_abs <- function(...) {
  function(x) {scales::label_number(...)(abs(x))}
}

#' @rdname label_number_abs
#' @export
label_percent_abs <- function(...) {
  function(x) {scales::label_percent(...)(abs(x))}
}
