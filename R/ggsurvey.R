#' Easy ggplot2 with survey objects
#'
#' A function to facilitate `ggplot2` graphs using a survey object.
#' It will initiate a ggplot and map survey weights to the
#' corresponding aesthetic.
#'
#' Graphs will be correct as long as only weights are required
#' to compute the graph. However, statistic or geometry requiring
#' correct variance computation (like [ggplot2::geom_smooth()]) will
#' be statistically incorrect.
#'
#' @param design A survey design object, usually created with
#' [survey::svydesign()]
#' @param mapping Default list of aesthetic mappings to use for plot,
#' to be created with [ggplot2::aes()].
#' @param ... Other arguments passed on to methods. Not currently used.
#' @importFrom stats weights
#' @return A `ggplot2` plot.
#' @export
#' @examplesIf requireNamespace("survey")
#' data(api, package = "survey")
#' dstrat <- survey::svydesign(
#'   id = ~1, strata = ~stype,
#'   weights = ~pw, data = apistrat,
#'   fpc = ~fpc
#' )
#' ggsurvey(dstrat) +
#'   ggplot2::aes(x = cnum, y = dnum) +
#'   ggplot2::geom_count()
#'
#' d <- as.data.frame(Titanic)
#' dw <- survey::svydesign(ids = ~1, weights = ~Freq, data = d)
#' ggsurvey(dw) +
#'   ggplot2::aes(x = Class, fill = Survived) +
#'   ggplot2::geom_bar(position = "fill")
ggsurvey <- function(design = NULL, mapping = NULL, ...) {
  if (!inherits(design, "survey.design")) {
    cli::cli_abort("{.var design} should be a {.cls survey.design} object.")
  }
  rlang::check_installed("survey")
  data <- design$variables
  data$.weights <- weights(design)

  if (is.null(mapping)) {
    mapping <- ggplot2::aes()
  }

  mapping$weight <- ggplot2::aes(weight = .data[[".weights"]])$weight

  ggplot2::ggplot(data, mapping, ...)
}
