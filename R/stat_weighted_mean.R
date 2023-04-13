#' Compute weighted y mean
#'
#' This statistic will compute the mean of **y** aesthetic for
#' each unique value of **x**, taking into account **weight**
#' aesthetic if provided.
#'
#' @section Computed variables:
#' \describe{
#'   \item{y}{weighted y (numerator / denominator)}
#'   \item{numerator}{numerator}
#'   \item{denominator}{denominator}
#' }
#'
#' @inheritParams ggplot2::stat_bin
#' @param geom Override the default connection with [ggplot2::geom_point()].
#' @seealso `vignette("stat_weighted_mean")`
#' @export
#' @return A `ggplot2` plot with the added statistic.
#' @examplesIf requireNamespace("reshape")
#' @examples
#' library(ggplot2)
#'
#' data(tips, package = "reshape")
#'
#' ggplot(tips) +
#'   aes(x = day, y = total_bill) +
#'   geom_point()
#'
#' ggplot(tips) +
#'   aes(x = day, y = total_bill) +
#'   stat_weighted_mean()
#'
#' \donttest{
#' ggplot(tips) +
#'   aes(x = day, y = total_bill, group = 1) +
#'   stat_weighted_mean(geom = "line")
#'
#' ggplot(tips) +
#'   aes(x = day, y = total_bill, colour = sex, group = sex) +
#'   stat_weighted_mean(geom = "line")
#'
#' ggplot(tips) +
#'   aes(x = day, y = total_bill, fill = sex) +
#'   stat_weighted_mean(geom = "bar", position = "dodge")
#'
#' # computing a proportion on the fly
#' if (requireNamespace("scales")) {
#'   ggplot(tips) +
#'     aes(x = day, y = as.integer(smoker == "Yes"), fill = sex) +
#'     stat_weighted_mean(geom = "bar", position = "dodge") +
#'     scale_y_continuous(labels = scales::percent)
#' }
#' }
#' @examples
#' library(ggplot2)
#'
#' # taking into account some weights
#' if (requireNamespace("scales")) {
#'   d <- as.data.frame(Titanic)
#'   ggplot(d) +
#'     aes(
#'       x = Class, y = as.integer(Survived == "Yes"),
#'       weight = Freq, fill = Sex
#'     ) +
#'     geom_bar(stat = "weighted_mean", position = "dodge") +
#'     scale_y_continuous(labels = scales::percent) +
#'     labs(y = "Survived")
#' }
stat_weighted_mean <- function(mapping = NULL,
                               data = NULL,
                               geom = "point",
                               position = "identity",
                               ...,
                               na.rm = FALSE,
                               orientation = NA,
                               show.legend = NA,
                               inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatWeightedMean,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#' @rdname stat_weighted_mean
#' @format NULL
#' @usage NULL
#' @export
StatWeightedMean <- ggplot2::ggproto(
  "StatSummary",
  ggplot2::Stat,
  required_aes = c("x", "y"),
  extra_params = c("na.rm", "orientation"),
  setup_params = function(data, params) {
    params$flipped_aes <- ggplot2::has_flipped_aes(data, params)
    params
  },
  compute_panel = function(data, scales, na.rm = FALSE, flipped_aes = FALSE) {
    data <- ggplot2::flip_data(data, flipped_aes)
    if (is.null(data$weight)) {
      data$weight <- rep(1, nrow(data))
    }

    summarised <- aggregate(
      cbind(numerator = y * weight, denominator = weight) ~ .,
      data,
      FUN = sum, na.rm = TRUE
    )
    summarised$y <- summarised$numerator / summarised$denominator

    summarised$flipped_aes <- flipped_aes
    ggplot2::flip_data(summarised, flipped_aes)
  }
)
