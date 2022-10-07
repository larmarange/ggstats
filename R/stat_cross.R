#' Compute cross-tabulation statistics
#'
#' Computes statistics of a 2-dimensional matrix using \code{\link[broom]{augment.htest}}
#' from \pkg{broom}.
#'
#' @inheritParams ggplot2::stat_identity
#' @param geom Override the default connection between \code{\link[ggplot2]{geom_point}}
#'   and \code{stat_prop}.
#' @param na.rm If \code{TRUE}, the default, missing values are removed with a warning.
#'   If `TRUE`, missing values are silently removed.
#' @param keep.zero.cells If \code{TRUE}, cells with no observations are kept.
#' @section Aesthetics:
#' \code{stat_prop} requires the \strong{x} and the \strong{y} aesthetics.
#' @section Computed variables:
#' \describe{
#'   \item{observed}{number of observations in x,y}
#'   \item{prop}{proportion of total}
#'   \item{row.prop}{row proportion}
#'   \item{col.prop}{column proportion}
#'   \item{expected}{expected count under the null hypothesis}
#'   \item{resid}{Pearson's residual}
#'   \item{std.resid}{standardized residual}
#' }
#'
#' @export
#' @examplesIf interactive()
#' library(ggplot2)
#' d <- as.data.frame(Titanic)
#'
#' # plot number of observations
#' ggplot(d) +
#'  aes(x = Class, y = Survived, weight = Freq, size = after_stat(observed)) +
#'  stat_cross() +
#'  scale_size_area(max_size = 20)
#'
#' # custom shape and fill colour based on chi-squared residuals
#' ggplot(d) +
#'  aes(
#'    x = Class, y = Survived, weight = Freq,
#'    size = after_stat(observed), fill = after_stat(std.resid)
#'  ) +
#'  stat_cross(shape = 22) +
#'  scale_fill_steps2(breaks = c(-3, -2, 2, 3), show.limits = TRUE) +
#'  scale_size_area(max_size = 20)
#'
#' # plotting the number of observations as a table
#' ggplot(d) +
#'  aes(
#'    x = Class, y = Survived, weight = Freq, label = after_stat(observed)
#'  ) +
#'  geom_text(stat = "cross")
#'
#' # Row proportions with standardized residuals
#' ggplot(d) +
#'   aes(
#'     x = Class, y = Survived, weight = Freq,
#'     label = scales::percent(after_stat(row.prop)),
#'     size = NULL, fill = after_stat(std.resid)
#'   ) +
#'   stat_cross(shape = 22, size = 30) +
#'   geom_text(stat = "cross") +
#'   scale_fill_steps2(breaks = c(-3, -2, 2, 3), show.limits = TRUE) +
#'   facet_grid(Sex ~ .) +
#'   labs(fill = "Standardized residuals") +
#'   theme_minimal()
#'
#' # can work with continuous or character variables
#' data(tips, package = "reshape")
#' ggplot(tips) +
#'   aes(x = tip, y = as.character(day), size = after_stat(observed)) +
#'   stat_cross(alpha = .1, color = "blue") +
#'   scale_size_area(max_size = 12)
stat_cross <- function(mapping = NULL, data = NULL,
                       geom = "point", position = "identity",
                       ...,
                       na.rm = TRUE,
                       show.legend = NA,
                       inherit.aes = TRUE,
                       keep.zero.cells = FALSE) {

  params <- list(
    na.rm = na.rm,
    keep.zero.cells = keep.zero.cells,
    ...
  )

  layer(
    data = data,
    mapping = mapping,
    stat = StatCross,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

#' @rdname stat_cross
#' @format NULL
#' @usage NULL
#' @export
StatCross <- ggplot2::ggproto("StatCross", ggplot2::Stat,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(
    weight = 1
  ),

  setup_params = function(data, params) {
    params
  },

  extra_params = c("na.rm"),

  compute_panel = function(self, data, scales, keep.zero.cells = FALSE) {
    if (is.null(data$weight))
      data$weight <- rep(1, nrow(data))

    # compute cross statistics
    panel <- broom::augment(chisq.test(xtabs(weight ~ y + x, data = data)))

    panel_names <- names(panel)
    for (to_name in c(
      "observed",
      "prop",
      "row.prop",
      "col.prop",
      "expected",
      "resid",
      "std.resid"
    )) {
      from_name <- paste0(".", to_name)
      panel_names[which(panel_names == from_name)] <- to_name
    }
    names(panel) <- panel_names

    # to handle the fact that ggplot2 could transform factors into integers
    # before computation of the statistic
    if(is.numeric(data$x)) panel$x <- as.numeric(panel$x)
    if(is.numeric(data$y)) panel$y <- as.numeric(panel$y)

    # keeping first value of other aesthetics in data
    panel <- merge(
      panel,
      dplyr::select(data, -.data$PANEL),
      by = c("x", "y"),
      all.x = TRUE
    )

    panel <- panel %>% dplyr::distinct(.data$x, .data$y, .keep_all = TRUE)

    if (!keep.zero.cells) {
      panel <- panel[panel$observed != 0,]
    }

    panel
  }
)
