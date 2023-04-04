#' Compute cross-tabulation statistics
#'
#' Computes statistics of a 2-dimensional matrix using [broom::augment.htest].
#'
#' @inheritParams ggplot2::stat_identity
#' @param geom Override the default connection with
#' [ggplot2::geom_point()].
#' @param na.rm If `TRUE`, the default, missing values are
#' removed with a warning.
#'   If `TRUE`, missing values are silently removed.
#' @param keep.zero.cells If `TRUE`, cells with no observations are kept.
#' @section Aesthetics:
#' `stat_cross()` requires the **x** and the **y** aesthetics.
#' @section Computed variables:
#' \describe{
#'   \item{observed}{number of observations in x,y}
#'   \item{prop}{proportion of total}
#'   \item{row.prop}{row proportion}
#'   \item{col.prop}{column proportion}
#'   \item{expected}{expected count under the null hypothesis}
#'   \item{resid}{Pearson's residual}
#'   \item{std.resid}{standardized residual}
#'   \item{row.observed}{total number of observations within row}
#'   \item{col.observed}{total number of observations within column}
#'   \item{total.observed}{total number of observations within the table}
#'   \item{phi}{phi coefficients, see [augment_chisq_add_phi()]}
#' }
#'
#' @export
#' @return A `ggplot2` plot with the added statistic.
#' @seealso [vignette("stat_cross")]
#' @examples
#' library(ggplot2)
#' d <- as.data.frame(Titanic)
#'
#' # plot number of observations
#' ggplot(d) +
#'   aes(x = Class, y = Survived, weight = Freq, size = after_stat(observed)) +
#'   stat_cross() +
#'   scale_size_area(max_size = 20)
#'
#' # custom shape and fill colour based on chi-squared residuals
#' ggplot(d) +
#'   aes(
#'     x = Class, y = Survived, weight = Freq,
#'     size = after_stat(observed), fill = after_stat(std.resid)
#'   ) +
#'   stat_cross(shape = 22) +
#'   scale_fill_steps2(breaks = c(-3, -2, 2, 3), show.limits = TRUE) +
#'   scale_size_area(max_size = 20)
#'
#' # custom shape and fill colour based on phi coeffients
#' ggplot(d) +
#'   aes(
#'     x = Class, y = Survived, weight = Freq,
#'     size = after_stat(observed), fill = after_stat(phi)
#'   ) +
#'   stat_cross(shape = 22) +
#'   scale_fill_steps2(show.limits = TRUE) +
#'   scale_size_area(max_size = 20)
#'
#'
#' # plotting the number of observations as a table
#' ggplot(d) +
#'   aes(
#'     x = Class, y = Survived, weight = Freq, label = after_stat(observed)
#'   ) +
#'   geom_text(stat = "cross")
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
StatCross <- ggplot2::ggproto(
  "StatCross",
  ggplot2::Stat,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(weight = 1),
  setup_params = function(data, params) {
    params
  },
  extra_params = c("na.rm"),
  compute_panel = function(self, data, scales, keep.zero.cells = FALSE) {
    if (is.null(data$weight)) {
      data$weight <- rep(1, nrow(data))
    }

    # compute cross statistics
    panel <- augment_chisq_add_phi(
      chisq.test(xtabs(weight ~ y + x, data = data))
    )

    panel_names <- names(panel)
    for (to_name in c(
      "observed",
      "prop",
      "row.prop",
      "col.prop",
      "expected",
      "resid",
      "std.resid",
      "row.observed",
      "col.observed",
      "total.observed",
      "phi"
    )) {
      from_name <- paste0(".", to_name)
      panel_names[which(panel_names == from_name)] <- to_name
    }
    names(panel) <- panel_names

    # to handle the fact that ggplot2 could transform factors into integers
    # before computation of the statistic
    if (is.numeric(data$x)) panel$x <- as.numeric(panel$x)
    if (is.numeric(data$y)) panel$y <- as.numeric(panel$y)

    # keeping first value of other aesthetics in data
    panel <- merge(
      panel,
      dplyr::select(data, -dplyr::all_of("PANEL")),
      by = c("x", "y"),
      all.x = TRUE
    )

    panel <- panel %>% dplyr::distinct(.data$x, .data$y, .keep_all = TRUE)

    if (!keep.zero.cells) {
      panel <- panel[panel$observed != 0, ]
    }

    panel
  }
)

# Compute phi coefficients
# see psych::phi() and GDAtools::phi.table()
.compute_phi <-
  function(.prop, .row.observed, .col.observed, .total.observed) {
    rp <- .row.observed / .total.observed
    cp <- .col.observed / .total.observed
    (.prop - rp * cp) / sqrt(rp * (1 - rp) * cp * (1 - cp))
  }

#' Augment a chi-squared test and compute phi coefficients
#' @details
#' Phi coefficients are a measurement of the degree of association
#' between two binary variables.
#'
#' - A value between -1.0 to -0.7 indicates a strong negative association.
#' - A value between -0.7 to -0.3 indicates a weak negative association.
#' - A value between -0.3 to +0.3 indicates a little or no association.
#' - A value between +0.3 to +0.7 indicates a weak positive association.
#' - A value between +0.7 to +1.0 indicates a strong positive association.
#' @export
#' @param x a chi-squared test as returned by [stats::chisq.test()]
#' @return A `tibble`.
#' @seealso [stat_cross()], `GDAtools::phi.table()` or `psych::phi()`
#' @examples
#' tab <- xtabs(Freq ~ Sex + Class, data = as.data.frame(Titanic))
#' augment_chisq_add_phi(chisq.test(tab))
augment_chisq_add_phi <- function(x) {
  if (!inherits(x, "htest") && names(x$statistic) != "X-squared")
    cli::cli_abort(paste(
      "{.arg x} should be the result of a chi-squared test",
      "(see {.fn stats::chisq.test}).")
    )

  broom::augment(x) %>%
    dplyr::group_by(dplyr::across(1)) %>%
    dplyr::mutate(.row.observed = sum(.data$.observed)) %>%
    dplyr::group_by(dplyr::across(2)) %>%
    dplyr::mutate(.col.observed = sum(.data$.observed)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      .total.observed = sum(.data$.observed),
      .phi = .compute_phi(
        .data$.prop,
        .data$.row.observed,
        .data$.col.observed,
        .data$.total.observed
      )
    )
}
