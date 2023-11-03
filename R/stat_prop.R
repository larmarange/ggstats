#' Compute proportions according to custom denominator
#'
#' `stat_prop()` is a variation of [ggplot2::stat_count()] allowing to
#' compute custom proportions according to the **by** aesthetic defining
#' the denominator (i.e. all proportions for a same value of **by** will
#' sum to 1). The **by** aesthetic should be a factor. If **by** is not
#' specified, proportions of the total will be computed.
#'
#' @inheritParams ggplot2::stat_count
#' @param geom Override the default connection with [ggplot2::geom_bar()].
#' @param complete Name (character) of an aesthetic for those statistics should
#'   be completed for unobserved values (see example)
#' @section Aesthetics:
#' `stat_prop()` understands the following aesthetics
#' (required aesthetics are in bold):
#'
#' - **x *or* y**
#' - by (this aesthetic should be a **factor**)
#' - group
#' - weight
#' @section Computed variables:
#' \describe{
#'   \item{count}{number of points in bin}
#'   \item{prop}{computed proportion}
#' }
#' @seealso `vignette("stat_prop")`, [ggplot2::stat_count()]. For an alternative
#' approach, see
#' <https://github.com/tidyverse/ggplot2/issues/5505#issuecomment-1791324008>.
#'
#' @import ggplot2
#' @return A `ggplot2` plot with the added statistic.
#' @export
#' @examples
#' library(ggplot2)
#' d <- as.data.frame(Titanic)
#'
#' p <- ggplot(d) +
#'   aes(x = Class, fill = Survived, weight = Freq, by = Class) +
#'   geom_bar(position = "fill") +
#'   geom_text(stat = "prop", position = position_fill(.5))
#' p
#' p + facet_grid(~Sex)
#'
#' ggplot(d) +
#'   aes(x = Class, fill = Survived, weight = Freq) +
#'   geom_bar(position = "dodge") +
#'   geom_text(
#'     aes(by = Survived),
#'     stat = "prop",
#'     position = position_dodge(0.9), vjust = "bottom"
#'   )
#' \donttest{
#' if (requireNamespace("scales")) {
#'   ggplot(d) +
#'     aes(x = Class, fill = Survived, weight = Freq, by = 1) +
#'     geom_bar() +
#'     geom_text(
#'       aes(label = scales::percent(after_stat(prop), accuracy = 1)),
#'       stat = "prop",
#'       position = position_stack(.5)
#'     )
#' }
#'
#' # displaying unobserved levels with complete
#' d <- diamonds %>%
#'   dplyr::filter(!(cut == "Ideal" & clarity == "I1")) %>%
#'   dplyr::filter(!(cut == "Very Good" & clarity == "VS2")) %>%
#'   dplyr::filter(!(cut == "Premium" & clarity == "IF"))
#' p <- ggplot(d) +
#'   aes(x = clarity, fill = cut, by = clarity) +
#'   geom_bar(position = "fill")
#' p + geom_text(stat = "prop", position = position_fill(.5))
#' p + geom_text(stat = "prop", position = position_fill(.5), complete = "fill")
#' }
stat_prop <- function(mapping = NULL,
                      data = NULL,
                      geom = "bar",
                      position = "fill",
                      ...,
                      width = NULL,
                      na.rm = FALSE,
                      orientation = NA,
                      show.legend = NA,
                      inherit.aes = TRUE,
                      complete = NULL) {
  params <- list(
    na.rm = na.rm,
    orientation = orientation,
    width = width,
    complete = complete,
    ...
  )
  if (!is.null(params$y)) {
    cli::cli_abort(
      "{.fn stat_prop} must not be used with a {.arg y} aesthetic.",
      call. = FALSE
    )
  }

  layer(
    data = data,
    mapping = mapping,
    stat = StatProp,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

#' @rdname stat_prop
#' @format NULL
#' @usage NULL
#' @export
StatProp <- ggplot2::ggproto("StatProp", ggplot2::Stat,
  required_aes = c("x|y"),
  default_aes = ggplot2::aes(
    x = after_stat(count), y = after_stat(count), weight = 1,
    label = scales::percent(after_stat(prop), accuracy = .1),
    by = 1
  ),
  setup_params = function(data, params) {
    params$flipped_aes <- ggplot2::has_flipped_aes(
      data,
      params,
      main_is_orthogonal = FALSE
    )

    has_x <- !(is.null(data$x) && is.null(params$x))
    has_y <- !(is.null(data$y) && is.null(params$y))
    if (!has_x && !has_y) {
      cli::cli_abort(
        "{.fn stat_prop} requires an {.arg x} or {.arg y} aesthetic.",
        call. = FALSE
      )
    }
    if (has_x && has_y) {
      cli::cli_abort(
        "{.fn stat_prop} can only have an {.arg x} or an {.arg y} aesthetic.",
        call. = FALSE
      )
    }
    # there is an unresolved bug when by is a character vector. To be explored.
    if (is.character(data$by)) {
      cli::cli_abort(
        "The {.arg by} aesthetic should be a factor instead of a character.",
        call. = FALSE
      )
    }
    params
  },
  extra_params = c("na.rm"),
  compute_panel = function(self, data, scales,
                           width = NULL, flipped_aes = FALSE, complete = NULL) {
    data <- ggplot2::flip_data(data, flipped_aes)
    data$weight <- data$weight %||% rep(1, nrow(data))
    data$by <- data$by %||% rep(1, nrow(data))
    width <- width %||% (ggplot2::resolution(data$x) * 0.9)

    # sum weights for each combination of by and aesthetics
    # the use of . allows to consider all aesthetics defined in data
    panel <- stats::aggregate(weight ~ ., data = data, sum, na.rm = TRUE)

    names(panel)[which(names(panel) == "weight")] <- "count"
    panel$count[is.na(panel$count)] <- 0

    if (!is.null(complete)) {
      panel <- panel %>% dplyr::select(-dplyr::all_of("group"))
      cols <- names(panel)
      cols <- cols[!cols %in% c("count", complete)]

      panel <- panel %>%
        tidyr::complete(
          tidyr::nesting(!!!syms(cols)),
          .data[[complete]],
          fill = list(count = 0)
        ) %>%
        dplyr::mutate(group = seq_len(dplyr::n()))
    }

    # compute proportions by by
    sum_abs <- function(x) {
      sum(abs(x))
    }
    panel$prop <- panel$count / ave(panel$count, panel$by, FUN = sum_abs)
    panel$width <- width
    panel$flipped_aes <- flipped_aes

    ggplot2::flip_data(panel, flipped_aes)
  }
)
