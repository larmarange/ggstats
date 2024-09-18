#' Stack objects on top of each another and center them around 0
#'
#' `position_diverging()` stacks bars on top of each other and
#' center them around zero (the same number of categories are displayed on
#' each side).
#' `position_likert()` uses proportions instead of counts. This type of
#' presentation is commonly used to display Likert-type scales.
#'
#'
#' It is recommended to use `position_likert()` with `stat_prop()`
#' and its `complete` argument (see examples).
#'
#' @param vjust Vertical adjustment for geoms that have a position
#'   (like points or lines), not a dimension (like bars or areas). Set to
#'   `0` to align with the bottom, `0.5` for the middle,
#'   and `1` (the default) for the top.
#' @param reverse If `TRUE`, will reverse the default stacking order.
#'   This is useful if you're rotating both the plot and legend.
#' @param exclude_fill_values Vector of values from the variable associated with
#'   the `fill` aesthetic that should not be displayed (but still taken into
#'   account for computing proportions)
#' @param cutoff number of categories to be displayed negatively (i.e. on the
#' left of the x axis or the bottom of the y axis), could be a decimal value:
#' `2` to display negatively the two first categories, `2.5` to display
#' negatively the two first categories and half of the third, `2.2` to display
#' negatively the two first categories and a fifth of the third (see examples).
#' By default (`NULL`), it will be equal to the number of categories divided
#' by 2, i.e. it will be centered.
#' @seealso See [ggplot2::position_stack()] and [ggplot2::position_fill()]
#' @export
#' @examples
#' library(ggplot2)
#'
#' ggplot(diamonds) +
#'   aes(y = clarity, fill = cut) +
#'   geom_bar(position = "fill") +
#'   scale_x_continuous(label = scales::label_percent()) +
#'   xlab("proportion")
#'
#' ggplot(diamonds) +
#'   aes(y = clarity, fill = cut) +
#'   geom_bar(position = "likert") +
#'   scale_x_continuous(label = label_percent_abs()) +
#'   scale_fill_likert() +
#'   xlab("proportion")
#'
#' ggplot(diamonds) +
#'   aes(y = clarity, fill = cut) +
#'   geom_bar(position = "stack") +
#'   scale_fill_likert(pal = scales::brewer_pal(palette = "PiYG"))
#'
#' ggplot(diamonds) +
#'   aes(y = clarity, fill = cut) +
#'   geom_bar(position = "diverging") +
#'   scale_x_continuous(label = label_number_abs()) +
#'   scale_fill_likert()
#'
#' \donttest{
#' # Reverse order -------------------------------------------------------------
#'
#' ggplot(diamonds) +
#'   aes(y = clarity, fill = cut) +
#'   geom_bar(position = position_likert(reverse = TRUE)) +
#'   scale_x_continuous(label = label_percent_abs()) +
#'   scale_fill_likert() +
#'   xlab("proportion")
#'
#' # Custom center -------------------------------------------------------------
#'
#' ggplot(diamonds) +
#'   aes(y = clarity, fill = cut) +
#'   geom_bar(position = position_likert(cutoff = 1)) +
#'   scale_x_continuous(label = label_percent_abs()) +
#'   scale_fill_likert(cutoff = 1) +
#'   xlab("proportion")
#'
#' ggplot(diamonds) +
#'   aes(y = clarity, fill = cut) +
#'   geom_bar(position = position_likert(cutoff = 3.75)) +
#'   scale_x_continuous(label = label_percent_abs()) +
#'   scale_fill_likert(cutoff = 3.75) +
#'   xlab("proportion")
#'
#' # Missing items -------------------------------------------------------------
#' # example with a level not being observed for a specific value of y
#' d <- diamonds
#' d <- d[!(d$cut == "Premium" & d$clarity == "I1"), ]
#' d <- d[!(d$cut %in% c("Fair", "Good") & d$clarity == "SI2"), ]
#'
#' # by default, the two lowest bar are not properly centered
#' ggplot(d) +
#'   aes(y = clarity, fill = cut) +
#'   geom_bar(position = "likert") +
#'   scale_fill_likert()
#'
#' # use stat_prop() with `complete = "fill"` to fix it
#' ggplot(d) +
#'   aes(y = clarity, fill = cut) +
#'   geom_bar(position = "likert", stat = "prop", complete = "fill") +
#'   scale_fill_likert()
#'
#' # Add labels ----------------------------------------------------------------
#'
#' custom_label <- function(x) {
#'   p <- scales::percent(x, accuracy = 1)
#'   p[x < .075] <- ""
#'   p
#' }
#'
#' ggplot(diamonds) +
#'   aes(y = clarity, fill = cut) +
#'   geom_bar(position = "likert") +
#'   geom_text(
#'     aes(by = clarity, label = custom_label(after_stat(prop))),
#'     stat = "prop",
#'     position = position_likert(vjust = .5)
#'   ) +
#'   scale_x_continuous(label = label_percent_abs()) +
#'   scale_fill_likert() +
#'   xlab("proportion")
#'
#' # Do not display specific fill values ---------------------------------------
#' # (but taken into account to compute proportions)
#'
#' ggplot(diamonds) +
#'   aes(y = clarity, fill = cut) +
#'   geom_bar(position = position_likert(exclude_fill_values = "Very Good")) +
#'   scale_x_continuous(label = label_percent_abs()) +
#'   scale_fill_likert() +
#'   xlab("proportion")
#' }
position_likert <- function(vjust = 1,
                            reverse = FALSE,
                            exclude_fill_values = NULL,
                            cutoff = NULL) {
  ggplot2::ggproto(
    NULL,
    PositionLikert,
    vjust = vjust,
    reverse = reverse,
    exclude_fill_values = exclude_fill_values,
    cutoff = cutoff
  )
}

#' @export
#' @rdname position_likert
position_diverging <- function(vjust = 1,
                               reverse = FALSE,
                               exclude_fill_values = NULL,
                               cutoff = NULL) {
  ggplot2::ggproto(
    NULL,
    PositionDiverging,
    vjust = vjust,
    reverse = reverse,
    exclude_fill_values = exclude_fill_values,
    cutoff = cutoff
  )
}

#' @rdname position_likert
#' @format NULL
#' @usage NULL
#' @export
PositionLikert <- ggplot2::ggproto("PositionLikert", Position,
  type = NULL,
  vjust = 1,
  fill = TRUE,
  exclude_fill_values = NULL,
  cutoff = NULL,
  reverse = FALSE,
  setup_params = function(self, data) {
    flipped_aes <- ggplot2::has_flipped_aes(data)
    data <- ggplot2::flip_data(data, flipped_aes)
    list(
      var = self$var %||% likert_var(data),
      fill = self$fill,
      vjust = self$vjust,
      reverse = self$reverse,
      exclude_fill_values = self$exclude_fill_values,
      cutoff = self$cutoff,
      flipped_aes = flipped_aes
    )
  },
  setup_data = function(self, data, params) {
    data <- ggplot2::flip_data(data, params$flipped_aes)
    if (is.null(params$var)) {
      return(data)
    }

    if (!"ymin" %in% names(data)) data$ymin <- 0
    data$ymax <- switch(params$var,
      y = data$y,
      ymax = as.numeric(ifelse(data$ymax == 0, data$ymin, data$ymax))
    )

    data <- ggplot2::remove_missing(
      data,
      vars = c("x", "xmin", "xmax", "y"),
      name = "position_likert"
    )
    ggplot2::flip_data(data, params$flipped_aes)
  },
  compute_panel = function(data, params, scales) {
    data <- ggplot2::flip_data(data, params$flipped_aes)
    if (is.null(params$var)) {
      return(data)
    }

    negative <- data$ymax < 0
    negative[is.na(negative)] <- FALSE

    if (any(negative)) {
      cli::cli_abort("{.fn position_liker} does not work with negative values")
    }

    data <- data %>%
      tidyr::nest(.by = "x", .key = "d") %>%
      dplyr::mutate(
        d = purrr::map(
          .data$d,
          function(x) {
            pos_likert(
              x,
              vjust = params$vjust,
              fill = params$fill,
              reverse = params$reverse,
              exclude_fill_values = params$exclude_fill_values,
              cutoff = params$cutoff
            )
          }
        )
      ) %>%
      tidyr::unnest(cols = "d")

    ggplot2::flip_data(data, params$flipped_aes)
  }
)

pos_likert <- function(df,
                       vjust = 1,
                       fill = FALSE,
                       reverse = FALSE,
                       exclude_fill_values = NULL,
                       cutoff = NULL) {
  if (reverse) {
    df <- df[nrow(df):1, ] # nolint
  }

  if (fill) {
    df$y <- df$y / sum(abs(df$y), na.rm = TRUE)
  }

  # Values to be excluded after computation of proportions
  if (!is.null(exclude_fill_values) && "fill" %in% names(df)) {
    exclude <- df$fill %in% exclude_fill_values
    df <- df[!exclude, ]
  }

  n <- nrow(df) + 1
  y <- ifelse(is.na(df$y), 0, df$y)
  heights <- c(0, cumsum(y))

  df$ymin <- pmin(heights[-n], heights[-1])
  df$ymax <- pmax(heights[-n], heights[-1])
  df$y <- (1 - vjust) * df$ymin + vjust * df$ymax

  # Now, we have to center the results
  if (is.null(cutoff))
    cutoff <- nrow(df) / 2
  if (cutoff < 0)
    cli::cli_abort("{.arg cutoff} cannot be negative.")
  if (cutoff > nrow(df))
    cli::cli_abort(
      "{.arg cutoff} cannot be higher than the number of categories."
    )
  if (cutoff == nrow(df)) {
    y_adjust <- df$ymax[cutoff]
  } else if (cutoff < 1) {
    y_adjust <- cutoff * df$ymax[1]
  } else {
    y_adjust <-
      df$ymax[cutoff %/% 1] +
      cutoff %% 1 * (df$ymax[cutoff %/% 1 + 1] - df$ymax[cutoff %/% 1])
  }

  df$y <- df$y - y_adjust
  df$ymin <- df$ymin - y_adjust
  df$ymax <- df$ymax - y_adjust

  df
}

#' @rdname position_likert
#' @format NULL
#' @usage NULL
#' @export
PositionDiverging <- ggproto("PositionDiverging", PositionLikert,
  fill = FALSE
)

likert_var <- function(data) {
  if (!is.null(data$ymax)) {
    "ymax"
  } else if (!is.null(data$y)) {
    "y"
  } else {
    cli::cli_warn(c(
      "Stacking requires either the {.field ymin} {.emph and} {.field ymin}",
      "or the {.field y} aesthetics",
      "i" = "Maybe you want {.code position = \"identity\"}?"
    ))
    NULL
  }
}
