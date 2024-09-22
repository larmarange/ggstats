#' Cascade plot
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param .data A data frame, or data frame extension (e.g. a tibble). For
#'   `plot_cascade()`, the variable displayed on the x-axis should be named
#'   `"x"` and the number of observations should be named `"n"`, like the
#'   tibble returned by `compute_cascade()`.
#' @param ... <[`data-masking`][rlang::args_data_masking]> Name-value pairs of
#'   conditions defining the different statuses to be plotted (see examples).
#' @param .weights <[`tidy-select`][dplyr::dplyr_tidy_select]> Optional weights.
#'   Should select only one variable.
#' @param .by <[`tidy-select`][dplyr::dplyr_tidy_select]> A variable or a set
#'   of variables to group by the computation of the cascade, and to generate
#'   facets. To select several variables, use [dplyr::pick()] (see examples).
#' @param .nrow,.ncol Number of rows and columns, for faceted plots.
#' @param .add_n Display the number of observations?
#' @param .text_size Size of the labels, passed to [ggplot2::geom_text()].
#' @param .arrows Display arrows between statuses?
#' @details
#'  `ggcascade()` calls `compute_cascade()` to generate a data set passed
#'   to `plot_cascade()`. Use `compute_cascade()` and `plot_cascade()` for
#'   more controls.
#' @return A `ggplot2` plot or a `tibble`.
#' @examples
#' ggplot2::diamonds |>
#'   ggcascade(
#'     all = TRUE,
#'     big = carat > .5,
#'     "big & ideal" = carat > .5 & cut == "Ideal"
#'   )
#'
#' ggplot2::mpg |>
#'   ggcascade(
#'     all = TRUE,
#'     recent = year > 2000,
#'     "recent & economic" = year > 2000 & displ < 3,
#'     .by = cyl,
#'     .ncol = 3,
#'     .arrows = FALSE,
#'     .text_size = 3
#'   )
#'
#' ggplot2::mpg |>
#'   ggcascade(
#'     all = TRUE,
#'     recent = year > 2000,
#'     "recent & economic" = year > 2000 & displ < 3,
#'     .by = pick(cyl, drv),
#'     .add_n = FALSE,
#'     .text_size = 2
#'   )
#' @export
ggcascade <- function(.data,
                      ...,
                      .weights = NULL,
                      .by = NULL,
                      .nrow = NULL,
                      .ncol = NULL,
                      .add_n = TRUE,
                      .text_size = 4,
                      .arrows = TRUE) {
  .data |>
    compute_cascade(..., .weights = {{ .weights }}, .by = {{ .by }}) |>
    plot_cascade(
      .by = {{ .by }},
      .nrow = .nrow,
      .ncol = .ncol,
      .add_n = .add_n,
      .text_size = .text_size,
      .arrows = .arrows
    )
}

#' @rdname ggcascade
#' @export
compute_cascade <- function(.data, ..., .weights = NULL, .by = NULL) {
  w <- .data |> dplyr::select({{ .weights }})
  if (ncol(w) > 1)
    cli::cli_abort("{.arg .weights} should select only one column.")
  if (ncol(w) == 0) {
    w <- 1
  } else {
    w <- w[[1]]
  }

  dots <- rlang::enquos(...)
  .data |>
    dplyr::mutate(.w = w) |>
    dplyr::mutate(!!! dots) |>
    dplyr::group_by({{ .by }}) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(names(dots)),
        \(x) {
          weighted.sum(x, .data$.w)
        }
      ),
      .groups = "keep"
    ) |>
    tidyr::pivot_longer(
      dplyr::all_of(names(dots)),
      names_to = "x",
      values_to = "n"
    ) |>
    dplyr::mutate(
      x = factor(.data$x, levels = names(dots), ordered = TRUE)
    ) |>
    dplyr::arrange(.data$x, .by_group = TRUE)
}

#' @rdname ggcascade
#' @export
plot_cascade <- function(.data,
                         .by = NULL,
                         .nrow = NULL,
                         .ncol = NULL,
                         .add_n = TRUE,
                         .text_size = 4,
                         .arrows = TRUE) {
  .data <- .data |>
    dplyr::group_by({{ .by }}) |>
    dplyr::mutate(
      prop = .data$n / max(.data$n),
      label = scales::percent(.data$prop, accuracy = .1),
      y_label = dplyr::if_else(.data$prop < .1 & .add_n, .1, .data$prop),
      xend = dplyr::lead(.data$x),
      yend = dplyr::lead(.data$prop) / 2,
      prop_step = dplyr::lead(.data$n) / .data$n,
      label_step = scales::percent(
        .data$prop_step,
        accuracy = .1,
        prefix = "\u00d7"
      )
    )

  p <- ggplot2::ggplot(.data) +
    ggplot2::aes(
      x = .data$x,
      y = .data$prop,
      fill = .data$x
    ) +
    ggplot2::geom_bar(
      stat = "identity",
      width = .5,
      colour = "black",
      linewidth = .25
    ) +
    ggplot2::geom_text(
      mapping = ggplot2::aes(
        y = .data$y_label,
        label = .data$label
      ),
      vjust = 0,
      nudge_y = .02,
      size = .text_size
    ) +
    ggplot2::scale_y_continuous(
      breaks = 0:5 / 5,
      labels = scales::percent
    ) +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(face = "bold")
    )

  if (.add_n) {
    p <- p +
      ggplot2::layer(
        geom = "text",
        stat = "identity",
        position = position_nudge(y = .02),
        mapping = ggplot2::aes(
          y = 0,
          label = paste0("n=", .data$n),
          prop = .data$prop,
          color = after_scale(hex_bw_threshold(.data$fill, .data$prop, .02))
        ),
        check.aes = FALSE,
        params = list(
          vjust = 0,
          size = .text_size
        )
      )
  }

  if (.arrows) {
    p <- p +
      ggplot2::geom_segment(
        mapping = ggplot2::aes(
          x = as.integer(.data$x) + .3,
          xend = as.integer(.data$xend) - .3,
          y = .data$yend,
          yend = .data$yend
        ),
        na.rm = TRUE,
        arrow = ggplot2::arrow(
          type = "closed",
          length = unit(0.25, "cm")
        )
      ) +
      ggplot2::geom_text(
        aes(
          x = as.integer(.data$x) + .5,
          y = .data$yend,
          label = .data$label_step
        ),
        vjust = 0,
        nudge_y = .04,
        na.rm = TRUE,
        size = .text_size
      )
  }

  .by_vars <- dplyr::group_vars(.data)
  if (length(.by_vars) > 0) {
    p <-
      p +
      ggplot2::facet_wrap(
        facets = .by_vars,
        nrow = .nrow,
        ncol = .ncol
      )
  }
  p
}
