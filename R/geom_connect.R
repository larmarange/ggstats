#' Connect bars / points
#'
#' `geom_connect()` is a variation of [ggplot2::geom_step()] adapted to
#' connect bars, but could also be used to connect points.
#'
#' @inheritParams ggplot2::geom_step
#' @param width Bar width (see examples).
#' @param continuous Should connect segments be continuous?
#' @param add_baseline Add connectors at baseline?
#' @export
#' @examples
#' library(ggplot2)
#'
#' ggplot(diamonds) +
#'   aes(x = clarity, fill = cut) +
#'   geom_bar(width = .5) +
#'   geom_connect_bars(width = .5, linewidth = .25) +
#'   theme_minimal() +
#'   theme(legend.position = "bottom")
#'
#' ggplot(diamonds) +
#'   aes(x = clarity, fill = cut) +
#'   geom_bar(width = .5) +
#'   geom_connect_bars(
#'     width = .5,
#'     continuous = TRUE,
#'     colour = "red",
#'     linetype = "dotted",
#'     add_baseline = FALSE,
#'    ) +
#'   theme(legend.position = "bottom")
#'
#' ggplot(diamonds) +
#'   aes(x = clarity, fill = cut) +
#'   geom_bar(width = .5, position = "fill") +
#'   geom_connect_bars(width = .5, position = "fill") +
#'   theme(legend.position = "bottom")
#'
#' ggplot(diamonds) +
#'   aes(x = clarity, fill = cut) +
#'   geom_bar(width = .5, position = "diverging") +
#'   geom_connect_bars(width = .5, position = "diverging", linewidth = .25) +
#'   theme(legend.position = "bottom")
geom_connect <- function(mapping = NULL,
                         data = NULL,
                         stat = "identity",
                         position = "identity",
                         width = 0.1,
                         continuous = FALSE,
                         na.rm = FALSE,
                         orientation = NA,
                         show.legend = NA,
                         inherit.aes = TRUE,
                         ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomConnect,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      width = width,
      continuous = continuous,
      orientation = orientation,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_connect
#' @export
geom_connect_bars <- function(mapping = NULL,
                              data = NULL,
                              stat = "prop",
                              position = "stack",
                              width = 0.9,
                              continuous = FALSE,
                              add_baseline = TRUE,
                              na.rm = FALSE,
                              orientation = NA,
                              show.legend = NA,
                              inherit.aes = TRUE,
                              ...) {
  params <- rlang::list2(
    width = width,
    continuous = continuous,
    orientation = orientation,
    add_baseline = add_baseline,
    na.rm = na.rm,
    ...
  )
  if (is.character(stat) && stat == "prop" && !"complete" %in% names(params))
    params$complete <- "fill"

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomConnect,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

#' @rdname geom_connect
#' @format NULL
#' @usage NULL
#' @export
GeomConnect <- ggproto(
  "GeomConnect",
  ggplot2::GeomPath,
  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, ambiguous = TRUE)
    params
  },
  extra_params = c("na.rm", "width", "orientation",
                   "continuous", "add_baseline"),
  draw_panel = function(data, panel_params, coord,
                        lineend = "butt", linejoin = "round",
                        linemitre = 10, arrow = NULL,
                        width = 0.1, continuous = FALSE,
                        add_baseline = FALSE,
                        flipped_aes = FALSE) {
    if (!is.numeric(width))
      cli::cli_abort(
        "{.arg width} should be a positive number.",
        call. = FALSE
      )
    if (width < 0)
      cli::cli_abort(
        "{.arg width} should be positive.",
        call. = FALSE
      )
    data <- flip_data(data, flipped_aes)
    if (add_baseline) {
      d0 <- data |>
        dplyr::filter(.data$group == min(.data$group))
      if ("ymin" %in% names(d0) && d0$ymin[1] < 0) {
        d0$y <- d0$ymin
      } else {
        d0$y <- 0
      }
      d0$group <- d0$group - 1
      data <- dplyr::bind_rows(d0, data)
    }
    data <- data |>
      by(
        data$group,
        connect_points,
        width = width,
        continuous = continuous
      ) |>
      unclass() |>
      as.list() |>
      dplyr::bind_rows()
    data <- flip_data(data, flipped_aes)
    GeomPath$draw_panel(
      data, panel_params, coord,
      lineend = lineend, linejoin = linejoin, linemitre = linemitre,
      arrow = arrow
    )
  }
)

#' Calculate connections for `geom_connect()`
#' Used by `GeomConnect()`
#'
#' @noRd
connect_points <- function(data, width = 0.9, continuous = FALSE) {
  data <- as.data.frame(data)[order(data$x), ]
  n <- nrow(data)

  if (n <= 1) {
    # Need at least one observation
    return(data[0, , drop = FALSE])
  }
  gaps <- data$x[-1] - data$x[-n]
  nudge <- pmin(gaps / 2, width / 2)

  d1 <- data
  d1[["..order.."]] <- 0
  if (!continuous) d1$y <- NA

  d2 <- data[-1, ]
  d2[["..order.."]] <- -1
  d2$x <- d2$x - nudge

  d3 <- data[-n, ]
  d3[["..order.."]] <- 1
  d3$x <- d3$x + nudge

  dplyr::bind_rows(d1, d2, d3) |>
    dplyr::arrange(.data$x, .data[["..order.."]]) |>
    dplyr::select(-dplyr::all_of("..order.."))
}
