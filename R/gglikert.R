#' Plotting Likert-type items
#'
#' `r lifecycle::badge("experimental")`
#'
#' Combines several factor variables using the same list of ordered levels
#' (e.g. Likert-type scales) into a unique data frame and generates a centered
#' bar plot.
#'
#' You could use `gglikert_data()` to just produce the dataset to be plotted.
#'
#' If variable labels have been defined (see [labelled::var_label()]), they will
#' be considered. You can also pass custom variables labels with the
#' `variable_labels` argument.
#'
#' @param data a data frame
#' @param include variables to include, accept [tidy-select][dplyr::select]
#' syntax
#' @param variable_labels a named list or a named vector of custom variable
#' labels
#' @param sort should variables be sorted?
#' @param sort_method method used to sort the variables: `"prop"` sort according
#' to the proportion of answers higher than the centered level, `"mean"`
#' considers answer as a score and sort according to the mean score
#' @param sort_prop_include_center when sorting with `"prop"` and if the number
#' of levels is uneven, should half of the central level be taken into account
#' to compute the proportion?
#' @param exclude_fill_values Vector of values that should not be displayed
#' (but still taken into account for computing proportions),
#' see [position_likert()]
#' @param add_labels should percentage labels be added to the plot?
#' @param labels_size size of the percentage labels
#' @param labels_accuracy accuracy of the percentages, see
#' [scales::label_percent()]
#' @param labels_hide_below if provided, values below will be masked, see
#' [label_percent_abs()]
#' @param add_totals should the total proportions of negative and positive
#' answers be added to plot?
#' @param totals_size size of total proportions
#' @param totals_accuracy accuracy of the total proportions, see
#' [scales::label_percent()]
#' @param totals_fontface font face of the total proportions
#' @param totals_include_center if the number of levels is uneven, should half
#' of the center level be added to the total proportions?
#' @param totals_hjust horizontal adjustment of totals labels on the x axis
#' @param y_reverse should the y axis be reversed?
#' @param y_label_wrap number of characters per line for y axis labels, see
#' [scales::label_wrap()]
#' @param reverse_likert if `TRUE`, will reverse the default stacking order,
#' see [position_likert()]
#' @param width bar width, see [ggplot2::geom_bar()]
#' @return A `ggplot2` plot or a `tibble`.
#' @seealso [vignette("gglikert")], [position_likert()], [stat_prop()]
#' @export
gglikert <- function(data,
                     include = dplyr::everything(),
                     variable_labels = NULL,
                     sort = c("none", "ascending", "descending"),
                     sort_method = c("prop", "mean"),
                     sort_prop_include_center = totals_include_center,
                     exclude_fill_values = NULL,
                     add_labels = TRUE,
                     labels_size = 3.5,
                     labels_accuracy = 1,
                     labels_hide_below = .05,
                     add_totals = TRUE,
                     totals_size = labels_size,
                     totals_accuracy = labels_accuracy,
                     totals_fontface = "bold",
                     totals_include_center = FALSE,
                     totals_hjust = .1,
                     y_reverse = TRUE,
                     y_label_wrap = 50,
                     reverse_likert = FALSE,
                     width = .9) {
  data <-
    gglikert_data(
      data,
      {{ include }},
      variable_labels = variable_labels,
      sort = sort,
      sort_method = sort_method,
      sort_prop_include_center = sort_prop_include_center,
      exclude_fill_values = exclude_fill_values
    )

  if (y_reverse)
    data$.question <- data$.question %>% forcats::fct_rev()

  p <- ggplot(data) +
    aes(
      y = .data[[".question"]],
      fill = .data[[".answer"]],
      by = .data[[".question"]]
    ) +
    geom_bar(
      position = position_likert(
        reverse = reverse_likert,
        exclude_fill_values = exclude_fill_values
      ),
      stat = "prop",
      complete = "fill",
      width = width
    )

  if (add_labels)
    p <- p +
    geom_text(
      mapping = aes(
        label = label_percent_abs(
          hide_below = labels_hide_below,
          accuracy = labels_accuracy
        )(after_stat(prop))
      ),
      stat = "prop",
      complete = "fill",
      position = position_likert(
        vjust = .5,
        reverse = reverse_likert,
        exclude_fill_values = exclude_fill_values
      ),
      size = labels_size
    )

  if (add_totals) {
    dtot <- data %>%
      dplyr::group_by(.data$.question) %>%
      dplyr::summarise(
        prop_lower = .prop_lower(
          .data$.answer,
          include_center = TRUE,
          exclude_fill_values = exclude_fill_values
        ),
        prop_higher = .prop_higher(
          .data$.answer,
          include_center = TRUE,
          exclude_fill_values = exclude_fill_values
        ),
        label_lower = .prop_lower(
          .data$.answer,
          include_center = totals_include_center,
          exclude_fill_values = exclude_fill_values
        ),
        label_higher = .prop_higher(
          .data$.answer,
          include_center = totals_include_center,
          exclude_fill_values = exclude_fill_values
        )
      ) %>%
      dplyr::mutate(
        label_lower =
          label_percent_abs(accuracy = totals_accuracy)(.data$label_lower),
        label_higher =
          label_percent_abs(accuracy = totals_accuracy)(.data$label_higher),
        x_lower = -1 * max(.data$prop_lower) - totals_hjust,
        x_higher = max(.data$prop_higher) + totals_hjust
      )
    dtot <- dplyr::bind_rows(
      dtot %>%
        dplyr::select(
          dplyr::all_of(c(".question", x = "x_lower", label = "label_lower"))
        ),
      dtot %>%
        dplyr::select(
          dplyr::all_of(c(".question", x = "x_higher", label = "label_higher"))
        )
    )

    p <- p +
      geom_text(
        mapping = aes(
          y = .data[[".question"]],
          x = .data[["x"]],
          label = .data[["label"]],
          fill = NULL,
          by = NULL
        ),
        data = dtot,
        size = totals_size,
        fontface = totals_fontface
      )
  }

  p <- p +
    labs(x = NULL, y = NULL, fill = NULL) +
    scale_x_continuous(labels = label_percent_abs()) +
    scale_y_discrete(labels = scales::label_wrap(y_label_wrap)) +
    theme_light() +
    theme(
      legend.position = "bottom",
      panel.grid.major.y = element_blank()
    )

  if (length(levels(data$.answer)) <= 11)
    p <- p + scale_fill_brewer(palette = "BrBG")

  p
}

#' @rdname gglikert
#' @export
gglikert_data <- function(data,
                          include = dplyr::everything(),
                          variable_labels = NULL,
                          sort = c("none", "ascending", "descending"),
                          sort_method = c("prop", "mean"),
                          sort_prop_include_center = TRUE,
                          exclude_fill_values = NULL) {
  rlang::check_installed("broom.helpers")
  rlang::check_installed("labelled")

  sort <- match.arg(sort)
  sort_method <- match.arg(sort_method)

  variables <- broom.helpers::.select_to_varnames(
    select = {{ include }},
    data = data,
    arg_name = "include"
  )

  if (is.list(variable_labels))
    variable_labels <- unlist(variable_labels)
  data_labels <- data %>%
    labelled::var_label(unlist = TRUE, null_action = "fill")
  if (!is.null(variable_labels))
    data_labels[names(variable_labels)] <- variable_labels
  data_labels <- data_labels[variables]

  data <- data %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(variables), labelled::to_factor)
    )

  data <- data %>%
    dplyr::mutate(
      dplyr::bind_cols(forcats::fct_unify(data[, variables]))
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(variables),
      names_to = ".question",
      values_to = ".answer"
    )

  data$.question <- data_labels[data$.question] %>%
    forcats::fct_inorder()

  if (sort == "ascending" && sort_method == "prop")
    data$.question <- data$.question %>%
      forcats::fct_reorder(
        data$.answer,
        .fun = .prop_higher,
        include_center = sort_prop_include_center,
        exclude_fill_values = exclude_fill_values,
        .na_rm = TRUE
      )
  if (sort == "descending" && sort_method == "prop")
    data$.question <- data$.question %>%
      forcats::fct_reorder(
        data$.answer,
        .fun = .prop_higher,
        include_center = sort_prop_include_center,
        exclude_fill_values = exclude_fill_values,
        .na_rm = TRUE,
        .desc = TRUE
      )
  if (sort == "ascending" && sort_method == "mean")
    data$.question <- data$.question %>%
    forcats::fct_reorder(
      data$.answer,
      .fun = .sort_mean,
      exclude_fill_values = exclude_fill_values,
      .na_rm = TRUE
    )
  if (sort == "descending" && sort_method == "mean")
    data$.question <- data$.question %>%
    forcats::fct_reorder(
      data$.answer,
      .fun = .sort_mean,
      exclude_fill_values = exclude_fill_values,
      .na_rm = TRUE,
      .desc = TRUE
    )

  data
}

# Compute the proportion being higher than the center
# Option to include the centre (if yes, only half taken into account)
.prop_higher <- function(x, include_center = TRUE, exclude_fill_values = NULL) {
  N <- sum(!is.na(x))
  if (!is.factor(x)) x <- factor(x)
  if (!is.null(exclude_fill_values)) {
    l <- levels(x)
    l <- l[!l %in% exclude_fill_values]
    x <- factor(x, levels = l)
  }
  m <- length(levels(x)) / 2 + 1 / 2
  x <- as.numeric(x)
  w <- ifelse(include_center, 1 / 2, 0)
  sum(as.integer(x > m), w * as.integer(x == m), na.rm = TRUE) / N
}

# Compute the proportion being higher than the center
# Option to include the centre (if yes, only half taken into account)
.prop_lower <- function(x, include_center = TRUE, exclude_fill_values = NULL) {
  N <- sum(!is.na(x))
  if (!is.factor(x)) x <- factor(x)
  if (!is.null(exclude_fill_values)) {
    l <- levels(x)
    l <- l[!l %in% exclude_fill_values]
    x <- factor(x, levels = l)
  }
  m <- length(levels(x)) / 2 + 1 / 2
  x <- as.numeric(x)
  w <- ifelse(include_center, 1 / 2, 0)
  sum(as.integer(x < m), w * as.integer(x == m), na.rm = TRUE) / N
}

.sort_mean <- function(x, exclude_fill_values = NULL) {
  if (!is.factor(x)) x <- factor(x)
  if (!is.null(exclude_fill_values)) {
    l <- levels(x)
    l <- l[!l %in% exclude_fill_values]
    x <- factor(x, levels = l)
  }
  x <- as.numeric(x)
  mean(x, na.rm = TRUE)
}
