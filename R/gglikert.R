#' Plotting Likert-type items
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
#' @param include variables to include, accepts [tidy-select][dplyr::select]
#' syntax
#' @param weights optional variable name of a weighting variable,
#' accepts [tidy-select][dplyr::select] syntax
#' @param y name of the variable to be plotted on `y` axis (relevant when
#' `.question` is mapped to "facets, see examples),
#' accepts [tidy-select][dplyr::select] syntax
#' @param variable_labels a named list or a named vector of custom variable
#' labels
#' @param sort should the factor defined by `factor_to_sort` be sorted according
#' to the answers (see `sort_method`)? One of "none" (default), "ascending" or
#' "descending"
#' @param sort_method method used to sort the variables: `"prop"` sort according
#' to the proportion of answers higher than the centered level, `"prop_lower"`
#' according to the proportion lower than the centered level,  `"mean"`
#' considers answer as a score and sort according to the mean score, `"median"`
#' used the median and the majority judgment rule for tie-breaking.
#' @param sort_prop_include_center when sorting with `"prop"` and if the number
#' of levels is uneven, should half of the central level be taken into account
#' to compute the proportion?
#' @param factor_to_sort name of the factor column to sort if `sort` is not
#' equal to `"none"`; by default the list of questions passed to `include`;
#' should be one factor column of the tibble returned by `gglikert_data()`;
#' accepts [tidy-select][dplyr::select] syntax
#' @param exclude_fill_values Vector of values that should not be displayed
#' (but still taken into account for computing proportions),
#' see [position_likert()]
#' @param cutoff number of categories to be displayed negatively (i.e. on the
#' left of the x axis or the bottom of the y axis), could be a decimal value:
#' `2` to display negatively the two first categories, `2.5` to display
#' negatively the two first categories and half of the third, `2.2` to display
#' negatively the two first categories and a fifth of the third (see examples).
#' By default (`NULL`), it will be equal to the number of categories divided
#' by 2, i.e. it will be centered.
#' @param data_fun for advanced usage, custom function to be applied to the
#' generated dataset at the end of `gglikert_data()`
#' @param add_labels should percentage labels be added to the plot?
#' @param labels_size size of the percentage labels
#' @param labels_color color of the percentage labels (`"auto"` to use
#' `hex_bw()` to determine a font color based on background color)
#' @param labels_accuracy accuracy of the percentages, see
#' [scales::label_percent()]
#' @param labels_hide_below if provided, values below will be masked, see
#' [label_percent_abs()]
#' @param add_totals should the total proportions of negative and positive
#' answers be added to plot? **This option is not compatible with facets!**
#' @param totals_size size of the total proportions
#' @param totals_color color of the total proportions
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
#' @param facet_rows,facet_cols A set of variables or expressions quoted by
#' [ggplot2::vars()] and defining faceting groups on the rows or columns
#' dimension (see examples)
#' @param facet_label_wrap number of characters per line for facet labels, see
#' [ggplot2::label_wrap_gen()]
#' @param symmetric should the x-axis be symmetric?
#' @return A `ggplot2` plot or a `tibble`.
#' @seealso `vignette("gglikert")`, [position_likert()], [stat_prop()]
#' @export
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' likert_levels <- c(
#'   "Strongly disagree",
#'   "Disagree",
#'   "Neither agree nor disagree",
#'   "Agree",
#'   "Strongly agree"
#' )
#' set.seed(42)
#' df <-
#'   tibble(
#'     q1 = sample(likert_levels, 150, replace = TRUE),
#'     q2 = sample(likert_levels, 150, replace = TRUE, prob = 5:1),
#'     q3 = sample(likert_levels, 150, replace = TRUE, prob = 1:5),
#'     q4 = sample(likert_levels, 150, replace = TRUE, prob = 1:5),
#'     q5 = sample(c(likert_levels, NA), 150, replace = TRUE),
#'     q6 = sample(likert_levels, 150, replace = TRUE, prob = c(1, 0, 1, 1, 0))
#'   ) %>%
#'   mutate(across(everything(), ~ factor(.x, levels = likert_levels)))
#'
#' gglikert(df)
#'
#' gglikert(df, include = q1:3) +
#'   scale_fill_likert(pal = scales::brewer_pal(palette = "PRGn"))
#'
#' gglikert(df, sort = "ascending")
#'
#' \donttest{
#' gglikert(df, sort = "ascending", sort_prop_include_center = TRUE)
#'
#' gglikert(df, sort = "ascending", sort_method = "mean")
#'
#' gglikert(df, reverse_likert = TRUE)
#'
#' gglikert(df, add_totals = FALSE, add_labels = FALSE)
#'
#' gglikert(
#'   df,
#'   totals_include_center = TRUE,
#'   totals_hjust = .25,
#'   totals_size = 4.5,
#'   totals_fontface = "italic",
#'   totals_accuracy = .01,
#'   labels_accuracy = 1,
#'   labels_size = 2.5,
#'   labels_hide_below = .25
#' )
#'
#' gglikert(df, exclude_fill_values = "Neither agree nor disagree")
#'
#' if (require("labelled")) {
#'   df %>%
#'     set_variable_labels(
#'       q1 = "First question",
#'       q2 = "Second question"
#'     ) %>%
#'     gglikert(
#'       variable_labels = c(
#'         q4 = "a custom label",
#'         q6 = "a very very very very very very very very very very long label"
#'       ),
#'       y_label_wrap = 25
#'     )
#' }
#'
#' # Facets
#' df_group <- df
#' df_group$group <- sample(c("A", "B"), 150, replace = TRUE)
#'
#' gglikert(df_group, q1:q6, facet_rows = vars(group))
#'
#' gglikert(df_group, q1:q6, facet_cols = vars(group))
#'
#' gglikert(df_group, q1:q6, y = "group", facet_rows = vars(.question))
#'
#' # Custom function to be applied on data
#' f <- function(d) {
#'   d$.question <- forcats::fct_relevel(d$.question, "q5", "q2")
#'   d
#' }
#' gglikert(df, include = q1:q6, data_fun = f)
#'
#' # Custom center
#' gglikert(df, cutoff = 2)
#'
#' gglikert(df, cutoff = 1)
#'
#' gglikert(df, cutoff = 1, symmetric = TRUE)
#'
#' }
gglikert <- function(data,
                     include = dplyr::everything(),
                     weights = NULL,
                     y = ".question",
                     variable_labels = NULL,
                     sort = c("none", "ascending", "descending"),
                     sort_method = c("prop", "prop_lower", "mean", "median"),
                     sort_prop_include_center = totals_include_center,
                     factor_to_sort = ".question",
                     exclude_fill_values = NULL,
                     cutoff = NULL,
                     data_fun = NULL,
                     add_labels = TRUE,
                     labels_size = 3.5,
                     labels_color = "auto",
                     labels_accuracy = 1,
                     labels_hide_below = .05,
                     add_totals = TRUE,
                     totals_size = labels_size,
                     totals_color = "black",
                     totals_accuracy = labels_accuracy,
                     totals_fontface = "bold",
                     totals_include_center = FALSE,
                     totals_hjust = .1,
                     y_reverse = TRUE,
                     y_label_wrap = 50,
                     reverse_likert = FALSE,
                     width = .9,
                     facet_rows = NULL,
                     facet_cols = NULL,
                     facet_label_wrap = 50,
                     symmetric = FALSE) {
  data <-
    gglikert_data(
      data,
      {{ include }},
      weights = {{ weights }},
      variable_labels = variable_labels,
      sort = sort,
      sort_method = sort_method,
      sort_prop_include_center = sort_prop_include_center,
      factor_to_sort = {{ factor_to_sort }},
      exclude_fill_values = exclude_fill_values,
      cutoff = cutoff,
      data_fun = data_fun
    )

  y <- data |> dplyr::select({{ y }}) |> colnames()
  if (length(y) != 1) cli::cli_abort("{.arg y} should select only one column.")

  if (!is.factor(data[[y]])) {
    data[[y]] <- factor(data[[y]])
  }

  if (y_reverse) {
    data[[y]] <- data[[y]] %>% forcats::fct_rev()
  }

  p <- ggplot(data) +
    aes(
      y = .data[[y]],
      fill = .data[[".answer"]],
      by = .data[[y]],
      weight = .data[[".weights"]]
    ) +
    geom_bar(
      position = position_likert(
        reverse = reverse_likert,
        exclude_fill_values = exclude_fill_values,
        cutoff = cutoff
      ),
      stat = StatProp,
      complete = "fill",
      width = width
    )

  if (add_labels && labels_color == "auto") {
    p <- p +
      geom_text(
        mapping = aes(
          label = label_percent_abs(
            hide_below = labels_hide_below,
            accuracy = labels_accuracy
          )(after_stat(prop)),
          color = after_scale(hex_bw(.data$fill))
        ),
        stat = StatProp,
        complete = "fill",
        position = position_likert(
          vjust = .5,
          reverse = reverse_likert,
          exclude_fill_values = exclude_fill_values,
          cutoff = cutoff
        ),
        size = labels_size
      )
  }

  if (add_labels && labels_color != "auto") {
    p <- p +
      geom_text(
        mapping = aes(
          label = label_percent_abs(
            hide_below = labels_hide_below,
            accuracy = labels_accuracy
          )(after_stat(prop))
        ),
        stat = StatProp,
        complete = "fill",
        position = position_likert(
          vjust = .5,
          reverse = reverse_likert,
          exclude_fill_values = exclude_fill_values,
          cutoff = cutoff
        ),
        size = labels_size,
        color = labels_color
      )
  }

  if (add_totals) {
    dtot <- data %>%
      dplyr::group_by(.data[[y]], !!!facet_rows, !!!facet_cols) %>%
      dplyr::summarise(
        prop_lower = .prop_lower(
          .data$.answer,
          .data$.weights,
          include_center = TRUE,
          exclude_fill_values = exclude_fill_values,
          cutoff = cutoff
        ),
        prop_higher = .prop_higher(
          .data$.answer,
          .data$.weights,
          include_center = TRUE,
          exclude_fill_values = exclude_fill_values,
          cutoff = cutoff
        ),
        label_lower = .prop_lower(
          .data$.answer,
          .data$.weights,
          include_center = totals_include_center,
          exclude_fill_values = exclude_fill_values,
          cutoff = cutoff
        ),
        label_higher = .prop_higher(
          .data$.answer,
          .data$.weights,
          include_center = totals_include_center,
          exclude_fill_values = exclude_fill_values,
          cutoff = cutoff
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        label_lower =
          label_percent_abs(accuracy = totals_accuracy)(.data$label_lower),
        label_higher =
          label_percent_abs(accuracy = totals_accuracy)(.data$label_higher),
        x_lower = dplyr::if_else(
          symmetric,
          -1 * max(.data$prop_lower, .data$prop_higher) - totals_hjust,
          -1 * max(.data$prop_lower) - totals_hjust
        ),
        x_higher = dplyr::if_else(
          symmetric,
          max(.data$prop_higher, .data$prop_lower) + totals_hjust,
          max(.data$prop_higher) + totals_hjust
        )
      ) %>%
      dplyr::group_by(!!!facet_rows, !!!facet_cols)
    dtot <- dplyr::bind_rows(
      dtot %>%
        dplyr::select(
          dplyr::all_of(c(y, x = "x_lower", label = "label_lower")),
          dplyr::group_cols()
        ),
      dtot %>%
        dplyr::select(
          dplyr::all_of(c(y, x = "x_higher", label = "label_higher")),
          dplyr::group_cols()
        )
    )

    p <- p +
      geom_text(
        mapping = aes(
          y = .data[[y]],
          x = .data[["x"]],
          label = .data[["label"]],
          fill = NULL,
          by = NULL,
          weight = NULL
        ),
        data = dtot,
        size = totals_size,
        color = totals_color,
        fontface = totals_fontface
      )
  }

  if (symmetric) {
    p <- p +
      scale_x_continuous(
        labels = label_percent_abs(),
        limits = symmetric_limits
      )
  } else {
    p <- p +
      scale_x_continuous(labels = label_percent_abs())
  }

  p <- p +
    labs(x = NULL, y = NULL, fill = NULL) +
    scale_y_discrete(labels = scales::label_wrap(y_label_wrap)) +
    theme_light() +
    theme(
      legend.position = "bottom",
      panel.grid.major.y = element_blank()
    ) +
    scale_fill_likert(cutoff = cutoff)

  p + facet_grid(
    rows = facet_rows, cols = facet_cols,
    labeller = ggplot2::label_wrap_gen(facet_label_wrap)
  )
}

#' @rdname gglikert
#' @export
gglikert_data <- function(data,
                          include = dplyr::everything(),
                          weights = NULL,
                          variable_labels = NULL,
                          sort = c("none", "ascending", "descending"),
                          sort_method = c(
                            "prop", "prop_lower", "mean", "median"
                          ),
                          sort_prop_include_center = TRUE,
                          factor_to_sort = ".question",
                          exclude_fill_values = NULL,
                          cutoff = NULL,
                          data_fun = NULL) {
  rlang::check_installed("labelled")

  sort <- match.arg(sort)
  sort_method <- match.arg(sort_method)

  variables <- data |> dplyr::select({{ include }}) |> colnames()

  weights_var <- data |> dplyr::select({{ weights }}) |> colnames()
  if (length(weights_var) > 1)
    cli::cli_abort("{.arg weights} should select only one column.")
  if (length(weights_var) == 0) {
    data$.weights <- 1
  } else {
    data$.weights <- data[[weights_var]]
  }
  if (!is.numeric(data$.weights)) {
    cli::cli_abort("{.arg weights} should correspond to a numerical variable.")
  }

  if (is.list(variable_labels)) {
    variable_labels <- unlist(variable_labels)
  }
  data_labels <- data %>%
    labelled::var_label(unlist = TRUE, null_action = "fill")
  if (!is.null(variable_labels)) {
    data_labels[names(variable_labels)] <- variable_labels
  }
  data_labels <- data_labels[variables]

  data <- data %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(variables), .fns = labelled::to_factor)
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

  factor_to_sort <- data |> dplyr::select({{ factor_to_sort }}) |> colnames()
  if (length(factor_to_sort) != 1)
    cli::cli_abort("{.arg factor_to_sort} should select only one column.")

  if (sort == "ascending" && sort_method == "prop") {
    data[[factor_to_sort]] <- data[[factor_to_sort]] %>%
      forcats::fct_reorder2(
        data$.answer,
        data$.weights,
        .fun = .prop_higher,
        include_center = sort_prop_include_center,
        exclude_fill_values = exclude_fill_values,
        cutoff = cutoff,
        .na_rm = FALSE,
        .desc = FALSE
      )
  }
  if (sort == "descending" && sort_method == "prop") {
    data[[factor_to_sort]] <- data[[factor_to_sort]] %>%
      forcats::fct_reorder2(
        data$.answer,
        data$.weights,
        .fun = .prop_higher,
        include_center = sort_prop_include_center,
        exclude_fill_values = exclude_fill_values,
        cutoff = cutoff,
        .na_rm = FALSE,
        .desc = TRUE
      )
  }
  if (sort == "ascending" && sort_method == "prop_lower") {
    data[[factor_to_sort]] <- data[[factor_to_sort]] %>%
      forcats::fct_reorder2(
        data$.answer,
        data$.weights,
        .fun = .prop_lower,
        include_center = sort_prop_include_center,
        exclude_fill_values = exclude_fill_values,
        cutoff = cutoff,
        .na_rm = FALSE,
        .desc = FALSE
      )
  }
  if (sort == "descending" && sort_method == "prop_lower") {
    data[[factor_to_sort]] <- data[[factor_to_sort]] %>%
      forcats::fct_reorder2(
        data$.answer,
        data$.weights,
        .fun = .prop_lower,
        include_center = sort_prop_include_center,
        exclude_fill_values = exclude_fill_values,
        cutoff = cutoff,
        .na_rm = FALSE,
        .desc = TRUE
      )
  }
  if (sort == "ascending" && sort_method == "mean") {
    data[[factor_to_sort]] <- data[[factor_to_sort]] %>%
      forcats::fct_reorder2(
        data$.answer,
        data$.weights,
        .fun = .sort_mean,
        exclude_fill_values = exclude_fill_values,
        .na_rm = FALSE,
        .desc = FALSE
      )
  }
  if (sort == "descending" && sort_method == "mean") {
    data[[factor_to_sort]] <- data[[factor_to_sort]] %>%
      forcats::fct_reorder2(
        data$.answer,
        data$.weights,
        .fun = .sort_mean,
        exclude_fill_values = exclude_fill_values,
        .na_rm = FALSE,
        .desc = TRUE
      )
  }
  if (sort == "ascending" && sort_method == "median") {
    data[[factor_to_sort]] <- data[[factor_to_sort]] %>%
      forcats::fct_reorder2(
        data$.answer,
        data$.weights,
        .fun = .sort_median,
        exclude_fill_values = exclude_fill_values,
        .na_rm = FALSE,
        .desc = FALSE
      )
  }
  if (sort == "descending" && sort_method == "median") {
    data[[factor_to_sort]] <- data[[factor_to_sort]] %>%
      forcats::fct_reorder2(
        data$.answer,
        data$.weights,
        .fun = .sort_median,
        exclude_fill_values = exclude_fill_values,
        .na_rm = FALSE,
        .desc = TRUE
      )
  }

  if (!is.null(data_fun)) {
    if (!is.function(data_fun))
      cli::cli_abort("{arg data_fun} should be a function.")
    data <- data_fun(data)
  }

  data
}

# Compute the proportion being higher than the center
# Option to include the centre (if yes, only half taken into account)
.prop_higher <- function(x, w, include_center = TRUE,
                         exclude_fill_values = NULL,
                         cutoff = NULL) {
  N <- sum(as.integer(!is.na(x)) * w)
  if (!is.factor(x)) x <- factor(x)
  if (!is.null(exclude_fill_values)) {
    l <- levels(x)
    l <- l[!l %in% exclude_fill_values]
    x <- factor(x, levels = l)
  }
  if (is.null(cutoff)) cutoff <- length(levels(x)) / 2
  x <- as.numeric(x)
  m <- ceiling(cutoff)
  sum(
    w * as.integer(x >= cutoff + 1),
    include_center * w * (x == m) * (m - cutoff),
    na.rm = TRUE
  ) / N
}

# Compute the proportion being higher than the center
# Option to include the centre (if yes, only half taken into account)
.prop_lower <- function(x, w, include_center = TRUE,
                        exclude_fill_values = NULL,
                        cutoff = NULL) {
  N <- sum(as.integer(!is.na(x)) * w)
  if (!is.factor(x)) x <- factor(x)
  if (!is.null(exclude_fill_values)) {
    l <- levels(x)
    l <- l[!l %in% exclude_fill_values]
    x <- factor(x, levels = l)
  }
  if (is.null(cutoff)) cutoff <- length(levels(x)) / 2
  x <- as.numeric(x)
  m <- ceiling(cutoff)
  sum(
    w * as.integer(x <= cutoff),
    include_center * w * (x == m) * (cutoff %% 1),
    na.rm = TRUE
  ) / N
}

#' @importFrom stats weighted.mean
.sort_mean <- function(x, w, exclude_fill_values = NULL) {
  if (!is.factor(x)) x <- factor(x)
  if (!is.null(exclude_fill_values)) {
    l <- levels(x)
    l <- l[!l %in% exclude_fill_values]
    x <- factor(x, levels = l)
  }
  x <- as.integer(x)
  stats::weighted.mean(x, w, na.rm = TRUE)
}

.sort_median <- function(x, w, exclude_fill_values = NULL) {
  if (!is.factor(x)) x <- factor(x)
  if (!is.null(exclude_fill_values)) {
    l <- levels(x)
    l <- l[!l %in% exclude_fill_values]
    x <- factor(x, levels = l)
  }
  x <- as.integer(x)
  med <- weighted.median(x, w, na.rm = TRUE)
  med +
    stats::weighted.mean(x > med, w, na.rm = TRUE) -
    stats::weighted.mean(x < med, w, na.rm = TRUE)
}

#' @rdname gglikert
#' @param add_median_line add a vertical line at 50%?
#' @param reverse_fill if `TRUE`, will reverse the default stacking order,
#' see [ggplot2::position_fill()]
#' @export
#' @examples
#' gglikert_stacked(df, q1:q6)
#'
#' gglikert_stacked(df, q1:q6, add_median_line = TRUE, sort = "asc")
#'
#' \donttest{
#' gglikert_stacked(df_group, q1:q6, y = "group", add_median_line = TRUE) +
#'   facet_grid(rows = vars(.question))
#' }
gglikert_stacked <- function(data,
                             include = dplyr::everything(),
                             weights = NULL,
                             y = ".question",
                             variable_labels = NULL,
                             sort = c("none", "ascending", "descending"),
                             sort_method = c(
                               "prop", "prop_lower", "mean", "median"
                             ),
                             sort_prop_include_center = FALSE,
                             factor_to_sort = ".question",
                             data_fun = NULL,
                             add_labels = TRUE,
                             labels_size = 3.5,
                             labels_color = "auto",
                             labels_accuracy = 1,
                             labels_hide_below = .05,
                             add_median_line = FALSE,
                             y_reverse = TRUE,
                             y_label_wrap = 50,
                             reverse_fill = TRUE,
                             width = .9) {
  data <-
    gglikert_data(
      data,
      {{ include }},
      weights = {{ weights }},
      variable_labels = variable_labels,
      sort = sort,
      sort_method = sort_method,
      sort_prop_include_center = sort_prop_include_center,
      factor_to_sort = {{ factor_to_sort }},
      exclude_fill_values = NULL,
      data_fun = data_fun
    )

  y <- data |> dplyr::select({{ y }}) |> colnames()
  if (length(y) != 1) cli::cli_abort("{.arg y} should select only one column.")

  if (!is.factor(data[[y]])) {
    data[[y]] <- factor(data[[y]])
  }

  if (y_reverse) {
    data[[y]] <- data[[y]] %>% forcats::fct_rev()
  }

  p <- ggplot(data) +
    aes(
      y = .data[[y]],
      fill = .data[[".answer"]],
      by = .data[[y]],
      weight = .data[[".weights"]]
    ) +
    geom_bar(
      position = position_fill(reverse = reverse_fill),
      stat = StatProp,
      complete = "fill",
      width = width
    )

  if (add_labels && labels_color == "auto") {
    p <- p +
      geom_text(
        mapping = aes(
          label = label_percent_abs(
            hide_below = labels_hide_below,
            accuracy = labels_accuracy
          )(after_stat(prop)),
          color = after_scale(hex_bw(.data$fill))
        ),
        stat = StatProp,
        complete = "fill",
        position = position_fill(
          vjust = .5,
          reverse = reverse_fill
        ),
        size = labels_size
      )
  }

  if (add_labels && labels_color != "auto") {
    p <- p +
      geom_text(
        mapping = aes(
          label = label_percent_abs(
            hide_below = labels_hide_below,
            accuracy = labels_accuracy
          )(after_stat(prop))
        ),
        stat = StatProp,
        complete = "fill",
        position = position_fill(
          vjust = .5,
          reverse = reverse_fill
        ),
        size = labels_size,
        color = labels_color
      )
  }

  if (add_median_line) {
    p <- p +
      ggplot2::geom_vline(xintercept = .5)
  }

  p <- p +
    labs(x = NULL, y = NULL, fill = NULL) +
    scale_x_continuous(labels = label_percent_abs()) +
    scale_y_discrete(labels = scales::label_wrap(y_label_wrap)) +
    theme_light() +
    theme(
      legend.position = "bottom",
      panel.grid.major.y = element_blank()
    ) +
    scale_fill_extended()

  p
}
