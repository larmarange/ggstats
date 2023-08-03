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
#' @param weights optional variable name of a weighting variable,
#' accept [tidy-select][dplyr::select] syntax
#' @param y name of the variable to be plotted on `y` axis (relevant when
#' `.question` is mapped to "facets, see examples),
#' accept [tidy-select][dplyr::select] syntax
#' @param variable_labels a named list or a named vector of custom variable
#' labels
#' @param sort should variables be sorted?
#' @param sort_method method used to sort the variables: `"prop"` sort according
#' to the proportion of answers higher than the centered level, `"mean"`
#' considers answer as a score and sort according to the mean score, `"median"`
#' used the median and the majority judgment rule for tie-breaking.
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
#' answers be added to plot? **This option is not compatible with facets!**
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
#' @param facet_rows,facet_cols A set of variables or expressions quoted by
#' [ggplot2::vars()] and defining faceting groups on the rows or columns
#' dimension (see examples)
#' @param facet_label_wrap number of characters per line for facet labels, see
#' [ggplot2::label_wrap_gen()]
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
#' gglikert(df, include = q1:3)
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
#' }
gglikert <- function(data,
                     include = dplyr::everything(),
                     weights = NULL,
                     y = ".question",
                     variable_labels = NULL,
                     sort = c("none", "ascending", "descending"),
                     sort_method = c("prop", "mean", "median"),
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
                     width = .9,
                     facet_rows = NULL,
                     facet_cols = NULL,
                     facet_label_wrap = 50) {
  data <-
    gglikert_data(
      data,
      {{ include }},
      weights = {{ weights }},
      variable_labels = variable_labels,
      sort = sort,
      sort_method = sort_method,
      sort_prop_include_center = sort_prop_include_center,
      exclude_fill_values = exclude_fill_values
    )

  y <- broom.helpers::.select_to_varnames(
    select = {{ y }},
    data = data,
    arg_name = "y",
    select_single = TRUE
  )

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
        exclude_fill_values = exclude_fill_values
      ),
      stat = "prop",
      complete = "fill",
      width = width
    )

  if (add_labels) {
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
  }

  if (add_totals) {
    dtot <- data %>%
      dplyr::group_by(.data[[y]], !!!facet_rows, !!!facet_cols) %>%
      dplyr::summarise(
        prop_lower = .prop_lower(
          .data$.answer,
          .data$.weights,
          include_center = TRUE,
          exclude_fill_values = exclude_fill_values
        ),
        prop_higher = .prop_higher(
          .data$.answer,
          .data$.weights,
          include_center = TRUE,
          exclude_fill_values = exclude_fill_values
        ),
        label_lower = .prop_lower(
          .data$.answer,
          .data$.weights,
          include_center = totals_include_center,
          exclude_fill_values = exclude_fill_values
        ),
        label_higher = .prop_higher(
          .data$.answer,
          .data$.weights,
          include_center = totals_include_center,
          exclude_fill_values = exclude_fill_values
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        label_lower =
          label_percent_abs(accuracy = totals_accuracy)(.data$label_lower),
        label_higher =
          label_percent_abs(accuracy = totals_accuracy)(.data$label_higher),
        x_lower = -1 * max(.data$prop_lower) - totals_hjust,
        x_higher = max(.data$prop_higher) + totals_hjust
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

  if (length(levels(data$.answer)) <= 11) {
    p <- p + scale_fill_brewer(palette = "BrBG")
  }

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
                          sort_method = c("prop", "mean", "median"),
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

  weights_var <- broom.helpers::.select_to_varnames(
    select = {{ weights }},
    data = data,
    arg_name = "weights",
    select_single = TRUE
  )
  if (is.null(weights_var)) {
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

  if (sort == "ascending" && sort_method == "prop") {
    data$.question <- data$.question %>%
      forcats::fct_reorder2(
        data$.answer,
        data$.weights,
        .fun = .prop_higher,
        include_center = sort_prop_include_center,
        exclude_fill_values = exclude_fill_values,
        .na_rm = FALSE,
        .desc = FALSE
      )
  }
  if (sort == "descending" && sort_method == "prop") {
    data$.question <- data$.question %>%
      forcats::fct_reorder2(
        data$.answer,
        data$.weights,
        .fun = .prop_higher,
        include_center = sort_prop_include_center,
        exclude_fill_values = exclude_fill_values,
        .na_rm = FALSE,
        .desc = TRUE
      )
  }
  if (sort == "ascending" && sort_method == "mean") {
    data$.question <- data$.question %>%
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
    data$.question <- data$.question %>%
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
    data$.question <- data$.question %>%
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
    data$.question <- data$.question %>%
      forcats::fct_reorder2(
        data$.answer,
        data$.weights,
        .fun = .sort_median,
        exclude_fill_values = exclude_fill_values,
        .na_rm = FALSE,
        .desc = TRUE
      )
  }

  data
}

# Compute the proportion being higher than the center
# Option to include the centre (if yes, only half taken into account)
.prop_higher <- function(x, w, include_center = TRUE,
                         exclude_fill_values = NULL) {
  N <- sum(as.integer(!is.na(x)) * w)
  if (!is.factor(x)) x <- factor(x)
  if (!is.null(exclude_fill_values)) {
    l <- levels(x)
    l <- l[!l %in% exclude_fill_values]
    x <- factor(x, levels = l)
  }
  m <- length(levels(x)) / 2 + 1 / 2
  x <- as.numeric(x)
  ic <- ifelse(include_center, 1 / 2, 0)
  sum(w * as.integer(x > m), w * ic * as.integer(x == m), na.rm = TRUE) / N
}

# Compute the proportion being higher than the center
# Option to include the centre (if yes, only half taken into account)
.prop_lower <- function(x, w, include_center = TRUE,
                        exclude_fill_values = NULL) {
  N <- sum(as.integer(!is.na(x)) * w)
  if (!is.factor(x)) x <- factor(x)
  if (!is.null(exclude_fill_values)) {
    l <- levels(x)
    l <- l[!l %in% exclude_fill_values]
    x <- factor(x, levels = l)
  }
  m <- length(levels(x)) / 2 + 1 / 2
  x <- as.numeric(x)
  ic <- ifelse(include_center, 1 / 2, 0)
  sum(w * as.integer(x < m), ic * w * as.integer(x == m), na.rm = TRUE) / N
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
                             sort_method = c("prop", "mean", "median"),
                             sort_prop_include_center = FALSE,
                             add_labels = TRUE,
                             labels_size = 3.5,
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
      exclude_fill_values = NULL
    )

  y <- broom.helpers::.select_to_varnames(
    select = {{ y }},
    data = data,
    arg_name = "y",
    select_single = TRUE
  )

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
      stat = "prop",
      complete = "fill",
      width = width
    )

  if (add_labels) {
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
        position = position_fill(
          vjust = .5,
          reverse = reverse_fill
        ),
        size = labels_size
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
    )

  if (length(levels(data$.answer)) <= 11) {
    p <- p + scale_fill_brewer(palette = "BrBG")
  }

  p
}
