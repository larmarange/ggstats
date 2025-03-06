#' Deprecated functions
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @inheritParams ggcoef_model
#' @param component_col name of the component column
#' @param component_label an optional named vector for labeling components
#' @export
ggcoef_multicomponents <- function(
    model,
    type = c("dodged", "faceted", "table"),
    component_col = "component",
    component_label = NULL,
    tidy_fun = broom.helpers::tidy_with_broom_or_parameters,
    tidy_args = NULL,
    conf.int = TRUE,
    conf.level = .95,
    exponentiate = FALSE,
    variable_labels = NULL,
    term_labels = NULL,
    interaction_sep = " * ",
    categorical_terms_pattern = "{level}",
    add_reference_rows = TRUE,
    no_reference_row = NULL,
    intercept = FALSE,
    include = dplyr::everything(),
    significance = 1 - conf.level,
    significance_labels = NULL,
    return_data = FALSE,
    table_stat = c("estimate", "ci", "p.value"),
    table_header = NULL,
    table_text_size = 3,
    table_stat_label = NULL,
    ci_pattern = "{conf.low}, {conf.high}",
    table_witdhs = c(3, 2),
    ...) {
  lifecycle::deprecate_soft(
    when = "0.9.0",
    what = "ggcoef_multicomponents()",
    with = "ggcoef_model()"
  )
  type <- match.arg(type)
  if (return_data && type == "table") type <- "faceted"
  if (type %in% c("dodged", "faceted")) {
    res <- ggcoef_multi_d_f(
      model = model,
      type = type,
      component_col = component_col,
      component_label = component_label,
      tidy_fun = tidy_fun,
      tidy_args = tidy_args,
      conf.int = conf.int,
      conf.level = conf.level,
      exponentiate = exponentiate,
      variable_labels = variable_labels,
      term_labels = term_labels,
      interaction_sep = interaction_sep,
      categorical_terms_pattern = categorical_terms_pattern,
      add_reference_rows = add_reference_rows,
      no_reference_row = {{ no_reference_row }},
      intercept = intercept,
      include = {{ include }},
      significance = significance,
      significance_labels = significance_labels,
      return_data = return_data,
      ...
    )
  } else {
    res <- ggcoef_multi_t(
      model = model,
      type = type,
      component_col = component_col,
      component_label = component_label,
      tidy_fun = tidy_fun,
      tidy_args = tidy_args,
      conf.int = conf.int,
      conf.level = conf.level,
      exponentiate = exponentiate,
      variable_labels = variable_labels,
      term_labels = term_labels,
      interaction_sep = interaction_sep,
      categorical_terms_pattern = categorical_terms_pattern,
      add_reference_rows = add_reference_rows,
      no_reference_row = {{ no_reference_row }},
      intercept = intercept,
      include = {{ include }},
      significance = significance,
      significance_labels = significance_labels,
      ...
    )
  }

  res
}

# dodged & faceted version
ggcoef_multi_d_f <- function(
    model,
    type = c("dodged", "faceted"),
    component_col = "component",
    component_label = NULL,
    tidy_fun = broom.helpers::tidy_with_broom_or_parameters,
    tidy_args = NULL,
    conf.int = TRUE,
    conf.level = .95,
    exponentiate = FALSE,
    variable_labels = NULL,
    term_labels = NULL,
    interaction_sep = " * ",
    categorical_terms_pattern = "{level}",
    add_reference_rows = TRUE,
    no_reference_row = NULL,
    intercept = FALSE,
    include = dplyr::everything(),
    significance = 1 - conf.level,
    significance_labels = NULL,
    return_data = FALSE,
    ...) {
  component_label_arg <- attr(model, "component_label_arg")
  if (is.null(component_label_arg)) component_label_arg <- "component_label"

  data <- ggcoef_data(
    model,
    tidy_fun = tidy_fun,
    tidy_args = {{ tidy_args }},
    conf.int = conf.int,
    conf.level = conf.level,
    exponentiate = exponentiate,
    variable_labels = variable_labels,
    term_labels = term_labels,
    interaction_sep = interaction_sep,
    categorical_terms_pattern = categorical_terms_pattern,
    add_reference_rows = add_reference_rows,
    no_reference_row = {{ no_reference_row }},
    intercept = intercept,
    include = {{ include }},
    significance = significance,
    significance_labels = significance_labels
  )

  if (!component_col %in% names(data)) data[[component_col]] <- " "
  data[[component_col]] <- .in_order(data[[component_col]])
  if (!is.null(component_label)) {
    if (
      is.null(names(component_label)) ||
        any(names(component_label) == "")
    ) {
      cli::cli_abort(
        "All elements of {.arg {component_label_arg}} should be named."
      )
    }
    keep <- names(component_label) %in% levels(data[[component_col]])
    drop <- component_label[!keep]
    if (length(drop) > 0) {
      cli::cli_alert_warning(c(
        "Error in {.arg {component_label_arg}}:\n",
        "value{?s} {.strong {drop}} not found in the data and ignored."
      ))
    }
    component_label <- component_label[keep]

    missing_levels <- setdiff(
      levels(.in_order(data[[component_col]])),
      names(component_label)
    )
    names(missing_levels) <- missing_levels
    data[[component_col]] <- factor(
      data[[component_col]],
      levels = c(names(component_label), missing_levels),
      labels = c(component_label, missing_levels)
    )
  }

  if (return_data) {
    return(data)
  }

  type <- match.arg(type)

  args <- list(...)
  args$data <- data
  args$exponentiate <- exponentiate
  if (!"y" %in% names(args) && !"facet_row" %in% names(args)) {
    args$y <- "label_light"
  }

  if (type == "dodged") {
    if (!"dodged " %in% names(args)) {
      args$dodged <- TRUE
    }
    if (!"colour" %in% names(args)) {
      args$colour <- component_col
    }
    if (!"errorbar_coloured" %in% names(args)) {
      args$errorbar_coloured <- TRUE
    }
  } else {
    if (!"facet_col" %in% names(args)) {
      args$facet_col <- component_col
    }
    if (!"colour" %in% names(args) && !all(is.na(data$var_label))) {
      args$colour <- "var_label"
      if (!"colour_guide" %in% names(args)) {
        args$colour_guide <- FALSE
      }
    }
  }

  do.call(ggcoef_plot, args)
}

# table version
ggcoef_multi_t <- function(
    model,
    type = c("table"),
    component_col = "component",
    component_label = NULL,
    tidy_fun = broom.helpers::tidy_with_broom_or_parameters,
    tidy_args = NULL,
    conf.int = TRUE,
    conf.level = .95,
    exponentiate = FALSE,
    variable_labels = NULL,
    term_labels = NULL,
    interaction_sep = " * ",
    categorical_terms_pattern = "{level}",
    add_reference_rows = TRUE,
    no_reference_row = NULL,
    intercept = FALSE,
    include = dplyr::everything(),
    significance = 1 - conf.level,
    significance_labels = NULL,
    table_stat = c("estimate", "ci", "p.value"),
    table_header = NULL,
    table_text_size = 3,
    table_stat_label = NULL,
    ci_pattern = "{conf.low}, {conf.high}",
    table_witdhs = c(3, 2),
    ...) {
  type <- match.arg(type)
  component_label_arg <- attr(model, "component_label_arg")
  if (is.null(component_label_arg)) component_label_arg <- "component_label"

  data <- ggcoef_data(
    model = model,
    tidy_fun = tidy_fun,
    tidy_args = {{ tidy_args }},
    conf.int = conf.int,
    conf.level = conf.level,
    exponentiate = exponentiate,
    variable_labels = variable_labels,
    term_labels = term_labels,
    interaction_sep = interaction_sep,
    categorical_terms_pattern = categorical_terms_pattern,
    add_reference_rows = add_reference_rows,
    no_reference_row = {{ no_reference_row }},
    intercept = intercept,
    include = {{ include }},
    significance = significance,
    significance_labels = significance_labels
  )

  if (!component_col %in% names(data)) data[[component_col]] <- " "

  data[[component_col]] <- .in_order(data[[component_col]])

  if (!is.null(component_label)) {
    if (
      is.null(names(component_label)) ||
        any(names(component_label) == "")
    ) {
      cli::cli_abort(
        "All elements of {.arg {component_label_arg}} should be named."
      )
    }

    keep <- names(component_label) %in% levels(data[[component_col]])
    drop <- component_label[!keep]
    if (length(drop) > 0) {
      cli::cli_alert_warning(c(
        "Error in {.arg {component_label_arg}}:\n",
        "value{?s} {.strong {drop}} not found in the data and ignored."
      ))
    }
    component_label <- component_label[keep]

    missing_levels <- setdiff(
      levels(.in_order(data[[component_col]])),
      names(component_label)
    )
    names(missing_levels) <- missing_levels
    data[[component_col]] <- factor(
      data[[component_col]],
      levels = c(names(component_label), missing_levels),
      labels = c(component_label, missing_levels)
    )
  }

  res <- levels(data[[component_col]]) |>
    purrr::map(
      ~ ggcoef_table(
        data = dplyr::filter(data, .data[[component_col]] == .x),
        plot_title = .x,
        model = model,
        tidy_fun = tidy_fun,
        tidy_args = tidy_args,
        conf.int = conf.int,
        conf.level = conf.level,
        exponentiate = exponentiate,
        variable_labels = variable_labels,
        term_labels = term_labels,
        interaction_sep = interaction_sep,
        categorical_terms_pattern = categorical_terms_pattern,
        add_reference_rows = add_reference_rows,
        no_reference_row = {{ no_reference_row }},
        intercept = intercept,
        include = {{ include }},
        significance = significance,
        significance_labels = significance_labels,
        show_p_values = FALSE,
        signif_stars = FALSE,
        table_stat = table_stat,
        table_header = table_header,
        table_text_size = table_text_size,
        table_stat_label = table_stat_label,
        ci_pattern = ci_pattern,
        table_witdhs = table_witdhs
      )
    )
  patchwork::wrap_plots(res, ncol = 1)
}

#' @rdname ggcoef_multicomponents
#' @param y.level_label an optional named vector for labeling `y.level`
#'   (see examples)
#' @export
ggcoef_multinom <- function(
    model,
    type = c("dodged", "faceted", "table"),
    y.level_label = NULL,
    tidy_fun = broom.helpers::tidy_with_broom_or_parameters,
    tidy_args = NULL,
    conf.int = TRUE,
    conf.level = .95,
    exponentiate = FALSE,
    variable_labels = NULL,
    term_labels = NULL,
    interaction_sep = " * ",
    categorical_terms_pattern = "{level}",
    add_reference_rows = TRUE,
    no_reference_row = NULL,
    intercept = FALSE,
    include = dplyr::everything(),
    significance = 1 - conf.level,
    significance_labels = NULL,
    return_data = FALSE,
    table_stat = c("estimate", "ci", "p.value"),
    table_header = NULL,
    table_text_size = 3,
    table_stat_label = NULL,
    ci_pattern = "{conf.low}, {conf.high}",
    table_witdhs = c(3, 2),
    ...) {
  lifecycle::deprecate_soft(
    when = "0.9.0",
    what = "ggcoef_multinom()",
    with = "ggcoef_model()"
  )
  type <- match.arg(type)
  attr(model, "component_label_arg") <- "y.level_label"
  ggcoef_multicomponents(
    model = model,
    type = type,
    component_col = "y.level",
    component_label = y.level_label,
    tidy_fun = tidy_fun,
    tidy_args = tidy_args,
    conf.int = conf.int,
    conf.level = conf.level,
    exponentiate = exponentiate,
    variable_labels = variable_labels,
    term_labels = term_labels,
    interaction_sep = interaction_sep,
    categorical_terms_pattern = categorical_terms_pattern,
    add_reference_rows = add_reference_rows,
    no_reference_row = {{ no_reference_row }},
    intercept = intercept,
    include = {{ include }},
    significance = significance,
    significance_labels = significance_labels,
    return_data = return_data,
    table_stat = table_stat,
    table_header = table_header,
    table_text_size = table_text_size,
    table_stat_label = table_stat_label,
    ci_pattern = ci_pattern,
    table_witdhs = table_witdhs,
    ...
  )
}
