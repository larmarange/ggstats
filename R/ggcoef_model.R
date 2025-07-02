#' Plot model coefficients
#'
#' `ggcoef_model()`, `ggcoef_table()`, `ggcoef_dodged()`,
#' `ggcoef_faceted()` and `ggcoef_compare()`
#' use [broom.helpers::tidy_plus_plus()]
#' to obtain a `tibble` of the model coefficients,
#' apply additional data transformation and then pass the
#' produced `tibble` to `ggcoef_plot()` to generate the plot.
#'
#' For more control, you can use the argument `return_data = TRUE` to
#' get the produced `tibble`, apply any transformation of your own and
#' then pass your customized `tibble` to `ggcoef_plot()`.
#' @inheritParams broom.helpers::tidy_plus_plus
#' @param tidy_args Additional arguments passed to
#' [broom.helpers::tidy_plus_plus()] and to `tidy_fun`
#' @param model a regression model object
#' @param conf.level the confidence level to use for the confidence
#'   interval if `conf.int = TRUE`; must be strictly greater than 0
#'   and less than 1; defaults to 0.95, which corresponds to a 95
#'   percent confidence interval
#' @param show_p_values if `TRUE`, add p-value to labels
#' @param signif_stars if `TRUE`, add significant stars to labels
#' @param significance level (between 0 and 1) below which a
#'   coefficient is consider to be significantly different from 0
#'   (or 1 if `exponentiate = TRUE`), `NULL` for not highlighting
#'   such coefficients
#' @param significance_labels optional vector with custom labels
#'   for significance variable
#' @param return_data if `TRUE`, will return the data.frame used
#'   for plotting instead of the plot
#' @param ... parameters passed to [ggcoef_plot()]
#' @return A `ggplot2` plot or a `tibble` if `return_data = TRUE`.
#' @export
#' @examples
#' mod <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
#' ggcoef_model(mod)
#'
#' ggcoef_table(mod)
#'
#'
#' \donttest{
#' ggcoef_table(mod, table_stat = c("estimate", "ci"))
#'
#' ggcoef_table(
#'   mod,
#'   table_stat_label = list(
#'     estimate = scales::label_number(.001)
#'   )
#' )
#'
#' ggcoef_table(mod, table_text_size = 5, table_widths = c(1, 1))
#'
#' # a logistic regression example
#' d_titanic <- as.data.frame(Titanic)
#' d_titanic$Survived <- factor(d_titanic$Survived, c("No", "Yes"))
#' mod_titanic <- glm(
#'   Survived ~ Sex * Age + Class,
#'   weights = Freq,
#'   data = d_titanic,
#'   family = binomial
#' )
#'
#' # use 'exponentiate = TRUE' to get the Odds Ratio
#' ggcoef_model(mod_titanic, exponentiate = TRUE)
#'
#' ggcoef_table(mod_titanic, exponentiate = TRUE)
#'
#' # display intercepts
#' ggcoef_model(mod_titanic, exponentiate = TRUE, intercept = TRUE)
#'
#' # customize terms labels
#' ggcoef_model(
#'   mod_titanic,
#'   exponentiate = TRUE,
#'   show_p_values = FALSE,
#'   signif_stars = FALSE,
#'   add_reference_rows = FALSE,
#'   categorical_terms_pattern = "{level} (ref: {reference_level})",
#'   interaction_sep = " x ",
#'   y_labeller = scales::label_wrap(15)
#' )
#'
#' # display only a subset of terms
#' ggcoef_model(mod_titanic, exponentiate = TRUE, include = c("Age", "Class"))
#'
#' # do not change points' shape based on significance
#' ggcoef_model(mod_titanic, exponentiate = TRUE, significance = NULL)
#'
#' # a black and white version
#' ggcoef_model(
#'   mod_titanic,
#'   exponentiate = TRUE,
#'   colour = NULL, stripped_rows = FALSE
#' )
#'
#' # show dichotomous terms on one row
#' ggcoef_model(
#'   mod_titanic,
#'   exponentiate = TRUE,
#'   no_reference_row = broom.helpers::all_dichotomous(),
#'   categorical_terms_pattern =
#'     "{ifelse(dichotomous, paste0(level, ' / ', reference_level), level)}",
#'   show_p_values = FALSE
#' )
#' }
#' @examplesIf requireNamespace("reshape")
#'
#' \donttest{
#' data(tips, package = "reshape")
#' mod_simple <- lm(tip ~ day + time + total_bill, data = tips)
#' ggcoef_model(mod_simple)
#'
#' # custom variable labels
#' # you can use the labelled package to define variable labels
#' # before computing model
#' if (requireNamespace("labelled")) {
#'   tips_labelled <- tips |>
#'     labelled::set_variable_labels(
#'       day = "Day of the week",
#'       time = "Lunch or Dinner",
#'       total_bill = "Bill's total"
#'     )
#'   mod_labelled <- lm(tip ~ day + time + total_bill, data = tips_labelled)
#'   ggcoef_model(mod_labelled)
#' }
#'
#' # you can provide custom variable labels with 'variable_labels'
#' ggcoef_model(
#'   mod_simple,
#'   variable_labels = c(
#'     day = "Week day",
#'     time = "Time (lunch or dinner ?)",
#'     total_bill = "Total of the bill"
#'   )
#' )
#' # if labels are too long, you can use 'facet_labeller' to wrap them
#' ggcoef_model(
#'   mod_simple,
#'   variable_labels = c(
#'     day = "Week day",
#'     time = "Time (lunch or dinner ?)",
#'     total_bill = "Total of the bill"
#'   ),
#'   facet_labeller = ggplot2::label_wrap_gen(10)
#' )
#'
#' # do not display variable facets but add colour guide
#' ggcoef_model(mod_simple, facet_row = NULL, colour_guide = TRUE)
#'
#' # works also with with polynomial terms
#' mod_poly <- lm(
#'   tip ~ poly(total_bill, 3) + day,
#'   data = tips,
#' )
#' ggcoef_model(mod_poly)
#'
#' # or with different type of contrasts
#' # for sum contrasts, the value of the reference term is computed
#' if (requireNamespace("emmeans")) {
#'   mod2 <- lm(
#'     tip ~ day + time + sex,
#'     data = tips,
#'     contrasts = list(time = contr.sum, day = contr.treatment(4, base = 3))
#'   )
#'   ggcoef_model(mod2)
#' }
#' }
#' @examplesIf requireNamespace("nnet") && requireNamespace("gtsummary")
#'
#' \donttest{
#' # multinomial model
#' mod <- nnet::multinom(grade ~ stage + trt + age, data = gtsummary::trial)
#' ggcoef_model(mod, exponentiate = TRUE)
#' ggcoef_table(mod, group_labels = c(II = "Stage 2 vs. 1"))
#' ggcoef_dodged(mod, exponentiate = TRUE)
#' ggcoef_faceted(mod, exponentiate = TRUE)
#' }
#' @examplesIf requireNamespace("pscl")
#' \donttest{
#' library(pscl)
#' data("bioChemists", package = "pscl")
#' mod <- zeroinfl(art ~ fem * mar | fem + mar, data = bioChemists)
#' ggcoef_model(mod)
#' ggcoef_table(mod)
#' ggcoef_dodged(mod)
#' ggcoef_faceted(
#'   mod,
#'   group_labels = c(conditional = "Count", zero_inflated = "Zero-inflated")
#' )
#'
#' mod2 <- zeroinfl(art ~ fem + mar | 1, data = bioChemists)
#' ggcoef_table(mod2)
#' ggcoef_table(mod2, intercept = TRUE)
#' }
ggcoef_model <- function(
    model,
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
    group_by = broom.helpers::auto_group_by(),
    group_labels = NULL,
    add_pairwise_contrasts = FALSE,
    pairwise_variables = broom.helpers::all_categorical(),
    keep_model_terms = FALSE,
    pairwise_reverse = TRUE,
    emmeans_args = list(),
    significance = 1 - conf.level,
    significance_labels = NULL,
    show_p_values = TRUE,
    signif_stars = TRUE,
    return_data = FALSE,
    ...) {
  args <- list(...)

  # undocumented feature, we can pass directly `data`
  if (is.null(args$data)) {
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
      group_by = {{ group_by }},
      group_labels = group_labels,
      add_pairwise_contrasts = add_pairwise_contrasts,
      pairwise_variables = {{ pairwise_variables }},
      keep_model_terms = keep_model_terms,
      pairwise_reverse = pairwise_reverse,
      emmeans_args = emmeans_args,
      significance = significance,
      significance_labels = significance_labels
    )
  } else {
    data <- args$data
  }

  if (show_p_values && signif_stars) {
    data$add_to_label <- paste0(data$p_value_label, data$signif_stars)
  }
  if (show_p_values && !signif_stars) {
    data$add_to_label <- data$p_value_label
  }
  if (!show_p_values && signif_stars) {
    data$add_to_label <- data$signif_stars
  }

  if (show_p_values || signif_stars) {
    data$label <- forcats::fct_inorder(
      factor(
        paste0(
          data$label,
          ifelse(
            data$add_to_label == "",
            "",
            paste0(" (", data$add_to_label, ")")
          )
        )
      )
    )
    data$label_light <- forcats::fct_inorder(
      factor(
        paste0(
          data$label_light,
          ifelse(
            data$add_to_label == "",
            "",
            paste0(" (", data$add_to_label, ")")
          )
        )
      )
    )
  }

  if (return_data) {
    return(data)
  }

  args$exponentiate <- exponentiate

  if (!"y" %in% names(args) && !"facet_row" %in% names(args)) {
    args$y <- "label_light"
  }

  if (!"colour" %in% names(args) && !all(is.na(data$var_label))) {
    args$colour <- "var_label"
    if (!"colour_guide" %in% names(args)) {
      args$colour_guide <- FALSE
    }
  }

  if ("group_by" %in% names(data)) {
    d <- data |>
      tidyr::nest(.by = dplyr::all_of("group_by"))
    purrr::map2(
      d$data,
      d$group_by,
      ~ .call_ggcoef_plot(args, .x, plot_title = as.character(.y))
    ) |> patchwork::wrap_plots(ncol = 1)
  } else {
    .call_ggcoef_plot(args, data)
  }
}

#' @describeIn ggcoef_model a variation of [ggcoef_model()] adding a table
#' with estimates, confidence intervals and p-values
#' @param table_stat statistics to display in the table, use any column name
#' returned by the tidier or `"ci"` for confidence intervals formatted
#' according to `ci_pattern`
#' @param table_header optional custom headers for the table
#' @param table_text_size text size for the table
#' @param table_stat_label optional named list of labeller functions for the
#' displayed statistic (see examples)
#' @param ci_pattern glue pattern for confidence intervals in the table
#' @param table_widths relative widths of the forest plot and the coefficients
#' table
#' @param table_witdhs `r lifecycle::badge("deprecated")`\cr
#' use `table_widths` instead
#' @importFrom lifecycle deprecated
#' @export
ggcoef_table <- function(
    model,
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
    group_by = broom.helpers::auto_group_by(),
    group_labels = NULL,
    add_pairwise_contrasts = FALSE,
    pairwise_variables = broom.helpers::all_categorical(),
    keep_model_terms = FALSE,
    pairwise_reverse = TRUE,
    emmeans_args = list(),
    significance = 1 - conf.level,
    significance_labels = NULL,
    show_p_values = FALSE,
    signif_stars = FALSE,
    table_stat = c("estimate", "ci", "p.value"),
    table_header = NULL,
    table_text_size = 3,
    table_stat_label = NULL,
    ci_pattern = "{conf.low}, {conf.high}",
    table_widths = c(3, 2),
    table_witdhs = deprecated(),
    ...) {
  if (lifecycle::is_present(table_witdhs)) {
    lifecycle::deprecate_warn(
      "0.10.0",
      "ggcoef_table(table_witdhs)",
      "ggcoef_table(table_widths)"
    )
    table_widths <- table_witdhs
  }

  args <- list(...)

  # undocumented feature, we can pass directly `data`
  if (is.null(args$data)) {
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
      group_by = {{ group_by }},
      group_labels = group_labels,
      add_pairwise_contrasts = add_pairwise_contrasts,
      pairwise_variables = {{ pairwise_variables }},
      keep_model_terms = keep_model_terms,
      pairwise_reverse = pairwise_reverse,
      emmeans_args = emmeans_args,
      significance = significance,
      significance_labels = significance_labels
    )
  } else {
    data <- args$data
  }

  if ("group_by" %in% colnames(data)) {
    d <- data |>
      tidyr::nest(.by = dplyr::all_of("group_by"))
    res <- purrr::map2(
      d$data,
      d$group_by,
      ~ ggcoef_table(
        data = .x,
        plot_title = .y,
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
        show_p_values = show_p_values,
        signif_stars = signif_stars,
        table_stat = table_stat,
        table_header = table_header,
        table_text_size = table_text_size,
        table_stat_label = table_stat_label,
        ci_pattern = ci_pattern,
        table_widths = table_widths
      )
    ) |> patchwork::wrap_plots(ncol = 1)
    return(res)
  }

  if (show_p_values && signif_stars) {
    data$add_to_label <- paste0(data$p_value_label, data$signif_stars)
  }
  if (show_p_values && !signif_stars) {
    data$add_to_label <- data$p_value_label
  }
  if (!show_p_values && signif_stars) {
    data$add_to_label <- data$signif_stars
  }

  if (show_p_values || signif_stars) {
    data$label <- forcats::fct_inorder(
      factor(
        paste0(
          data$label,
          ifelse(
            data$add_to_label == "",
            "",
            paste0(" (", data$add_to_label, ")")
          )
        )
      )
    )
    data$label_light <- forcats::fct_inorder(
      factor(
        paste0(
          data$label_light,
          ifelse(
            data$add_to_label == "",
            "",
            paste0(" (", data$add_to_label, ")")
          )
        )
      )
    )
  }

  args$data <- data
  args$exponentiate <- exponentiate

  if (!"y" %in% names(args) && !"facet_row" %in% names(args)) {
    args$y <- "label_light"
  }

  if (!"colour" %in% names(args) && !all(is.na(data$var_label))) {
    args$colour <- "var_label"
    if (!"colour_guide" %in% names(args)) {
      args$colour_guide <- FALSE
    }
  }

  if (!"y" %in% names(args)) args$y <- "label"
  if (!"facet_row" %in% names(args)) args$facet_row <- "var_label"
  if (!"stripped_rows" %in% names(args)) args$stripped_rows <- TRUE
  if (!"strips_odd" %in% names(args)) args$strips_odd <- "#11111111"
  if (!"strips_even" %in% names(args)) args$strips_even <- "#00000000"

  coef_plot <- do.call(ggcoef_plot, args)

  if (args$stripped_rows) {
    if (!"term" %in% names(data)) {
      data$term <- data[[args$y]]
    }
    data <- data |>
      dplyr::mutate(.fill = dplyr::if_else(
        as.integer(.in_order(.data$term)) %% 2L == 1,
        args$strips_even,
        args$strips_odd
      ))
  }

  # building the coefficient table
  tbl_data <- data

  if (!"estimate" %in% names(table_stat_label)) {
    table_stat_label$estimate <- scales::label_number(accuracy = .1)
  }
  if (!"conf.low" %in% names(table_stat_label)) {
    table_stat_label$conf.low <- scales::label_number(accuracy = .1)
  }
  if (!"conf.high" %in% names(table_stat_label)) {
    table_stat_label$conf.high <- scales::label_number(accuracy = .1)
  }
  if (!"p.value" %in% names(table_stat_label)) {
    table_stat_label$p.value <- scales::label_pvalue(add_p = FALSE)
  }
  for (v in names(table_stat_label)) {
    tbl_data[[v]] <- table_stat_label[[v]](tbl_data[[v]])
    tbl_data[[v]][is.na(tbl_data[[v]])] <- ""
  }

  tbl_data$ci <- stringr::str_glue_data(tbl_data, ci_pattern)
  tbl_data$ci[is.na(data$conf.low) & is.na(data$conf.high)] <- " "
  tbl_data <- tbl_data |>
    tidyr::pivot_longer(
      dplyr::any_of(table_stat),
      names_to = "stat",
      values_to = "value",
      values_transform = as.character
    )
  tbl_data$stat <- factor(tbl_data$stat, levels = table_stat)

  if (!is.null(table_header) && length(table_header) != length(table_stat)) {
    cli::cli_abort("{.arg table_header} should have the same length as {.arg table_stat}.") # nolint
  }

  if (is.null(table_header)) {
    table_header <- table_stat
    if ("estimate" %in% table_header) {
      table_header[table_header == "estimate"] <-
        attr(data, "coefficients_label")
    }
    if ("ci" %in% table_header) {
      table_header[table_header == "ci"] <-
        paste(scales::percent(conf.level), "CI")
    }
    if ("p.value" %in% table_header) {
      table_header[table_header == "p.value"] <- "p"
    }
  }

  table_plot <- ggplot2::ggplot(tbl_data) +
    ggplot2::aes(
      x = .data[["stat"]],
      y = .data[[args$y]],
      label = .data[["value"]]
    ) +
    ggplot2::geom_text(hjust = .5, vjust = .5, size = table_text_size) +
    ggplot2::scale_x_discrete(position = "top", labels = table_header) +
    ggplot2::scale_y_discrete(
      limits = rev,
      expand = ggplot2::expansion(mult = 0, add = .5)
    ) +
    ggplot2::facet_grid(
      rows = args$facet_row,
      scales = "free_y", space = "free_y", switch = "y"
    ) +
    ggplot2::theme_light() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(face = "bold", hjust = .5),
      axis.text.y = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )

  if (args$stripped_rows) {
    table_plot <- table_plot +
      geom_stripped_rows(
        mapping = ggplot2::aes(
          odd = .data[[".fill"]], even = .data[[".fill"]],
          colour = NULL, linetype = NULL
        )
      )
  }

  # join the plots
  patchwork::wrap_plots(coef_plot, table_plot, nrow = 1, widths = table_widths)
}

#' @describeIn ggcoef_model a dodged variation of [ggcoef_model()] for
#' multi groups models
#' @export
ggcoef_dodged <- function(
    model,
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
    group_by = broom.helpers::auto_group_by(),
    group_labels = NULL,
    significance = 1 - conf.level,
    significance_labels = NULL,
    return_data = FALSE,
    ...) {

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
    group_by = {{ group_by }},
    group_labels = group_labels,
    significance = significance,
    significance_labels = significance_labels
  )

  if (return_data) {
    return(data)
  }

  if (!"group_by" %in% colnames(data))
    cli::cli_abort("No groups detected. Define groups with {.arg group_by}.")

  args <- list(...)
  args$exponentiate <- exponentiate
  if (!"y" %in% names(args) && !"facet_row" %in% names(args)) {
    args$y <- "label_light"
  }

  if (!"dodged " %in% names(args)) {
    args$dodged <- TRUE
  }
  if (!"colour" %in% names(args)) {
    args$colour <- "group_by"
  }
  if (!"errorbar_coloured" %in% names(args)) {
    args$errorbar_coloured <- TRUE
  }

  .call_ggcoef_plot(args, data)
}

#' @describeIn ggcoef_model a faceted variation of [ggcoef_model()] for
#' multi groups models
#' @export
ggcoef_faceted <- function(
    model,
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
    group_by = broom.helpers::auto_group_by(),
    group_labels = NULL,
    significance = 1 - conf.level,
    significance_labels = NULL,
    return_data = FALSE,
    ...) {

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
    group_by = {{ group_by }},
    group_labels = group_labels,
    significance = significance,
    significance_labels = significance_labels
  )

  if (return_data) {
    return(data)
  }

  if (!"group_by" %in% colnames(data))
    cli::cli_abort("No groups detected. Define groups with {.arg group_by}.")

  args <- list(...)
  args$exponentiate <- exponentiate
  if (!"y" %in% names(args) && !"facet_row" %in% names(args)) {
    args$y <- "label_light"
  }

  if (!"facet_col" %in% names(args)) {
    args$facet_col <- "group_by"
  }
  if (!"colour" %in% names(args) && !all(is.na(data$var_label))) {
    args$colour <- "var_label"
    if (!"colour_guide" %in% names(args)) {
      args$colour_guide <- FALSE
    }
  }

  .call_ggcoef_plot(args, data)
}

#' @describeIn ggcoef_model designed for displaying several models on the same
#'   plot.
#' @export
#' @param models named list of models
#' @param type a dodged plot, a faceted plot or multiple table plots?
#' @examples
#' \donttest{
#' # Use ggcoef_compare() for comparing several models on the same plot
#' mod1 <- lm(Fertility ~ ., data = swiss)
#' mod2 <- step(mod1, trace = 0)
#' mod3 <- lm(Fertility ~ Agriculture + Education * Catholic, data = swiss)
#' models <- list(
#'   "Full model" = mod1,
#'   "Simplified model" = mod2,
#'   "With interaction" = mod3
#' )
#'
#' ggcoef_compare(models)
#' ggcoef_compare(models, type = "faceted")
#'
#' # you can reverse the vertical position of the point by using a negative
#' # value for dodged_width (but it will produce some warnings)
#' ggcoef_compare(models, dodged_width = -.9)
#' }
ggcoef_compare <- function(
    models,
    type = c("dodged", "faceted"),
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
    add_pairwise_contrasts = FALSE,
    pairwise_variables = broom.helpers::all_categorical(),
    keep_model_terms = FALSE,
    pairwise_reverse = TRUE,
    emmeans_args = list(),
    significance = 1 - conf.level,
    significance_labels = NULL,
    return_data = FALSE,
    ...) {
  data <- lapply(
    X = models,
    FUN = ggcoef_data,
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
    add_pairwise_contrasts = add_pairwise_contrasts,
    pairwise_variables = {{ pairwise_variables }},
    keep_model_terms = keep_model_terms,
    pairwise_reverse = pairwise_reverse,
    emmeans_args = emmeans_args,
    significance = significance,
    significance_labels = significance_labels
  )

  data <- dplyr::bind_rows(data, .id = "model")
  coefficients_label <- attr(data, "coefficients_label")

  data$model <- .in_order(data$model)
  data$term <- .in_order(data$term)
  data$var_label <- .in_order(data$var_label)
  data$variable <- .in_order(data$variable)
  data$label <- .in_order(data$label)

  # include should be applied after lapply
  data <- data |>
    broom.helpers::tidy_select_variables(
      include = {{ include }},
      model = models[[1]] # just need to pass 1 model for the function to work
    ) |>
    broom.helpers::tidy_detach_model()

  # Add NA values for unobserved combinations
  # (i.e. for a term present in one model but not in another)
  data <- data |>
    tidyr::complete(
      .data$model,
      tidyr::nesting(
        !!sym("var_label"), !!sym("variable"), !!sym("var_class"),
        !!sym("var_type"), !!sym("contrasts"),
        !!sym("label"), !!sym("label_light"), !!sym("term")
      )
    ) |>
    # order lost after nesting
    dplyr::arrange(.data$model, .data$variable, .data$term)

  attr(data, "coefficients_label") <- coefficients_label

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
      args$colour <- "model"
    }
    if (!"errorbar_coloured" %in% names(args)) {
      args$errorbar_coloured <- TRUE
    }
  } else {
    if (!"facet_col" %in% names(args)) {
      args$facet_col <- "model"
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

# not exporting ggcoef_data
ggcoef_data <- function(
    model,
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
    group_by = broom.helpers::auto_group_by(),
    group_labels = NULL,
    add_pairwise_contrasts = FALSE,
    pairwise_variables = broom.helpers::all_categorical(),
    keep_model_terms = FALSE,
    pairwise_reverse = TRUE,
    emmeans_args = list(),
    significance = conf.level,
    significance_labels = NULL) {
  rlang::check_installed("broom.helpers")

  if (length(significance) == 0) {
    significance <- NULL
  }

  data <- rlang::inject(broom.helpers::tidy_plus_plus(
    model = model,
    tidy_fun = tidy_fun,
    conf.int = conf.int,
    conf.level = conf.level,
    exponentiate = exponentiate,
    variable_labels = variable_labels,
    term_labels = term_labels,
    interaction_sep = interaction_sep,
    categorical_terms_pattern = categorical_terms_pattern,
    add_reference_rows = add_reference_rows,
    no_reference_row = {{ no_reference_row }},
    add_pairwise_contrasts = add_pairwise_contrasts,
    pairwise_variables = {{ pairwise_variables }},
    keep_model_terms = keep_model_terms,
    pairwise_reverse = pairwise_reverse,
    emmeans_args = emmeans_args,
    add_estimate_to_reference_rows = TRUE,
    add_header_rows = FALSE,
    intercept = intercept,
    include = {{ include }},
    group_by = {{ group_by }},
    group_labels = group_labels,
    keep_model = FALSE,
    !!!tidy_args
  ))

  if (!"p.value" %in% names(data)) {
    data$p.value <- NA_real_
    significance <- NULL
  }

  if (!is.null(significance)) {
    if (is.null(significance_labels)) {
      significance_labels <- paste(c("p <=", "p >"), significance)
    }
    data$significance <- factor(
      !is.na(data$p.value) & data$p.value <= significance,
      levels = c(TRUE, FALSE),
      labels = significance_labels
    )
  }

  data$signif_stars <- signif_stars(data$p.value, point = NULL)

  data$p_value_label <- ifelse(
    is.na(data$p.value),
    "",
    scales::pvalue(data$p.value, add_p = TRUE)
  )

  # keep only rows with estimate
  data <- data[!is.na(data$estimate), ]

  data$term <- .in_order(data$term)
  data$var_label <- .in_order(data$var_label)
  data$variable <- .in_order(data$variable)
  data$label <- .in_order(data$label)

  data$label_light <- dplyr::if_else(
    as.character(data$label) == as.character(data$var_label) &
      ((!grepl("^nmatrix", data$var_class)) | is.na(data$var_class)),
    "",
    as.character(data$label)
  ) |>
    .in_order()

  data
}

#' @describeIn ggcoef_model plot a tidy `tibble` of coefficients
#' @param data a data frame containing data to be plotted,
#' typically the output of `ggcoef_model()`, `ggcoef_compare()`
#' or `ggcoef_multinom()` with the option `return_data = TRUE`
#' @param x,y variables mapped to x and y axis
#' @param exponentiate if `TRUE` a logarithmic scale will
#' be used for x-axis
#' @param y_labeller optional function to be applied on y labels (see examples)
#' @param point_size size of the points
#' @param point_stroke thickness of the points
#' @param point_fill fill colour for the points
#' @param colour optional variable name to be mapped to
#' colour aesthetic
#' @param colour_guide should colour guide be displayed
#' in the legend?
#' @param colour_lab label of the colour aesthetic in the legend
#' @param colour_labels labels argument passed to
#' [ggplot2::scale_colour_discrete()] and
#' [ggplot2::discrete_scale()]
#' @param shape optional variable name to be mapped to the
#' shape aesthetic
#' @param shape_values values of the different shapes to use in
#' [ggplot2::scale_shape_manual()]
#' @param shape_guide should shape guide be displayed in the legend?
#' @param shape_lab label of the shape aesthetic in the legend
#' @param errorbar should error bars be plotted?
#' @param errorbar_height height of error bars
#' @param errorbar_coloured should error bars be colored as the points?
#' @param stripped_rows should stripped rows be displayed in the background?
#' @param strips_odd color of the odd rows
#' @param strips_even color of the even rows
#' @param vline should a vertical line be drawn at 0 (or 1 if
#'   `exponentiate = TRUE`)?
#' @param vline_colour colour of vertical line
#' @param dodged should points be dodged (according to the colour aesthetic)?
#' @param dodged_width width value for [ggplot2::position_dodge()]
#' @param facet_row variable name to be used for row facets
#' @param facet_col optional variable name to be used for column facets
#' @param facet_labeller labeller function to be used for labeling facets;
#'   if labels are too long, you can use [ggplot2::label_wrap_gen()] (see
#'   examples), more information in the documentation of [ggplot2::facet_grid()]
#' @param plot_title an optional plot title
#' @seealso `vignette("ggcoef_model")`
#' @export
ggcoef_plot <- function(
    data,
    x = "estimate",
    y = "label",
    exponentiate = FALSE,
    y_labeller = NULL,
    point_size = 2,
    point_stroke = 2,
    point_fill = "white",
    colour = NULL,
    colour_guide = TRUE,
    colour_lab = "",
    colour_labels = ggplot2::waiver(),
    shape = "significance",
    shape_values = c(16, 21),
    shape_guide = TRUE,
    shape_lab = "",
    errorbar = TRUE,
    errorbar_height = .1,
    errorbar_coloured = FALSE,
    stripped_rows = TRUE,
    strips_odd = "#11111111",
    strips_even = "#00000000",
    vline = TRUE,
    vline_colour = "grey50",
    dodged = FALSE,
    dodged_width = .8,
    facet_row = "var_label",
    facet_col = NULL,
    facet_labeller = "label_value",
    plot_title = NULL) {

  if (!is.null(facet_row)) {
    data[[facet_row]] <- .in_order(data[[facet_row]])
  }
  data[[y]] <- forcats::fct_rev(.in_order(data[[y]]))
  if (!"term" %in% names(data)) {
    data$term <- data[[y]]
  }
  data$term <- forcats::fct_rev(.in_order(data$term))

  if (stripped_rows) {
    data <- data |>
      dplyr::mutate(.fill = dplyr::if_else(
        as.integer(.in_order(.data$term)) %% 2L == 1,
        strips_even,
        strips_odd
      ))
  }

  # mapping
  mapping <- ggplot2::aes(x = .data[[x]], y = .data$term)

  errorbar <- errorbar & all(c("conf.low", "conf.high") %in% names(data))
  if (errorbar) {
    mapping$xmin <- ggplot2::aes(xmin = .data[["conf.low"]])$xmin
    mapping$xmax <- ggplot2::aes(xmax = .data[["conf.high"]])$xmax
  }
  if (!is.null(shape) && shape %in% names(data)) {
    mapping$shape <- ggplot2::aes(shape = .data[[shape]])$shape
  }
  if (!is.null(colour) && colour %in% names(data)) {
    mapping$colour <- ggplot2::aes(colour = .data[[colour]])$colour
    mapping$group <- ggplot2::aes(group = .data[[colour]])$group
  }

  # position
  if (dodged) {
    position <- ggplot2::position_dodge(dodged_width)
  } else {
    position <- ggplot2::position_identity()
  }

  # plot
  p <- ggplot2::ggplot(data = data, mapping = mapping)

  if (stripped_rows) {
    p <- p +
      geom_stripped_rows(
        mapping = ggplot2::aes(
          odd = .data[[".fill"]], even = .data[[".fill"]],
          colour = NULL, linetype = NULL
        )
      )
  }

  if (vline) {
    p <- p + ggplot2::geom_vline(
      xintercept = ifelse(exponentiate, 1, 0),
      colour = vline_colour
    )
  }

  if (errorbar) {
    if (!is.null(colour) && errorbar_coloured) {
      p <- p +
        ggplot2::geom_errorbarh(
          na.rm = TRUE,
          height = errorbar_height,
          position = position
        )
    } else {
      p <- p +
        ggplot2::geom_errorbarh(
          mapping = ggplot2::aes(colour = NULL),
          na.rm = TRUE,
          height = errorbar_height,
          colour = "black",
          position = position
        )
    }
  }

  if (!is.null(facet_col) && is.character(facet_col)) {
    facet_col <- ggplot2::vars(!!sym(facet_col))
  }
  if (!is.null(facet_row) && is.character(facet_row)) {
    facet_row <- ggplot2::vars(!!sym(facet_row))
  }

  # used later for reapplying appropriate labels
  l <- data[[y]]
  names(l) <- data$term

  p <- p +
    ggplot2::geom_point(
      size = point_size,
      stroke = point_stroke,
      fill = point_fill,
      position = position,
      na.rm = TRUE
    ) +
    ggplot2::facet_grid(
      rows = facet_row,
      cols = facet_col,
      labeller = facet_labeller,
      scales = "free_y", space = "free_y", switch = "y"
    ) +
    ggplot2::ylab("") +
    ggplot2::scale_y_discrete(
      expand = ggplot2::expansion(mult = 0, add = .5),
      labels = .find_label(l, y_labeller)
    ) +
    ggplot2::theme_light() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "vertical",
      strip.placement = "outside",
      strip.text.y.left = ggplot2::element_text(
        face = "bold", angle = 0, colour = "black",
        hjust = 0, vjust = 1
      ),
      strip.text.x = ggplot2::element_text(face = "bold", colour = "black"),
      strip.background = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(linetype = "dashed"),
      axis.title.x = ggplot2::element_text(face = "bold"),
      axis.ticks.y = ggplot2::element_blank()
    )

  if (!is.null(colour) && colour %in% names(data)) {
    if (colour_guide) {
      colour_guide <- ggplot2::guide_legend()
    } else {
      colour_guide <- "none"
    }
    p <- p +
      ggplot2::scale_colour_discrete(
        guide = colour_guide,
        labels = colour_labels
      ) +
      ggplot2::labs(colour = colour_lab)
  }

  if (!is.null(shape) && shape %in% names(data)) {
    if (shape_guide) {
      shape_guide <- ggplot2::guide_legend()
    } else {
      shape_guide <- "none"
    }
    p <- p +
      ggplot2::scale_shape_manual(
        values = shape_values,
        drop = FALSE,
        guide = shape_guide,
        na.translate = FALSE
      ) +
      ggplot2::labs(shape = shape_lab)
  }

  if (exponentiate) {
    p <- p + ggplot2::scale_x_log10()
  }

  if (!is.null(attr(data, "coefficients_label"))) {
    p <- p + ggplot2::xlab(attr(data, "coefficients_label"))
  }

  if (!is.null(plot_title)) {
    p <- p +
      ggplot2::ggtitle(plot_title) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold"),
        plot.title.position = "plot"
      )
  }

  p
}

.call_ggcoef_plot <- function(args, data, plot_title = NULL) {
  args$data <- data
  if (!is.null(plot_title)) args$plot_title <- plot_title
  do.call(ggcoef_plot, args)
}

.in_order <- function(x) {
  # droping unobserved value if needed
  forcats::fct_inorder(as.character(x))
}

.find_label <- function(l, y_labeller = NULL) {
  function(y) {
    if (is.null(y_labeller))
      y_labeller <- function(x) {x} # nolint
    y_labeller(as.character(l[y]))
  }
}
