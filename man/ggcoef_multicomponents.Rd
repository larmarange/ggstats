% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprecated.R
\name{ggcoef_multicomponents}
\alias{ggcoef_multicomponents}
\alias{ggcoef_multinom}
\title{Deprecated functions}
\usage{
ggcoef_multicomponents(
  model,
  type = c("dodged", "faceted", "table"),
  component_col = "component",
  component_label = NULL,
  tidy_fun = broom.helpers::tidy_with_broom_or_parameters,
  tidy_args = NULL,
  conf.int = TRUE,
  conf.level = 0.95,
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
  ...
)

ggcoef_multinom(
  model,
  type = c("dodged", "faceted", "table"),
  y.level_label = NULL,
  tidy_fun = broom.helpers::tidy_with_broom_or_parameters,
  tidy_args = NULL,
  conf.int = TRUE,
  conf.level = 0.95,
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
  ...
)
}
\arguments{
\item{model}{a regression model object}

\item{type}{a dodged plot, a faceted plot or multiple table plots?}

\item{component_col}{name of the component column}

\item{component_label}{an optional named vector for labeling components}

\item{tidy_fun}{(\code{function})\cr
Option to specify a custom tidier function.}

\item{tidy_args}{Additional arguments passed to
\code{\link[broom.helpers:tidy_plus_plus]{broom.helpers::tidy_plus_plus()}} and to \code{tidy_fun}}

\item{conf.int}{(\code{logical})\cr
Should confidence intervals be computed? (see \code{\link[broom:reexports]{broom::tidy()}})}

\item{conf.level}{the confidence level to use for the confidence
interval if \code{conf.int = TRUE}; must be strictly greater than 0
and less than 1; defaults to 0.95, which corresponds to a 95
percent confidence interval}

\item{exponentiate}{if \code{TRUE} a logarithmic scale will
be used for x-axis}

\item{variable_labels}{(\code{\link[gtsummary:syntax]{formula-list-selector}})\cr
A named list or a named vector of custom variable labels.}

\item{term_labels}{(\code{list} or \code{vector})\cr
A named list or a named vector of custom term labels.}

\item{interaction_sep}{(\code{string})\cr
Separator for interaction terms.}

\item{categorical_terms_pattern}{(\code{\link[glue:glue]{glue pattern}})\cr
A \link[glue:glue]{glue pattern} for labels of categorical terms with treatment
or sum contrasts (see \code{\link[broom.helpers:model_list_terms_levels]{model_list_terms_levels()}}).}

\item{add_reference_rows}{(\code{logical})\cr
Should reference rows be added?}

\item{no_reference_row}{(\code{\link[dplyr:dplyr_tidy_select]{tidy-select}})\cr
Variables for those no reference row should be added,
when \code{add_reference_rows = TRUE}.}

\item{intercept}{(\code{logical})\cr
Should the intercept(s) be included?}

\item{include}{(\code{\link[dplyr:dplyr_tidy_select]{tidy-select}})\cr
Variables to include. Default is \code{everything()}.
See also \code{\link[broom.helpers:all_continuous]{all_continuous()}}, \code{\link[broom.helpers:all_categorical]{all_categorical()}}, \code{\link[broom.helpers:all_dichotomous]{all_dichotomous()}}
and \code{\link[broom.helpers:all_interaction]{all_interaction()}}.}

\item{significance}{level (between 0 and 1) below which a
coefficient is consider to be significantly different from 0
(or 1 if \code{exponentiate = TRUE}), \code{NULL} for not highlighting
such coefficients}

\item{significance_labels}{optional vector with custom labels
for significance variable}

\item{return_data}{if \code{TRUE}, will return the data.frame used
for plotting instead of the plot}

\item{table_stat}{statistics to display in the table, use any column name
returned by the tidier or \code{"ci"} for confidence intervals formatted
according to \code{ci_pattern}}

\item{table_header}{optional custom headers for the table}

\item{table_text_size}{text size for the table}

\item{table_stat_label}{optional named list of labeller functions for the
displayed statistic (see examples)}

\item{ci_pattern}{glue pattern for confidence intervals in the table}

\item{table_witdhs}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}\cr
use \code{table_widths} instead}

\item{...}{parameters passed to \code{\link[=ggcoef_plot]{ggcoef_plot()}}}

\item{y.level_label}{an optional named vector for labeling \code{y.level}
(see examples)}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}
}
