# Deprecated functions

**\[deprecated\]**

## Usage

``` r
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
```

## Arguments

- model:

  a regression model object

- type:

  a dodged plot, a faceted plot or multiple table plots?

- component_col:

  name of the component column

- component_label:

  an optional named vector for labeling components

- tidy_fun:

  (`function`)  
  Option to specify a custom tidier function.

- tidy_args:

  Additional arguments passed to
  [`broom.helpers::tidy_plus_plus()`](https://larmarange.github.io/broom.helpers/reference/tidy_plus_plus.html)
  and to `tidy_fun`

- conf.int:

  (`logical`)  
  Should confidence intervals be computed? (see
  [`broom::tidy()`](https://broom.tidymodels.org/reference/reexports.html))

- conf.level:

  the confidence level to use for the confidence interval if
  `conf.int = TRUE`; must be strictly greater than 0 and less than 1;
  defaults to 0.95, which corresponds to a 95 percent confidence
  interval

- exponentiate:

  if `TRUE` a logarithmic scale will be used for x-axis

- variable_labels:

  ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/reference/syntax.html))  
  A named list or a named vector of custom variable labels.

- term_labels:

  (`list` or `vector`)  
  A named list or a named vector of custom term labels.

- interaction_sep:

  (`string`)  
  Separator for interaction terms.

- categorical_terms_pattern:

  ([`glue pattern`](https://glue.tidyverse.org/reference/glue.html))  
  A [glue pattern](https://glue.tidyverse.org/reference/glue.html) for
  labels of categorical terms with treatment or sum contrasts (see
  [`model_list_terms_levels()`](https://larmarange.github.io/broom.helpers/reference/model_list_terms_levels.html)).

- add_reference_rows:

  (`logical`)  
  Should reference rows be added?

- no_reference_row:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variables for those no reference row should be added, when
  `add_reference_rows = TRUE`.

- intercept:

  (`logical`)  
  Should the intercept(s) be included?

- include:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  Variables to include. Default is
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html).
  See also
  [`all_continuous()`](https://larmarange.github.io/broom.helpers/reference/select_helpers.html),
  [`all_categorical()`](https://larmarange.github.io/broom.helpers/reference/select_helpers.html),
  [`all_dichotomous()`](https://larmarange.github.io/broom.helpers/reference/select_helpers.html)
  and
  [`all_interaction()`](https://larmarange.github.io/broom.helpers/reference/select_helpers.html).

- significance:

  level (between 0 and 1) below which a coefficient is consider to be
  significantly different from 0 (or 1 if `exponentiate = TRUE`), `NULL`
  for not highlighting such coefficients

- significance_labels:

  optional vector with custom labels for significance variable

- return_data:

  if `TRUE`, will return the data.frame used for plotting instead of the
  plot

- table_stat:

  statistics to display in the table, use any column name returned by
  the tidier or `"ci"` for confidence intervals formatted according to
  `ci_pattern`

- table_header:

  optional custom headers for the table

- table_text_size:

  text size for the table

- table_stat_label:

  optional named list of labeller functions for the displayed statistic
  (see examples)

- ci_pattern:

  glue pattern for confidence intervals in the table

- table_witdhs:

  **\[deprecated\]**  
  use `table_widths` instead

- ...:

  parameters passed to
  [`ggcoef_plot()`](https://larmarange.github.io/ggstats/reference/ggcoef_model.md)

- y.level_label:

  an optional named vector for labeling `y.level` (see examples)
