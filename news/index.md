# Changelog

## ggstats 0.12.0

CRAN release: 2025-12-22

**Improvements**

- [`gglikert()`](https://larmarange.github.io/ggstats/reference/gglikert.md),
  [`gglikert_stacked()`](https://larmarange.github.io/ggstats/reference/gglikert.md)
  and
  [`gglikert_data()`](https://larmarange.github.io/ggstats/reference/gglikert.md)
  now accepts survey objects
  ([\#110](https://github.com/larmarange/ggstats/issues/110))
- compatibility with `vctrs` version 0.7.0 added
  ([\#109](https://github.com/larmarange/ggstats/issues/109))

## ggstats 0.11.0

CRAN release: 2025-09-15

**Improvements**

- new `type = "table"` for
  [`ggcoef_compare()`](https://larmarange.github.io/ggstats/reference/ggcoef_model.md)
  ([\#102](https://github.com/larmarange/ggstats/issues/102))
- new argument `x_limits` for
  [`ggcoef_plot()`](https://larmarange.github.io/ggstats/reference/ggcoef_model.md)
- limits of x-axis are now harmonized between sub-plots returned by
  [`ggcoef_table()`](https://larmarange.github.io/ggstats/reference/ggcoef_model.md)
  ([\#102](https://github.com/larmarange/ggstats/issues/102))
- [`ggcoef_plot()`](https://larmarange.github.io/ggstats/reference/ggcoef_model.md)
  has been updated to account for the deprecation of
  [`ggplot2::geom_errorbarh()`](https://ggplot2.tidyverse.org/reference/geom_linerange.html)
  in version 4.0.0 of `ggplot2`
  ([\#104](https://github.com/larmarange/ggstats/issues/104))

## ggstats 0.10.0

CRAN release: 2025-07-02

**Improvements**

- [`gglikert()`](https://larmarange.github.io/ggstats/reference/gglikert.md):
  legend order is reversed when `reverse_likert = TRUE`
  ([\#95](https://github.com/larmarange/ggstats/issues/95))
- [`gglikert_stacked()`](https://larmarange.github.io/ggstats/reference/gglikert.md):
  legend order is reversed when `reverse_fill = TRUE`
  ([\#95](https://github.com/larmarange/ggstats/issues/95))

**Renamed argument**

- the `table_witdhs` argument of
  [`ggcoef_table()`](https://larmarange.github.io/ggstats/reference/ggcoef_model.md)
  is deprecated. Please use the `table_widths` argument instead
  ([\#99](https://github.com/larmarange/ggstats/issues/99))

## ggstats 0.9.0

CRAN release: 2025-03-10

**Improvements**

- `ggccoef_model()` and
  [`ggcoef_table()`](https://larmarange.github.io/ggstats/reference/ggcoef_model.md)
  are now compatible with multinomial and multi-component models,
  following `broom.helpers` 1.20.0: both functions gained new arguments
  `group_by` and `group_labels`
  ([\#93](https://github.com/larmarange/ggstats/issues/93))
- new functions
  [`ggcoef_dodged()`](https://larmarange.github.io/ggstats/reference/ggcoef_model.md)
  and
  [`ggcoef_faceted()`](https://larmarange.github.io/ggstats/reference/ggcoef_model.md)
  ([\#93](https://github.com/larmarange/ggstats/issues/93))
- [`ggcoef_plot()`](https://larmarange.github.io/ggstats/reference/ggcoef_model.md)
  got a new argument `y_labeller`
  ([\#86](https://github.com/larmarange/ggstats/issues/86))

**Deprecated functions**

- [`ggcoef_multinom()`](https://larmarange.github.io/ggstats/reference/ggcoef_multicomponents.md)
  and
  [`ggcoef_multicomponents()`](https://larmarange.github.io/ggstats/reference/ggcoef_multicomponents.md)
  are now soft-deprecated and may be removed in a future release. Use
  instead
  [`ggcoef_model()`](https://larmarange.github.io/ggstats/reference/ggcoef_model.md),
  [`ggcoef_table()`](https://larmarange.github.io/ggstats/reference/ggcoef_model.md),
  [`ggcoef_dodged()`](https://larmarange.github.io/ggstats/reference/ggcoef_model.md)
  or
  [`ggcoef_faceted()`](https://larmarange.github.io/ggstats/reference/ggcoef_model.md)
  ([\#93](https://github.com/larmarange/ggstats/issues/93))

**Bug fix**

- fix terms order in
  [`ggcoef_model()`](https://larmarange.github.io/ggstats/reference/ggcoef_model.md)
  for specific cases when two modalities of two different variables have
  the same name
  ([\#86](https://github.com/larmarange/ggstats/issues/86))
- bug fix in
  [`stat_prop()`](https://larmarange.github.io/ggstats/reference/stat_prop.md)
  when `complete = "color"` or
  `complete = "group"`([\#89](https://github.com/larmarange/ggstats/issues/89))
- bug fix in
  [`gglikert()`](https://larmarange.github.io/ggstats/reference/gglikert.md)
  when `reverse_likert = TRUE` and `add_totals = TRUE`
  ([\#91](https://github.com/larmarange/ggstats/issues/91))

## ggstats 0.8.0

CRAN release: 2025-01-07

**Breaking changes**

- [`geom_diverging()`](https://larmarange.github.io/ggstats/reference/geom_diverging.md),
  [`geom_likert()`](https://larmarange.github.io/ggstats/reference/geom_diverging.md),
  [`geom_pyramid()`](https://larmarange.github.io/ggstats/reference/geom_diverging.md),
  [`geom_diverging_text()`](https://larmarange.github.io/ggstats/reference/geom_diverging.md),
  [`geom_likert_text()`](https://larmarange.github.io/ggstats/reference/geom_diverging.md),
  and
  [`geom_pyramid_text()`](https://larmarange.github.io/ggstats/reference/geom_diverging.md)
  have been redesigned
  ([\#73](https://github.com/larmarange/ggstats/issues/73))
- [`stat_prop()`](https://larmarange.github.io/ggstats/reference/stat_prop.md):
  arguments `height`, `labels` and `labeller` have been removed
  ([\#73](https://github.com/larmarange/ggstats/issues/73))

**Improvements**

- new geoms
  [`geom_connector()`](https://larmarange.github.io/ggstats/reference/geom_connector.md),
  [`geom_bar_connector()`](https://larmarange.github.io/ggstats/reference/geom_connector.md)
  and
  [`geom_prop_connector()`](https://larmarange.github.io/ggstats/reference/geom_prop_bar.md)
  ([\#81](https://github.com/larmarange/ggstats/issues/81))
- new shortcut `auto_contrast`
  ([\#75](https://github.com/larmarange/ggstats/issues/75))

## ggstats 0.7.0

CRAN release: 2024-09-22

**Minor breaking change**

- `position_likert_count()` has been renamed as
  [`position_diverging()`](https://larmarange.github.io/ggstats/reference/position_likert.md)
  ([\#69](https://github.com/larmarange/ggstats/issues/69))
- R minimum version 4.2.0 is now required.

**Improvements**

- new experimental plot:
  [`ggcascade()`](https://larmarange.github.io/ggstats/reference/ggcascade.md)
  for “cascade” plots
  ([\#71](https://github.com/larmarange/ggstats/issues/71))

- new scale
  [`scale_fill_likert()`](https://larmarange.github.io/ggstats/reference/scale_fill_likert.md)
  ([\#64](https://github.com/larmarange/ggstats/issues/64))

- new geometries:
  [`geom_prop_bar()`](https://larmarange.github.io/ggstats/reference/geom_prop_bar.md)
  and
  [`geom_prop_text()`](https://larmarange.github.io/ggstats/reference/geom_prop_bar.md)
  ([\#69](https://github.com/larmarange/ggstats/issues/69))

- new geometries:
  [`geom_diverging()`](https://larmarange.github.io/ggstats/reference/geom_diverging.md),
  [`geom_likert()`](https://larmarange.github.io/ggstats/reference/geom_diverging.md),
  [`geom_pyramid()`](https://larmarange.github.io/ggstats/reference/geom_diverging.md)
  and
  [`geom_diverging_text()`](https://larmarange.github.io/ggstats/reference/geom_diverging.md),
  [`geom_likert_text()`](https://larmarange.github.io/ggstats/reference/geom_diverging.md),
  [`geom_pyramid_text()`](https://larmarange.github.io/ggstats/reference/geom_diverging.md)
  ([\#69](https://github.com/larmarange/ggstats/issues/69))

- new helper
  [`symmetric_limits()`](https://larmarange.github.io/ggstats/reference/symmetric_limits.md)
  to make a scale symmetric
  ([\#66](https://github.com/larmarange/ggstats/issues/66))

- new helper
  [`pal_extender()`](https://larmarange.github.io/ggstats/reference/pal_extender.md)
  and corresponding `scale_fill_extender()` and
  `scale_colour_extender()`

- new helper
  [`weighted.sum()`](https://larmarange.github.io/ggstats/reference/weighted.sum.md)
  for weighted sums
  ([\#71](https://github.com/larmarange/ggstats/issues/71))

- new sorting option `"prop_lower"` for
  [`gglikert()`](https://larmarange.github.io/ggstats/reference/gglikert.md)
  ([\#62](https://github.com/larmarange/ggstats/issues/62))

- new argument `symmetric` for
  [`gglikert()`](https://larmarange.github.io/ggstats/reference/gglikert.md)
  ([\#66](https://github.com/larmarange/ggstats/issues/66))

- new arguments `default_by`, `height`, `labels` and `labeller` for
  [`stat_prop()`](https://larmarange.github.io/ggstats/reference/stat_prop.md)
  ([\#69](https://github.com/larmarange/ggstats/issues/69))

- new returned statistics for
  [`stat_prop()`](https://larmarange.github.io/ggstats/reference/stat_prop.md):
  `after_stat(denominator)`, `after_stat(height)` and
  `after_stat(labels)`

## ggstats 0.6.0

CRAN release: 2024-04-05

**Improvements**

- new function
  [`hex_bw()`](https://larmarange.github.io/ggstats/reference/hex_bw.md)
  to identify a suitable font color given a background color
  ([\#57](https://github.com/larmarange/ggstats/issues/57))
- new default value `"auto"` for `labels_color` argument in
  [`gglikert()`](https://larmarange.github.io/ggstats/reference/gglikert.md)
  and
  [`gglikert_stacked()`](https://larmarange.github.io/ggstats/reference/gglikert.md)
  (using
  [`hex_bw()`](https://larmarange.github.io/ggstats/reference/hex_bw.md))
  ([\#57](https://github.com/larmarange/ggstats/issues/57))
- new argument `data_fun` for
  [`gglikert()`](https://larmarange.github.io/ggstats/reference/gglikert.md),
  [`gglikert_data()`](https://larmarange.github.io/ggstats/reference/gglikert.md)
  and
  [`gglikert_stacked()`](https://larmarange.github.io/ggstats/reference/gglikert.md)
  ([\#60](https://github.com/larmarange/ggstats/issues/60))

## ggstats 0.5.1

CRAN release: 2023-11-21

**Bug fixes**

- fix in
  [`ggcoef_model()`](https://larmarange.github.io/ggstats/reference/ggcoef_model.md)
  and other similar functions: Unicode character removed in significance
  labels ([\#49](https://github.com/larmarange/ggstats/issues/49))

## ggstats 0.5.0

CRAN release: 2023-09-28

**Improvements**

- new options `labels_color` and `totals_color` in
  [`gglikert()`](https://larmarange.github.io/ggstats/reference/gglikert.md)
  and
  [`gglikert_stacked()`](https://larmarange.github.io/ggstats/reference/gglikert.md)
  ([\#43](https://github.com/larmarange/ggstats/issues/43))

**Bug fixes**

- fix in
  [`ggcoef_multicomponents()`](https://larmarange.github.io/ggstats/reference/ggcoef_multicomponents.md)
  when `type = "table"` and `exponentiate = TRUE`
- fix in
  [`gglikert()`](https://larmarange.github.io/ggstats/reference/gglikert.md):
  the function could be called directly with
  [`ggstats::gglikert()`](https://larmarange.github.io/ggstats/reference/gglikert.md)
  without requiring the full package to be loaded
  ([\#47](https://github.com/larmarange/ggstats/issues/47))

## ggstats 0.4.0

CRAN release: 2023-08-13

**New features**

- new function
  [`ggcoef_table()`](https://larmarange.github.io/ggstats/reference/ggcoef_model.md)
  displaying a coefficient table at the right of the forest plot
  ([\#32](https://github.com/larmarange/ggstats/issues/32))
- new function
  [`ggcoef_multicomponents()`](https://larmarange.github.io/ggstats/reference/ggcoef_multicomponents.md)
  for multi-components models such as zero-inflated Poisson or beta
  regressions ([\#38](https://github.com/larmarange/ggstats/issues/38))
- new type `"table"` for
  [`ggcoef_multinom()`](https://larmarange.github.io/ggstats/reference/ggcoef_multicomponents.md)

**Improvements**

- [`gglikert()`](https://larmarange.github.io/ggstats/reference/gglikert.md)
  now aligns total proportions when faceting
  ([\#28](https://github.com/larmarange/ggstats/issues/28))
- new `weights` argument for
  [`gglikert()`](https://larmarange.github.io/ggstats/reference/gglikert.md),
  [`gglikert_stacked()`](https://larmarange.github.io/ggstats/reference/gglikert.md)
  and
  [`gglikert_data()`](https://larmarange.github.io/ggstats/reference/gglikert.md)
  ([\#29](https://github.com/larmarange/ggstats/issues/29))
- new `y` argument for
  [`gglikert()`](https://larmarange.github.io/ggstats/reference/gglikert.md)
  and
  [`gglikert_stacked()`](https://larmarange.github.io/ggstats/reference/gglikert.md)
  ([\#31](https://github.com/larmarange/ggstats/issues/31))
- new `facet_label_wrap` argument for
  [`gglikert()`](https://larmarange.github.io/ggstats/reference/gglikert.md)
  ([\#31](https://github.com/larmarange/ggstats/issues/31))

**New helpers**

- [`weighted.median()`](https://larmarange.github.io/ggstats/reference/weighted.median.md)
  and
  [`weighted.quantile()`](https://larmarange.github.io/ggstats/reference/weighted.median.md)
  functions

## ggstats 0.3.0

CRAN release: 2023-04-12

**New features**

- New functions
  [`gglikert()`](https://larmarange.github.io/ggstats/reference/gglikert.md),
  [`gglikert_stacked()`](https://larmarange.github.io/ggstats/reference/gglikert.md)
  and
  [`gglikert_data()`](https://larmarange.github.io/ggstats/reference/gglikert.md)
  ([\#25](https://github.com/larmarange/ggstats/issues/25))
- New positions
  [`position_likert()`](https://larmarange.github.io/ggstats/reference/position_likert.md)
  and `position_likert_count()`
  ([\#25](https://github.com/larmarange/ggstats/issues/25))
- New `complete` argument for
  [`stat_prop()`](https://larmarange.github.io/ggstats/reference/stat_prop.md)
  ([\#25](https://github.com/larmarange/ggstats/issues/25))

**Bug fixes**

- Bug fix in
  [`ggcoef_compare()`](https://larmarange.github.io/ggstats/reference/ggcoef_model.md)
  to preserve the order of model terms and to avoid an error with
  `add_reference_rows = FALSE`
  ([\#23](https://github.com/larmarange/ggstats/issues/23))

## ggstats 0.2.1

CRAN release: 2023-01-12

- Bug fix in
  [`geom_stripped_rows()`](https://larmarange.github.io/ggstats/reference/geom_stripped_rows.md)
  and
  [`geom_stripped_cols()`](https://larmarange.github.io/ggstats/reference/geom_stripped_rows.md)
  ([\#20](https://github.com/larmarange/ggstats/issues/20))

## ggstats 0.2.0

CRAN release: 2023-01-06

- Support for pairwise contrasts
  ([\#14](https://github.com/larmarange/ggstats/issues/14))
- New argument `tidy_args` in `ggcoef_*()` to pass additional arguments
  to
  [`broom.helpers::tidy_plus_plus()`](https://larmarange.github.io/broom.helpers/reference/tidy_plus_plus.html)
  and to `tidy_fun`
  ([\#17](https://github.com/larmarange/ggstats/issues/17))
- Now requires `ggplot2` version 3.4.0 or more
  ([\#15](https://github.com/larmarange/ggstats/issues/15))
- Following change in
  [`geom_rect()`](https://ggplot2.tidyverse.org/reference/geom_tile.html),
  the `size` aesthetic is now deprecated in
  [`geom_stripped_cols()`](https://larmarange.github.io/ggstats/reference/geom_stripped_rows.md)
  and
  [`geom_stripped_rows()`](https://larmarange.github.io/ggstats/reference/geom_stripped_rows.md):
  please use the `linewidth` aesthetic instead
  ([\#15](https://github.com/larmarange/ggstats/issues/15))

## ggstats 0.1.1

CRAN release: 2022-11-23

- Examples relying on Internet resources have been removed
  ([\#11](https://github.com/larmarange/ggstats/issues/11))

## ggstats 0.1.0

CRAN release: 2022-10-17

- First version, based on dev version of GGally
- Fix in
  [`ggcoef_multinom()`](https://larmarange.github.io/ggstats/reference/ggcoef_multicomponents.md)
  to display y levels not listed in `y.level_label`
- [`stat_cross()`](https://larmarange.github.io/ggstats/reference/stat_cross.md)
  now returns phi coefficients (see also
  [`augment_chisq_add_phi()`](https://larmarange.github.io/ggstats/reference/augment_chisq_add_phi.md))
  ([\#6](https://github.com/larmarange/ggstats/issues/6))
