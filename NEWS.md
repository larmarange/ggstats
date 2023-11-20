# ggstats 0.5.1

**Bug fixes**

* fix in `ggcoef_model()` and other similar functions:
  Unicode character removed in significance labels (#49)

# ggstats 0.5.0

**Improvements**

* New options `labels_color` and `totals_color` in `gglikert()` and
  `gglikert_stacked()` (#43)

**Bug fixes**

* fix in `ggcoef_multicomponents()` when `type = "table"` and
  `exponentiate = TRUE`
* fix in `gglikert()`: the function could be called directly with
  `ggstats::gglikert()` without requiring the full package to be loaded (#47)

# ggstats 0.4.0

**New features**

* new function `ggcoef_table()` displaying a coefficient table at the right
  of the forest plot (#32)
* new function `ggcoef_multicomponents()` for multi-components models such
  as zero-inflated Poisson or beta regressions (#38)
* new type `"table"` for `ggcoef_multinom()`

**Improvements**

* `gglikert()` now aligns total proportions when faceting (#28)
* new `weights` argument for `gglikert()`, `gglikert_stacked()` and 
  `gglikert_data()` (#29)
* new `y` argument for `gglikert()` and `gglikert_stacked()` (#31)
* new `facet_label_wrap` argument for `gglikert()` (#31)

**New helpers**

* `weighted.median()` and `weighted.quantile()` functions

# ggstats 0.3.0

**New features**

* New functions `gglikert()`, `gglikert_stacked()` and `gglikert_data()` (#25)
* New positions `position_likert()` and `position_likert_count()` (#25)
* New `complete` argument for `stat_prop()` (#25)

**Bug fixes**

* Bug fix in `ggcoef_compare()` to preserve the order of model terms and to 
  avoid an error with `add_reference_rows = FALSE` (#23)

# ggstats 0.2.1

* Bug fix in `geom_stripped_rows()` and `geom_stripped_cols()` (#20)

# ggstats 0.2.0

* Support for pairwise contrasts (#14)
* New argument `tidy_args` in `ggcoef_*()` to pass additional arguments to
  `broom.helpers::tidy_plus_plus()` and to `tidy_fun` (#17)
* Now requires `ggplot2` version 3.4.0 or more (#15)
* Following change in `geom_rect()`, the `size` aesthetic is now deprecated
  in `geom_stripped_cols()` and `geom_stripped_rows()`: please use the
  `linewidth` aesthetic instead (#15)

# ggstats 0.1.1

* Examples relying on Internet resources have been removed (#11)

# ggstats 0.1.0

* First version, based on dev version of GGally
* Fix in `ggcoef_multinom()` to display y levels not listed in `y.level_label` 
* `stat_cross()` now returns phi coefficients (see also 
  `augment_chisq_add_phi()`) (#6)
