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
