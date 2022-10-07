
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggcoef

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R build
status](https://github.com/larmarange/ggcoef/workflows/R-CMD-check/badge.svg)](https://github.com/larmarange/ggcoef/actions)
[![Codecov test
coverage](https://codecov.io/gh/larmarange/ggcoef/branch/main/graph/badge.svg)](https://app.codecov.io/gh/larmarange/ggcoef?branch=main)
[![CRAN
status](https://www.r-pkg.org/badges/version/ggcoef)](https://CRAN.R-project.org/package=ggcoef)
<!-- [![DOI](https://zenodo.org/badge/286680847.svg)](https://zenodo.org/badge/latestdoi/286680847) -->
<!-- badges: end -->

The `ggcoef` package provides a suite of functions to plot regression
model coefficients (“forest plots”) using `ggplot2`.

The suite includes new geometries to add alternating backgroung color to
a plot.

## Installation

<!-- To install stable version:


```r
install.packages("ggcoef")
```

-->

To install development version:

``` r
remotes::install_github("larmarange/ggcoef")
```

## A basic example

``` r
library(ggcoef)
#> Le chargement a nécessité le package : ggplot2

data(tips, package = "reshape")
mod_simple <- lm(tip ~ day + time + total_bill, data = tips)
ggcoef_model(mod_simple)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

## Comparing several models

``` r
mod1 <- lm(Fertility ~ ., data = swiss)
  mod2 <- step(mod1, trace = 0)
  mod3 <- lm(Fertility ~ Agriculture + Education * Catholic, data = swiss)
  models <- list("Full model" = mod1, "Simplified model" = mod2, "With interaction" = mod3)

  ggcoef_compare(models)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

``` r
  ggcoef_compare(models, type = "faceted")
```

<img src="man/figures/README-unnamed-chunk-5-2.png" width="100%" />
