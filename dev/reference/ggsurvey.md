# Easy ggplot2 with survey objects

A function to facilitate `ggplot2` graphs using a survey object. It will
initiate a ggplot and map survey weights to the corresponding aesthetic.

## Usage

``` r
ggsurvey(design = NULL, mapping = NULL, ...)
```

## Arguments

- design:

  A survey design object, usually created with
  [`survey::svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html)

- mapping:

  Default list of aesthetic mappings to use for plot, to be created with
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html).

- ...:

  Other arguments passed on to methods. Not currently used.

## Value

A `ggplot2` plot.

## Details

Graphs will be correct as long as only weights are required to compute
the graph. However, statistic or geometry requiring correct variance
computation (like
[`ggplot2::geom_smooth()`](https://ggplot2.tidyverse.org/reference/geom_smooth.html))
will be statistically incorrect.

## Examples

``` r
data(api, package = "survey")
dstrat <- survey::svydesign(
  id = ~1, strata = ~stype,
  weights = ~pw, data = apistrat,
  fpc = ~fpc
)
ggsurvey(dstrat) +
  ggplot2::aes(x = cnum, y = dnum) +
  ggplot2::geom_count()


d <- as.data.frame(Titanic)
dw <- survey::svydesign(ids = ~1, weights = ~Freq, data = d)
ggsurvey(dw) +
  ggplot2::aes(x = Class, fill = Survived) +
  ggplot2::geom_bar(position = "fill")
```
