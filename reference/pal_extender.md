# Extend a discrete colour palette

If the palette returns less colours than requested, the list of colours
will be expanded using
[`scales::pal_gradient_n()`](https://scales.r-lib.org/reference/pal_gradient_n.html).
To be used with a sequential or diverging palette. Not relevant for
qualitative palettes.

## Usage

``` r
pal_extender(pal = scales::brewer_pal(palette = "BrBG"))

scale_fill_extended(
  name = waiver(),
  ...,
  pal = scales::brewer_pal(palette = "BrBG"),
  aesthetics = "fill"
)

scale_colour_extended(
  name = waiver(),
  ...,
  pal = scales::brewer_pal(palette = "BrBG"),
  aesthetics = "colour"
)
```

## Arguments

- pal:

  A palette function, such as returned by
  [scales::brewer_pal](https://scales.r-lib.org/reference/pal_brewer.html),
  taking a number of colours as entry and returning a list of colours.

- name:

  The name of the scale. Used as the axis or legend title. If
  [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html), the
  default, the name of the scale is taken from the first mapping used
  for that aesthetic. If `NULL`, the legend title will be omitted.

- ...:

  Other arguments passed on to
  [`discrete_scale()`](https://ggplot2.tidyverse.org/reference/discrete_scale.html)
  to control name, limits, breaks, labels and so forth.

- aesthetics:

  Character string or vector of character strings listing the name(s) of
  the aesthetic(s) that this scale works with. This can be useful, for
  example, to apply colour settings to the colour and fill aesthetics at
  the same time, via `aesthetics = c("colour", "fill")`.

## Value

A palette function.

## Examples

``` r
pal <- scales::pal_brewer(palette = "PiYG")
scales::show_col(pal(16))
#> Warning: n too large, allowed maximum for palette PiYG is 11
#> Returning the palette you asked for with that many colors

scales::show_col(pal_extender(pal)(16))
```
