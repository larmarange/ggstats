# Identify a suitable font color (black or white) given a background HEX color

You could use `auto_contrast` as a shortcut of
`aes(colour = after_scale(hex_bw(.data$fill)))`. You should use `!!!` to
inject it within
[`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html)
(see examples).

`hex_bw_threshold()` is a variation of `hex_bw()`. For `values` below
`threshold`, black (`"#000000"`) will always be returned, regardless of
`hex_code`.

## Usage

``` r
hex_bw(hex_code)

hex_bw_threshold(hex_code, values, threshold)

auto_contrast
```

## Format

An object of class `ggplot2::mapping` (inherits from `uneval`, `gg`,
`S7_object`) of length 1.

## Source

Adapted from `saros` for `hex_code()` and from
<https://github.com/teunbrand/ggplot_tricks?tab=readme-ov-file#text-contrast>
for `auto_contrast`.

## Arguments

- hex_code:

  Background color in hex-format.

- values:

  Values to be compared.

- threshold:

  Threshold.

## Value

Either black or white, in hex-format

## Examples

``` r
hex_bw("#0dadfd")
#> [1] "#000000"

library(ggplot2)
ggplot(diamonds) +
  aes(x = cut, fill = color, label = after_stat(count)) +
  geom_bar() +
  geom_text(
    mapping = aes(color = after_scale(hex_bw(.data$fill))),
    position = position_stack(.5),
    stat = "count",
    size = 2
  )


ggplot(diamonds) +
  aes(x = cut, fill = color, label = after_stat(count)) +
  geom_bar() +
  geom_text(
    mapping = auto_contrast,
    position = position_stack(.5),
    stat = "count",
    size = 2
  )


ggplot(diamonds) +
  aes(x = cut, fill = color, label = after_stat(count), !!!auto_contrast) +
  geom_bar() +
  geom_text(
    mapping = auto_contrast,
    position = position_stack(.5),
    stat = "count",
    size = 2
  )
```
