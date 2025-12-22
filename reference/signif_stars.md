# Significance Stars

Calculate significance stars

## Usage

``` r
signif_stars(x, three = 0.001, two = 0.01, one = 0.05, point = 0.1)
```

## Arguments

- x:

  numeric values that will be compared to the `point`, `one`, `two`, and
  `three` values

- three:

  threshold below which to display three stars

- two:

  threshold below which to display two stars

- one:

  threshold below which to display one star

- point:

  threshold below which to display one point (`NULL` to deactivate)

## Value

Character vector containing the appropriate number of stars for each `x`
value.

## Author

Joseph Larmarange

## Examples

``` r
x <- c(0.5, 0.1, 0.05, 0.01, 0.001)
signif_stars(x)
#> [1] ""    "."   "*"   "**"  "***"
signif_stars(x, one = .15, point = NULL)
#> [1] ""    "*"   "*"   "**"  "***"
```
