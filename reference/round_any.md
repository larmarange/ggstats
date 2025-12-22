# Round to multiple of any number.

Round to multiple of any number.

## Usage

``` r
round_any(x, accuracy, f = round)
```

## Source

adapted from `plyr`

## Arguments

- x:

  numeric or date-time (POSIXct) vector to round

- accuracy:

  number to round to; for POSIXct objects, a number of seconds

- f:

  rounding function: [`floor`](https://rdrr.io/r/base/Round.html),
  [`ceiling`](https://rdrr.io/r/base/Round.html) or
  [`round`](https://rdrr.io/r/base/Round.html)

## Examples

``` r
round_any(1.865, accuracy = .25)
#> [1] 1.75
```
