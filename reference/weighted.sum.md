# Weighted Sum

Weighted Sum

## Usage

``` r
weighted.sum(x, w, na.rm = TRUE)
```

## Arguments

- x:

  a numeric vector of values

- w:

  a numeric vector of weights

- na.rm:

  a logical indicating whether to ignore `NA` values

## Value

A numeric vector.

## Examples

``` r
x <- 1:20
w <- runif(20)
weighted.sum(x, w)
#> [1] 115.7631
```
