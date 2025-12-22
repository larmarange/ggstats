# Weighted Median and Quantiles

Compute the median or quantiles a set of numbers which have weights
associated with them.

## Usage

``` r
weighted.median(x, w, na.rm = TRUE, type = 2)

weighted.quantile(x, w, probs = seq(0, 1, 0.25), na.rm = TRUE, type = 4)
```

## Source

These functions are adapted from their homonyms developed by Adrian
Baddeley in the `spatstat` package.

## Arguments

- x:

  a numeric vector of values

- w:

  a numeric vector of weights

- na.rm:

  a logical indicating whether to ignore `NA` values

- type:

  Integer specifying the rule for calculating the median or quantile,
  corresponding to the rules available for `stats:quantile()`. The only
  valid choices are type=1, 2 or 4. See Details.

- probs:

  probabilities for which the quantiles should be computed, a numeric
  vector of values between 0 and 1

## Value

A numeric vector.

## Details

The `i`th observation `x[i]` is treated as having a weight proportional
to `w[i]`.

The weighted median is a value `m` such that the total weight of data
less than or equal to `m` is equal to half the total weight. More
generally, the weighted quantile with probability `p` is a value `q`
such that the total weight of data less than or equal to `q` is equal to
`p` times the total weight.

If there is no such value, then

- if `type = 1`, the next largest value is returned (this is the
  right-continuous inverse of the left-continuous cumulative
  distribution function);

- if `type = 2`, the average of the two surrounding values is returned
  (the average of the right-continuous and left-continuous inverses);

- if `type = 4`, linear interpolation is performed.

Note that the default rule for `weighted.median()` is `type = 2`,
consistent with the traditional definition of the median, while the
default for `weighted.quantile()` is `type = 4`.

## Examples

``` r
x <- 1:20
w <- runif(20)
weighted.median(x, w)
#> [1] 11.5
weighted.quantile(x, w)
#>        0%       25%       50%       75%      100% 
#>  1.000000  5.888026 11.795509 14.939129 20.000000 
```
