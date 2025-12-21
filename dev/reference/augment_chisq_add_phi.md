# Augment a chi-squared test and compute phi coefficients

Augment a chi-squared test and compute phi coefficients

## Usage

``` r
augment_chisq_add_phi(x)
```

## Arguments

- x:

  a chi-squared test as returned by
  [`stats::chisq.test()`](https://rdrr.io/r/stats/chisq.test.html)

## Value

A `tibble`.

## Details

Phi coefficients are a measurement of the degree of association between
two binary variables.

- A value between -1.0 to -0.7 indicates a strong negative association.

- A value between -0.7 to -0.3 indicates a weak negative association.

- A value between -0.3 to +0.3 indicates a little or no association.

- A value between +0.3 to +0.7 indicates a weak positive association.

- A value between +0.7 to +1.0 indicates a strong positive association.

## See also

[`stat_cross()`](https://larmarange.github.io/ggstats/dev/reference/stat_cross.md),
`GDAtools::phi.table()` or `psych::phi()`

## Examples

``` r
tab <- xtabs(Freq ~ Sex + Class, data = as.data.frame(Titanic))
augment_chisq_add_phi(chisq.test(tab))
#> # A tibble: 8 × 13
#>   Sex    Class .observed  .prop .row.prop .col.prop .expected .resid .std.resid
#>   <fct>  <fct>     <dbl>  <dbl>     <dbl>     <dbl>     <dbl>  <dbl>      <dbl>
#> 1 Male   1st         180 0.0818    0.104     0.554      256.   -4.73     -11.1 
#> 2 Female 1st         145 0.0659    0.309     0.446       69.4   9.07      11.1 
#> 3 Male   2nd         179 0.0813    0.103     0.628      224.   -3.02      -6.99
#> 4 Female 2nd         106 0.0482    0.226     0.372       60.9   5.79       6.99
#> 5 Male   3rd         510 0.232     0.295     0.722      555.   -1.92      -5.04
#> 6 Female 3rd         196 0.0891    0.417     0.278      151.    3.68       5.04
#> 7 Male   Crew        862 0.392     0.498     0.974      696.    6.29      17.6 
#> 8 Female Crew         23 0.0104    0.0489    0.0260     189.  -12.1      -17.6 
#> # ℹ 4 more variables: .row.observed <dbl>, .col.observed <dbl>,
#> #   .total.observed <dbl>, .phi <dbl>
```
