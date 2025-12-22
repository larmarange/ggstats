# Label absolute values

Label absolute values

## Usage

``` r
label_number_abs(..., hide_below = NULL)

label_percent_abs(..., hide_below = NULL)
```

## Arguments

- ...:

  arguments passed to
  [`scales::label_number()`](https://scales.r-lib.org/reference/label_number.html)
  or
  [`scales::label_percent()`](https://scales.r-lib.org/reference/label_percent.html)

- hide_below:

  if provided, values below `hide_below` will be masked (i.e. an empty
  string `""` will be returned)

## Value

A "labelling" function, , i.e. a function that takes a vector and
returns a character vector of same length giving a label for each input
value.

## See also

[`scales::label_number()`](https://scales.r-lib.org/reference/label_number.html),
[`scales::label_percent()`](https://scales.r-lib.org/reference/label_percent.html)

## Examples

``` r
x <- c(-0.2, -.05, 0, .07, .25, .66)

scales::label_number()(x)
#> [1] "-0.20" "-0.05" "0.00"  "0.07"  "0.25"  "0.66" 
label_number_abs()(x)
#> [1] "0.20" "0.05" "0.00" "0.07" "0.25" "0.66"

scales::label_percent()(x)
#> [1] "-20%" "-5%"  "0%"   "7%"   "25%"  "66%" 
label_percent_abs()(x)
#> [1] "20%" "5%"  "0%"  "7%"  "25%" "66%"
label_percent_abs(hide_below = .1)(x)
#> [1] "20%" ""    ""    ""    "25%" "66%"
```
