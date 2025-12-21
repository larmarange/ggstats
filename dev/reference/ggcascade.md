# Cascade plot

**\[experimental\]**

## Usage

``` r
ggcascade(
  .data,
  ...,
  .weights = NULL,
  .by = NULL,
  .nrow = NULL,
  .ncol = NULL,
  .add_n = TRUE,
  .text_size = 4,
  .arrows = TRUE
)

compute_cascade(.data, ..., .weights = NULL, .by = NULL)

plot_cascade(
  .data,
  .by = NULL,
  .nrow = NULL,
  .ncol = NULL,
  .add_n = TRUE,
  .text_size = 4,
  .arrows = TRUE
)
```

## Arguments

- .data:

  A data frame, or data frame extension (e.g. a tibble). For
  `plot_cascade()`, the variable displayed on the x-axis should be named
  `"x"` and the number of observations should be named `"n"`, like the
  tibble returned by `compute_cascade()`.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Name-value pairs of conditions defining the different statuses to be
  plotted (see examples).

- .weights:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Optional weights. Should select only one variable.

- .by:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  A variable or a set of variables to group by the computation of the
  cascade, and to generate facets. To select several variables, use
  [`dplyr::pick()`](https://dplyr.tidyverse.org/reference/pick.html)
  (see examples).

- .nrow, .ncol:

  Number of rows and columns, for faceted plots.

- .add_n:

  Display the number of observations?

- .text_size:

  Size of the labels, passed to
  [`ggplot2::geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html).

- .arrows:

  Display arrows between statuses?

## Value

A `ggplot2` plot or a `tibble`.

## Details

`ggcascade()` calls `compute_cascade()` to generate a data set passed to
`plot_cascade()`. Use `compute_cascade()` and `plot_cascade()` for more
controls.

## Examples

``` r
ggplot2::diamonds |>
  ggcascade(
    all = TRUE,
    big = carat > .5,
    "big & ideal" = carat > .5 & cut == "Ideal"
  )


ggplot2::mpg |>
  ggcascade(
    all = TRUE,
    recent = year > 2000,
    "recent & economic" = year > 2000 & displ < 3,
    .by = cyl,
    .ncol = 3,
    .arrows = FALSE,
    .text_size = 3
  )


ggplot2::mpg |>
  ggcascade(
    all = TRUE,
    recent = year > 2000,
    "recent & economic" = year > 2000 & displ < 3,
    .by = pick(cyl, drv),
    .add_n = FALSE,
    .text_size = 2
  )
```
