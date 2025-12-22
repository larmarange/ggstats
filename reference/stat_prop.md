# Compute proportions according to custom denominator

`stat_prop()` is a variation of
[`ggplot2::stat_count()`](https://ggplot2.tidyverse.org/reference/geom_bar.html)
allowing to compute custom proportions according to the **by** aesthetic
defining the denominator (i.e. all proportions for a same value of
**by** will sum to 1). If the **by** aesthetic is not specified,
denominators will be determined according to the `default_by` argument.

## Usage

``` r
stat_prop(
  mapping = NULL,
  data = NULL,
  geom = "bar",
  position = "fill",
  ...,
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE,
  complete = NULL,
  default_by = "total"
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options:

  If `NULL`, the default, the data is inherited from the plot data as
  specified in the call to
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

  A `data.frame`, or other object, will override the plot data. All
  objects will be fortified to produce a data frame. See
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  for which variables will be created.

  A `function` will be called with a single argument, the plot data. The
  return value must be a `data.frame`, and will be used as the layer
  data. A `function` can be created from a `formula` (e.g.
  `~ head(.x, 10)`).

- geom:

  Override the default connection with
  [`ggplot2::geom_bar()`](https://ggplot2.tidyverse.org/reference/geom_bar.html).

- position:

  A position adjustment to use on the data for this layer. This can be
  used in various ways, including to prevent overplotting and improving
  the display. The `position` argument accepts the following:

  - The result of calling a position function, such as
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html).
    This method allows for passing extra arguments to the position.

  - A string naming the position adjustment. To give the position as a
    string, strip the function name of the `position_` prefix. For
    example, to use
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html),
    give the position as `"jitter"`.

  - For more information and other ways to specify the position, see the
    [layer
    position](https://ggplot2.tidyverse.org/reference/layer_positions.html)
    documentation.

- ...:

  Other arguments passed on to
  [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html)'s
  `params` argument. These arguments broadly fall into one of 4
  categories below. Notably, further arguments to the `position`
  argument, or aesthetics that are required can *not* be passed through
  `...`. Unknown arguments that are not part of the 4 categories below
  are ignored.

  - Static aesthetics that are not mapped to a scale, but are at a fixed
    value and apply to the layer as a whole. For example,
    `colour = "red"` or `linewidth = 3`. The geom's documentation has an
    **Aesthetics** section that lists the available options. The
    'required' aesthetics cannot be passed on to the `params`. Please
    note that while passing unmapped aesthetics as vectors is
    technically possible, the order and required length is not
    guaranteed to be parallel to the input data.

  - When constructing a layer using a `stat_*()` function, the `...`
    argument can be used to pass on parameters to the `geom` part of the
    layer. An example of this is
    `stat_density(geom = "area", outline.type = "both")`. The geom's
    documentation lists which parameters it can accept.

  - Inversely, when constructing a layer using a `geom_*()` function,
    the `...` argument can be used to pass on parameters to the `stat`
    part of the layer. An example of this is
    `geom_area(stat = "density", adjust = 0.5)`. The stat's
    documentation lists which parameters it can accept.

  - The `key_glyph` argument of
    [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html) may
    also be passed on through `...`. This can be one of the functions
    described as [key
    glyphs](https://ggplot2.tidyverse.org/reference/draw_key.html), to
    change the display of the layer in the legend.

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- orientation:

  The orientation of the layer. The default (`NA`) automatically
  determines the orientation from the aesthetic mapping. In the rare
  event that this fails it can be given explicitly by setting
  `orientation` to either `"x"` or `"y"`. See the *Orientation* section
  for more detail.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display. To include legend
  keys for all levels, even when no data exists, use `TRUE`. If `NA`,
  all levels are shown in legend, but unobserved levels are omitted.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`annotation_borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

- complete:

  Name (character) of an aesthetic for those statistics should be
  completed for unobserved values (see example).

- default_by:

  If the **by** aesthetic is not available, name of another aesthetic
  that will be used to determine the denominators (e.g. `"fill"`), or
  `NULL` or `"total"` to compute proportions of the total. To be noted,
  `default_by = "x"` works both for vertical and horizontal bars.

## Value

A `ggplot2` plot with the added statistic.

## Aesthetics

`stat_prop()` understands the following aesthetics (required aesthetics
are in bold):

- **x *or* y**

- by

- weight

## Computed variables

- `after_stat(count)`:

  number of points in bin

- `after_stat(denominator)`:

  denominator for the proportions

- `after_stat(prop)`:

  computed proportion, i.e.
  `after_stat(count)`/`after_stat(denominator)`

## See also

[`vignette("stat_prop")`](https://larmarange.github.io/ggstats/articles/stat_prop.md),
[`ggplot2::stat_count()`](https://ggplot2.tidyverse.org/reference/geom_bar.html).
For an alternative approach, see
<https://github.com/tidyverse/ggplot2/issues/5505#issuecomment-1791324008>.

## Examples

``` r
library(ggplot2)
d <- as.data.frame(Titanic)

p <- ggplot(d) +
  aes(x = Class, fill = Survived, weight = Freq, by = Class) +
  geom_bar(position = "fill") +
  geom_text(stat = "prop", position = position_fill(.5))
p

p + facet_grid(~Sex)


ggplot(d) +
  aes(x = Class, fill = Survived, weight = Freq) +
  geom_bar(position = "dodge") +
  geom_text(
    aes(by = Survived),
    stat = "prop",
    position = position_dodge(0.9), vjust = "bottom"
  )

# \donttest{
if (requireNamespace("scales")) {
  ggplot(d) +
    aes(x = Class, fill = Survived, weight = Freq, by = 1) +
    geom_bar() +
    geom_text(
      aes(label = scales::percent(after_stat(prop), accuracy = 1)),
      stat = "prop",
      position = position_stack(.5)
    )
}


# displaying unobserved levels with complete
d <- diamonds |>
  dplyr::filter(!(cut == "Ideal" & clarity == "I1")) |>
  dplyr::filter(!(cut == "Very Good" & clarity == "VS2")) |>
  dplyr::filter(!(cut == "Premium" & clarity == "IF"))
p <- ggplot(d) +
  aes(x = clarity, fill = cut, by = clarity) +
  geom_bar(position = "fill")
p + geom_text(stat = "prop", position = position_fill(.5))

p + geom_text(stat = "prop", position = position_fill(.5), complete = "fill")

# }
```
