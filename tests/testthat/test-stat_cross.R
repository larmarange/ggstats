test_that("stat_cross()", {
  expect_print <- function(x) {
    expect_silent(print(x))
  }
  d <- as.data.frame(Titanic)
  library(ggplot2)

  # plot number of observations
  expect_print(ggplot(d) +
    aes(x = Class, y = Survived, weight = Freq, size = after_stat(observed)) +
    stat_cross() +
    scale_size_area(max_size = 20))

  # custom shape and fill colour based on chi-squared residuals
  expect_print(ggplot(d) +
    aes(
      x = Class, y = Survived, weight = Freq,
      size = after_stat(observed), fill = after_stat(std.resid)
    ) +
    stat_cross(shape = 22) +
    scale_fill_steps2(breaks = c(-3, -2, 2, 3), show.limits = TRUE) +
    scale_size_area(max_size = 20))

  # plotting the number of observations as a table
  expect_print(ggplot(d) +
    aes(
      x = Class, y = Survived, weight = Freq, label = after_stat(observed)
    ) +
    geom_text(stat = "cross"))

  # Row proportions with standardized residuals
  expect_print(ggplot(d) +
    aes(
      x = Class, y = Survived, weight = Freq,
      label = scales::percent(after_stat(row.prop)),
      size = NULL, fill = after_stat(std.resid)
    ) +
    stat_cross(shape = 22, size = 30) +
    geom_text(stat = "cross") +
    scale_fill_steps2(breaks = c(-3, -2, 2, 3), show.limits = TRUE) +
    facet_grid(Sex ~ .) +
    labs(fill = "Standardized residuals") +
    theme_minimal())
})
