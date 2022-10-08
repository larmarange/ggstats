test_that("stat_cross()", {
  library(ggplot2)

  d <- as.data.frame(Titanic)

  # plot number of observations
  p <- ggplot(d) +
    aes(x = Class, y = Survived, weight = Freq, size = after_stat(observed)) +
    stat_cross() +
    scale_size_area(max_size = 20)
  vdiffr::expect_doppelganger("stat_cross() n obs", p)

  # custom shape and fill colour based on chi-squared residuals
  p <- ggplot(d) +
    aes(
      x = Class, y = Survived, weight = Freq,
      size = after_stat(observed), fill = after_stat(std.resid)
    ) +
    stat_cross(shape = 22) +
    scale_fill_steps2(breaks = c(-3, -2, 2, 3), show.limits = TRUE) +
    scale_size_area(max_size = 20)
  vdiffr::expect_doppelganger("stat_cross() shape-22", p)

  # plotting the number of observations as a table
  p <- ggplot(d) +
    aes(
      x = Class, y = Survived, weight = Freq, label = after_stat(observed)
    ) +
    geom_text(stat = "cross")
  vdiffr::expect_doppelganger("stat_cross() table", p)

  # Row proportions with standardized residuals
  p <- ggplot(d) +
    aes(
      x = Class, y = Survived, weight = Freq,
      label = scales::percent(after_stat(row.prop)),
      size = NULL, fill = after_stat(std.resid)
    ) +
    stat_cross(shape = 22, size = 30) +
    geom_text(stat = "cross") +
    scale_fill_steps2(breaks = c(-3, -2, 2, 3), show.limits = TRUE) +
    facet_grid(Sex ~ .) +
    labs(fill = "Standardized residuals")
  vdiffr::expect_doppelganger("stat_cross() residuals", p)
})
