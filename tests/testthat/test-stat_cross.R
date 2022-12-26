test_that("stat_cross()", {
  skip_on_cran()
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

  # custom shape and fill colour based phi coefficients
  p <- ggplot(d) +
    aes(
      x = Class, y = Survived, weight = Freq,
      size = after_stat(observed), fill = after_stat(phi)
    ) +
    stat_cross(shape = 22) +
    scale_fill_steps2(show.limits = TRUE) +
    scale_size_area(max_size = 20)
  vdiffr::expect_doppelganger("stat_cross() phi coefficients", p)

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

test_that("phi coefficients", {
  res <- Titanic %>%
    as.data.frame() %>%
    xtabs(Freq ~ Sex + Class, data = .) %>%
    chisq.test() %>%
    augment_chisq_add_phi() %>%
    dplyr::mutate(.phi = round(.data$.phi, digits = 3))
  expect_equal(
    res$.phi,
    c(-0.236, 0.236, -0.149, 0.149, -0.107, 0.107, 0.375, -0.375)
  )
})
