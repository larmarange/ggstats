test_that("geom_connect() and geom_connect_bars()() works", {
  skip_on_cran()
  library(ggplot2)

  p <- ggplot(diamonds) +
    aes(x = clarity, fill = cut) +
    geom_bar(width = .5) +
    geom_connect_bars(width = .5, linewidth = .25) +
    theme_minimal() +
    theme(legend.position = "bottom")

  vdiffr::expect_doppelganger(
    "geom_connect_bars",
    p
  )

  p <- ggplot(diamonds) +
    aes(x = clarity, fill = cut) +
    geom_bar(width = .5) +
    geom_connect_bars(
      width = .5,
      continuous = TRUE,
      colour = "red",
      linetype = "dotted",
      add_baseline = FALSE,
    ) +
    theme(legend.position = "bottom")

  vdiffr::expect_doppelganger(
    "geom_connect_bars continuous and no baseline",
    p
  )

  p <- ggplot(diamonds) +
    aes(x = clarity, fill = cut) +
    geom_bar(width = .5, position = "fill") +
    geom_connect_bars(width = .5, position = "fill") +
    theme(legend.position = "bottom")

  vdiffr::expect_doppelganger(
    "geom_connect_bars position fill",
    p
  )

  p <- ggplot(diamonds) +
    aes(x = clarity, fill = cut) +
    geom_bar(width = .5, position = "diverging") +
    geom_connect_bars(width = .5, position = "diverging", linewidth = .25) +
    theme(legend.position = "bottom")

  vdiffr::expect_doppelganger(
    "geom_connect_bars position diverging",
    p
  )

  p <- ggplot(mtcars) +
    aes(x = wt, y = mpg, colour = factor(cyl)) +
    geom_connect() +
    geom_point()

  vdiffr::expect_doppelganger(
    "geom_connect",
    p
  )

  p <- ggplot(mtcars) +
    aes(x = wt, y = mpg, colour = factor(cyl)) +
    geom_connect(continuous = TRUE) +
    geom_point()

  vdiffr::expect_doppelganger(
    "geom_connect continuous",
    p
  )

  p <- ggplot(mtcars) +
    aes(x = wt, y = mpg, colour = factor(cyl)) +
    geom_connect(width = 0) +
    geom_point()

  vdiffr::expect_doppelganger(
    "geom_connect zero width",
    p
  )

  p <- ggplot(mtcars) +
    aes(x = wt, y = mpg, colour = factor(cyl)) +
    geom_connect(width = Inf) +
    geom_point()

  vdiffr::expect_doppelganger(
    "geom_connect infinite width",
    p
  )

  p <- ggplot(mtcars) +
    aes(x = wt, y = mpg, colour = factor(cyl)) +
    geom_connect(width = Inf, continuous = TRUE) +
    geom_point()

  vdiffr::expect_doppelganger(
    "geom_connect infinite width and continuous",
    p
  )
})
