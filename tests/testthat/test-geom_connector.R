test_that("geom_connector() and geom_bar_connector()() works", {
  skip_on_cran()
  library(ggplot2)

  p <- ggplot(diamonds) +
    aes(x = clarity, fill = cut) +
    geom_bar(width = .5) +
    geom_bar_connector(width = .5, linewidth = .25) +
    theme_minimal() +
    theme(legend.position = "bottom")

  vdiffr::expect_doppelganger(
    "geom_bar_connector",
    p
  )

  p <- ggplot(diamonds) +
    aes(x = clarity, fill = cut) +
    geom_bar(width = .5) +
    geom_bar_connector(
      width = .5,
      continuous = TRUE,
      colour = "red",
      linetype = "dotted",
      add_baseline = FALSE,
    ) +
    theme(legend.position = "bottom")

  vdiffr::expect_doppelganger(
    "geom_bar_connector continuous and no baseline",
    p
  )

  p <- ggplot(diamonds) +
    aes(x = clarity, fill = cut) +
    geom_bar(width = .5, position = "fill") +
    geom_bar_connector(width = .5, position = "fill") +
    theme(legend.position = "bottom")

  vdiffr::expect_doppelganger(
    "geom_bar_connector position fill",
    p
  )

  p <- ggplot(diamonds) +
    aes(x = clarity, fill = cut) +
    geom_bar(width = .5, position = "diverging") +
    geom_bar_connector(width = .5, position = "diverging", linewidth = .25) +
    theme(legend.position = "bottom")

  vdiffr::expect_doppelganger(
    "geom_bar_connector position diverging",
    p
  )

  p <- ggplot(mtcars) +
    aes(x = wt, y = mpg, colour = factor(cyl)) +
    geom_connector() +
    geom_point()

  vdiffr::expect_doppelganger(
    "geom_connector",
    p
  )

  p <- ggplot(mtcars) +
    aes(x = wt, y = mpg, colour = factor(cyl)) +
    geom_connector(continuous = TRUE) +
    geom_point()

  vdiffr::expect_doppelganger(
    "geom_connector continuous",
    p
  )

  p <- ggplot(mtcars) +
    aes(x = wt, y = mpg, colour = factor(cyl)) +
    geom_connector(width = 0) +
    geom_point()

  vdiffr::expect_doppelganger(
    "geom_connector zero width",
    p
  )

  p <- ggplot(mtcars) +
    aes(x = wt, y = mpg, colour = factor(cyl)) +
    geom_connector(width = Inf) +
    geom_point()

  vdiffr::expect_doppelganger(
    "geom_connector infinite width",
    p
  )

  p <- ggplot(mtcars) +
    aes(x = wt, y = mpg, colour = factor(cyl)) +
    geom_connector(width = Inf, continuous = TRUE) +
    geom_point()

  vdiffr::expect_doppelganger(
    "geom_connector infinite width and continuous",
    p
  )
})
