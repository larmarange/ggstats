test_that("geom_stripped_cols() and geom_stripped_rows() works", {
  skip_on_cran()
  library(ggplot2)

  p <- ggplot(iris) +
    aes(x = Species, y = Petal.Length) +
    geom_count()

  vdiffr::expect_doppelganger(
    "stripped rows and cols",
    p +
      geom_stripped_rows(
        odd = "blue", even = "yellow",
        alpha = .1, nudge_y = .5
      ) +
      geom_stripped_cols()
  )
})
