
test_that("geom_stripped_cols() and geom_stripped_rows() works", {
  library(ggplot2)
  p <- ggplot(iris) +
    aes(x = Species, y = Petal.Length) +
    geom_count()

  expect_print <- function(x) {
    expect_error(print(x), NA)
  }

  expect_print(
    p +
      geom_stripped_rows(
        odd = "blue", even = "yellow",
        alpha = .1, nudge_y = .5
      ) +
      geom_stripped_cols()
  )
})
