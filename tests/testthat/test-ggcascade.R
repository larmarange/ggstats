test_that("ggcascade() works", {
  skip_on_cran()
  library(ggplot2)

  p <- ggplot2::diamonds |>
    ggcascade(
      all = TRUE,
      big = carat > .5,
      "big & ideal" = carat > .5 & cut == "Ideal"
    )

  vdiffr::expect_doppelganger(
    "ggcascade diamonds",
    p
  )

  p <- ggplot2::mpg |>
    ggcascade(
      all = TRUE,
      recent = year > 2000,
      "recent & economic" = year > 2000 & displ < 3,
      .by = cyl,
      .ncol = 3,
      .arrows = FALSE
    )
  vdiffr::expect_doppelganger(
    "ggcascade mpg by, no arrow and ncol",
    p
  )

  p <- ggplot2::mpg |>
    ggcascade(
      all = TRUE,
      recent = year > 2000,
      "recent & economic" = year > 2000 & displ < 3,
      .by = pick(cyl, drv),
      .add_n = FALSE,
      .text_size = 2
    )
  vdiffr::expect_doppelganger(
    "ggcascade mpg py pick, no n, text_size",
    p
  )

  d <- as.data.frame(Titanic)
  p <- d |>
    ggcascade(
      all = TRUE,
      female = Sex == "Female",
      "female & survived" = Sex == "Female" & Survived == "Yes",
      .weights = Freq,
      .by = Class
    )
  vdiffr::expect_doppelganger(
    "ggcascade titanic weights",
    p
  )
})
