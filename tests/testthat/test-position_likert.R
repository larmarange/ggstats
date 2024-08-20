test_that("position_likert()", {
  skip_on_cran()
  library(ggplot2)

  p <- ggplot(diamonds) +
    aes(y = clarity, fill = cut) +
    geom_bar(position = "likert") +
    scale_x_continuous(label = label_percent_abs()) +
    scale_fill_brewer(palette = "PiYG") +
    xlab("proportion")

  vdiffr::expect_doppelganger(
    "position_likert() base",
    p
  )

  vdiffr::expect_doppelganger(
    "position_likert() facet",
    p + facet_grid(~ price > 2500)
  )

  p <- ggplot(diamonds) +
    aes(y = clarity, fill = cut) +
    geom_bar(position = "diverging") +
    scale_x_continuous(label = label_number_abs()) +
    scale_fill_brewer(palette = "PiYG")

  vdiffr::expect_doppelganger(
    "position_diverging() base",
    p
  )

  vdiffr::expect_doppelganger(
    "position_diverging() facet",
    p + facet_grid(~ price > 2500)
  )

  p <- ggplot(diamonds) +
    aes(y = clarity, fill = cut) +
    geom_bar(position = position_likert(reverse = TRUE))

  vdiffr::expect_doppelganger(
    "position_likert() reverse",
    p
  )

  p <- ggplot(diamonds) +
    aes(y = clarity, fill = cut) +
    geom_bar(position = position_diverging(reverse = TRUE))

  vdiffr::expect_doppelganger(
    "position_diverging() reverse",
    p
  )

  custom_label <- function(x) {
    p <- scales::percent(x, accuracy = 1)
    p[x < .075] <- ""
    p
  }

  p <- ggplot(diamonds) +
    aes(y = clarity, fill = cut) +
    geom_bar(position = "likert") +
    geom_text(
      aes(by = clarity, label = custom_label(after_stat(prop))),
      stat = "prop",
      position = position_likert(vjust = .5)
    ) +
    scale_x_continuous(label = label_percent_abs()) +
    scale_fill_brewer(palette = "PiYG", direction = -1) +
    xlab("proportion")

  vdiffr::expect_doppelganger(
    "position_likert() vjust",
    p
  )

  p <- ggplot(diamonds) +
    aes(y = clarity, fill = cut) +
    geom_bar(position = position_likert(exclude_fill_values = "Very Good")) +
    scale_x_continuous(label = label_percent_abs()) +
    scale_fill_brewer(palette = "PiYG") +
    xlab("proportion")

  vdiffr::expect_doppelganger(
    "position_likert() exclude_fill_values",
    p
  )
})
