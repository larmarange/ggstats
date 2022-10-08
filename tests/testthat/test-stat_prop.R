test_that("stat_prop()", {
  library(ggplot2)
  d <- as.data.frame(Titanic)

  p <- ggplot(d) +
    aes(x = Class, fill = Survived, weight = Freq, by = Class) +
    geom_bar(position = "fill") +
    geom_text(stat = "prop", position = position_fill(.5))

  vdiffr::expect_doppelganger(
    "stat_prop() titanic",
    p
  )

  vdiffr::expect_doppelganger(
    "stat_prop() titanic-facet",
    p + facet_grid(~Sex)
  )

  vdiffr::expect_doppelganger(
    "stat_prop() titanic-dodge",
    ggplot(d) +
      aes(x = Class, fill = Survived, weight = Freq) +
      geom_bar(position = "dodge") +
      geom_text(
        aes(by = Survived),
        stat = "prop",
        position = position_dodge(0.9), vjust = "bottom"
      )
  )

  vdiffr::expect_doppelganger(
    "stat_prop() titanic-stack",
    ggplot(d) +
      aes(x = Class, fill = Survived, weight = Freq, by = 1) +
      geom_bar() +
      geom_text(
        aes(label = scales::percent(after_stat(prop), accuracy = 1)),
        stat = "prop",
        position = position_stack(.5)
      )
  )
})

test_that("stat_prop() works with an y aesthetic", {
  library(ggplot2)

  d <- as.data.frame(Titanic)
  p <- ggplot(d) +
    aes(y = Class, fill = Survived, weight = Freq, by = Class) +
    geom_bar(position = "fill") +
    geom_text(stat = "prop", position = position_fill(.5))

  vdiffr::expect_doppelganger("stat_prop() y-aes", p)
})
