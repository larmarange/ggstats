test_that("stat_prop()", {
  skip_on_cran()
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
    "stat_prop() direct call",
    ggplot(d) +
      aes(x = Class, fill = Survived, weight = Freq, by = Class) +
      stat_prop(geom = "bar")
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
    "stat_prop() titanic-dodge (not specifying by)",
    ggplot(d) +
      aes(x = Class, fill = Survived, weight = Freq) +
      geom_bar(position = "dodge") +
      geom_text(
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
  skip_on_cran()

  d <- as.data.frame(Titanic)
  p <- ggplot(d) +
    aes(y = Class, fill = Survived, weight = Freq, by = Class) +
    geom_bar(position = "fill") +
    geom_text(stat = "prop", position = position_fill(.5))

  vdiffr::expect_doppelganger("stat_prop() y-aes", p)
})

test_that("stat_prop() works with a character by", {
  library(ggplot2)
  skip_on_cran()

  d <- as.data.frame(Titanic)
  p <- ggplot(d) +
    aes(y = Class, fill = Survived, weight = Freq, by = as.character(Class)) +
    geom_bar(position = "fill") +
    geom_text(stat = "prop", position = position_fill(.5))

  vdiffr::expect_doppelganger("stat_prop() by-character", p)
})

test_that("stat_prop() works with default_by", {
  library(ggplot2)
  skip_on_cran()

  d <- as.data.frame(Titanic)

  p <- ggplot(d) +
    aes(x = Class, fill = Survived, weight = Freq, y = after_stat(prop)) +
    geom_bar(stat = "prop")
  vdiffr::expect_doppelganger("stat_prop() default_by none", p)

  p <- ggplot(d) +
    aes(x = Class, fill = Survived, weight = Freq, y = after_stat(prop)) +
    geom_bar(stat = "prop", default_by = "fill")
  vdiffr::expect_doppelganger("stat_prop() default_by fill", p)

  p <- ggplot(d) +
    aes(x = Class, fill = Survived, weight = Freq, y = after_stat(prop)) +
    geom_bar(stat = "prop", default_by = "x")
  vdiffr::expect_doppelganger("stat_prop() default_by x", p)

  p <- ggplot(d) +
    aes(y = Class, fill = Survived, weight = Freq, x = after_stat(prop)) +
    geom_bar(stat = "prop", default_by = "x")
  vdiffr::expect_doppelganger("stat_prop() default_by x horizontal", p)
})

test_that("geom_prop_bar() & geom_prop_text()", {
  d <- as.data.frame(Titanic)
  p <- ggplot(d) +
    aes(y = Class, fill = Survived, weight = Freq) +
    geom_prop_bar() +
    geom_prop_text()
  vdiffr::expect_doppelganger("geom_prop_bar() & geom_prop_text()", p)

  p <- ggplot(d) +
    aes(x = Class, fill = Survived, weight = Freq) +
    geom_prop_bar(height = "count") +
    geom_prop_text(
      height = "count",
      labels = "count",
      labeller = scales::number
    )
  vdiffr::expect_doppelganger("geom_prop_bar() & geom_prop_text() - count", p)
})
