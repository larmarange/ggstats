test_that("stat_prop()", {
  expect_print <- function(x) {
    expect_silent(print(x))
  }
  d <- as.data.frame(Titanic)
  library(ggplot2)

  p <- ggplot(d) +
    aes(x = Class, fill = Survived, weight = Freq, by = Class) +
    geom_bar(position = "fill") +
    geom_text(stat = "prop", position = position_fill(.5))
  expect_print(p)
  expect_print(p + facet_grid(~Sex))

  expect_print(ggplot(d) +
    aes(x = Class, fill = Survived, weight = Freq) +
    geom_bar(position = "dodge") +
    geom_text(
      aes(by = Survived),
      stat = "prop",
      position = position_dodge(0.9), vjust = "bottom"
    ))

  expect_print(ggplot(d) +
    aes(x = Class, fill = Survived, weight = Freq, by = 1) +
    geom_bar() +
    geom_text(
      aes(label = scales::percent(after_stat(prop), accuracy = 1)),
      stat = "prop",
      position = position_stack(.5)
    ))
})

test_that("stat_prop() works with an y aesthetic", {
  expect_print <- function(x) {
    expect_silent(print(x))
  }

  d <- as.data.frame(Titanic)
  p <- ggplot(d) +
    aes(y = Class, fill = Survived, weight = Freq, by = Class) +
    geom_bar(position = "fill") +
    geom_text(stat = "prop", position = position_fill(.5))
  expect_print(p)
})
