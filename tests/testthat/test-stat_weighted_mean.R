test_that("stat_weighted_mean()", {
  expect_print <- function(x) {
    expect_silent(print(x))
  }

  skip_if_not_installed("reshape")
  data(tips, package = "reshape")
  library(ggplot2)

  expect_print(ggplot(tips) +
    aes(x = day, y = total_bill) +
    geom_point())

  expect_print(ggplot(tips) +
    aes(x = day, y = total_bill) +
    stat_weighted_mean())

  expect_print(ggplot(tips) +
    aes(x = day, y = total_bill, group = 1) +
    stat_weighted_mean(geom = "line"))

  expect_print(ggplot(tips) +
    aes(x = day, y = total_bill, colour = sex, group = sex) +
    stat_weighted_mean(geom = "line"))

  expect_print(ggplot(tips) +
    aes(x = day, y = total_bill, fill = sex) +
    stat_weighted_mean(geom = "bar", position = "dodge"))

  # computing a proportion on the fly
  expect_print(ggplot(tips) +
    aes(x = day, y = as.integer(smoker == "Yes"), fill = sex) +
    stat_weighted_mean(geom = "bar", position = "dodge") +
    scale_y_continuous(labels = scales::percent))

  # taking into account some weights
  d <- as.data.frame(Titanic)
  expect_print(ggplot(d) +
    aes(
      x = Class, y = as.integer(Survived == "Yes"),
      weight = Freq, fill = Sex
    ) +
    geom_bar(stat = "weighted_mean", position = "dodge") +
    scale_y_continuous(labels = scales::percent) +
    labs(y = "Survived"))
})
