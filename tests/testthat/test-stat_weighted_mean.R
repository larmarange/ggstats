test_that("stat_weighted_mean()", {
  skip_on_cran()
  skip_if_not_installed("reshape")

  library(ggplot2)
  data(tips, package = "reshape")

  vdiffr::expect_doppelganger(
    "stat_weighted_mean() point",
    ggplot(tips) +
      aes(x = day, y = total_bill) +
      geom_point()
  )

  vdiffr::expect_doppelganger(
    "stat_weighted_mean() geom-default",
    ggplot(tips) +
      aes(x = day, y = total_bill) +
      stat_weighted_mean()
  )

  vdiffr::expect_doppelganger(
    "stat_weighted_mean() geom-line",
    ggplot(tips) +
      aes(x = day, y = total_bill, group = 1) +
      stat_weighted_mean(geom = "line")
  )

  vdiffr::expect_doppelganger(
    "stat_weighted_mean() geom-line-grouped",
    ggplot(tips) +
      aes(x = day, y = total_bill, colour = sex, group = sex) +
      stat_weighted_mean(geom = "line")
  )

  vdiffr::expect_doppelganger(
    "stat_weighted_mean() geom-bar-dodge",
    ggplot(tips) +
      aes(x = day, y = total_bill, fill = sex) +
      stat_weighted_mean(geom = "bar", position = "dodge")
  )

  # computing a proportion on the fly
  vdiffr::expect_doppelganger(
    "stat_weighted_mean() geom-bar-dodge-percent",
    ggplot(tips) +
      aes(x = day, y = as.integer(smoker == "Yes"), fill = sex) +
      stat_weighted_mean(geom = "bar", position = "dodge") +
      scale_y_continuous(labels = scales::percent)
  )

  # taking into account some weights
  d <- as.data.frame(Titanic)
  vdiffr::expect_doppelganger(
    "stat_weighted_mean() titanic",
    ggplot(d) +
      aes(
        x = Class, y = as.integer(Survived == "Yes"),
        weight = Freq, fill = Sex
      ) +
      geom_bar(stat = "weighted_mean", position = "dodge") +
      scale_y_continuous(labels = scales::percent) +
      labs(y = "Survived")
  )
})
