test_that("gglikert)", {
  skip_on_cran()
  skip_if_not_installed("labelled")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")

  likert_levels <- c(
    "Strongly disagree",
    "Disagree",
    "Neither agree nor disagree",
    "Agree",
    "Strongly agree"
  )
  set.seed(42)
  df <-
    dplyr::tibble(
      q1 = sample(likert_levels, 150, replace = TRUE),
      q2 = sample(likert_levels, 150, replace = TRUE, prob = 5:1),
      q3 = sample(likert_levels, 150, replace = TRUE, prob = 1:5),
      q4 = sample(likert_levels, 150, replace = TRUE, prob = 1:5),
      q5 = sample(c(likert_levels, NA), 150, replace = TRUE),
      q6 = sample(likert_levels, 150, replace = TRUE, prob = c(1, 0, 1, 1, 0))
    ) %>%
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      ~ factor(.x, levels = likert_levels)
    ))

  likert_levels_dk <- c(
    "Strongly disagree",
    "Disagree",
    "Neither agree nor disagree",
    "Agree",
    "Strongly agree",
    "Don't know"
  )
  df_dk <-
    dplyr::tibble(
      q1 = sample(likert_levels_dk, 150, replace = TRUE),
      q2 = sample(likert_levels_dk, 150, replace = TRUE, prob = 6:1),
      q3 = sample(likert_levels_dk, 150, replace = TRUE, prob = 1:6),
      q4 = sample(likert_levels_dk, 150, replace = TRUE, prob = 1:6),
      q5 = sample(c(likert_levels_dk, NA), 150, replace = TRUE),
      q6 = sample(
        likert_levels_dk, 150,
        replace = TRUE, prob = c(1, 0, 1, 1, 0, 1)
      )
    ) %>%
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      ~ factor(.x, levels = likert_levels_dk)
    ))

  vdiffr::expect_doppelganger(
    "gglikert() mod simple",
    gglikert(df)
  )

  expect_error(
    d <- gglikert_data(df),
    NA
  )
  expect_equal(levels(d$.answer), likert_levels)

  vdiffr::expect_doppelganger(
    "gglikert() include and width",
    gglikert(df, include = q1:q3, width = .5)
  )

  vdiffr::expect_doppelganger(
    "gglikert() variable_labels",
    gglikert(df, variable_labels = c(q2 = "second question"))
  )

  vdiffr::expect_doppelganger(
    "gglikert() sort prop asc",
    gglikert(df, sort = "asc")
  )

  vdiffr::expect_doppelganger(
    "gglikert() sort prop desc",
    gglikert(df, sort = "desc")
  )

  vdiffr::expect_doppelganger(
    "gglikert() sort mean asc",
    gglikert(df, sort = "asc", sort_method = "mean")
  )

  vdiffr::expect_doppelganger(
    "gglikert() sort mean desc",
    gglikert(df, sort = "desc", sort_method = "mean")
  )

  vdiffr::expect_doppelganger(
    "gglikert() sort median asc",
    gglikert(df, sort = "asc", sort_method = "median")
  )

  vdiffr::expect_doppelganger(
    "gglikert() sort median desc",
    gglikert(df, sort = "desc", sort_method = "median")
  )

  vdiffr::expect_doppelganger(
    "gglikert() sort prop asc include_center",
    gglikert(df, sort = "asc", sort_prop_include_center = TRUE)
  )

  vdiffr::expect_doppelganger(
    "gglikert() exclude_fill_values",
    gglikert(df, exclude_fill_values = "Neither agree nor disagree")
  )

  vdiffr::expect_doppelganger(
    "gglikert() add_labels",
    gglikert(df, add_labels = FALSE)
  )

  vdiffr::expect_doppelganger(
    "gglikert() customize labels",
    gglikert(df, labels_size = 5, labels_hide_below = .3, labels_accuracy = .1)
  )

  vdiffr::expect_doppelganger(
    "gglikert() add_totals",
    gglikert(df, add_totals = FALSE)
  )

  vdiffr::expect_doppelganger(
    "gglikert() customize totals",
    gglikert(
      df,
      totals_size = 5,
      totals_fontface = "italic",
      totals_include_center = TRUE,
      totals_hjust = 0
    )
  )

  vdiffr::expect_doppelganger(
    "gglikert() reverse",
    gglikert(df, y_reverse = TRUE, reverse_likert = TRUE)
  )

  vdiffr::expect_doppelganger(
    "gglikert() variable labels and y_label_wrap",
    df %>%
      labelled::set_variable_labels(
        q1 = "first question",
        q2 = "second question",
        q3 = "third question with a very very very veru very very long label"
      ) %>%
      gglikert(
        variable_labels = c(
          q2 = "question 2",
          q4 = "another question with a long long long long long long label"
        ),
        y_label_wrap = 20
      )
  )

  vdiffr::expect_doppelganger(
    "gglikert_stacked()",
    gglikert_stacked(df)
  )

  vdiffr::expect_doppelganger(
    "gglikert_stacked() add_median_line",
    gglikert_stacked(df, add_median_line = TRUE)
  )

  df_group <- df
  df_group$group1 <- sample(c("A", "B"), 150, replace = TRUE)
  df_group$group2 <- sample(c("a", "b", "c"), 150, replace = TRUE)

  vdiffr::expect_doppelganger(
    "gglikert() facet_cols",
    gglikert(df_group, q1:q6, facet_cols = vars(group1))
  )

  vdiffr::expect_doppelganger(
    "gglikert() facet_rows",
    gglikert(df_group, q1:q2, facet_rows = vars(group1, group2))
  )

  vdiffr::expect_doppelganger(
    "gglikert() facet_rows and facet_cols",
    gglikert(
      df_group, q3:q6,
      facet_cols = vars(group1), facet_rows = vars(group2)
    )
  )
})
