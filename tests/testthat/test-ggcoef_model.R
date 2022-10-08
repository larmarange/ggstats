test_that("ggcoef_model()", {
  skip_if_not_installed("broom.helpers")
  skip_if_not_installed("reshape")

  data(tips, package = "reshape")
  mod_simple <- lm(tip ~ day + time + total_bill, data = tips)

  vdiffr::expect_doppelganger(
    "ggcoef_model() mod simple",
    ggcoef_model(mod_simple)
  )

  vdiffr::expect_doppelganger(
    "ggcoef_model() mod simple no guide",
    ggcoef_model(mod_simple, shape_guide = FALSE, colour_guide = FALSE)
  )

  # custom variable labels
  # you can use to define variable labels before computing model
  if (requireNamespace("labelled")) {
    tips_labelled <- tips %>%
      labelled::set_variable_labels(
        day = "Day of the week",
        time = "Lunch or Dinner",
        total_bill = "Bill's total"
      )
    mod_labelled <- lm(tip ~ day + time + total_bill, data = tips_labelled)
    vdiffr::expect_doppelganger(
      "ggcoef_model() mod labelled",
      ggcoef_model(mod_labelled)
    )
  }

  vdiffr::expect_doppelganger(
    "ggcoef_model() mod simple with variable labels",
    ggcoef_model(
      mod_simple,
      variable_labels = c(
        day = "Week day",
        time = "Time (lunch or dinner ?)",
        total_bill = "Total of the bill"
      )
    )
  )

  # if labels are too long, you can use 'facet_labeller' to wrap them
  vdiffr::expect_doppelganger(
    "ggcoef_model() mod simple facet_labeller",
    ggcoef_model(
      mod_simple,
      variable_labels = c(
        day = "Week day",
        time = "Time (lunch or dinner ?)",
        total_bill = "Total of the bill"
      ),
      facet_labeller = ggplot2::label_wrap_gen(10)
    )
  )

  # do not display variable facets but add colour guide
  vdiffr::expect_doppelganger(
    "ggcoef_model() mod simple no variable facets",
    ggcoef_model(
      mod_simple,
      facet_row = NULL,
      colour_guide = TRUE
    )
  )

  # a logistic regression example
  d_titanic <- as.data.frame(Titanic)
  d_titanic$Survived <- factor(d_titanic$Survived, c("No", "Yes"))
  mod_titanic <- glm(
    Survived ~ Sex * Age + Class,
    weights = Freq,
    data = d_titanic,
    family = binomial
  )

  vdiffr::expect_doppelganger(
    "ggcoef_model() logistic regression",
    ggcoef_model(mod_titanic, exponentiate = TRUE)
  )

  # display intercept
  vdiffr::expect_doppelganger(
    "ggcoef_model() logistic regression with intercept",
    ggcoef_model(mod_titanic, exponentiate = TRUE, intercept = TRUE)
  )

  # display only a subset of terms
  vdiffr::expect_doppelganger(
    "ggcoef_model() logistic regression subset",
    ggcoef_model(mod_titanic, exponentiate = TRUE, include = c("Age", "Class"))
  )

  # do not change points' shape based on significance
  vdiffr::expect_doppelganger(
    "ggcoef_model() logistic regression no significance",
    ggcoef_model(mod_titanic, exponentiate = TRUE, significance = NULL)
  )

  # a black and white version
  vdiffr::expect_doppelganger(
    "ggcoef_model() logistic regression black and white",
    ggcoef_model(
      mod_titanic,
      exponentiate = TRUE,
      colour = NULL,
      stripped_rows = FALSE
    )
  )

  # show dichotomous terms on one row
  vdiffr::expect_doppelganger(
    "ggcoef_model() logistic regression no reference row",
    ggcoef_model(
      mod_titanic,
      exponentiate = TRUE,
      no_reference_row = broom.helpers::all_dichotomous(),
      categorical_terms_pattern =
        "{ifelse(dichotomous, paste0(level, ' / ', reference_level), level)}",
      show_p_values = FALSE
    )
  )

  # works also with with polynomial terms
  mod_poly <- lm(
    tip ~ poly(total_bill, 3) + day,
    data = tips,
  )
  vdiffr::expect_doppelganger(
    "ggcoef_model() polynomial terms",
    ggcoef_model(mod_poly)
  )

  # or with different type of contrasts
  # for sum contrasts, the value of the reference term is computed
  if (requireNamespace("emmeans")) {
    mod2 <- lm(
      tip ~ day + time + sex,
      data = tips,
      contrasts = list(time = contr.sum, day = contr.treatment(4, base = 3))
    )
    vdiffr::expect_doppelganger(
      "ggcoef_model() different types of contrasts",
      ggcoef_model(mod2)
    )
  }
})

test_that("ggcoef_compare()", {
  skip_if_not_installed("broom.helpers")

  # Use ggcoef_compare() for comparing several models on the same plot
  mod1 <- lm(Fertility ~ ., data = swiss)
  mod2 <- step(mod1, trace = 0)
  mod3 <- lm(Fertility ~ Agriculture + Education * Catholic, data = swiss)
  models <- list(
    "Full model" = mod1,
    "Simplified model" = mod2,
    "With interaction" = mod3
  )

  vdiffr::expect_doppelganger(
    "ggcoef_compare() dodged",
    ggcoef_compare(models)
  )

  vdiffr::expect_doppelganger(
    "ggcoef_compare() faceted",
    ggcoef_compare(models, type = "faceted")
  )
})

test_that("ggcoef_multinom()", {
  skip_if_not_installed("broom.helpers")
  skip_if_not_installed("nnet")

  library(nnet)
  mod <- multinom(Species ~ ., data = iris)

  vdiffr::expect_doppelganger(
    "ggcoef_multinom() dodged",
    ggcoef_multinom(mod, exponentiate = TRUE)
  )

  vdiffr::expect_doppelganger(
    "ggcoef_multinom() faceted",
    ggcoef_multinom(mod, type = "faceted")
  )

  vdiffr::expect_doppelganger(
    "ggcoef_multinom() faceted custom y level label",
    ggcoef_multinom(
      mod,
      type = "faceted",
      y.level_label = c("versicolor" = "versicolor\n(ref: setosa)")
    )
  )
})


test_that("ggcoef_model() works with tieders not returning p-values", {
  skip_if_not_installed("broom.helpers")

  mod <- lm(Sepal.Width ~ Species, iris)
  my_tidier <- function(x, ...) {
    x %>%
      broom::tidy(...) %>%
      dplyr::select(-.data$p.value)
  }
  vdiffr::expect_doppelganger(
    "ggcoef_model() no p values",
    ggcoef_model(mod, tidy_fun = my_tidier)
  )
})

test_that("ggcoef_compare() complete NA respecting variables order", {
  skip_if_not_installed("broom.helpers")

  m1 <- lm(Fertility ~ Education + Catholic, data = swiss)
  m2 <- lm(Fertility ~ Education + Catholic + Agriculture, data = swiss)
  m3 <- lm(
    Fertility ~ Education + Catholic + Agriculture + Infant.Mortality,
    data = swiss
  )
  res <- ggcoef_compare(models = list(m1, m2, m3), return_data = TRUE)
  expect_equal(
    res$variable[1:4],
    structure(1:4, .Label = c(
      "Education", "Catholic", "Agriculture",
      "Infant.Mortality"
    ), class = "factor")
  )
})

test_that("ggcoef_compare() does not produce an error with an include", {
  skip_if_not_installed("survival")
  skip_if_not_installed("broom.helpers")
  m1 <- survival::coxph(
    survival::Surv(time, status) ~ prior + age,
    data = survival::veteran
  )
  m2 <- survival::coxph(
    survival::Surv(time, status) ~ prior + celltype,
    data = survival::veteran
  )
  models <- list("Model 1" = m1, "Model 2" = m2)

  vdiffr::expect_doppelganger(
    "ggcoef_compare() with include",
    ggcoef_compare(models, include = broom.helpers::starts_with("p"))
  )
})
