test_that("ggcoef_model()", {
  skip_on_cran()
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
  skip_on_cran()

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

  d <- as.data.frame(Titanic)
  m1 <- glm(Survived ~ Sex + Age, family = binomial, data = d, weights = Freq)
  m2 <- glm(
    Survived ~ Sex + Age + Class,
    family = binomial,
    data = d,
    weights = Freq
  )
  models <- list("Model 1" = m1, "Model 2" = m2)

  vdiffr::expect_doppelganger(
    "ggcoef_compare() titanic dodged",
    ggcoef_compare(models)
  )

  vdiffr::expect_doppelganger(
    "ggcoef_compare() titanic faceted",
    ggcoef_compare(models, type = "faceted")
  )

  rd <- ggcoef_compare(models, return_data = TRUE)
  expect_equal(
    levels(rd$label),
    c("Male", "Female", "Child", "Adult", "1st", "2nd", "3rd", "Crew")
  )

  expect_no_error(
    ggcoef_compare(models, add_reference_rows = FALSE)
  )
})

test_that("ggcoef_multinom()", {
  skip_if_not_installed("broom.helpers")
  skip_if_not_installed("nnet")
  skip_on_cran()

  library(nnet)
  hec <- as.data.frame(HairEyeColor)
  mod <- multinom(
    Hair ~ Eye + Sex,
    data = hec,
    weights = hec$Freq
  )

  vdiffr::expect_doppelganger(
    "ggcoef_multinom() dodged",
    ggcoef_multinom(mod, exponentiate = TRUE)
  )

  vdiffr::expect_doppelganger(
    "ggcoef_multinom() faceted",
    ggcoef_multinom(mod, type = "faceted")
  )

  vdiffr::expect_doppelganger(
    "ggcoef_multinom() table",
    ggcoef_multinom(mod, type = "table", exponentiate = TRUE)
  )

  vdiffr::expect_doppelganger(
    "ggcoef_multinom() faceted custom y level label",
    ggcoef_multinom(
      mod,
      type = "faceted",
      y.level_label = c("Brown" = "Brown\n(ref: Black)"),
      exponentiate = TRUE
    )
  )
})


test_that("ggcoef_model() works with tieders not returning p-values", {
  skip_if_not_installed("broom.helpers")
  skip_on_cran()

  mod <- lm(Sepal.Width ~ Species, iris)
  my_tidier <- function(x, ...) {
    x %>%
      broom::tidy(...) %>%
      dplyr::select(-dplyr::all_of("p.value"))
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
  skip_on_cran()
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

test_that("ggcoef_model() works with pairwise contratst", {
  skip_if_not_installed("broom.helpers")
  skip_if_not_installed("emmeans")
  mod <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
  expect_no_error(
    ggcoef_model(mod, add_pairwise_contrasts = TRUE)
  )
  expect_no_error(
    ggcoef_model(
      mod,
      add_pairwise_contrasts = TRUE,
      pairwise_variables = dplyr::starts_with("Sp"),
      keep_model_terms = TRUE
    )
  )
  mod2 <- lm(Sepal.Length ~ Species, data = iris)
  expect_no_error(
    ggcoef_compare(list(mod, mod2), add_pairwise_contrasts = TRUE)
  )
})

test_that("tidy_args is supported", {
  mod <- lm(Sepal.Length ~ Sepal.Width, data = iris)
  custom <- function(x, force = 1, ...) {
    broom::tidy(x, ...) %>%
      dplyr::mutate(estimate = force)
  }
  res <- ggcoef_model(
    mod,
    tidy_fun = custom,
    tidy_args = list(force = 3),
    return_data = TRUE
  )
  expect_equal(res$estimate, 3)
})


test_that("ggcoef_table()", {
  skip_on_cran()
  skip_if_not_installed("broom.helpers")
  skip_if_not_installed("reshape")

  data(tips, package = "reshape")
  mod_simple <- lm(tip ~ day + time + total_bill, data = tips)

  vdiffr::expect_doppelganger(
    "ggcoef_table() mod simple",
    ggcoef_table(mod_simple)
  )

  vdiffr::expect_doppelganger(
    "ggcoef_table() table_stat",
    ggcoef_table(mod_simple, table_stat = c("p.value", "ci"))
  )

  vdiffr::expect_doppelganger(
    "ggcoef_table() table_header",
    ggcoef_table(mod_simple, table_header = c("A", "B", "C"))
  )

  expect_error(
    ggcoef_table(mod_simple, table_header = c("A", "B", "C", "D"))
  )

  vdiffr::expect_doppelganger(
    "ggcoef_table() table_text_size",
    ggcoef_table(mod_simple, table_text_size = 5)
  )

  vdiffr::expect_doppelganger(
    "ggcoef_table() table_stat_label ",
    ggcoef_table(
      mod_simple,
      table_stat_label = list(
        estimate = scales::label_percent(.1)
      )
    )
  )

  vdiffr::expect_doppelganger(
    "ggcoef_table() ci_pattern",
    ggcoef_table(mod_simple, ci_pattern = "{conf.low} to {conf.high}")
  )

  vdiffr::expect_doppelganger(
    "ggcoef_table() table_widths",
    ggcoef_table(mod_simple, table_witdhs = c(1, 2))
  )

  vdiffr::expect_doppelganger(
    "ggcoef_table() stripped_rows",
    ggcoef_table(mod_simple, stripped_rows = FALSE)
  )

  vdiffr::expect_doppelganger(
    "ggcoef_table() show_p_values & signif_stars",
    ggcoef_table(mod_simple, show_p_values = TRUE, signif_stars = TRUE)
  )

  vdiffr::expect_doppelganger(
    "ggcoef_table() show_p_values only",
    ggcoef_table(mod_simple, show_p_values = TRUE, signif_stars = FALSE)
  )

  vdiffr::expect_doppelganger(
    "ggcoef_table() signif_stars only",
    ggcoef_table(mod_simple, show_p_values = FALSE, signif_stars = TRUE)
  )

  vdiffr::expect_doppelganger(
    "ggcoef_table() customized statistics",
    ggcoef_table(
      mod_simple,
      table_stat = c("label", "estimate", "std.error", "ci"),
      ci_pattern = "{conf.low} to {conf.high}",
      table_stat_label = list(
        estimate = scales::label_number(accuracy = .01),
        conf.low = scales::label_number(accuracy = .1),
        conf.high = scales::label_number(accuracy = .1),
        std.error = scales::label_number(accuracy = .001),
        label = toupper
      ),
      table_header = c("Term", "Coef.", "SE", "CI"),
      table_witdhs = c(2, 3)
    )
  )
})

test_that("ggcoef_multicomponents()", {
  skip_on_cran()
  skip_if_not_installed("broom.helpers")
  skip_if_not_installed("pscl")

  library(pscl)
  data("bioChemists", package = "pscl")
  mod <- zeroinfl(art ~ fem * mar | fem + mar, data = bioChemists)

  vdiffr::expect_doppelganger(
    "ggcoef_multicomponents() dodged",
    ggcoef_multicomponents(mod, tidy_fun = broom.helpers::tidy_zeroinfl)
  )

  vdiffr::expect_doppelganger(
    "ggcoef_multicomponents() faceted",
    ggcoef_multicomponents(
      mod,
      tidy_fun = broom.helpers::tidy_zeroinfl,
      type = "f"
    )
  )

  vdiffr::expect_doppelganger(
    "ggcoef_multicomponents() faceted exponentiated",
    ggcoef_multicomponents(
      mod,
      tidy_fun = broom.helpers::tidy_zeroinfl,
      type = "f",
      exponentiate = TRUE
    )
  )

  vdiffr::expect_doppelganger(
    "ggcoef_multicomponents() table",
    ggcoef_multicomponents(
      mod,
      tidy_fun = broom.helpers::tidy_zeroinfl,
      type = "t"
    )
  )

  vdiffr::expect_doppelganger(
    "ggcoef_multicomponents() table exponentiated",
    ggcoef_multicomponents(
      mod,
      tidy_fun = broom.helpers::tidy_zeroinfl,
      type = "t",
      exponentiate = TRUE
    )
  )

  expect_s3_class(
    ggcoef_multicomponents(
      mod,
      tidy_fun = broom.helpers::tidy_zeroinfl,
      type = "t",
      return_data = TRUE
    ),
    "tbl"
  )

  vdiffr::expect_doppelganger(
    "ggcoef_multicomponents() table component_label",
    ggcoef_multicomponents(
      mod,
      tidy_fun = broom.helpers::tidy_zeroinfl,
      type = "t",
      component_label = c(conditional = "Count", zero_inflated = "Zero-inflated") # nolint
    )
  )

  vdiffr::expect_doppelganger(
    "ggcoef_multicomponents() faceted component_label",
    ggcoef_multicomponents(
      mod,
      tidy_fun = broom.helpers::tidy_zeroinfl,
      type = "f",
      component_label = c(conditional = "Count", zero_inflated = "Zero-inflated")  # nolint
    )
  )

  # message if unfound values for component_label
  expect_message(
    ggcoef_multicomponents(
      mod,
      tidy_fun = broom.helpers::tidy_zeroinfl,
      type = "t",
      component_label = c(c = "Count", zi = "Zero-inflated")
    )
  )

  # error if unnamed values for component_label
  expect_error(
    ggcoef_multicomponents(
      mod,
      tidy_fun = broom.helpers::tidy_zeroinfl,
      type = "t",
      component_label = c("Count", zi = "Zero-inflated")
    )
  )

  mod2 <- zeroinfl(art ~ fem + mar | 1, data = bioChemists)
  vdiffr::expect_doppelganger(
    "ggcoef_multicomponents() mod2 table",
    ggcoef_multicomponents(
      mod2,
      tidy_fun = broom.helpers::tidy_zeroinfl,
      type = "t"
    )
  )

  skip_if_not_installed("betareg")
  skip_if_not_installed("parameters")

  library(betareg)
  data("GasolineYield", package = "betareg")
  m1 <- betareg(yield ~ batch + temp, data = GasolineYield)
  m2 <- betareg(yield ~ batch + temp | temp + pressure, data = GasolineYield)

  vdiffr::expect_doppelganger(
    "ggcoef_multicomponents() betareg m1 table",
    ggcoef_multicomponents(
      m1,
      type = "t",
      tidy_fun = broom.helpers::tidy_parameters,
      tidy_args = list(component = "all")
    )
  )

  vdiffr::expect_doppelganger(
    "ggcoef_multicomponents() betareg m2 table",
    ggcoef_multicomponents(
      m2,
      type = "t",
      tidy_fun = broom.helpers::tidy_parameters,
      tidy_args = list(component = "all")
    )
  )

  modlm <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)

  vdiffr::expect_doppelganger(
    "ggcoef_multicomponents() linear model table",
    ggcoef_multicomponents(modlm, type = "t")
  )

  vdiffr::expect_doppelganger(
    "ggcoef_multicomponents() linear model faceted",
    ggcoef_multicomponents(modlm, type = "f")
  )
})
