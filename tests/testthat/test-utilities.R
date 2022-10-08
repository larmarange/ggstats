test_that("signif_stars() works", {
  x <- c(0.5, 0.1, 0.05, 0.01, 0.001)
  expect_equal(
    signif_stars(x),
    c("", ".", "*", "**", "***")
  )
  expect_equal(
    signif_stars(x, one = .15, point = NULL),
    c("", "*", "*", "**", "***")
  )
})
