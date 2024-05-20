test_that("pal_extender() works", {
  skip_if_not_installed("scales")
  pal <- scales::pal_brewer(palette = "PiYG")
  pal_e <- pal_extender(pal = pal)

  expect_equal(pal(5), pal_e(5))
  expect_false(any(is.na(pal_e(20))))
  expect_length(pal_e(20), 20L)
})
