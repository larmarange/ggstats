if (
  requireNamespace("vdiffr", quietly = TRUE) &&
    utils::packageVersion("testthat") >= "3.0.3" &&
    !identical(Sys.getenv("VDIFFR_RUN_TESTS"), "false")
) {
  expect_doppelganger <- vdiffr::expect_doppelganger
} else {
  # Otherwise, assign a dummy function
  expect_doppelganger <- function(...) testthat::skip("vdiffr not run.")
}
