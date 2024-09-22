#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @importFrom lifecycle deprecate_soft
#' @importFrom lifecycle deprecated
#' @importFrom dplyr .data sym
#' @importFrom ggplot2 after_stat after_scale
## usethis namespace: end
NULL

utils::globalVariables(c("prop"))

# \lifecycle{experimental}
# \lifecycle{maturing}
# \lifecycle{stable}
# \lifecycle{superseded}
# \lifecycle{questioning}
# \lifecycle{soft-deprecated}
# \lifecycle{deprecated}
# \lifecycle{defunct}
# \lifecycle{archived}

# from ggplot2 (but not exported by ggplot2)

`%||%` <- function(a, b) {
  if (!is.null(a)) {
    a
  } else {
    b
  }
}
