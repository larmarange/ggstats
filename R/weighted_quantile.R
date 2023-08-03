#' Weighted Median and Quantiles
#'
#' Compute the median or quantiles a set of numbers which have weights
#' associated with them.
#'
#' @param x a numeric vector of values
#' @param w a numeric vector of weights
#' @param probs probabilities for which the quantiles should be computed, a
#' numeric vector of values between 0 and 1
#' @param na.rm a logical indicating whether to ignore `NA` values
#' @param type Integer specifying the rule for calculating the median or
#' quantile, corresponding to the rules available for `stats:quantile()`.
#' The only valid choices are type=1, 2 or 4. See Details.
#' @details
#' The `i`th observation `x[i]` is treated as having a weight proportional to
#' `w[i]`.
#'
#' The weighted median is a value `m` such that the total weight of data less
#' than or equal to `m` is equal to half the total weight. More generally, the
#' weighted quantile with probability `p` is a value `q` such that the total
#' weight of data less than or equal to `q` is equal to `p` times the total
#' weight.
#'
#' If there is no such value, then
#'
#' - if `type = 1`, the next largest value is returned (this is the
#'   right-continuous inverse of the left-continuous cumulative distribution
#'   function);
#' - if `type = 2`, the average of the two surrounding values is returned
#'   (the average of the right-continuous and left-continuous inverses);
#' - if `type = 4`, linear interpolation is performed.
#'
#' Note that the default rule for `weighted.median()` is `type = 2`, consistent
#' with the traditional definition of the median, while the default for
#' `weighted.quantile()` is `type = 4`.
#' @source These functions are adapted from their homonyms developed by Adrian
#' Baddeley in the `spatstat` package.
#' @export
#' @examples
#' x <- 1:20
#' w <- runif(20)
#' weighted.median(x, w)
#' weighted.quantile(x, w)
weighted.median <- function(x, w, na.rm = TRUE, type = 2) {
  unname(weighted.quantile(x,
    probs = 0.5, w = w, na.rm = na.rm,
    type = type
  ))
}

#' @export
#' @rdname weighted.median
weighted.quantile <- function(x, w, probs = seq(0, 1, 0.25),
                              na.rm = TRUE, type = 4) {
  x <- as.numeric(as.vector(x))
  w <- as.numeric(as.vector(w))
  if (length(x) == 0) {
    stop("No data given")
  }
  stopifnot(length(x) == length(w))
  if (is.na(m <- match(type, c(1, 2, 4)))) {
    stop("Argument 'type' must equal 1, 2 or 4", call. = FALSE)
  }
  type <- c(1, 2, 4)[m]
  if (anyNA(x) || anyNA(w)) {
    ok <- !(is.na(x) | is.na(w))
    x <- x[ok]
    w <- w[ok]
  }
  if (length(x) == 0) {
    stop("At least one non-NA value is required")
  }
  stopifnot(all(w >= 0))
  if (all(w == 0)) {
    stop("All weights are zero", call. = FALSE)
  }
  oo <- order(x)
  x <- x[oo]
  w <- w[oo]
  Fx <- cumsum(w) / sum(w)
  if (length(x) > 1) {
    out <- switch(as.character(type),
      `1` = stats::approx(Fx, x,
        xout = probs, ties = "ordered", rule = 2,
        method = "constant",
        f = 1
      ),
      `2` = stats::approx(Fx, x,
        xout = probs, ties = "ordered",
        rule = 2, method = "constant", f = 1 / 2
      ),
      `4` = stats::approx(Fx,
        x,
        xout = probs, ties = "ordered", rule = 2,
        method = "linear"
      )
    )
    result <- out$y
  } else {
    result <- rep.int(x, length(probs))
  }
  names(result) <- paste0(
    format(100 * probs, trim = TRUE),
    "%"
  )
  return(result)
}
