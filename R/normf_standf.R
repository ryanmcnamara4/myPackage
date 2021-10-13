#' Normalizes a numeric vector
#'
#' Takes a numeric vector and normalizes it with new min and max
#'
#' @param x Numeric vector
#' @param newMin Numeric value for the new minimum of the vector
#' @param newMax Numeric value for the new maximum of the vector
#' @param na.rm Logical value determining whether or not to remove NA
#'     values when calculating min and max. Defaults to FALSE.
#'
#' @return Numeric vector that is normalized
#' @export
#'
#' @examples
#' normf(c(1, 2, 3, 4, 5), 10, 20)
#' normf(c(1, 2, 10, -10), 5, 10, na.rm = TRUE)
normf = function(x, newMin, newMax, na.rm = FALSE) {

  if (length(x) <= 1) {
    stop('Numeric vector "x" needs to be longer than 1.')
  }
  if (na.rm == TRUE & sum(!is.na(x)) <= 1) {
    stop('Numeric vector "x" needs to have more than 1 non-missing values.')
  }
  if (newMin >= newMax) {
    warning('"newMin" is larger than or equal to "newMax"')
  }

  (newMax - newMin) * ((x - min(x, na.rm = na.rm)) / (max(x, na.rm = na.rm) - min(x, na.rm = na.rm))) + newMin

}

#' Standardize a numeric vector
#'
#' Takes a numeric vector and standardizes it based on standard deviation
#'     and mean.
#'
#' @param x Numeric vector
#' @param na.rm Logical value determining whether or not to remove NA values
#'     when calculating standard deviation and mean. Defaults to FALSE.
#'
#' @return Numeric vector that is standardized
#' @export
#'
#' @examples
#' standf(c(1, 2, 3, 4, 5))
#' standf(c(1, 2, 3, 4, 5), na.rm = TRUE)
standf = function(x, na.rm = FALSE) {

  if (length(x) <= 1) {
    stop('Numeric vector "x" needs to be longer than 1.')
  }
  if (na.rm == TRUE & sum(!is.na(x)) <= 1) {
    stop('Numeric vector "x" needs to have more than 1 non-missing values.')
  }

  stdev = sd(x, na.rm = na.rm)
  mean_val = mean(x, na.rm = na.rm)

  (x - mean_val) / stdev

}
