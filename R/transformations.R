
# Adapted from R.Eyzaguirre Package ---------------------------------------

#' Data transformations
#'
#' This function performs different data transformations.
#' @param trait The trait to transform.
#' @param type The transformation type. See details.
#' @param base Base for the logarithmic transformation. Base 10 by default.
#' @param n Additional parameter for arc-sine transformation. See details.
#' @param data The name of the data frame containing the data.
#' @author Raul Eyzaguirre.
#' @details Available transformations are:
#'
#' \code{none} for no transformation.
#'
#' \code{logy} for the logarithmic transformation log(y). This transformation is
#' recommended for data that follow a multiplicative instead of an additive model.
#'
#' \code{logy1} for the logarithmic transformation log(y + 1). The same as the
#' previous case, but when the data set includes small values (e.g. less than 10).
#'
#' \code{sqrty} for the square root transformation sqrt(y). This transformation is
#' recommended for count data, which typically follow a Poisson distribution where
#' the variance is proportional to the mean. It is also recommended for percentage
#' data where the range is between 0 and 20\% or between 80 and 100\%. However, note
#' that for Poisson data a Poisson regression model could be a better option.
#'
#' \code{sqrty1} for the square root transformation sqrt(y + 0.5). The same as the
#' previous case but when most of the values in the data set are less than 10,
#' especially if zeros are present.
#'
#' \code{arcsin} for the arc-sine transformation arcsin(y^0.5). This transformation
#' is recommended for data on proportions, which typically follow a binomial
#' distribution. Data must lie between 0 and 1. Where the values of 0 or 1 are present,
#' these should be substituted by 1/4n and 1-1/4n, where \code{n} is the denominator for
#' the computation of the proportions. When the proportions are in the range 0.2 to 0.8
#' no transformation could be needed, and where some are on either the range 0 to 0.2 or
#' 0.8 to 1 a square root transformation could be useful. Finally, Note that for binomial
#' data, a binomial regression model could be a better option.
#' @return It returns the transformed trait.
#' @examples
#' dtr("Sepal.Length", "logy", data = iris)
#' @export

transformations <- function(trait, type = c("none", "logy", "logy1", "sqrty", "sqrty1", "arcsin"),
                base = 10, n = NULL, data) {

  # match arguments

  type <- match.arg(type)

  # log transformations

  if (type == "logy") {
    nn <- paste("log", trait, sep = "_")
    data[, nn] <- data[, trait]
    if (sum(data[, nn] <= 0, na.rm = T) > 0) {
      data[data[, nn] <= 0 & !is.na(data[, nn]), nn] <- NA
      warning("Values <= 0 converted to NA", call. = FALSE)
    }
    data[, nn] <- log(data[, nn], base)
  }

  if (type == "logy1"){
    nn <- paste("log", trait, sep = "_")
    data[, nn] <- data[, trait]
    if (sum(data[, nn] + 1 <= 0, na.rm = T) > 0) {
      data[data[, nn] + 1 <= 0 & !is.na(data[, nn]), nn] <- NA
      warning("Values <= -1 converted to NA", call. = FALSE)
    }
    data[, nn] <- log(data[, nn] + 1, base)
  }

  # sqrt transformation

  if (type == "sqrty") {
    nn <- paste("sqrt", trait, sep = "_")
    data[, nn] <- data[, trait]
    if (sum(data[, nn] < 0, na.rm = T) > 0) {
      data[data[, nn] < 0 & !is.na(data[, nn]), nn] <- NA
      warning("Values < 0 converted to NA", call. = FALSE)
    }
    data[, nn] <- sqrt(data[, nn])
  }

  if (type == "sqrty1") {
    nn <- paste("sqrt", trait, sep = "_")
    data[, nn] <- data[, trait]
    if (sum(data[, nn] + 0.5 < 0, na.rm = T) > 0) {
      data[data[, nn] + 0.5 < 0 & !is.na(data[, nn]), nn] <- NA
      warning("Values < -0.5 converted to NA", call. = FALSE)
    }
    data[, nn] <- sqrt(data[, nn] + 0.5)
  }

  # arc-sine transformation

  if (type == "arcsin") {
    nn <- paste("arcsin", trait, sep = "_")
    data[, nn] <- data[, trait]
    if (sum(data[, nn] < 0, na.rm = T) > 0 | sum(data[, nn] > 1, na.rm = T) > 0) {
      data[data[, nn] < 0 & !is.na(data[, nn]), nn] <- NA
      data[data[, nn] > 1 & !is.na(data[, nn]), nn] <- NA
      warning("Values < 0 or > 1 converted to NA", call. = FALSE)
    }
    if (sum(data[, nn] == 0, na.rm = T) > 0 | sum(data[, nn] == 1, na.rm = T) > 0) {
      if (!is.null(n)) {
        data[data[, nn] == 0 & !is.na(data[, nn]), nn] <- 1/(4*n)
        data[data[, nn] == 1 & !is.na(data[, nn]), nn] <- 1 - 1/(4*n)
        warning("Values = 0 and = 1 replaced with 1/4n and 1 - 1/4n", call. = FALSE)
      }
      if (is.null(n)) {
        warning("n is not specified. Values = 0 or = 1 are not replaced", call. = FALSE)
      }
    }
    data[, nn] <- asin(sqrt(data[, nn]))
  }

  # results

  data
}
