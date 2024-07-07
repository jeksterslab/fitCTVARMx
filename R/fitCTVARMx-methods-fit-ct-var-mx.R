#' Print Method for Object of Class `fitctvarmx`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param x an object of class `fitctvarmx`.
#' @param ... further arguments.
#'
#' @method print fitctvarmx
#' @keywords methods
#' @export
print.fitctvarmx <- function(x,
                             ...) {
  base::print(
    summary(x$output)
  )
}

#' Summary Method for Object of Class `fitctvarmx`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param object an object of class `fitctvarmx`.
#' @param ... further arguments.
#'
#' @method summary fitctvarmx
#' @keywords methods
#' @export
summary.fitctvarmx <- function(object,
                               ...) {
  return(
    summary(object$output)
  )
}

#' Parameter Estimates
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `fitctvarmx`.
#' @param sigma Logical.
#'   If `sigma = TRUE`,
#'   include estimates of the `sigma` matrix, if available.
#'   If `sigma = FALSE`,
#'   exclude estimates of the `sigma` matrix.
#' @param theta Logical.
#'   If `theta = TRUE`,
#'   include estimates of the `theta` matrix, if available.
#'   If `theta = FALSE`,
#'   exclude estimates of the `theta` matrix.
#' @param ... additional arguments.
#' @return Returns a vector of parameter estimates.
#'
#' @method coef fitctvarmx
#' @keywords methods
#' @export
coef.fitctvarmx <- function(object,
                            sigma = FALSE,
                            theta = FALSE,
                            ...) {
  coefs <- coef(object$output)
  parnames <- names(coefs)
  idx <- grep(
    pattern = "^phi_",
    x = parnames
  )
  if (sigma) {
    idx <- c(
      idx,
      grep(
        pattern = "^sigma_",
        x = parnames
      )
    )
  }
  if (theta) {
    idx <- c(
      idx,
      grep(
        pattern = "^theta_",
        x = parnames
      )
    )
  }
  return(
    coefs[idx]
  )
}

#' Sampling Covariance Matrix of the Parameter Estimates
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `fitctvarmx`.
#' @param sigma Logical.
#'   If `sigma = TRUE`,
#'   include estimates of the `sigma` matrix, if available.
#'   If `sigma = FALSE`,
#'   exclude estimates of the `sigma` matrix.
#' @param theta Logical.
#'   If `theta = TRUE`,
#'   include estimates of the `theta` matrix, if available.
#'   If `theta = FALSE`,
#'   exclude estimates of the `theta` matrix.
#' @param ... additional arguments.
#' @return Returns a list of sampling variance-covariance matrices.
#'
#' @method vcov fitctvarmx
#' @keywords methods
#' @export
vcov.fitctvarmx <- function(object,
                            sigma = FALSE,
                            theta = FALSE,
                            ...) {
  vcovs <- vcov(object$output)
  parnames <- rownames(vcovs)
  idx <- grep(
    pattern = "^phi_",
    x = parnames
  )
  if (sigma) {
    idx <- c(
      idx,
      grep(
        pattern = "^sigma_",
        x = parnames
      )
    )
  }
  if (theta) {
    idx <- c(
      idx,
      grep(
        pattern = "^theta_",
        x = parnames
      )
    )
  }
  return(
    vcovs[idx, idx, drop = FALSE]
  )
}
