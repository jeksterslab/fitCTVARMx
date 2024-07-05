#' Print Method for Object of Class `fitctvaridmx`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param x an object of class `fitctvaridmx`.
#' @param means Logical.
#'   If `means = TRUE`, return means.
#'   Otherwise, the function returns raw estimates.
#' @param ... further arguments.
#'
#' @method print fitctvaridmx
#' @keywords methods
#' @export
print.fitctvaridmx <- function(x,
                               means = TRUE,
                               ...) {
  out <- do.call(
    what = "rbind",
    args = lapply(
      X = x$output,
      FUN = coef
    )
  )
  if (means) {
    cat("\nMeans of the estimated paramaters per individual.\n")
    out <- colMeans(out)
  } else {
    cat("\nEstimated paramaters per individual.\n")
  }
  base::print(out)
}

#' Summary Method for Object of Class `fitctvaridmx`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param object an object of class `fitctvaridmx`.
#' @param means Logical.
#'   If `means = TRUE`, return means.
#'   Otherwise, the function returns raw estimates.
#' @param ... further arguments.
#'
#' @method summary fitctvaridmx
#' @keywords methods
#' @export
summary.fitctvaridmx <- function(object,
                                 means = TRUE,
                                 ...) {
  out <- do.call(
    what = "rbind",
    args = lapply(
      X = object$output,
      FUN = coef
    )
  )
  if (means) {
    # nocov start
    if (interactive()) {
      cat("\nMeans of the estimated paramaters per individual.\n")
    }
    # nocov end
    out <- colMeans(out)
  } else {
    # nocov start
    if (interactive()) {
      cat("\nEstimated paramaters per individual.\n")
    }
    # nocov end
  }
  return(out)
}

#' Parameter Estimates
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `fitctvaridmx`.
#' @param sigma Logical.
#'   If `sigma = TRUE`,
#'   include estimates of the `sigma` matrix.
#'   If `sigma = FALSE`,
#'   exclude estimates of the `sigma` matrix.
#' @param theta Logical.
#'   If `theta = TRUE`,
#'   include estimates of the `theta` matrix if available.
#'   If `theta = FALSE`,
#'   exclude estimates of the `theta` matrix.
#' @param ... additional arguments.
#' @return Returns a list of vectors of parameter estimates.
#'
#' @method coef fitctvaridmx
#' @keywords methods
#' @export
coef.fitctvaridmx <- function(object,
                              sigma = FALSE,
                              theta = FALSE,
                              ...) {
  parnames <- names(
    coef(object$output[[1]])
  )
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
    lapply(
      X = object$output,
      FUN = function(x,
                     idx) {
        return(coef(x)[idx])
      },
      idx = idx
    )
  )
}

#' Sampling Covariance Matrix of the Parameter Estimates
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `fitctvaridmx`.
#' @param sigma Logical.
#'   If `sigma = TRUE`,
#'   include estimates of the `sigma` matrix.
#'   If `sigma = FALSE`,
#'   exclude estimates of the `sigma` matrix.
#' @param theta Logical.
#'   If `theta = TRUE`,
#'   include estimates of the `theta` matrix if available.
#'   If `theta = FALSE`,
#'   exclude estimates of the `theta` matrix.
#' @param ... additional arguments.
#' @return Returns a list of sampling variance-covariance matrices.
#'
#' @method vcov fitctvaridmx
#' @keywords methods
#' @export
vcov.fitctvaridmx <- function(object,
                              sigma = FALSE,
                              theta = FALSE,
                              ...) {
  parnames <- names(
    coef(object$output[[1]])
  )
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
    lapply(
      X = object$output,
      FUN = function(x,
                     idx) {
        return(vcov(x)[idx, idx])
      },
      idx = idx
    )
  )
}
