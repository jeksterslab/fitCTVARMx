.FitCTVARMu0Vec <- function(k,
                            idx,
                            statenames,
                            mu0_values = NULL,
                            mu0_lbound = NULL,
                            mu0_ubound = NULL) {
  # x0
  # initial condition
  # mean
  # nocov start
  if (is.null(mu0_values)) {
    mu0_values <- matrix(
      data = 0,
      nrow = k,
      ncol = 1
    )
  } else {
    if (is.vector(mu0_values)) {
      mu0_values <- matrix(
        data = mu0_values,
        nrow = k,
        ncol = 1
      )
    } else {
      stopifnot(
        is.matrix(mu0_values),
        dim(mu0_values) == c(k, 1)
      )
    }
  }
  if (is.null(mu0_lbound)) {
    mu0_lbound <- matrix(
      data = -1,
      nrow = k,
      ncol = 1
    )
  } else {
    if (is.vector(mu0_lbound)) {
      mu0_lbound <- matrix(
        data = mu0_lbound,
        nrow = k,
        ncol = 1
      )
    } else {
      stopifnot(
        is.matrix(mu0_lbound),
        dim(mu0_lbound) == c(k, 1)
      )
    }
  }
  if (is.null(mu0_ubound)) {
    mu0_ubound <- matrix(
      data = +1,
      nrow = k,
      ncol = 1
    )
  } else {
    if (is.vector(mu0_ubound)) {
      mu0_ubound <- matrix(
        data = mu0_ubound,
        nrow = k,
        ncol = 1
      )
    } else {
      stopifnot(
        is.matrix(mu0_ubound),
        dim(mu0_ubound) == c(k, 1)
      )
    }
  }
  return(
    OpenMx::mxMatrix(
      type = "Full",
      nrow = k,
      ncol = 1,
      free = TRUE,
      values = mu0_values,
      labels = paste0("mu0_", idx),
      lbound = mu0_lbound,
      ubound = mu0_ubound,
      byrow = FALSE,
      dimnames = list(
        statenames,
        "mu0"
      ),
      name = "mu0"
    )
  )
  # nocov end
}
