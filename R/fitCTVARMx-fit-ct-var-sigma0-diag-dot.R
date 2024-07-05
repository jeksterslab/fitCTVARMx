.FitCTVARSigma0Diag <- function(k,
                                idx,
                                sigma0_start = NULL,
                                sigma0_lbound = NULL,
                                sigma0_ubound = NULL) {
  # R0
  # initial condition
  # covariance
  if (is.null(sigma0_start)) {
    sigma0_start <- rep(
      x = 1,
      times = k
    )
  } else {
    if (is.matrix(sigma0_start)) {
      sigma0_start <- diag(sigma0_start)
    }
    stopifnot(
      is.vector(sigma0_start),
      length(sigma0_start) == k
    )
  }
  sigma0_labels <- paste0(
    "sigma0_",
    paste0(
      idx,
      idx
    )
  )
  if (is.null(sigma0_lbound)) {
    sigma0_lbound <- rep(
      x = .Machine$double.xmin,
      times = k
    )
  } else {
    if (is.matrix(sigma0_lbound)) {
      sigma0_lbound <- diag(sigma0_lbound)
    }
    stopifnot(
      is.vector(sigma0_lbound),
      length(sigma0_lbound) == k
    )
  }
  if (is.null(sigma0_ubound)) {
    sigma0_ubound <- rep(
      x = NA,
      times = k
    )
  } else {
    if (is.matrix(sigma0_ubound)) {
      sigma0_ubound <- diag(sigma0_ubound)
    }
    stopifnot(
      is.vector(sigma0_ubound),
      length(sigma0_ubound) == k
    )
  }
  return(
    OpenMx::mxMatrix(
      type = "Diag",
      nrow = k,
      ncol = k,
      free = TRUE,
      values = sigma0_start,
      labels = sigma0_labels,
      lbound = sigma0_lbound,
      ubound = sigma0_ubound,
      byrow = FALSE,
      name = "sigma0"
    )
  )
}
