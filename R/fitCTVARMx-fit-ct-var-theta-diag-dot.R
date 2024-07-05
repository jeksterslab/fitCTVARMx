.FitCTVARThetaDiag <- function(k,
                               idx,
                               observed,
                               theta_start = NULL,
                               theta_lbound = NULL,
                               theta_ubound = NULL) {
  # R
  # measurement error
  if (is.null(theta_start)) {
    theta_start <- rep(
      x = 0.10,
      times = k
    )
  } else {
    if (is.matrix(theta_start)) {
      theta_start <- diag(theta_start)
    }
    stopifnot(
      is.vector(theta_start),
      length(theta_start) == k
    )
  }
  theta_labels <- paste0(
    "theta_",
    idx,
    idx
  )
  if (is.null(theta_lbound)) {
    theta_lbound <- rep(
      x = .Machine$double.xmin,
      times = k
    )
  } else {
    if (is.matrix(theta_lbound)) {
      theta_lbound <- diag(theta_lbound)
    }
    stopifnot(
      is.vector(theta_lbound),
      length(theta_lbound) == k
    )
  }
  if (is.null(theta_ubound)) {
    theta_ubound <- rep(
      x = NA,
      times = k
    )
  } else {
    if (is.matrix(theta_ubound)) {
      theta_ubound <- diag(theta_ubound)
    }
    stopifnot(
      is.vector(theta_ubound),
      length(theta_ubound) == k
    )
  }
  return(
    OpenMx::mxMatrix(
      type = "Diag",
      nrow = k,
      ncol = k,
      free = TRUE,
      values = theta_start,
      labels = theta_labels,
      lbound = theta_lbound,
      ubound = theta_ubound,
      byrow = FALSE,
      dimnames = list(
        observed,
        observed
      ),
      name = "theta"
    )
  )
}
