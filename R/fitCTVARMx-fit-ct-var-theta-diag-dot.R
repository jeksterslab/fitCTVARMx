.FitCTVARThetaDiag <- function(k,
                               idx,
                               observed,
                               theta_values = NULL,
                               theta_lbound = NULL,
                               theta_ubound = NULL) {
  # R
  # measurement error
  if (is.null(theta_values)) {
    theta_values <- rep(
      x = 0.10,
      times = k
    )
  } else {
    if (is.matrix(theta_values)) {
      theta_values <- diag(theta_values)
    }
    stopifnot(
      is.vector(theta_values),
      length(theta_values) == k
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
      values = theta_values,
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
