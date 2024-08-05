.FitCTVARSigma0FixedDiag <- function(k,
                                     sigma0_values = NULL) {
  # sigma0_values will be the fixed value
  # R0
  # initial condition
  # covariance
  # nocov start
  if (is.null(sigma0_values)) {
    sigma0_values <- diag(k)
  } else {
    if (is.matrix(sigma0_values)) {
      sigma0_values <- diag(sigma0_values)
    }
    stopifnot(
      is.vector(sigma0_values),
      length(sigma0_values) == k
    )
  }
  return(
    OpenMx::mxMatrix(
      type = "Diag",
      nrow = k,
      ncol = k,
      free = FALSE,
      values = sigma0_values,
      labels = NA,
      lbound = NA,
      ubound = NA,
      byrow = FALSE,
      name = "sigma0"
    )
  )
  # nocov end
}
