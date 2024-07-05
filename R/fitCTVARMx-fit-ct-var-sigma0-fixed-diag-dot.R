.FitCTVARSigma0FixedDiag <- function(k,
                                     sigma0_start = NULL) {
  # sigma0_start will be the fixed value
  # R0
  # initial condition
  # covariance
  if (is.null(sigma0_start)) {
    sigma0_start <- diag(k)
  } else {
    if (is.matrix(sigma0_start)) {
      sigma0_start <- diag(sigma0_start)
    }
    stopifnot(
      is.vector(sigma0_start),
      length(sigma0_start) == k
    )
  }
  return(
    OpenMx::mxMatrix(
      type = "Diag",
      nrow = k,
      ncol = k,
      free = FALSE,
      values = sigma0_start,
      labels = NA,
      lbound = NA,
      ubound = NA,
      byrow = FALSE,
      name = "sigma0"
    )
  )
}
