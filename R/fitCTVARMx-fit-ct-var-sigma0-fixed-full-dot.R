.FitCTVARSigma0FixedFull <- function(k,
                                     sigma0_start = NULL) {
  # sigma0_start will be the fixed value
  # R0
  # initial condition
  # covariance
  if (is.null(sigma0_start)) {
    sigma0_start <- diag(k)
  } else {
    stopifnot(
      is.matrix(sigma0_start),
      dim(sigma0_start) == c(k, k)
    )
  }
  return(
    OpenMx::mxMatrix(
      type = "Symm",
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
