.FitCTVARSigma0FixedFull <- function(k,
                                     sigma0_values = NULL) {
  # sigma0_values will be the fixed value
  # R0
  # initial condition
  # covariance
  # nocov start
  if (is.null(sigma0_values)) {
    sigma0_values <- diag(k)
  } else {
    stopifnot(
      is.matrix(sigma0_values),
      dim(sigma0_values) == c(k, k)
    )
  }
  return(
    OpenMx::mxMatrix(
      type = "Symm",
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
