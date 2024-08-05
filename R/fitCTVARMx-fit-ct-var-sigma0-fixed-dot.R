.FitCTVARSigma0Fixed <- function(k,
                                 sigma0_diag = TRUE,
                                 sigma0_values = NULL) {
  # sigma0_values will be the fixed value
  # R0
  # initial condition
  # covariance
  # nocov start
  if (sigma0_diag) {
    return(
      .FitCTVARSigma0FixedDiag(
        k = k,
        sigma0_values = sigma0_values
      )
    )
  } else {
    return(
      .FitCTVARSigma0FixedFull(
        k = k,
        sigma0_values = sigma0_values
      )
    )
  }
  # nocov end
}
