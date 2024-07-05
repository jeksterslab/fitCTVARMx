.FitCTVARSigma0Fixed <- function(k,
                                 sigma0_diag = TRUE,
                                 sigma0_start = NULL) {
  # sigma0_start will be the fixed value
  # R0
  # initial condition
  # covariance
  if (sigma0_diag) {
    return(
      .FitCTVARSigma0FixedDiag(
        k = k,
        sigma0_start = sigma0_start
      )
    )
  } else {
    return(
      .FitCTVARSigma0FixedFull(
        k = k,
        sigma0_start = sigma0_start
      )
    )
  }
}
