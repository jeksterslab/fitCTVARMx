.FitCTVARSigma0 <- function(k,
                            idx,
                            sigma0_fixed = TRUE,
                            sigma0_diag = TRUE,
                            sigma0_start = NULL,
                            sigma0_lbound = NULL,
                            sigma0_ubound = NULL) {
  # R0
  # initial condition
  # covariance
  if (sigma0_fixed) {
    return(
      .FitCTVARSigma0Fixed(
        k = k,
        sigma0_diag = sigma0_diag,
        sigma0_start = sigma0_start
      )
    )
  } else {
    if (sigma0_diag) {
      return(
        .FitCTVARSigma0Diag(
          k = k,
          idx = idx,
          sigma0_start = sigma0_start,
          sigma0_lbound = sigma0_lbound,
          sigma0_ubound = sigma0_ubound
        )
      )
    } else {
      return(
        .FitCTVARSigma0Full(
          k = k,
          idx = idx,
          sigma0_start = sigma0_start,
          sigma0_lbound = sigma0_lbound,
          sigma0_ubound = sigma0_ubound
        )
      )
    }
  }
}
