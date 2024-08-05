.FitCTVARSigma0 <- function(k,
                            idx,
                            sigma0_fixed = TRUE,
                            sigma0_diag = TRUE,
                            sigma0_values = NULL,
                            sigma0_lbound = NULL,
                            sigma0_ubound = NULL) {
  # R0
  # initial condition
  # covariance
  # nocov start
  if (sigma0_fixed) {
    return(
      .FitCTVARSigma0Fixed(
        k = k,
        sigma0_diag = sigma0_diag,
        sigma0_values = sigma0_values
      )
    )
  } else {
    if (sigma0_diag) {
      return(
        .FitCTVARSigma0Diag(
          k = k,
          idx = idx,
          sigma0_values = sigma0_values,
          sigma0_lbound = sigma0_lbound,
          sigma0_ubound = sigma0_ubound
        )
      )
    } else {
      return(
        .FitCTVARSigma0Full(
          k = k,
          idx = idx,
          sigma0_values = sigma0_values,
          sigma0_lbound = sigma0_lbound,
          sigma0_ubound = sigma0_ubound
        )
      )
    }
  }
  # nocov end
}
