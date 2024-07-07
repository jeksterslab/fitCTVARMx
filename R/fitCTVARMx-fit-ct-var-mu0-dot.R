.FitCTVARMu0 <- function(k,
                         idx,
                         mu0_fixed = TRUE,
                         mu0_start = NULL,
                         mu0_lbound = NULL,
                         mu0_ubound = NULL) {
  # x0
  # initial condition
  # mean
  # nocov start
  if (mu0_fixed) {
    return(
      .FitCTVARMu0Fixed(
        k = k,
        mu0_start = mu0_start
      )
    )
  } else {
    return(
      .FitCTVARMu0Vec(
        k = k,
        idx = idx,
        mu0_start = mu0_start,
        mu0_lbound = mu0_lbound,
        mu0_ubound = mu0_ubound
      )
    )
  }
  # nocov end
}
