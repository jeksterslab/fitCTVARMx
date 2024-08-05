.FitCTVARMu0 <- function(k,
                         idx,
                         statenames,
                         mu0_fixed = TRUE,
                         mu0_values = NULL,
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
        statenames = statenames,
        mu0_values = mu0_values
      )
    )
  } else {
    return(
      .FitCTVARMu0Vec(
        k = k,
        idx = idx,
        statenames = statenames,
        mu0_values = mu0_values,
        mu0_lbound = mu0_lbound,
        mu0_ubound = mu0_ubound
      )
    )
  }
  # nocov end
}
