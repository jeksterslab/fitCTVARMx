.FitCTVARTheta <- function(k,
                           idx,
                           observed,
                           theta_fixed = TRUE,
                           theta_start = NULL,
                           theta_lbound = NULL,
                           theta_ubound = NULL) {
  # R
  # measurement error
  if (theta_fixed) {
    return(
      .FitCTVARThetaFixed(
        k = k,
        observed = observed
      )
    )
  } else {
    return(
      .FitCTVARThetaDiag(
        k = k,
        idx = idx,
        observed = observed,
        theta_start = theta_start,
        theta_lbound = theta_lbound,
        theta_ubound = theta_ubound
      )
    )
  }
}
