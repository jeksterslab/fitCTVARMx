.FitCTVARSigma <- function(k,
                           idx,
                           statenames,
                           sigma_diag = TRUE,
                           sigma_values = NULL,
                           sigma_lbound = NULL,
                           sigma_ubound = NULL) {
  # Q
  # process noise
  if (sigma_diag) {
    return(
      .FitCTVARSigmaDiag(
        k = k,
        idx = idx,
        statenames = statenames,
        sigma_values = sigma_values,
        sigma_lbound = sigma_lbound,
        sigma_ubound = sigma_ubound
      )
    )
  } else {
    return(
      .FitCTVARSigmaFull(
        k = k,
        idx = idx,
        statenames = statenames,
        sigma_values = sigma_values,
        sigma_lbound = sigma_lbound,
        sigma_ubound = sigma_ubound
      )
    )
  }
}
