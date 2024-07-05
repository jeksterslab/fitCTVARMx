.FitCTVARSigma <- function(k,
                           idx,
                           statenames,
                           sigma_diag = TRUE,
                           sigma_start = NULL,
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
        sigma_start = sigma_start,
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
        sigma_start = sigma_start,
        sigma_lbound = sigma_lbound,
        sigma_ubound = sigma_ubound
      )
    )
  }
}
