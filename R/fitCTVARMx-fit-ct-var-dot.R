.FitCTVAR <- function(data,
                      observed,
                      id,
                      time,
                      phi_start = NULL,
                      phi_lbound = NULL,
                      phi_ubound = NULL,
                      sigma_diag = TRUE,
                      sigma_start = NULL,
                      sigma_lbound = NULL,
                      sigma_ubound = NULL,
                      theta_fixed = TRUE,
                      theta_start = NULL,
                      theta_lbound = NULL,
                      theta_ubound = NULL,
                      mu0_fixed = TRUE,
                      mu0_start = NULL,
                      mu0_lbound = NULL,
                      mu0_ubound = NULL,
                      sigma0_fixed = TRUE,
                      sigma0_diag = TRUE,
                      sigma0_start = NULL,
                      sigma0_lbound = NULL,
                      sigma0_ubound = NULL,
                      try = 1000,
                      ncores = NULL,
                      byid = TRUE) {
  k <- length(observed)
  idx <- seq_len(k)
  statenames <- paste0("eta", idx)
  return(
    .FitCTVARRun(
      data = data,
      observed = observed,
      id = id,
      phi = .FitCTVARPhi(
        k = k,
        idx = idx,
        statenames = statenames,
        phi_start = phi_start,
        phi_lbound = phi_lbound,
        phi_ubound = phi_ubound
      ),
      gamma = .FitCTVARGamma(k = k),
      lambda = .FitCTVARLambda(
        k = k,
        observed = observed,
        statenames = statenames
      ),
      kappa = .FitCTVARKappa(k = k),
      sigma = .FitCTVARSigma(
        k = k,
        idx = idx,
        statenames = statenames,
        sigma_diag = sigma_diag,
        sigma_start = sigma_start,
        sigma_lbound = sigma_lbound,
        sigma_ubound = sigma_ubound
      ),
      theta = .FitCTVARTheta(
        k = k,
        idx = idx,
        observed = observed,
        theta_fixed = theta_fixed,
        theta_start = theta_start,
        theta_lbound = theta_lbound,
        theta_ubound = theta_ubound
      ),
      mu0 = .FitCTVARMu0(
        k = k,
        idx = idx,
        mu0_fixed = mu0_fixed,
        mu0_start = mu0_start,
        mu0_lbound = mu0_lbound,
        mu0_ubound = mu0_ubound
      ),
      sigma0 = .FitCTVARSigma0(
        k = k,
        idx = idx,
        sigma0_fixed = sigma0_fixed,
        sigma0_diag = sigma0_diag,
        sigma0_start = sigma0_start,
        sigma0_lbound = sigma0_lbound,
        sigma0_ubound = sigma0_ubound
      ),
      covariate = .FitCTVARX(),
      time = time,
      try = try,
      ncores = ncores,
      byid = byid
    )
  )
}
