.FitCTVARRun <- function(data,
                         observed,
                         id,
                         time,
                         phi,
                         gamma,
                         lambda,
                         kappa,
                         sigma,
                         theta,
                         mu0,
                         sigma0,
                         covariate,
                         try = 1000,
                         ncores = NULL,
                         byid = TRUE,
                         ...) {
  if (byid) {
    return(
      .FitCTVARRunID(
        data = data,
        observed = observed,
        id = id,
        time = time,
        phi = phi,
        gamma = gamma,
        lambda = lambda,
        kappa = kappa,
        sigma = sigma,
        theta = theta,
        mu0 = mu0,
        sigma0 = sigma0,
        covariate = covariate,
        try = try,
        ncores = ncores,
        ...
      )
    )
  } else {
    return(
      .FitCTVARRunMultiGroup(
        data = data,
        observed = observed,
        id = id,
        time = time,
        phi = phi,
        gamma = gamma,
        lambda = lambda,
        kappa = kappa,
        sigma = sigma,
        theta = theta,
        mu0 = mu0,
        sigma0 = sigma0,
        covariate = covariate,
        try = try,
        ncores = ncores,
        ...
      )
    )
  }
}
