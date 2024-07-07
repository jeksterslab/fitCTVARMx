.FitCTVARModel <- function(data,
                           observed,
                           time,
                           phi,
                           gamma,
                           lambda,
                           kappa,
                           sigma,
                           theta,
                           mu0,
                           sigma0,
                           covariate) {
  time <- OpenMx::mxMatrix(
    "Full",
    nrow = 1,
    ncol = 1,
    free = FALSE,
    labels = paste0(
      "data.",
      time
    ),
    name = "time"
  )
  return(
    OpenMx::mxModel(
      model = "CTVAR",
      phi,
      gamma,
      lambda,
      kappa,
      sigma,
      theta,
      mu0,
      sigma0,
      covariate,
      time,
      OpenMx::mxExpectationStateSpaceContinuousTime(
        A = "phi",
        B = "gamma",
        C = "lambda",
        D = "kappa",
        Q = "sigma",
        R = "theta",
        x0 = "mu0",
        P0 = "sigma0",
        u = "covariate",
        t = "time",
        dimnames = observed
      ),
      OpenMx::mxFitFunctionML(),
      OpenMx::mxData(
        observed = data,
        type = "raw"
      )
    )
  )
}
