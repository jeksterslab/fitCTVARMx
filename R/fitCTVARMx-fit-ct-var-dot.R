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
                      ncores = NULL) {
  k <- length(observed)
  idx <- seq_len(k)
  statenames <- paste0("eta", idx)
  ids <- sort(
    unique(data[, id])
  )
  phi <- .FitCTVARPhi(
    k = k,
    idx = idx,
    statenames = statenames,
    phi_start = phi_start,
    phi_lbound = phi_lbound,
    phi_ubound = phi_ubound
  )
  gamma <- .FitCTVARGamma(k = k)
  lambda <- .FitCTVARLambda(
    k = k,
    observed = observed,
    statenames = statenames
  )
  kappa <- .FitCTVARKappa(k = k)
  sigma <- .FitCTVARSigma(
    k = k,
    idx = idx,
    statenames = statenames,
    sigma_diag = sigma_diag,
    sigma_start = sigma_start,
    sigma_lbound = sigma_lbound,
    sigma_ubound = sigma_ubound
  )
  theta <- .FitCTVARTheta(
    k = k,
    idx = idx,
    observed = observed,
    theta_fixed = theta_fixed,
    theta_start = theta_start,
    theta_lbound = theta_lbound,
    theta_ubound = theta_ubound
  )
  mu0 <- .FitCTVARMu0(
    k = k,
    idx = idx,
    mu0_fixed = mu0_fixed,
    mu0_start = mu0_start,
    mu0_lbound = mu0_lbound,
    mu0_ubound = mu0_ubound
  )
  sigma0 <- .FitCTVARSigma0(
    k = k,
    idx = idx,
    sigma0_fixed = sigma0_fixed,
    sigma0_diag = sigma0_diag,
    sigma0_start = sigma0_start,
    sigma0_lbound = sigma0_lbound,
    sigma0_ubound = sigma0_ubound
  )
  x <- .FitCTVARX()
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
  par <- FALSE
  if (!is.null(ncores)) {
    ncores <- as.integer(ncores)
    if (ncores > 1) {
      par <- TRUE
    }
  }
  expectation <- OpenMx::mxExpectationStateSpaceContinuousTime(
    A = "phi",
    B = "gamma",
    C = "lambda",
    D = "kappa",
    Q = "sigma",
    R = "theta",
    x0 = "mu0",
    P0 = "sigma0",
    u = "x",
    t = "time",
    dimnames = observed
  )
  if (par) {
    OpenMx::mxOption(
      key = "Number of Threads",
      value = 1
    )
    cl <- parallel::makeCluster(ncores)
    on.exit(
      parallel::stopCluster(cl = cl)
    )
    output <- parallel::parLapply(
      cl = cl,
      X = ids,
      fun = function(i) {
        model <- OpenMx::mxModel(
          model = "CTVAR",
          phi,
          gamma,
          lambda,
          kappa,
          sigma,
          theta,
          mu0,
          sigma0,
          x,
          time,
          expectation,
          OpenMx::mxFitFunctionML(),
          OpenMx::mxData(
            observed = data[which(data[, id] == i), ],
            type = "raw"
          )
        )
        OpenMx::mxTryHardctsem(
          model = model,
          extraTries = try
        )
      }
    )
  } else {
    output <- lapply(
      X = ids,
      FUN = function(i) {
        model <- OpenMx::mxModel(
          model = "CTVAR",
          phi,
          gamma,
          lambda,
          kappa,
          sigma,
          theta,
          mu0,
          sigma0,
          x,
          expectation,
          OpenMx::mxFitFunctionML(),
          OpenMx::mxData(
            observed = data[which(data[, id] == i), ],
            type = "raw"
          )
        )
        OpenMx::mxTryHardctsem(
          model = model,
          extraTries = try
        )
      }
    )
  }
  return(output)
}
