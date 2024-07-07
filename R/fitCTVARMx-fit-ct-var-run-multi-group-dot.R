.FitCTVARRunMultiGroup <- function(data,
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
                                   ncores = NULL) {
  ids <- sort(
    unique(data[, id])
  )
  on.exit(
    OpenMx::mxOption(
      key = "Number of Threads",
      value = OpenMx::mxOption(
        key = "Number of Threads"
      )
    )
  )
  model <- .FitCTVARModel(
    data = data,
    observed = observed,
    time = time,
    phi = phi,
    gamma = gamma,
    lambda = lambda,
    kappa = kappa,
    sigma = sigma,
    theta = theta,
    mu0 = mu0,
    sigma0 = sigma0,
    covariate = covariate
  )
  model_id <- function(i,
                       data,
                       id,
                       model) {
    return(
      OpenMx::mxModel(
        name = paste0("CTVAR", "_", i),
        model = model,
        OpenMx::mxData(
          observed = data[
            which(
              data[, id] == i
            ), ,
            drop = FALSE
          ],
          type = "raw"
        )
      )
    )
  }
  par <- FALSE
  # nocov start
  if (!is.null(ncores)) {
    ncores <- as.integer(ncores)
    if (ncores > 1) {
      par <- TRUE
    }
  }
  if (length(ids) == 1) {
    par <- FALSE
  }
  # nocov end
  if (par) {
    # nocov start
    foo <- function(ids,
                    data,
                    observed,
                    id,
                    model,
                    ncores) {
      OpenMx::mxOption(
        key = "Number of Threads",
        value = 1
      )
      cl <- parallel::makeCluster(ncores)
      on.exit(
        parallel::stopCluster(cl = cl)
      )
      return(
        parallel::parLapply(
          cl = cl,
          X = ids,
          fun = model_id,
          data = data,
          id = id,
          model = model
        )
      )
    }
    model_i <- foo(
      ids = ids,
      data = data,
      observed = observed,
      id = id,
      model = model,
      ncores = ncores
    )
    OpenMx::mxOption(
      key = "Number of Threads",
      value = ncores
    )
    # nocov end
  } else {
    model_i <- lapply(
      X = ids,
      FUN = model_id,
      data = data,
      id = id,
      model = model
    )
  }
  fit <- OpenMx::mxTryHardctsem(
    model = OpenMx::mxModel(
      name = "CTVAR",
      model_i,
      mxFitFunctionMultigroup(
        paste0(
          "CTVAR",
          "_",
          ids
        )
      )
    ),
    extraTries = try
  )
  if (fit@output[["status"]][["code"]] > 1) {
    fit <- OpenMx::mxTryHardctsem(
      model = fit,
      extraTries = try
    )
  }
  return(fit)
}
