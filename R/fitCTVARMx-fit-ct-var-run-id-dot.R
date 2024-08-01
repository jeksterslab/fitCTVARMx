.FitCTVARRunID <- function(data,
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
                           ...) {
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
  fit <- function(i,
                  data,
                  id,
                  model,
                  try) {
    out <- OpenMx::mxTryHardctsem(
      model = OpenMx::mxModel(
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
      ),
      extraTries = try,
      ...
    )
    if (out@output[["status"]][["code"]] > 1) {
      out <- OpenMx::mxTryHardctsem(
        model = out,
        extraTries = try,
        ...
      )
      if (out@output[["status"]][["code"]] > 1) {
        warning(
          paste0(
            "Exit code for ",
            "ID = ",
            i,
            " is ",
            out@output[["status"]][["code"]],
            ".\n"
          )
        )
      }
    }
    return(out)
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
                    phi,
                    gamma,
                    lambda,
                    kappa,
                    sigma,
                    theta,
                    mu0,
                    sigma0,
                    covariate,
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
          fun = fit,
          data = data,
          id = id,
          model = model,
          try = try
        )
      )
    }
    output <- foo(
      ids = ids,
      data = data,
      observed = observed,
      id = id,
      phi = phi,
      gamma = gamma,
      lambda = lambda,
      kappa = kappa,
      sigma = sigma,
      theta = theta,
      mu0 = mu0,
      sigma0 = sigma0,
      covariate = covariate,
      ncores = ncores
    )
    # nocov end
  } else {
    output <- lapply(
      X = ids,
      FUN = fit,
      data = data,
      id = id,
      model = model,
      try = try
    )
  }
  return(output)
}
