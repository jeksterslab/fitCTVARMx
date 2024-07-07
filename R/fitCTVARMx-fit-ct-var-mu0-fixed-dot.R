.FitCTVARMu0Fixed <- function(k,
                              mu0_start = NULL) {
  # mu0_start will be the fixed value
  # x0
  # initial condition
  # mean
  # nocov start
  if (is.null(mu0_start)) {
    mu0_start <- rep(x = 0, times = k)
  } else {
    if (is.matrix(mu0_start)) {
      mu0_start <- as.vector(mu0_start)
    } else {
      stopifnot(
        is.vector(mu0_start),
        length(mu0_start) == k
      )
    }
  }
  return(
    OpenMx::mxMatrix(
      type = "Full",
      nrow = k,
      ncol = 1,
      free = FALSE,
      values = mu0_start,
      labels = NA,
      lbound = NA,
      ubound = NA,
      byrow = FALSE,
      name = "mu0"
    )
  )
  # nocov end
}
