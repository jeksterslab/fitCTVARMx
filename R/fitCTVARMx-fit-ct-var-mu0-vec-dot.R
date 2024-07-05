.FitCTVARMu0Vec <- function(k,
                            idx,
                            mu0_start = NULL,
                            mu0_lbound = NULL,
                            mu0_ubound = NULL) {
  # x0
  # initial condition
  # mean
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
  if (is.null(mu0_lbound)) {
    mu0_lbound <- rep(x = -1, times = k)
  } else {
    if (is.matrix(mu0_lbound)) {
      mu0_lbound <- as.vector(mu0_lbound)
    } else {
      stopifnot(
        is.vector(mu0_lbound),
        length(mu0_lbound) == k
      )
    }
  }
  if (is.null(mu0_ubound)) {
    mu0_ubound <- rep(x = +1, times = k)
  } else {
    if (is.matrix(mu0_ubound)) {
      mu0_ubound <- as.vector(mu0_ubound)
    } else {
      stopifnot(
        is.vector(mu0_ubound),
        length(mu0_ubound) == k
      )
    }
  }
  return(
    OpenMx::mxMatrix(
      type = "Full",
      nrow = k,
      ncol = 1,
      free = TRUE,
      values = mu0_start,
      labels = paste0("mu0_", idx, idx),
      lbound = mu0_lbound,
      ubound = mu0_ubound,
      byrow = FALSE,
      name = "mu0"
    )
  )
}
