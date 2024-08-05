.FitCTVARMu0Fixed <- function(k,
                              statenames,
                              mu0_values = NULL) {
  # mu0_values will be the fixed value
  # x0
  # initial condition
  # mean
  # nocov start
  if (is.null(mu0_values)) {
    mu0_values <- rep(x = 0, times = k)
  } else {
    if (is.matrix(mu0_values)) {
      mu0_values <- as.vector(mu0_values)
    } else {
      stopifnot(
        is.vector(mu0_values),
        length(mu0_values) == k
      )
    }
  }
  return(
    OpenMx::mxMatrix(
      type = "Full",
      nrow = k,
      ncol = 1,
      free = FALSE,
      values = mu0_values,
      labels = NA,
      lbound = NA,
      ubound = NA,
      byrow = FALSE,
      dimnames = list(
        statenames,
        "mu0"
      ),
      name = "mu0"
    )
  )
  # nocov end
}
