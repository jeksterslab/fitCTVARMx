.FitCTVARSigmaDiag <- function(k,
                               idx,
                               statenames,
                               sigma_start = NULL,
                               sigma_lbound = NULL,
                               sigma_ubound = NULL) {
  # Q
  # process noise
  if (is.null(sigma_start)) {
    sigma_start <- rep(
      x = 0.10,
      times = k
    )
  } else {
    if (is.matrix(sigma_start)) {
      sigma_start <- diag(sigma_start)
    }
    stopifnot(
      is.vector(sigma_start),
      length(sigma_start) == k
    )
  }
  sigma_labels <- paste0(
    "sigma_",
    paste0(
      idx,
      idx
    )
  )
  if (is.null(sigma_lbound)) {
    sigma_lbound <- rep(
      x = .Machine$double.xmin,
      times = k
    )
  } else {
    if (is.matrix(sigma_lbound)) {
      sigma_lbound <- diag(sigma_lbound)
    }
    stopifnot(
      is.vector(sigma_lbound),
      length(sigma_lbound) == k
    )
  }
  if (is.null(sigma_ubound)) {
    sigma_ubound <- rep(
      x = NA,
      times = k
    )
  } else {
    if (is.matrix(sigma_ubound)) {
      sigma_ubound <- diag(sigma_ubound)
    }
    stopifnot(
      is.vector(sigma_ubound),
      length(sigma_ubound) == k
    )
  }
  return(
    OpenMx::mxMatrix(
      type = "Diag",
      nrow = k,
      ncol = k,
      free = TRUE,
      values = sigma_start,
      labels = sigma_labels,
      lbound = sigma_lbound,
      ubound = sigma_ubound,
      byrow = FALSE,
      dimnames = list(
        statenames,
        statenames
      ),
      name = "sigma"
    )
  )
}
