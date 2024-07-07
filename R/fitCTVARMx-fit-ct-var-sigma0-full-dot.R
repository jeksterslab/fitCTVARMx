.FitCTVARSigma0Full <- function(k,
                                idx,
                                sigma0_start = NULL,
                                sigma0_lbound = NULL,
                                sigma0_ubound = NULL) {
  # R0
  # initial condition
  # covariance
  # nocov start
  if (is.null(sigma0_start)) {
    sigma0_start <- diag(k)
  } else {
    stopifnot(
      is.matrix(sigma0_start),
      dim(sigma0_start) == c(k, k)
    )
  }
  sigma0_labels <- matrix(
    data = "0",
    nrow = k,
    ncol = k
  )
  for (i in idx) {
    for (j in idx) {
      sigma0_labels[i, j] <- paste0(
        "sigma0",
        "_",
        idx[i],
        idx[j]
      )
    }
  }
  if (is.null(sigma0_lbound)) {
    sigma0_lbound <- matrix(
      data = NA,
      nrow = k,
      ncol = k
    )
    diag(sigma0_lbound) <- .Machine$double.xmin
  } else {
    stopifnot(
      is.matrix(sigma0_lbound),
      dim(sigma0_lbound) == c(k, k)
    )
  }
  if (is.null(sigma0_ubound)) {
    sigma0_ubound <- matrix(
      data = NA,
      nrow = k,
      ncol = k
    )
  } else {
    stopifnot(
      is.matrix(sigma0_ubound),
      dim(sigma0_ubound) == c(k, k)
    )
  }
  # make sure matrices are symmetric
  sigma0_start[
    upper.tri(sigma0_start)
  ] <- t(sigma0_start)[
    upper.tri(sigma0_start)
  ]
  sigma0_labels[
    upper.tri(sigma0_labels)
  ] <- t(sigma0_labels)[
    upper.tri(sigma0_labels)
  ]
  sigma0_lbound[
    upper.tri(sigma0_lbound)
  ] <- t(sigma0_lbound)[
    upper.tri(sigma0_lbound)
  ]
  sigma0_ubound[
    upper.tri(sigma0_ubound)
  ] <- t(sigma0_ubound)[
    upper.tri(sigma0_ubound)
  ]
  return(
    OpenMx::mxMatrix(
      type = "Symm",
      nrow = k,
      ncol = k,
      free = TRUE,
      values = sigma0_start,
      labels = sigma0_labels,
      lbound = sigma0_lbound,
      ubound = sigma0_ubound,
      byrow = FALSE,
      name = "sigma0"
    )
  )
  # nocov end
}
