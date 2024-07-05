.FitCTVARSigmaFull <- function(k,
                               idx,
                               statenames,
                               sigma_start = NULL,
                               sigma_lbound = NULL,
                               sigma_ubound = NULL) {
  # Q
  # process noise
  if (is.null(sigma_start)) {
    sigma_start <- 0.10 * diag(k)
  } else {
    stopifnot(
      is.matrix(sigma_start),
      dim(sigma_start) == c(k, k)
    )
  }
  sigma_labels <- matrix(
    data = NA,
    nrow = k,
    ncol = k
  )
  for (j in idx) {
    for (i in idx) {
      sigma_labels[i, j] <- paste0(
        "sigma_",
        i,
        j
      )
    }
  }
  if (is.null(sigma_lbound)) {
    sigma_lbound <- matrix(
      data = NA,
      nrow = k,
      ncol = k
    )
    diag(sigma_lbound) <- .Machine$double.xmin
  } else {
    stopifnot(
      is.matrix(sigma_lbound),
      dim(sigma_lbound) == c(k, k)
    )
  }
  if (is.null(sigma_ubound)) {
    sigma_ubound <- matrix(
      data = NA,
      nrow = k,
      ncol = k
    )
  } else {
    stopifnot(
      is.matrix(sigma_ubound),
      dim(sigma_ubound) == c(k, k)
    )
  }
  # make sure matrices are symmetric
  sigma_start[
    upper.tri(sigma_start)
  ] <- t(sigma_start)[
    upper.tri(sigma_start)
  ]
  sigma_labels[
    upper.tri(sigma_labels)
  ] <- t(sigma_labels)[
    upper.tri(sigma_labels)
  ]
  sigma_lbound[
    upper.tri(sigma_lbound)
  ] <- t(sigma_lbound)[
    upper.tri(sigma_lbound)
  ]
  sigma_ubound[
    upper.tri(sigma_ubound)
  ] <- t(sigma_ubound)[
    upper.tri(sigma_ubound)
  ]
  return(
    OpenMx::mxMatrix(
      type = "Symm",
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
