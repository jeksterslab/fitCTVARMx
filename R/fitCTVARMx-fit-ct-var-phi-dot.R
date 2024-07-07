.FitCTVARPhi <- function(k,
                         idx,
                         statenames,
                         phi_start = NULL,
                         phi_lbound = NULL,
                         phi_ubound = NULL) {
  # A
  # auto regression and cross regression coefficients
  if (is.null(phi_start)) {
    phi_start <- 0.10 * diag(k)
  } else {
    stopifnot(
      is.matrix(phi_start),
      dim(phi_start) == c(k, k)
    )
  }
  phi_labels <- matrix(
    data = "",
    nrow = k,
    ncol = k
  )
  for (i in idx) {
    for (j in idx) {
      phi_labels[i, j] <- paste0(
        "phi",
        "_",
        idx[i],
        idx[j]
      )
    }
  }
  if (is.null(phi_lbound)) {
    phi_lbound <- NA
  } else {
    stopifnot(
      is.matrix(phi_lbound),
      dim(phi_lbound) == c(k, k)
    )
  }
  if (is.null(phi_ubound)) {
    phi_ubound <- matrix(
      data = NA,
      nrow = k,
      ncol = k
    )
    diag(phi_ubound) <- .Machine$double.xmin
  } else {
    stopifnot(
      is.matrix(phi_ubound),
      dim(phi_ubound) == c(k, k)
    )
  }
  return(
    OpenMx::mxMatrix(
      type = "Full",
      nrow = k,
      ncol = k,
      free = TRUE,
      values = phi_start,
      labels = phi_labels,
      lbound = phi_lbound,
      ubound = phi_ubound,
      byrow = FALSE,
      dimnames = list(
        statenames,
        statenames
      ),
      name = "phi"
    )
  )
}
