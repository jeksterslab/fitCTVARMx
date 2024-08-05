.FitCTVARGamma <- function(k,
                           idx,
                           iota_fixed = TRUE,
                           iota_values = NULL,
                           iota_lbound = NULL,
                           iota_ubound = NULL) {
  # B
  # latent variables on covariates
  if (iota_fixed) {
    if (is.null(iota_values)) {
      return(
        OpenMx::mxMatrix(
          type = "Zero",
          nrow = k,
          ncol = 1,
          name = "gamma"
        )
      )
    } else {
      return(
        OpenMx::mxMatrix(
          type = "Full",
          nrow = k,
          ncol = 1,
          free = FALSE,
          values = iota_values,
          byrow = FALSE,
          name = "gamma"
        )
      )
    }
  } else {
    if (is.null(iota_values)) {
      iota_values <- matrix(
        data = 0,
        nrow = k,
        ncol = 1
      )
    } else {
      if (is.vector(iota_values)) {
        iota_values <- matrix(
          data = iota_values,
          nrow = k,
          ncol = 1
        )
      } else {
        stopifnot(
          is.matrix(iota_values),
          dim(iota_values) == c(k, 1)
        )
      }
    }
    if (is.null(iota_lbound)) {
      iota_lbound <- matrix(
        data = NA,
        nrow = k,
        ncol = 1
      )
    } else {
      if (is.vector(iota_lbound)) {
        iota_lbound <- matrix(
          data = iota_lbound,
          nrow = k,
          ncol = 1
        )
      } else {
        stopifnot(
          is.matrix(iota_lbound),
          dim(iota_lbound) == c(k, 1)
        )
      }
    }
    if (is.null(iota_ubound)) {
      iota_ubound <- matrix(
        data = NA,
        nrow = k,
        ncol = 1
      )
    } else {
      if (is.vector(iota_ubound)) {
        iota_ubound <- matrix(
          data = iota_ubound,
          nrow = k,
          ncol = 1
        )
      } else {
        stopifnot(
          is.matrix(iota_ubound),
          dim(iota_ubound) == c(k, 1)
        )
      }
    }
    return(
      OpenMx::mxMatrix(
        type = "Full",
        nrow = k,
        ncol = 1,
        free = TRUE,
        values = iota_values,
        labels = paste0("iota_", idx),
        lbound = iota_lbound,
        ubound = iota_ubound,
        byrow = FALSE,
        name = "gamma"
      )
    )
  }
}
