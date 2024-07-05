.FitCTVARKappa <- function(k) {
  # D
  # observed variables on covariates
  return(
    OpenMx::mxMatrix(
      type = "Zero",
      nrow = k,
      ncol = 1,
      name = "kappa"
    )
  )
}
