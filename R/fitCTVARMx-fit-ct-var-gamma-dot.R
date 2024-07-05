.FitCTVARGamma <- function(k) {
  # B
  # latent variables on covariates
  return(
    OpenMx::mxMatrix(
      type = "Zero",
      nrow = k,
      ncol = 1,
      name = "gamma"
    )
  )
}
