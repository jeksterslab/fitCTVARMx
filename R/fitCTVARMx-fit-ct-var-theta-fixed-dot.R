.FitCTVARThetaFixed <- function(k,
                                observed) {
  # R
  # measurement error
  return(
    OpenMx::mxMatrix(
      type = "Zero",
      nrow = k,
      ncol = k,
      dimnames = list(
        observed,
        observed
      ),
      name = "theta"
    )
  )
}
