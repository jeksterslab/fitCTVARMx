.FitCTVARLambda <- function(k,
                            observed,
                            statenames) {
  # C
  # measurement model factor loadings
  return(
    OpenMx::mxMatrix(
      type = "Diag",
      nrow = k,
      ncol = k,
      free = FALSE,
      values = 1,
      labels = NA,
      lbound = NA,
      ubound = NA,
      byrow = FALSE,
      dimnames = list(
        observed,
        statenames
      ),
      name = "lambda"
    )
  )
}
