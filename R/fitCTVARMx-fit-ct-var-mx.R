#' Fit the First-Order Continuous-Time Vector Autoregressive Model
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams FitCTVARIDMx
#'
#' @return Returns an object of class `fitctvarmx` which is
#'   a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{args}{List of function arguments.}
#'     \item{fun}{Function used ("FitCTVARMx").}
#'     \item{output}{A fitted OpenMx model.}
#'   }
#'
#' @family CTVAR Functions
#' @keywords fitCTVARMx fit
#' @export
FitCTVARMx <- function(data,
                       observed,
                       id,
                       time,
                       phi_start = NULL,
                       phi_lbound = NULL,
                       phi_ubound = NULL,
                       sigma_diag = TRUE,
                       sigma_start = NULL,
                       sigma_lbound = NULL,
                       sigma_ubound = NULL,
                       theta_fixed = TRUE,
                       theta_start = NULL,
                       theta_lbound = NULL,
                       theta_ubound = NULL,
                       mu0_fixed = TRUE,
                       mu0_start = NULL,
                       mu0_lbound = NULL,
                       mu0_ubound = NULL,
                       sigma0_fixed = TRUE,
                       sigma0_diag = TRUE,
                       sigma0_start = NULL,
                       sigma0_lbound = NULL,
                       sigma0_ubound = NULL,
                       try = 1000,
                       ncores = NULL) {
  byid <- FALSE
  args <- list(
    data = data,
    observed = observed,
    id = id,
    time = time,
    phi_start = phi_start,
    phi_lbound = phi_lbound,
    phi_ubound = phi_ubound,
    sigma_diag = sigma_diag,
    sigma_start = sigma_start,
    sigma_lbound = sigma_lbound,
    sigma_ubound = sigma_ubound,
    theta_fixed = theta_fixed,
    theta_start = theta_start,
    theta_lbound = theta_lbound,
    theta_ubound = theta_ubound,
    mu0_fixed = mu0_fixed,
    mu0_start = mu0_start,
    mu0_lbound = mu0_lbound,
    mu0_ubound = mu0_ubound,
    sigma0_fixed = sigma0_fixed,
    sigma0_diag = sigma0_diag,
    sigma0_start = sigma0_start,
    sigma0_lbound = sigma0_lbound,
    sigma0_ubound = sigma0_ubound,
    try = try,
    ncores = ncores,
    byid = byid
  )
  output <- .FitCTVAR(
    data = data,
    observed = observed,
    id = id,
    time = time,
    phi_start = phi_start,
    phi_lbound = phi_lbound,
    phi_ubound = phi_ubound,
    sigma_diag = sigma_diag,
    sigma_start = sigma_start,
    sigma_lbound = sigma_lbound,
    sigma_ubound = sigma_ubound,
    theta_fixed = theta_fixed,
    theta_start = theta_start,
    theta_lbound = theta_lbound,
    theta_ubound = theta_ubound,
    mu0_fixed = mu0_fixed,
    mu0_start = mu0_start,
    mu0_lbound = mu0_lbound,
    mu0_ubound = mu0_ubound,
    sigma0_fixed = sigma0_fixed,
    sigma0_diag = sigma0_diag,
    sigma0_start = sigma0_start,
    sigma0_lbound = sigma0_lbound,
    sigma0_ubound = sigma0_ubound,
    try = try,
    ncores = ncores,
    byid = byid
  )
  out <- list(
    call = match.call(),
    args = args,
    fun = "FitCTVARMx",
    output = output
  )
  class(out) <- c(
    "fitctvarmx",
    class(out)
  )
  return(out)
}
