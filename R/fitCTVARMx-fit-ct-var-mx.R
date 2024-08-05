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
#' @examples
#' \dontrun{
#' # Generate data using the simStateSpace package------------------------------
#' set.seed(42)
#' sim <- simStateSpace::SimSSMOUFixed(
#'   n = 5,
#'   time = 100,
#'   delta_t = 0.10,
#'   mu0 = rep(x = 0, times = 3),
#'   sigma0_l = t(chol(diag(3))),
#'   mu = rep(x = 0, times = 3),
#'   phi = matrix(
#'     data = c(
#'       -0.357, 0.771, -0.450,
#'       0.0, -0.511, 0.729,
#'       0, 0, -0.693
#'     ),
#'     nrow = 3
#'   ),
#'   sigma_l = t(chol(diag(3))),
#'   nu = rep(x = 0, times = 3),
#'   lambda = diag(3),
#'   theta_l = matrix(data = 0, nrow = 3, ncol = 3)
#' )
#' data <- as.data.frame(sim)
#'
#' # Fit the model--------------------------------------------------------------
#' library(fitCTVARMx)
#' fit <- FitCTVARMx(
#'   data = data,
#'   observed = c("y1", "y2", "y3"),
#'   id = "id"
#' )
#' print(fit)
#' summary(fit)
#' coef(fit)
#' vcov(fit)
#' }
#'
#' @references
#' Hunter, M. D. (2017).
#' State space modeling in an open source, modular,
#' structural equation modeling environment.
#' *Structural Equation Modeling: A Multidisciplinary Journal*,
#' *25*(2), 307–324.
#' \doi{10.1080/10705511.2017.1369354}
#'
#' Neale, M. C., Hunter, M. D., Pritikin, J. N.,
#' Zahery, M., Brick, T. R., Kirkpatrick, R. M., Estabrook, R.,
#' Bates, T. C., Maes, H. H., & Boker, S. M. (2015).
#' OpenMx 2.0: Extended structural equation and statistical modeling.
#' *Psychometrika*,
#' *81*(2), 535–549.
#' \doi{10.1007/s11336-014-9435-8}
#'
#' @family CTVAR Functions
#' @keywords fitCTVARMx fit
#' @export
FitCTVARMx <- function(data,
                       observed,
                       id,
                       time,
                       iota_fixed = TRUE,
                       iota_values = NULL,
                       iota_lbound = NULL,
                       iota_ubound = NULL,
                       phi_values = NULL,
                       phi_lbound = NULL,
                       phi_ubound = NULL,
                       sigma_diag = TRUE,
                       sigma_values = NULL,
                       sigma_lbound = NULL,
                       sigma_ubound = NULL,
                       theta_fixed = TRUE,
                       theta_values = NULL,
                       theta_lbound = NULL,
                       theta_ubound = NULL,
                       mu0_fixed = TRUE,
                       mu0_values = NULL,
                       mu0_lbound = NULL,
                       mu0_ubound = NULL,
                       sigma0_fixed = TRUE,
                       sigma0_diag = TRUE,
                       sigma0_values = NULL,
                       sigma0_lbound = NULL,
                       sigma0_ubound = NULL,
                       try = 1000,
                       ncores = NULL,
                       ...) {
  byid <- FALSE
  args <- list(
    data = data,
    observed = observed,
    id = id,
    time = time,
    iota_fixed = iota_fixed,
    iota_values = iota_values,
    iota_lbound = iota_lbound,
    iota_ubound = iota_ubound,
    phi_values = phi_values,
    phi_lbound = phi_lbound,
    phi_ubound = phi_ubound,
    sigma_diag = sigma_diag,
    sigma_values = sigma_values,
    sigma_lbound = sigma_lbound,
    sigma_ubound = sigma_ubound,
    theta_fixed = theta_fixed,
    theta_values = theta_values,
    theta_lbound = theta_lbound,
    theta_ubound = theta_ubound,
    mu0_fixed = mu0_fixed,
    mu0_values = mu0_values,
    mu0_lbound = mu0_lbound,
    mu0_ubound = mu0_ubound,
    sigma0_fixed = sigma0_fixed,
    sigma0_diag = sigma0_diag,
    sigma0_values = sigma0_values,
    sigma0_lbound = sigma0_lbound,
    sigma0_ubound = sigma0_ubound,
    try = try,
    ncores = ncores,
    byid = byid,
    ...
  )
  output <- .FitCTVAR(
    data = data,
    observed = observed,
    id = id,
    time = time,
    iota_fixed = iota_fixed,
    iota_values = iota_values,
    iota_lbound = iota_lbound,
    iota_ubound = iota_ubound,
    phi_values = phi_values,
    phi_lbound = phi_lbound,
    phi_ubound = phi_ubound,
    sigma_diag = sigma_diag,
    sigma_values = sigma_values,
    sigma_lbound = sigma_lbound,
    sigma_ubound = sigma_ubound,
    theta_fixed = theta_fixed,
    theta_values = theta_values,
    theta_lbound = theta_lbound,
    theta_ubound = theta_ubound,
    mu0_fixed = mu0_fixed,
    mu0_values = mu0_values,
    mu0_lbound = mu0_lbound,
    mu0_ubound = mu0_ubound,
    sigma0_fixed = sigma0_fixed,
    sigma0_diag = sigma0_diag,
    sigma0_values = sigma0_values,
    sigma0_lbound = sigma0_lbound,
    sigma0_ubound = sigma0_ubound,
    try = try,
    ncores = ncores,
    byid = byid,
    ...
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
