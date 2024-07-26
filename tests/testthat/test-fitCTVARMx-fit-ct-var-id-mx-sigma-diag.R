## ---- test-fitCTVARMx-fit-ct-var-id-mx-sigma-diag
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    message(text)
    set.seed(42)
    n <- 2
    time <- 100
    delta_t <- 0.10
    k <- p <- 3
    iden <- diag(k)
    null_vec <- rep(x = 0, times = k)
    null_mat <- matrix(
      data = 0,
      nrow = p,
      ncol = p
    )
    mu0 <- list(
      null_vec
    )
    sigma0 <- diag(p)
    sigma0_l <- list(
      t(chol(sigma0))
    )
    mu <- list(
      null_vec
    )
    sigma <- 0.1 * iden
    sigma_l <- list(
      t(chol(sigma))
    )
    nu <- list(
      null_vec
    )
    lambda <- list(
      iden
    )
    theta <- null_mat
    theta_l <- list(
      null_mat
    )
    phi_mu <- matrix(
      data = c(
        -0.357,
        0.771,
        -0.450,
        0.0,
        -0.511,
        0.729,
        0,
        0,
        -0.693
      ),
      nrow = k
    )
    phi_sigma <- 0.00001 * diag(p * p)
    phi <- simStateSpace::SimPhiN(
      n = n,
      phi = phi_mu,
      vcov_phi_vec_l = t(chol(phi_sigma))
    )
    sim <- simStateSpace::SimSSMOUIVary(
      n = n,
      time = time,
      delta_t = delta_t,
      mu0 = mu0,
      sigma0_l = sigma0_l,
      mu = mu,
      phi = phi,
      sigma_l = sigma_l,
      nu = nu,
      lambda = lambda,
      theta_l = theta_l
    )
    data <- as.data.frame(sim)
    fit <- fitCTVARMx::FitCTVARIDMx(
      data = data,
      observed = paste0("y", seq_len(k)),
      id = "id",
      time = "time",
      sigma_diag = TRUE,
      ncores = NULL
    )
    print.fitctvaridmx(fit)
    summary.fitctvaridmx(fit)
    print.fitctvaridmx(fit, means = FALSE)
    summary.fitctvaridmx(fit, means = FALSE)
    coef.fitctvaridmx(fit, sigma = TRUE, theta = TRUE)
    vcov.fitctvaridmx(fit, sigma = TRUE, theta = TRUE)
    testthat::test_that(
      paste(text, 1),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                phi_mu,
                diag(sigma)
              ) - summary.fitctvaridmx(fit)
            ) <= tol
          )
        )
      }
    )
    phi_ubound <- phi_lbound <- matrix(
      data = NA,
      nrow = p,
      ncol = p
    )
    sigma_ubound <- sigma_lbound <- phi_lbound
    diag(phi_ubound) <- .Machine$double.xmin
    fit <- fitCTVARMx::FitCTVARIDMx(
      data = data,
      observed = paste0("y", seq_len(k)),
      id = "id",
      time = "time",
      phi_start = phi_mu,
      phi_lbound = phi_lbound,
      phi_ubound = phi_ubound,
      sigma_diag = TRUE,
      sigma_start = sigma,
      sigma_lbound = sigma_lbound,
      sigma_ubound = sigma_ubound,
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
      ncores = NULL
    )
    testthat::test_that(
      paste(text, 2),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                phi_mu,
                diag(sigma)
              ) - summary.fitctvaridmx(fit)
            ) <= tol
          )
        )
      }
    )
  },
  text = "test-fitCTVARMx-fit-ct-var-id-mx-sigma-diag",
  tol = 1
)
