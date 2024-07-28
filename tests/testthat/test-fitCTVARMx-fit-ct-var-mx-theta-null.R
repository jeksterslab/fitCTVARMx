## ---- test-fitCTVARMx-fit-ct-var-mx-theta-null
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
    mu0 <- null_vec
    sigma0 <- diag(p)
    sigma0_l <- t(chol(sigma0))
    mu <- null_vec
    sigma <- 0.1 * iden
    sigma_l <- t(chol(sigma))
    nu <- null_vec
    lambda <- iden
    theta <- null_mat
    theta_l <- null_mat
    phi <- matrix(
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
    sim <- simStateSpace::SimSSMOUFixed(
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
    fit <- fitCTVARMx::FitCTVARMx(
      data = data,
      observed = paste0("y", seq_len(k)),
      id = "id",
      time = "time",
      sigma_diag = TRUE,
      ncores = NULL
    )
    print.fitctvarmx(fit)
    summary.fitctvarmx(fit)
    print.fitctvarmx(fit, means = FALSE)
    summary.fitctvarmx(fit, means = FALSE)
    coef.fitctvarmx(fit, iota = TRUE, sigma = TRUE, theta = TRUE)
    vcov.fitctvarmx(fit, iota = TRUE, sigma = TRUE, theta = TRUE)
    testthat::test_that(
      paste(text, 1),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                phi,
                diag(sigma)
              ) - coef.fitctvarmx(fit, sigma = TRUE)
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
    fit <- fitCTVARMx::FitCTVARMx(
      data = data,
      observed = paste0("y", seq_len(k)),
      id = "id",
      time = "time",
      phi_start = phi,
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
                phi,
                diag(sigma)
              ) - coef.fitctvarmx(fit, sigma = TRUE)
            ) <= tol
          )
        )
      }
    )
  },
  text = "test-fitCTVARMx-fit-ct-var-mx-theta-null",
  tol = 1
)
