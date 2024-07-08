## ---- test-fitCTVARMx-fit-ct-var-id-mx-theta-diag
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    message(text)
    set.seed(42)
    n <- 2
    time <- 500
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
    fit <- FitCTVARIDMx(
      data = data,
      observed = paste0("y", seq_len(k)),
      id = "id",
      time = "time",
      sigma_diag = TRUE,
      theta_fixed = FALSE,
      ncores = NULL
    )
    print(fit)
    summary(fit)
    print(fit, means = FALSE)
    summary(fit, means = FALSE)
    coef(fit, sigma = TRUE, theta = TRUE)
    vcov(fit, sigma = TRUE, theta = TRUE)
    testthat::test_that(
      paste(text, 1),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                phi_mu,
                diag(sigma),
                rep(x = 0, times = p)
              ) - summary(fit)
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
    theta_ubound <- theta_lbound <- phi_lbound
    diag(theta_lbound) <- .Machine$double.xmin
    diag(phi_ubound) <- .Machine$double.xmin
    fit2 <- FitCTVARIDMx(
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
      theta_fixed = FALSE,
      theta_start = 0.10 * diag(p),
      theta_lbound = theta_lbound,
      theta_ubound = theta_ubound,
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
    print(fit2)
    summary(fit2)
    print(fit2, means = FALSE)
    summary(fit2, means = FALSE)
    coef(fit2, sigma = TRUE, theta = TRUE)
    vcov(fit2, sigma = TRUE, theta = TRUE)
    testthat::test_that(
      paste(text, 2),
      {
        testthat::expect_true(
          all(
            abs(
              summary(fit) - summary(fit2)
            ) <= tol
          )
        )
      }
    )
  },
  text = "test-fitCTVARMx-fit-ct-var-id-mx-theta-diag",
  tol = 0.3
)
