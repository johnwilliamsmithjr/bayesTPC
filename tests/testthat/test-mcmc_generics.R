
test_that("summary errors checked", {
  withr::local_package("nimble")
  #bad object type
  expect_error(summary.btpc_MCMC("object"), regexp = "Only use this method")

  set.seed(12345)
  N <- 16
  q <- .75
  T_min <- 10
  T_max <- 35
  sd_trait <- 2
  Temps <- rep(c(15, 20, 25, 30), N / 4)
  Traits <- rep(0, N)
  for (i in 1:N) {
    while (Traits[i] <= 0) {
      Traits[i] <- rnorm(1, -1 * q * (Temps[i] - T_max) * (Temps[i] - T_min) * (Temps[i] > T_min) * (Temps[i] < T_max), 2)
    }
  }
  dat <- list(Trait = Traits, Temp = Temps)
  quad <- b_TPC(dat, "quadratic")

  #bad link
  expect_error(summary.btpc_MCMC(quad, type = "teehee"), regexp = "'type'")
  #bad quantiles / hdi
  expect_error(summary.btpc_MCMC(quad, summaryType = "whatever"), regexp = "summaryType")
  #bad centers
  expect_error(summary.btpc_MCMC(quad, centralSummary = "whatever"), regexp = "centralSummary")

  #broken model spec. this should NEVER happen.
  bad_quad <- quad
  class(bad_quad$model_spec) <- "btpc_model"
  expect_error(summary.btpc_MCMC(bad_quad), regexp = "Misconfigured Model Specification")

  bad_quad$model_spec <- "quadratic"
  expect_error(summary.btpc_MCMC(bad_quad), regexp = "Misconfigured Model Specification")

})

test_that("plot errors checked", {
  expect_error(plot.btpc_MCMC("object"), regexp = "Only use this method")

  set.seed(12345)
  N <- 16
  q <- .75
  T_min <- 10
  T_max <- 35
  sd_trait <- 2
  Temps <- rep(c(15, 20, 25, 30), N / 4)
  Traits <- rep(0, N)
  for (i in 1:N) {
    while (Traits[i] <= 0) {
      Traits[i] <- rnorm(1, -1 * q * (Temps[i] - T_max) * (Temps[i] - T_min) * (Temps[i] > T_min) * (Temps[i] < T_max), 2)
    }
  }
  dat <- list(Trait = Traits, Temp = Temps)
  quad <- b_TPC(dat, "quadratic")

  #bad link
  expect_error(plot.btpc_MCMC(quad, type = "teehee"), regexp = "'type'")
  #bad quantiles / hdi
  expect_error(plot.btpc_MCMC(quad, summaryType = "whatever"), regexp = "summaryType")
  #bad centers
  expect_error(plot.btpc_MCMC(quad, centralSummary = "whatever"), regexp = "centralSummary")
})

test_that("posterior_predictive errors checked", {
  #use expect_error
})

test_that("plot_prediction errors checked", {
  #use expect_error
})

test_that("print outputs correctly", {
  #use expect_output
})

test_that("summary outputs correctly", {
  #use expect_output
})


test_that("posterior_predictive branch statements", {
  #test for coverage? no real a posteriori way
})


test_that("plot output correct", {
  #use visualTest package
})

test_that("plot_prediction output correct", {
  #use visualTest package
})
