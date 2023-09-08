test_that("MCMC generics errors checked", {
  withr::local_package("nimble")
  # bad object type
  expect_error(summary.btpc_MCMC("object"), regexp = "Only use this method")
  expect_error(print.btpc_MCMC("good morning!"), regexp = "Only use this method")
  expect_error(plot.btpc_MCMC("object"), regexp = "Only use this method")
  expect_error(posterior_predictive("object"), regexp = "Only use this method")

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

  ## Summary
  # bad link
  expect_error(summary.btpc_MCMC(quad, type = "teehee"), regexp = "'type'")
  # bad quantiles / hdi
  expect_error(summary.btpc_MCMC(quad, summaryType = "whatever"), regexp = "summaryType")
  # bad centers
  expect_error(summary.btpc_MCMC(quad, centralSummary = "whatever"), regexp = "centralSummary")

  # broken model spec. this should NEVER happen.
  bad_quad <- quad
  class(bad_quad$model_spec) <- "btpc_model"
  expect_error(summary.btpc_MCMC(bad_quad), regexp = "Misconfigured Model Specification")

  bad_quad$model_spec <- "quadratic"
  expect_error(summary.btpc_MCMC(bad_quad), regexp = "Misconfigured Model Specification")

  ## Plot (shouldn't be necessary because this is just passed to summary)

  # bad link
  expect_error(plot.btpc_MCMC(quad, type = "teehee"), regexp = "'type'")
  # bad quantiles / hdi
  expect_error(plot.btpc_MCMC(quad, summaryType = "whatever"), regexp = "summaryType")
  # bad centers
  expect_error(plot.btpc_MCMC(quad, centralSummary = "whatever"), regexp = "centralSummary")

  ## posterior_predictive

  # he's a bad seed >:)
  expect_error(posterior_predictive(quad, seed = 1.5), regexp = "must be integer valued")

  # bad model specification
  # don't actually care where this is caught as long as it is
  bad_quad <- quad
  class(bad_quad$model_spec) <- "btpc_model"
  expect_error(posterior_predictive(bad_quad), regexp = "Misconfigured Model Specification")
  class(bad_quad$model_spec) <- c("btpc_model", "btpc_normal")
  expect_error(posterior_predictive(bad_quad), regexp = "Misconfigured Model Specification")
  class(bad_quad$model_spec) <- c("btpc_model", "btpc_identity")
  expect_error(posterior_predictive(bad_quad), regexp = "Misconfigured Model Specification")

  bad_quad$model_spec <- "quadratic"
  expect_error(posterior_predictive(bad_quad), regexp = "Misconfigured Model Specification")

  ## plot_prediction
  expect_error(plot_prediction("heeeey"), regexp = "should be the output of")
})

test_that("print outputs correctly", {
  withr::local_package("nimble")

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
  load(testthat::test_path("example_data", "bin_data.rda"))

  bin <- b_TPC(bin_test_list, "binomial_glm_lin")
  quad <- b_TPC(dat, "quadratic")
  bri <- b_TPC(dat, "briere")

  expect_output(print_MCMC_metadata(quad), regexp = "quadratic")
  expect_output(print_MCMC_metadata(quad), regexp = "m[i] <- ( ", fixed = T)
  expect_output(print_MCMC_metadata(quad), regexp = "Trait[i] ~ T(dnorm(mean = m[i], tau = 1/sigma.sq), 0, )", fixed = T)

  expect_output(print_MCMC_metadata(bri), regexp = "briere")
  expect_output(print_MCMC_metadata(bri), regexp = "m[i] <- ( ", fixed = T)
  expect_output(print_MCMC_metadata(bri), regexp = "Trait[i] ~ T(dnorm(mean = m[i], tau = 1/sigma.sq), 0, )", fixed = T)


  expect_output(print_MCMC_metadata(bin), regexp = "binomial_glm_lin")
  expect_output(print_MCMC_metadata(bin), regexp = "logit(m[i]) <- ( ", fixed = T)
  expect_output(print_MCMC_metadata(bin), regexp = "Trait[i] ~ dbinom(m[i], n[i])", fixed = T)
})

test_that("summary outputs correctly", {
  # use expect_output
})


test_that("posterior_predictive branch statements", {
  # test for coverage? no real automated way to prove correctness?
})


test_that("plot output correct", {
  # use visualTest package?
})

test_that("plot_prediction output correct", {
  # use visualTest package?
})
