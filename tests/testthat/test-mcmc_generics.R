# Sean Sorek

test_that("MCMC methods errors checked", {
  withr::local_package("nimble")
  # bad object type
  expect_error(summary.btpc_MCMC("object"), regexp = "Only use this method")
  expect_error(print.btpc_MCMC("good morning!"), regexp = "Only use this method")
  expect_error(predict.btpc_MCMC("good morning!"), regexp = "Only use this method")
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

  ## Predict
  # bad link
  expect_error(predict.btpc_MCMC(quad, type = "teehee"), regexp = "'type'")
  # bad quantiles / hdi
  expect_error(predict.btpc_MCMC(quad, summaryType = "whatever"), regexp = "summaryType")
  # bad centers
  expect_error(predict.btpc_MCMC(quad, centralSummary = "whatever"), regexp = "centralSummary")

  # broken model spec. this should NEVER happen.
  bad_quad <- quad
  class(bad_quad$model_spec) <- "btpc_model"
  expect_error(predict.btpc_MCMC(bad_quad), regexp = "Misconfigured Model Specification")

  bad_quad$model_spec <- "quadratic"
  expect_error(predict.btpc_MCMC(bad_quad), regexp = "Misconfigured Model Specification")

  ## Plot (shouldn't be necessary because this is just passed to predict)

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

  ## hist

  expect_error(hist.btpc_MCMC("not a model"), regexp = "output of b")
  expect_error(hist.btpc_MCMC(quad, burn = "hehe"), regexp = "Parameter 'burn' must be numeric")
  expect_error(hist.btpc_MCMC(quad, burn = 100000), regexp = "Parameter 'burn' must be smaller than")
  expect_error(hist.btpc_MCMC(quad, plot = "not_logical"))
})

test_that("MCMC methods output correctly", {
  skip_on_cran()
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

  ## print
  print_quad <- paste0(capture.output(print(quad)), collapse = "\n")
  print_bin <- paste0(capture.output(print(bin)), collapse = "\n")

  expect_output(print_MCMC_metadata(quad), regexp = "quadratic")
  expect_output(print_MCMC_metadata(quad), regexp = "m[i] <- ( ", fixed = T)
  expect_output(print_MCMC_metadata(quad), regexp = "Trait[i] ~ T(dnorm(mean = m[i], tau = 1/sigma.sq), 0, )", fixed = T)

  expect_output(print_MCMC_metadata(bin), regexp = "binomial_glm_lin")
  expect_output(print_MCMC_metadata(bin), regexp = "logit(m[i]) <- ( ", fixed = T)
  expect_output(print_MCMC_metadata(bin), regexp = "Trait[i] ~ dbinom(m[i], n[i])", fixed = T)

  expect_match(print_quad, regexp = "quadratic")
  expect_match(print_quad, regexp = "m[i] <- ( ", fixed = T)
  expect_match(print_quad, regexp = "Trait[i] ~ T(dnorm(mean = m[i], tau = 1/sigma.sq), 0, )", fixed = T)
  expect_match(print_quad, regexp = get_default_priors("quadratic")[1], fixed = T)
  expect_match(print_quad, regexp = get_default_priors("quadratic")[2], fixed = T)
  expect_match(print_quad, regexp = get_default_priors("quadratic")[3], fixed = T)
  expect_match(print_quad, regexp = "dexp(1)", fixed = T)

  expect_match(print_bin, regexp = "binomial_glm_lin")
  expect_match(print_bin, regexp = "logit(m[i]) <- ( ", fixed = T)
  expect_match(print_bin, regexp = "Trait[i] ~ dbinom(m[i], n[i])", fixed = T)
  expect_match(print_bin, regexp = get_default_priors("binomial_glm_lin")[1], fixed = T)
  expect_match(print_bin, regexp = get_default_priors("binomial_glm_lin")[2], fixed = T)

  ## summary
  summary_quad <- paste0(capture.output(summary(quad)), collapse = "\n")
  summary_bin <- paste0(capture.output(summary(bin)), collapse = "\n")


  expect_match(summary_quad, regexp = "quadratic")
  expect_match(summary_quad, regexp = "m[i] <- ( ", fixed = T)
  expect_match(summary_quad, regexp = "Trait[i] ~ T(dnorm(mean = m[i], tau = 1/sigma.sq), 0, )", fixed = T)
  expect_match(summary_quad, regexp = get_default_priors("quadratic")[1], fixed = T)
  expect_match(summary_quad, regexp = get_default_priors("quadratic")[2], fixed = T)
  expect_match(summary_quad, regexp = get_default_priors("quadratic")[3], fixed = T)
  expect_match(summary_quad, regexp = "dexp(1)", fixed = T)
  expect_match(summary_quad, regexp = "Empirical mean")

  expect_match(summary_bin, regexp = "binomial_glm_lin")
  expect_match(summary_bin, regexp = "logit(m[i]) <- ( ", fixed = T)
  expect_match(summary_bin, regexp = "Trait[i] ~ dbinom(m[i], n[i])", fixed = T)
  expect_match(summary_bin, regexp = get_default_priors("binomial_glm_lin")[1], fixed = T)
  expect_match(summary_bin, regexp = get_default_priors("binomial_glm_lin")[2], fixed = T)

  # checking different central summaries and interval types
  median_hdi_90 <- predict(quad)[[1]]
  mean_hdi_95 <- predict(quad, centralSummary = "mean", prob = .95)[[1]]
  mean_quantile_90 <- predict(quad, summaryType = "quantile", centralSummary = "mean")[[1]]
  median_quantile_95 <- predict(quad, summaryType = "quantile", quantiles = c(.025, .975))[[1]]

  quad_evals <- simplify2array(.mapply(
    FUN = get_model_function("quadratic"), dots = data.frame(quad$samples[, !colnames(quad$samples) %in% "sigma.sq"]),
    MoreArgs = list(Temp = seq(from = min(quad$data$Temp), to = max(quad$data$Temp), length.out = 1000))
  ))

  quad_medians <- matrixStats::rowMedians(quad_evals)
  quad_means <- matrixStats::rowMeans2(quad_evals)
  quad_hdi_90 <- apply(FUN = HDInterval::hdi, X = quad_evals, MARGIN = 1, credMass = .9)
  quad_hdi_95 <- apply(FUN = HDInterval::hdi, X = quad_evals, MARGIN = 1, credMass = .95)
  quad_q_90 <- matrixStats::rowQuantiles(quad_evals, probs = c(.05, .95))
  quad_q_95 <- matrixStats::rowQuantiles(quad_evals, probs = c(.025, .975))

  expect_equal(median_hdi_90$medians, quad_medians)
  expect_equal(mean_hdi_95$means, quad_means)
  expect_equal(mean_quantile_90$means, quad_means)
  expect_equal(median_quantile_95$medians, quad_medians)

  expect_equal(median_hdi_90$lower_bounds, quad_hdi_90[1, ])
  expect_equal(median_hdi_90$upper_bounds, quad_hdi_90[2, ])
  expect_equal(mean_hdi_95$lower_bounds, quad_hdi_95[1, ])
  expect_equal(mean_hdi_95$upper_bounds, quad_hdi_95[2, ])

  expect_equal(mean_quantile_90$lower_bounds, quad_q_90[, 1])
  expect_equal(mean_quantile_90$upper_bounds, quad_q_90[, 2])
  expect_equal(median_quantile_95$lower_bounds, quad_q_95[, 1])
  expect_equal(median_quantile_95$upper_bounds, quad_q_95[, 2])
  link <- predict(bin, type = "link")[[1]]
  response <- predict(bin, type = "response")[[1]]

  bin_link_evals <- simplify2array(.mapply(
    FUN = get_model_function("binomial_glm_lin", type = "link"), dots = data.frame(bin$samples[, !colnames(bin$samples) %in% "sigma.sq"]),
    MoreArgs = list(Temp = seq(from = min(bin$data$Temp), to = max(bin$data$Temp), length.out = 1000))
  ))
  bin_response_evals <- exp(bin_link_evals) / (1 + exp(bin_link_evals))
  link_medians <- matrixStats::rowMedians(bin_link_evals)
  response_medians <- matrixStats::rowMedians(bin_response_evals)

  expect_equal(link$medians, link_medians)
  expect_equal(response$medians, response_medians)

  ## hist

  expect_no_error(hist.btpc_MCMC(quad))
  expect_no_error(hist.btpc_MCMC(quad, burn = 1000))
  expect_invisible(hist.btpc_MCMC(quad))
  expect_visible(hist.btpc_MCMC(quad, plot = F))
})
