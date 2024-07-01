# Sean Sorek

test_that("bad data is caught", {
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
  correct <- list(Trait = Traits, Temp = Temps)
  vector <- c(Trait = Traits, Temp = Temps)
  no_temp <- list(Trait = Traits)
  null_temp <- list(Trait = Traits, Temp = NULL)
  no_trait <- list(Temp = Temps)
  null_trait <- list(Trait = NULL, Temp = Temps)
  diff_len <- list(Trait = rep(Traits, 2), Temp = Temps)
  bad_type1 <- list(Trait = c("hiii", "heey"), Temp = c(1, 2))
  bad_type2 <- list(Trait = c(1, 2), Temp = c("weee", "wooo"))
  large_temps <- list(Trait = Traits, Temp = Temps + 50)
  small_temps <- list(Trait = Traits, Temp = Temps - 50)
  temp_NA <- list(Trait = c(1, 2, 3), Temp = c(4, 5, NA))
  trait_NA <- list(Trait = c(1, 2, NA), Temp = c(4, 5, 6))

  expect_equal(check_data(correct), list(data = list(Trait = Traits, Temp = Temps), N = N))
  expect_error(check_data(NULL))
  expect_error(check_data(list()))
  expect_error(check_data(vector), regexp = "class")
  expect_error(check_data(no_temp), regexp = "called 'Temp'")
  expect_error(check_data(no_trait), regexp = "called 'Trait'")
  expect_error(check_data(null_temp), regexp = "called 'Temp'")
  expect_error(check_data(null_trait), regexp = "called 'Trait'")
  expect_error(check_data(diff_len), regexp = "same length")
  expect_error(check_data(bad_type1), regexp = "must be numeric")
  expect_error(check_data(bad_type2), regexp = "must be numeric")
  expect_warning(check_data(large_temps), regexp = "Temp>50")
  expect_warning(check_data(small_temps), regexp = "Temp<0")
  expect_error(check_data(temp_NA), regexp = "Temperature data")
  expect_error(check_data(trait_NA), regexp = "Trait data")
})

test_that("bad inits are caught", {
  expect_equal(.check_inits(NULL), list())
  expect_error(.check_inits(list()), regexp = "cannot be empty")
  expect_error(.check_inits(c(hi = 4)), regexp = "Unexpected type")
  expect_error(.check_inits(list(5)), regexp = "must be named")
  expect_equal(.check_inits(list(hi = 5)), list(hi = 5))
})

test_that("b_TPC catches errors", {
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

  # bad model specification
  expect_error(b_TPC(dat, model = NULL), regexp = "view implemented")
  expect_error(b_TPC(dat, model = "something_completely_wrong"), regexp = "Unsupported model")
  weird_model_spec <- new_btpc_model("weee",
    parameters = c(a = "dunif(0,1)"),
    formula = expression(a * Temp),
    link = "identity",
    distribution = "normal"
  )
  expect_error(b_TPC(dat, model = weird_model_spec), regexp = "incorrectly")

  # misc errors
  expect_error(b_TPC(dat, model = "quadratic", sampler = "something silly"), regexp = "Currently only RW")
  expect_error(b_TPC(dat, model = "binomial_glm_lin"), regexp = "must have a variable called 'n'")
})

test_that("b_TPC parameters work", {
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
  default_quad <- b_TPC(dat, "quadratic")
  mult_chain <- b_TPC(dat, "quadratic", nchains = 2)

  expect_equal(c(default_quad$model_spec), "quadratic")
  expect_equal(length(default_quad$constants), 0)
  expect_equal(nrow(default_quad$samples), 10000)
  expect_equal(default_quad$priors, get_default_priors("quadratic"))
  expect_equal(MAP_estimate(default_quad), default_quad$MAP_parameters)
  expect_equal(names(default_quad$MAP_parameters), c(colnames(default_quad$samples), "log_prob"))
  expect_true(is(default_quad$samples, "mcmc"))
  expect_true(is(mult_chain$samples, "mcmc.list"))
  changed_quad <- b_TPC(dat, "quadratic",
    niter = 8000, burn = 1000,
    samplerType = "slice",
    priors = list(
      q = "dunif(0, 1.5)",
      T_min = "dunif(0, 35)",
      sigma.sq = "dexp(1)"
    )
  )

  expect_equal(nrow(changed_quad$samples), 7000)
  expect_equal(changed_quad$priors[1:3], c(q = "dunif(0, 1.5)", T_max = "dunif(25, 60)", T_min = "dunif(0, 35)"))
  expect_equal(changed_quad$priors[4], c(sigma.sq = "dexp(1)"))

  # constants
  # want a smaller example dataset for faster testing
  load(testthat::test_path("example_data", "chlorella_tpc.rda"))
  ps_test <- list(
    Trait = chlorella_tpc$rate[which(chlorella_tpc$flux == "photosynthesis")],
    Temp = chlorella_tpc$temp[which(chlorella_tpc$flux == "photosynthesis")]
  )


  ps_default <- b_TPC(data = ps_test, model = "pawar_shsch")
  ps_changed <- b_TPC(
    data = ps_test, model = "pawar_shsch",
    constants = list(T_ref = 30)
  )
  expect_equal(ps_default$constants, c(T_ref = 20))
  expect_equal(ps_changed$constants, c(T_ref = 30))
  expect_output(print(ps_default), regexp = "T_ref = 20", fixed = T)
  expect_output(print(ps_changed), regexp = "T_ref = 30", fixed = T)
  expect_output(summary(ps_default), regexp = "T_ref = 20", fixed = T)
  expect_output(summary(ps_changed), regexp = "T_ref = 30", fixed = T)

  # binomial
  load(testthat::test_path("example_data", "bin_data.rda"))
  bin_tpc <- b_TPC(bin_test_list, "binomial_glm_quad",
    samplerType = "AF_slice", verbose = T
  ) # need to check verbose manually (maybe expect_output in the future idk)
})
