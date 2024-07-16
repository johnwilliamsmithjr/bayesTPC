test_that("plotting errors are checked", {
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
  one_chain <- b_TPC(dat, "quadratic")

  mult_chain <- b_TPC(dat, "quadratic", nchains = 2)

  expect_no_error(traceplot(one_chain))
  expect_no_error(traceplot(mult_chain))
  expect_no_error(traceplot(one_chain$samples))
  expect_no_error(traceplot(mult_chain$samples))

  expect_error(traceplot("something"), regexp = "Invalid input")
  expect_error(traceplot("something"), regexp = "Invalid input")
  expect_error(traceplot(list(some = "thing")), regexp = "Object of class list must have element")

  mult_chain$samples$chain1 <- NULL
  mult_chain$samples$chain2 <- NULL

  expect_error(traceplot(mult_chain), regexp = "Sample list must have at least one element")
  expect_error(traceplot(mult_chain$samples), regexp = "Sample list must have at least one element")
})