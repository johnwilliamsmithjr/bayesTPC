test_that("configure_model catches errors", {
  # bad model specification
  expect_error(configure_model(model = NULL), regexp = "view implemented")
  expect_error(configure_model(model = "something_completely_wrong"), regexp = "Unsupported model")
  weird_model_spec <- new_btpc_normal_model("weee",
    parameters = c(a = "dunif(0,1)"),
    formula = expression(a * Temp)
  )
  expect_error(configure_model(model = weird_model_spec), regexp = "incorrectly")

  # bad priors
  expect_error(configure_model(model = "quadratic", priors = list()), regexp = "cannot be empty")
  expect_error(configure_model(model = "quadratic", priors = c(q = "dunif(0,.5)")), regexp = "Unexpected type")
  expect_error(configure_model(model = "quadratic", priors = list("dunif(0,.5)")), regexp = "must be named")

  # bad constants
  expect_error(configure_model(model = "quadratic", constants = list()), regexp = "cannot be empty")
  expect_error(configure_model(model = "quadratic", constants = c(q = 10)), regexp = "Unexpected type")
  expect_error(configure_model(model = "quadratic", constants = list(10)), regexp = "must be named")
})

test_that("configure model works", {
  default_quad <- configure_model("quadratic")
  changed_quad <- configure_model("quadratic",
    priors = list(
      q = "dunif(0, 1.5)",
      T_min = "dunif(0, 35)",
      sigma.sq = "dexp(1)"
  ))

  #i dont know how to use regex :)
  expect_match(default_quad, regexp = "q ~ dunif(0, 1)", fixed = TRUE)
  expect_match(default_quad, regexp = "T_max ~ dunif(25, 60)", fixed = TRUE)
  expect_match(default_quad, regexp = "T_min ~ dunif(0, 24)", fixed = TRUE)
  expect_match(default_quad, regexp = "sigma.sq ~ T(dt(mu = 0, tau = 1/10, df = 1), 0, )", fixed = TRUE)

  expect_match(changed_quad, regexp = "q ~ dunif(0, 1.5)", fixed = TRUE)
  expect_match(changed_quad, regexp = "T_max ~ dunif(25, 60)", fixed = TRUE)
  expect_match(changed_quad, regexp = "T_min ~ dunif(0, 35)", fixed = TRUE)
  expect_match(changed_quad, regexp = "sigma.sq ~ dexp(1)", fixed = TRUE)

})
test_that("default S3 BUGS functions throw errors", {
  expect_error(.loop_string("random string"), regexp = "Misconfigured")
  expect_error(.priors_string("random string"), regexp = "Misconfigured")
})
