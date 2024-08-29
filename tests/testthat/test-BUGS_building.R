# Sean Sorek

test_that("configure_model catches errors", {
  # bad model specification
  expect_error(configure_model(model = NULL), regexp = "view implemented")
  expect_error(configure_model(model = "something_completely_wrong"), regexp = "Unsupported model")
  weird_model_spec <- new_btpc_model("weee",
    parameters = c(a = "dunif(0,1)"),
    formula = expression(a * Temp),
    link = "identity",
    distribution = "normal"
  )
  expect_error(configure_model(model = weird_model_spec), regexp = "incorrectly")

  # bad priors
  expect_equal(configure_model(model = "quadratic", priors = list()), configure_model("quadratic"))
  expect_error(configure_model(model = "quadratic", priors = list("dunif(0,.5)")), regexp = "must be named")

  # bad constants
  expect_equal(configure_model(model = "quadratic", constants = list()), configure_model("quadratic"))
  expect_error(configure_model(model = "quadratic", constants = c(q = 10)), regexp = "model without constants")
  expect_error(configure_model(model = "pawar_shsch", constants = list(10)), regexp = "must be named")
})

test_that("configure model works", {
  default_quad <- configure_model("quadratic")
  changed_quad <- configure_model("quadratic",
    priors = list(
      q = "dunif(0, 1.5)",
      T_min = "dunif(0, 35)",
      sigma.sq = "dexp(2)"
    )
  )

  # i dont know how to use regex :)
  expect_match(default_quad, regexp = "q ~ dexp(1)", fixed = TRUE)
  expect_match(default_quad, regexp = "T_max ~ dunif(25, 60)", fixed = TRUE)
  expect_match(default_quad, regexp = "T_min ~ dunif(-10, 20)", fixed = TRUE)
  expect_match(default_quad, regexp = "sigma.sq ~ dexp(1)", fixed = TRUE)

  expect_match(changed_quad, regexp = "q ~ dunif(0, 1.5)", fixed = TRUE)
  expect_match(changed_quad, regexp = "T_max ~ dunif(25, 60)", fixed = TRUE)
  expect_match(changed_quad, regexp = "T_min ~ dunif(0, 35)", fixed = TRUE)
  expect_match(changed_quad, regexp = "sigma.sq ~ dexp(2)", fixed = TRUE)
})
test_that("default S3 BUGS functions throw errors", {
  expect_error(.link_string("random string"),         regexp = "Misconfigured Model Specification")
  expect_error(.distribution_string("random string"), regexp = "Invalid input")
  expect_error(.priors_string("random string"),       regexp = "Invalid input")
})
