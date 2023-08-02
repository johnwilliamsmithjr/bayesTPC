test_that("configure_model catches errors", {
  #bad model specification
  expect_error(configure_model(model = NULL), regexp = "view implemented")
  expect_error(configure_model(model = "something_completely_wrong"), regexp = "Unsupported model")
  weird_model_spec <- new_btpc_normal_model("weee",
                                            parameters = c(a = "dunif(0,1)"),
                                            formula = expression(a * Temp))
  expect_error(configure_model(model = weird_model_spec), regexp = "incorrectly")

  #bad priors
  expect_error(configure_model(model = "quadratic", priors = list()), regexp = "cannot be empty")
  expect_error(configure_model(model = "quadratic", priors = c(q = "dunif(0,.5)")), regexp = "Unexpected type")
  expect_error(configure_model(model = "quadratic", priors = list("dunif(0,.5)")), regexp = "must be named")

  #bad constants
  expect_error(configure_model(model = "quadratic", constants = list()), regexp = "cannot be empty")
  expect_error(configure_model(model = "quadratic", constants = c(q = 10)), regexp = "Unexpected type")
  expect_error(configure_model(model = "quadratic", constants = list(10)), regexp = "must be named")
})
