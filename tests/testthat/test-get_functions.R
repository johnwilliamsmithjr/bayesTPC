test_that("get errors checked", {
  expect_error(get_formula("heeey"), regexp = "Unsupported model")
  expect_error(get_model_params("heeey"), regexp = "Unsupported model")
  expect_error(get_default_priors("heeey"), regexp = "Unsupported model")
  expect_error(get_model_constants("heeey"), regexp = "Unsupported model")
  expect_error(get_default_constants("heeey"), regexp = "Unsupported model")
  expect_error(get_model_function("heeey"), regexp = "Unsupported model")
  expect_error(get_default_model_specification("heeey"), regexp = "Unsupported model")
})

test_that("get functions assign properly", {
  ## These tests shouldn't ever really fail unless something goes really wrong
  ## This may be useful if/when i switch the model types from chars to lists
  # formula
  expect_equal(get_formula("quadratic"),
               expression(-1 * q * (Temp - T_min) * (Temp - T_max) * (T_max > Temp) * (Temp > T_min)))
  expect_equal(get_formula("briere"),
               expression(q * Temp * (Temp - T_min) * sqrt((T_max > Temp) * abs(T_max - Temp)) * (T_max > Temp) * (Temp > T_min)))

  # params
  expect_equal(get_model_params("quadratic"), c("q", "T_max", "T_min"))
  expect_equal(get_model_params("stinner"), c("C", "k1", "k2", "T_opt"))
  # priors

  # constants

  # default spec
})
