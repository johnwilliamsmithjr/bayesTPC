# Sean Sorek

test_that("get errors checked", {
  expect_error(get_formula("heeey"), regexp = "Unsupported model")
  expect_error(get_model_params("heeey"), regexp = "Unsupported model")
  expect_error(get_default_priors("heeey"), regexp = "Unsupported model")
  expect_error(get_model_constants("heeey"), regexp = "Unsupported model")
  expect_error(get_default_constants("heeey"), regexp = "Unsupported model")
  expect_error(get_model_function("heeey"), regexp = "Unsupported model")
  expect_error(get_model_function("quadratic", type = "goofy"), regexp = "Invalid input")
  expect_error(get_default_model_specification("heeey"), regexp = "Unsupported model")
  expect_error(get_WAIC("garbage"), regexp = "Unexpected type for parameter")
  })

test_that("get functions work", {
  ## These tests shouldn't ever really fail unless something goes really wrong
  ## This may be useful if/when i switch the model types from chars to lists
  # formula
  expect_equal(
    get_formula("quadratic"),
    expression(-1 * q * (Temp - T_min) * (Temp - T_max) * (T_max > Temp) * (Temp > T_min))
  )
  expect_equal(
    get_formula("briere"),
    expression(q * Temp * (Temp - T_min) * sqrt((T_max > Temp) * abs(T_max - Temp)) * (T_max > Temp) * (Temp > T_min))
  )

  # params
  expect_equal(get_model_params("quadratic"), c("q", "T_max", "T_min", "sigma.sq"))
  expect_equal(get_model_params("stinner"), c("C", "k1", "k2", "T_opt", "sigma.sq"))
  # priors
  dq <- c(q = "dexp(1)", T_max = "dunif(25, 60)", T_min = "dunif(-10, 20)", sigma.sq = "dexp(1)")
  ds <- c(C = "dunif(0, 1000)", k1 = "dunif(-100, 100)", k2 = "dunif(-10, 10)", T_opt = "dunif(15, 70)", sigma.sq = "dexp(1)")
  expect_equal(get_default_priors("quadratic"), dq)
  expect_equal(get_default_priors("stinner"), ds)
  # constants
  expect_message(get_model_constants("quadratic"), regexp = "no associated constants.")
  expect_message(get_default_constants("quadratic"), regexp = "no associated constants.")

  expect_equal(get_model_constants("pawar_shsch"), c("T_ref"))
  expect_equal(get_default_constants("pawar_shsch"), c(T_ref = 20))
  # default spec
  expect_equal(get_default_model_specification("quadratic"), model_list[["quadratic"]])
  expect_equal(get_default_model_specification("briere"), model_list[["briere"]])

  # all models
  expect_equal(get_models(), names(model_list))

  # get model function

  qd <- function(q, T_max, T_min, Temp) {
    l <- -1 * q * (Temp - T_min) * (Temp - T_max) * (T_max >
                                                       Temp) * (Temp > T_min)
    l
  }

  bin_resp <- function (B0, B1, Temp)
  {
    l <- B0 + B1 * Temp
    exp(l)/(1 + exp(l))
  }

  bin_link <- function (B0, B1, Temp)
  {
    l <- B0 + B1 * Temp
    l
  }

  quad_test <- get_model_function("quadratic")
  bin_resp_test <- get_model_function("binomial_glm_lin")
  bin_link_test <- get_model_function("binomial_glm_lin", type = "link")

  expect_equal(sort(names(formals(quad_test))), sort(names(formals(qd))))
  expect_equal(body(quad_test), body(qd))

  expect_equal(sort(names(formals(bin_resp_test))), sort(names(formals(bin_resp))))
  expect_equal(   body(bin_resp_test),    body(bin_resp))

  expect_equal(sort(names(formals(bin_link_test))), sort(names(formals(bin_link))))
  expect_equal(   body(bin_link_test),    body(bin_link))


})
