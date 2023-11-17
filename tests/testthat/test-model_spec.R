test_that("bad model specification", {
  # model name
  expect_error(specify_model(), regexp = "must have a name")
  expect_error(specify_model(name = c("name1","name2")), regexp = "must only have one name")
  expect_error(specify_model("quadratic"), regexp = "must have unique name")
  expect_error(specify_model(2), regexp = "must be a string")

  # model parameters
  expect_error(specify_model("my_cool_new_model"), regexp = "must have parameters")
  expect_error(specify_model("my_cool_new_model",
                             parameters = c(1,2,3)), regexp = "must be written as strings")
  expect_error(specify_model("my_cool_new_model",
                             parameters = c("dunif(0,1)")), regexp = "must be named")
  expect_error(specify_model("my_cool_new_model",
                             parameters = c(a = "dunif(0,1)", "dunif(0,2)")), regexp = "All model parameters must be named")
  expect_error(specify_model("my_cool_new_model",
                             parameters = c(a = "dunif(0,1)", a = "dunif(0,2)")), regexp = "must have unique names")

  #formula
  expect_error(specify_model("my_cool_new_model",
                             parameters = c(a = "dunif(0,1)", b = "dunif(0,2)")), regexp = "must have a formula")
  expect_error(specify_model("my_cool_new_model",
                             parameters = c(a = "dunif(0,1)", b = "dunif(0,2)"),
                             formula ="not an expression"), regexp = "must be an expression")
  expect_error(specify_model("my_cool_new_model",
                             parameters = c(a = "dunif(0,1)", b = "dunif(0,2)"),
                             formula =c(expression(q), expression(p))), regexp = "can only have one formula")
  expect_error(specify_model("my_cool_new_model",
                             parameters = c(a = "dunif(0,1)", b = "dunif(0,2)"),
                             formula =expression(a)), regexp = "must contain variable 'Temp'")
  expect_error(specify_model("my_cool_new_model",
                             parameters = c(a = "dunif(0,1)", b = "dunif(0,2)"),
                             formula =expression(a*Temp)),
               regexp = "One or more parameters are not included in the model formula")
  expect_error(specify_model("my_cool_new_model",
                             parameters = c(a = "dunif(0,1)", b = "dunif(0,2)"),
                             formula =expression(a*Temp + b*c)),
               regexp = "variables in the model formula is not named as a parameter")

  # constants
  expect_error(specify_model("my_cool_new_model",
                             parameters = c(a = "dunif(0,1)", b = "dunif(0,2)"),
                             formula =expression(a*Temp + b),
                             constants = "hiiii"),
               regexp = "Model constants must be numeric")
  expect_error(specify_model("my_cool_new_model",
                             parameters = c(a = "dunif(0,1)", b = "dunif(0,2)"),
                             formula =expression(a*Temp + b),
                             constants = "hiiii"),
               regexp = "Model constants must be numeric")
  expect_error(specify_model("my_cool_new_model",
                             parameters = c(a = "dunif(0,1)", b = "dunif(0,2)"),
                             formula =expression(a*Temp + b),
                             constants = 2),
               regexp = "vector must be named")
  expect_error(specify_model("my_cool_new_model",
                             parameters = c(a = "dunif(0,1)", b = "dunif(0,2)"),
                             formula =expression(a*Temp + b),
                             constants = c(c = 3, 4)),
               regexp = "All model constants must be named")
  expect_error(specify_model("my_cool_new_model",
                             parameters = c(a = "dunif(0,1)", b = "dunif(0,2)"),
                             formula =expression(a*Temp + b),
                             constants = c(c = 3, c = 4)),
               regexp = "Model constants must have unique names")
  expect_error(specify_model("my_cool_new_model",
                             parameters = c(a = "dunif(0,1)", b = "dunif(0,2)"),
                             formula =expression(a*Temp + b),
                             constants = c(c = 3, d = 4)),
               regexp = "One or more constants are not included in the model formula")

  # link/dist
  expect_error(specify_model("my_cool_new_model",
                             parameters = c(a = "dunif(0,1)", b = "dunif(0,2)"),
                             formula =expression(a*Temp/c + b*d),
                             constants = c(c = 3, d = 4), link = character()),
               regexp = "Model must have one and only one link function")
  expect_error(specify_model("my_cool_new_model",
                             parameters = c(a = "dunif(0,1)", b = "dunif(0,2)"),
                             formula =expression(a*Temp/c + b*d),
                             constants = c(c = 3, d = 4), link = c("identity", "log")),
               regexp = "Model must have one and only one link function")

  expect_error(specify_model("my_cool_new_model",
                             parameters = c(a = "dunif(0,1)", b = "dunif(0,2)"),
                             formula =expression(a*Temp/c + b*d),
                             constants = c(c = 3, d = 4), link = "identity",
                             distribution = character()),
               regexp = "Model must have one and only one distribution")
  expect_error(specify_model("my_cool_new_model",
                             parameters = c(a = "dunif(0,1)", b = "dunif(0,2)"),
                             formula =expression(a*Temp/c + b*d),
                             constants = c(c = 3, d = 4), link = "identity",
                             distribution = c("normal", "poisson")),
               regexp = "Model must have one and only one distribution")

  expect_error(specify_model("my_cool_new_model",
                             parameters = c(a = "dunif(0,1)", b = "dunif(0,2)"),
                             formula =expression(a*Temp/c + b*d),
                             constants = c(c = 3, d = 4), link = "nonsense",
                             distribution = "normal"),
               regexp = "Unsupported link function")
  expect_error(specify_model("my_cool_new_model",
                             parameters = c(a = "dunif(0,1)", b = "dunif(0,2)"),
                             formula =expression(a*Temp/c + b*d),
                             constants = c(c = 3, d = 4), link = "identity",
                             distribution = "nonsense"),
               regexp = "Unsupported distribution")
  })

test_that("validate special cases work", {

  expect_error(validate("garbage"), regexp = "Misconfigured Model Specification")
  #normal
  expect_output(specify_model("my_cool_new_normal_model",
                              parameters = c(a = "dunif(0,1)", b = "dunif(0,2)"),
                              formula =expression(a*Temp/c + b*d),
                              constants = c(c = 3, d = 4), link = "identity",
                              distribution = "normal"),
                regexp = "Using default prior for model variance")
  expect_equal(attr(get_default_model_specification("my_cool_new_normal_model"), "sigma.sq"),"dexp(1)")

  #gamma
  expect_output(specify_model("my_cool_new_gamma_model",
                                   parameters = c(a = "dunif(0,1)", b = "dunif(0,2)"),
                                   formula =expression(a*Temp/c + b*d),
                                   constants = c(c = 3, d = 4), link = "identity",
                                   distribution = "gamma"),
                regexp = "Using default prior for shape parameter")
  expect_equal(attr(get_default_model_specification("my_cool_new_gamma_model"), "shape_par"),"dexp(1)")

})

test_that("bad change of priors / constants", {
  expect_error(change_priors("garbage", c(garbage = "garb")), regexp = "Invalid type for model")
  expect_error(change_constants("garbage", c(garbage = "garb")), regexp = "Invalid type for model")

  def_quad <- get_default_model_specification("quadratic")
  ps <- get_default_model_specification("pawar_shsch")

  expect_error(change_priors(def_quad, c(a = 1,b = 2,c = 3)), regexp = "Invalid type for new priors")
  expect_error(change_constants(def_quad, c(a = "a",b = "b",c = "c")), regexp = "Invalid type for new constants")

  expect_error(change_priors(def_quad, c("a","b","c")), regexp = "New priors must be named")
  expect_error(change_constants(def_quad, c(1,2,3)), regexp = "New constants must be named")
})
