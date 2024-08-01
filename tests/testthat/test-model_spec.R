# Sean Sorek

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
  expect_no_error(specify_model("my_cool_new_normal_model",
                              parameters = c(a = "dunif(0,1)", b = "dunif(0,2)"),
                              formula =expression(a*Temp/c + b*d),
                              constants = c(c = 3, d = 4), link = "identity",
                              distribution = "normal"))
  expect_equal(attr(attr(get_default_model_specification("my_cool_new_normal_model"), "distribution"), "llh_parameters"),c(sigma.sq = "dexp(1)"))

  expect_no_error(specify_model("my_cooler_new_normal_model",
                                parameters = c(a = "dunif(0,1)", b = "dunif(0,2)"),
                                formula =expression(a*Temp/c + b*d),
                                constants = c(c = 3, d = 4), link = "identity",
                                distribution = "normal",
                                llh_parameters = c(sigma.sq = "dexp(2)")))
  expect_equal(attr(attr(get_default_model_specification("my_cooler_new_normal_model"), "distribution"), "llh_parameters"),c(sigma.sq = "dexp(2)"))
  #gamma
  expect_no_error(specify_model("my_cool_new_gamma_model",
                                   parameters = c(a = "dunif(0,1)", b = "dunif(0,2)"),
                                   formula =expression(a*Temp/c + b*d),
                                   constants = c(c = 3, d = 4), link = "identity",
                                   distribution = "gamma"))
  expect_equal(attr(attr(get_default_model_specification("my_cool_new_gamma_model"), "distribution"), "llh_parameters"),c(shape_par = "dexp(1)"))

  expect_no_error(specify_model("my_cooler_new_gamma_model",
                                parameters = c(a = "dunif(0,1)", b = "dunif(0,2)"),
                                formula =expression(a*Temp/c + b*d),
                                constants = c(c = 3, d = 4), link = "identity",
                                distribution = "gamma",
                                llh_parameters = c(shape_par = "dexp(2)")))
  expect_equal(attr(attr(get_default_model_specification("my_cooler_new_gamma_model"), "distribution"), "llh_parameters"),c(shape_par = "dexp(2)"))
})

test_that("change of priors / constants", {
  expect_error(change_priors("garbage", c(garbage = "garb")), regexp = "Invalid type")
  expect_error(change_constants("garbage", c(garbage = "garb")), regexp = "Invalid type")

  def_quad <- get_default_model_specification("quadratic")
  ps <- get_default_model_specification("pawar_shsch")
  bin <- get_default_model_specification("binomial_glm_lin")
  gam <- specify_model("cooler_gamma_model",
                             parameters = c(a = "dunif(0,1)", b = "dunif(0,2)"),
                             formula =expression(a*Temp/c + b*d),
                             constants = c(c = 3, d = 4), link = "identity",
                             distribution = "gamma")

  expect_error(change_priors(def_quad, c(a = 1,b = 2,c = 3)), regexp = "Invalid type for new priors")
  expect_error(change_constants(ps, c(a = "a",b = "b",c = "c")), regexp = "Invalid type for new constants")
  expect_error(change_constants(def_quad, c(q = 2)), regexp = "model without constants")
  expect_error(change_constants(ps, c(k = 1)), regexp = "existent")

  expect_error(change_priors(def_quad, c("a","b","c")), regexp = "New priors must be named")
  expect_error(change_priors(def_quad, c(q = "a","b","c")), regexp = "All new priors")
  expect_error(change_priors(def_quad, c(q = "a",q= "b")), regexp = "have unique")
  expect_error(change_constants(ps, c(1,2,3)), regexp = "New constants must be named")
  expect_error(change_constants(ps, c(T_ref = 1,2,3)), regexp = "All new constants")
  expect_error(change_constants(ps, c(T_ref = 1, T_ref= 2)), regexp = "have unique")

  expect_equal(change_priors(def_quad, priors = list()), def_quad)
  expect_equal(change_priors(def_quad, priors = c()), def_quad)
  expect_equal(change_constants(def_quad, constants = list()), def_quad)
  expect_equal(change_constants(def_quad, constants = c()), def_quad)

  expect_error(change_priors(def_quad, c(sigma.sq = "dexp(5)",k = "dnorm(0,1)")), regexp = "existent")
  expect_error(change_priors(def_quad, c(k = "dnorm(0,1)")), regexp = "existent")
  expect_error(change_priors(gam, c(shape_par = "dexp(5)",k = "dnorm(0,1)")), regexp = "existent")

  expect_equal(attr(attr(gam, "distribution"), "llh_parameters"),c(shape_par = "dexp(1)"))
  gam2 <- change_priors(gam, c(shape_par = "dexp(5)"))
  expect_equal(attr(attr(gam, "distribution"), "llh_parameters"),c(shape_par = "dexp(1)"))
  expect_equal(attr(attr(gam2, "distribution"), "llh_parameters"),c(shape_par = "dexp(5)"))
})

test_that("printing models work", {
  def_quad <- get_default_model_specification("quadratic")
  gam <- get_default_model_specification("cooler_gamma_model")
  expect_output(print(def_quad), regexp = "quadratic")
  expect_output(print(def_quad), regexp = "Model Formula")
  expect_output(print(def_quad), regexp = "sigma.sq", fixed = T)

  expect_output(print(gam), regexp = "cooler_gamma_model")
  expect_output(print(gam), regexp = "Model Formula")
  expect_output(print(gam), regexp = "shape")
})

test_that("removing models works", {
  expect_true("cooler_gamma_model" %in% get_models())

  remove_model("cooler_gamma_model")

  expect_false("cooler_gamma_model" %in% get_models())

  gam <- specify_model("cooler_gamma_model",
                       parameters = c(a = "dunif(0,1)", b = "dunif(0,2)"),
                       formula =expression(a*Temp/c + b*d),
                       constants = c(c = 3, d = 4), link = "identity",
                       distribution = "gamma")

  expect_true("cooler_gamma_model" %in% get_models())

  reset_models()

  expect_false("cooler_gamma_model" %in% get_models())

  expect_error(remove_model("quadratic"), regexp = "Only user")
  expect_error(remove_model("not a model"), regexp = "Attempting to remove non")
})

test_that("specialized model spec constructors work", {
  ## normal
  #copying this code from paul's aedes project
  my_posquad_formula <- expression(a *(Temp - mu)^2 + c)
  my_posquad_priors <- c(a = "dexp(0.01)",
                         mu = "dunif(0.01,50)",
                         c = "dexp(0.01)")

  my_posquad <- specify_normal_model("my_posquad", #model name
                                               parameters = my_posquad_priors, #names are parameters, values are priors
                                               formula = my_posquad_formula)
  expect_equal(attr(my_posquad, "link"), "identity")
  expect_equal(attr(my_posquad, "distribution"), llh_list[["normal"]])
  ## binomial

  my_bin <- specify_binomial_model("my_bin",
                                   parameters = my_posquad_priors,
                                   formula = my_posquad_formula)

  expect_equal(attr(my_bin, "link"), "logit")
  expect_equal(attr(my_bin, "distribution"), llh_list[["binomial"]])
  ## bernoulli

  my_bern <- specify_bernoulli_model("my_bern",
                                   parameters = my_posquad_priors,
                                   formula = my_posquad_formula)

  expect_equal(attr(my_bern, "link"), "logit")
  expect_equal(attr(my_bern, "distribution"), llh_list[["bernoulli"]])
})
