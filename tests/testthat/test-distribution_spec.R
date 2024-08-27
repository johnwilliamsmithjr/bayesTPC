test_that("likelihood validation works", {

  # name
  expect_error(specify_likelihood(), regexp = "must have a name")
  expect_error(specify_likelihood(name = 3), regexp = "name must be a string")
  expect_error(specify_likelihood(name = c("one", "two")), regexp = "must only have one name" )
  expect_error(specify_likelihood(name = "normal"), regexp = "must have unique name")

  # formula
  expect_error(specify_likelihood(name = "my_normal"), regexp = "must have a formula")
  expect_error(specify_likelihood(name = "my_normal", formula = "my_formula"), regexp = "must be an expression")
  expect_error(specify_likelihood(name = "my_normal", formula = c(expression(a + b), expression(b+c))), regexp = "can only have one formula")
  expect_error(specify_likelihood(name = "my_normal", formula = expression(a + b)), regexp = "Inferential parameter")

  # parameters
  expect_error(specify_likelihood(name = "my_normal", formula = expression(a + m),
                                  llh_parameters = c(a = 5)), regexp = "must be written as strings")
  expect_error(specify_likelihood(name = "my_normal", formula = expression(a + m),
                                  llh_parameters = c("b")), regexp = "vector must be named")
  expect_error(specify_likelihood(name = "my_normal", formula = expression(a + m),
                                  llh_parameters = c(a = "a", "b")), regexp = "All likelihood parameters must be named")
  expect_error(specify_likelihood(name = "my_normal", formula = expression(a + m),
                                  llh_parameters = c(a = "a", a = "b")), regexp = "parameters must have unique names")
  expect_error(specify_likelihood(name = "my_normal", formula = expression(a + m),
                                  llh_parameters = c(a = "a", c = "b")), regexp = "not included in the model formula")

  # constants
  expect_error(specify_likelihood(name = "my_normal", formula = expression(a + m),
                                  llh_parameters = c(a = "a"), llh_constants = "me"),
               regexp = "constants must be numeric")
  expect_error(specify_likelihood(name = "my_normal", formula = expression(a + m),
                                  llh_parameters = c(a = "a"), llh_constants = 5),
               regexp = "vector must be named")
  expect_error(specify_likelihood(name = "my_normal", formula = expression(a + m),
                                  llh_parameters = c(a = "a"), llh_constants = c(b = 5, 6)),
               regexp = "All model constants must be named")
  expect_error(specify_likelihood(name = "my_normal", formula = expression(a + m),
                                  llh_parameters = c(a = "a"), llh_constants = c(b = 5, b = 6)),
               regexp = "constants must have unique names")
  expect_error(specify_likelihood(name = "my_normal", formula = expression(a + m),
                                  llh_parameters = c(a = "a"), llh_constants = c(b = 5)),
               regexp = "not included in the model formula")
  })

test_that("changing likelihoods works", {
  expect_error(change_priors.btpc_likelihood("garbage"), regexp = "Invalid type for likelihood")

  def_normal <- llh_list[["normal"]]
  expect_equal(change_priors(def_normal), def_normal)
  expect_equal(change_priors(def_normal, priors = character()), def_normal)
  expect_equal(change_priors(def_normal, priors = NULL), def_normal)

  expect_error(change_priors(def_normal, priors = 5), regexp = "Invalid type for new priors")
  expect_error(change_priors(def_normal, priors = c(garbage = "dnorm(1,1)")), regexp = "Attempting to change prior of non-existent parameter")

  expect_equal(attr(def_normal, "llh_parameters"), c(sigma.sq = "dexp(1)"))
  changed <- change_priors(def_normal, priors = c(sigma.sq = "dexp(2)"))
  expect_equal(attr(changed, "llh_parameters"), c(sigma.sq = "dexp(2)"))
  expect_equal(attr(def_normal, "llh_parameters"), c(sigma.sq = "dexp(1)"))
})

test_that("removing likelihoods works", {

  expect_output(specify_likelihood(name = "my_normal", formula = expression(a + m[i]),
                     llh_parameters = c(a = "dnorm(0,1)")), regexp = "can now be accessed using other bayesTPC functions")
  expect_error(specify_likelihood(name = "my_normal", formula = expression(a + m),
                                  llh_parameters = c(a = "a")), regexp = "must have unique name")

  expect_error(remove_likelihood("garbage"), regexp = "Attempting to remove non")
  expect_error(remove_likelihood("normal"), regexp = "defined likelihoods can be removed")
  expect_output(remove_likelihood("my_normal"), regexp = "has been removed and can no longer")
  expect_error(remove_likelihood("my_normal"), regexp = "Attempting to remove non")

  expect_output(specify_likelihood(name = "my_normal", formula = expression(a + m[i]),
                                   llh_parameters = c(a = "dnorm(0,1)")), regexp = "can now be accessed using other bayesTPC functions")
  expect_error(specify_likelihood(name = "my_normal", formula = expression(a + m),
                                  llh_parameters = c(a = "a")), regexp = "must have unique name")

  reset_likelihoods()
  expect_error(remove_likelihood("my_normal"), regexp = "Attempting to remove non")
})
