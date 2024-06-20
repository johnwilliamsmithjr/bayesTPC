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
