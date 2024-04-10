# Sean Sorek

#' Specify a Bernoulli model
#'
#' Creates an object with the required formatting to be fit using other `bayesTPC` functions.
#' @export
#' @details `bayesTPC` does not verify if the priors specified are compatible with NIMBLE's dialect of BUGS.
#'   All available distributions and formatting are provided on the
#'  \href{https://r-nimble.org/html_manual/cha-writing-models.html#subsec:dists-and-functions}{NIMBLE user manual}.
#'
#'  This model type should be used for data with a binary result.
#' @inheritParams specify_model
#' @param ... Additional model specification attributes. Unused by other functions.
#' @returns Returns an object of type `btpc_bernoulli_model`, which can then be used in other `bayesTPC` functions.
#'   The model name is also registered, and so can be accessed using by passing only the name into functions.
#'   However, user-defined models are not saved between sessions, and will be reset whenever the package is reloaded.
#' @examples
#' my_name <- "my_model"
#' my_formula <- expression(a * Temp^c + b)
#' my_parameters <- c(a = "dunif(0,1)", b = "dnorm(0,1)")
#' my_constants <- c(c = 1.5)
#'
#' \dontrun{
#' my_model <- specify_bernoulli_model(
#'   name = my_name,
#'   parameters = my_parameters,
#'   formula = my_formula,
#'   constants = my_constants
#' )
#' }
specify_bernoulli_model <- function(name = character(),
                                    parameters = character(), # names are parameters, values are priors
                                    formula = expression(),
                                    constants = double(), # names are constant names, values are default values
                                    ...) {
  specify_model(
    name = name,
    parameters = parameters,
    formula = formula,
    constants = constants,
    link = "logit",
    distribution = "bernoulli",
    ...
  )
}

#' Specify a binomial model
#'
#' Creates an object with the required formatting to be fit using other `bayesTPC` functions.
#' @export
#' @details `bayesTPC` does not verify if the priors specified are compatible with NIMBLE's dialect of BUGS.
#'   All available distributions and formatting are provided on the
#'  \href{https://r-nimble.org/html_manual/cha-writing-models.html#subsec:dists-and-functions}{NIMBLE user manual}.
#'
#'  This model type should be used for counts of binary results.
#' @inheritParams specify_model
#' @param ... Additional model specification attributes. Unused by other functions.
#' @returns Returns an object of type `btpc_binomial_model`, which can then be used in other `bayesTPC` functions.
#'   The model name is also registered, and so can be accessed using by passing only the name into functions.
#'   However, user-defined models are not saved between sessions, and will be reset whenever the package is reloaded.
#' @examples
#' my_name <- "my_model"
#' my_formula <- expression(a * Temp^c + b)
#' my_parameters <- c(a = "dunif(0,1)", b = "dnorm(0,1)")
#' my_constants <- c(c = 1.5)
#'
#' \dontrun{
#' my_model <- specify_binomial_model(
#'   name = my_name,
#'   parameters = my_parameters,
#'   formula = my_formula,
#'   constants = my_constants
#' )
#' }
specify_binomial_model <- function(name = character(),
                                   parameters = character(), # names are parameters, values are priors
                                   formula = expression(),
                                   constants = double(), # names are constant names, values are default values
                                   ...) {
  specify_model(
    name = name,
    parameters = parameters,
    formula = formula,
    constants = constants,
    link = "logit",
    distribution = "binomial",
    ...
  )
}

#' Specify model with normally distributed error
#'
#' Creates an object with the required formatting to be fit using other `bayesTPC` functions.
#'
#' @export
#' @details `bayesTPC` does not verify if the priors specified are compatible with NIMBLE's dialect of BUGS.
#'   All available distributions and formatting are provided on the
#'  \href{https://r-nimble.org/html_manual/cha-writing-models.html#subsec:dists-and-functions}{NIMBLE user manual}.
#' @inheritParams specify_model
#' @param sigma.sq optional character. The desired prior for the model variance.
#'   If not provided, a Student t distribution with with mu = 0, tau = 1/20, and df = 1 is used.
#' @returns Returns an object of type `btpc_normal_model`, which can then be used in other `bayesTPC` functions.
#'   The model name is also registered, and so can be accessed using by passing only the name into functions.
#'   However, user-defined models are not saved between sessions, and will be reset whenever the package is reloaded.
#' @examples
#' my_name <- "my_model"
#' my_formula <- expression(a * Temp^c + b)
#' my_parameters <- c(a = "dunif(0,1)", b = "dnorm(0,1)")
#' my_constants <- c(c = 1.5)
#'
#' \dontrun{
#' my_model <- specify_normal_model(
#'   name = my_name,
#'   parameters = my_parameters,
#'   formula = my_formula,
#'   constants = my_constants
#' )
#' }
specify_normal_model <- function(name = character(),
                                 parameters = character(), # names are parameters, values are priors
                                 formula = expression(),
                                 constants = double(), # names are constant names, values are default values
                                 sigma.sq = "dexp(1)",
                                 ...) {
  specify_model(
    name = name,
    parameters = parameters,
    formula = formula,
    constants = constants,
    link = "identity",
    distribution = "normal",
    sigma.sq = sigma.sq,
    ...
  )
}
