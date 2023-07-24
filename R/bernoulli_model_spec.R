## Constructors

# internal
new_btpc_bernoulli_model <- function(name = character(),
                                    parameters = character(), # names are parameters, values are priors
                                    formula = character(),
                                    constants = double(),
                                    ...) { # names are constant names, values are default values

  new_btpc_model(
    name = name,
    parameters = parameters,
    formula = formula,
    constants = constants,
    class = "btpc_bernoulli_model",
    ...
  )
}

#' Specify a Bernoulli model
#'
#' Creates an object with the required formatting to be fit using other `bayesTPC` functions.
#' @export
#' @details `bayesTPC` does not verify if the priors specified are compatible with NIMBLE's dialect of BUGS.
#'   All available distributions and formatting are provided on the
#'  \href{https://r-nimble.org/html_manual/cha-writing-models.html#subsec:dists-and-functions}{NIMBLE user manual}.
#'
#'  This model type should be used for data with a binary result.
#' @param name character, The desired name of the model specification.
#' @param parameters named character vector. The names should correspond to the parameters being fit,
#'   and the values should be the prior distributions to be drawn from for each respective parameter.
#'   Uniform distributions should be used unless there is good reason to draw from another.
#' @param formula expression. The actual formula being fit.
#'   Must include 'Temp' to represent temperature and all specified parameters and constants
#' @param constants optional double. Represents any terms in the formula that should not be fit.
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
  x <- new_btpc_bernoulli_model(name, parameters, formula, constants, ...)
  x <- validate(x)
  model_list[[name]] <- x
  utils::assignInMyNamespace("model_list", model_list)
  cat(paste0(
    "Bernoulli model type '", name, "' can now be accessed using other bayesTPC functions. ",
    "Reload the package to reset back to defaults.\n"
  ))
}

## Methods

#' @export
.loop_string.btpc_bernoulli_model <- function(model) {
  model_string <- paste0(
    "{\n    for (i in 1:N){\n            ",
    "logit(p[i]) <- ", gsub("Temp", "Temp[i]", attr(model, "formula")),
    "\n            Trait[i] ~ dbern( p[i] )",
    "\n    }\n"
  )

  return(model_string)
}

