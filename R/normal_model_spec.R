## Constructors

# internal
new_btpc_normal_model <- function(name = character(),
                                  parameters = character(), # names are parameters, values are priors
                                  formula = character(),
                                  constants = double(),
                                  sigma.sq = character(),
                                  ...) { # names are constant names, values are default values

  stopifnot(is.character(sigma.sq))

  new_btpc_model(
    name = name,
    parameters = parameters,
    formula = formula,
    constants = constants,
    sigma.sq = sigma.sq,
    class = "btpc_normal_model",
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
#' @inheritParams specify_binomial_model
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
                                 sigma.sq = character(),
                                 ...) {
  x <- new_btpc_normal_model(name, parameters, formula, constants, ...)
  x <- validate(x)
  model_list[[name]] <- x
  utils::assignInMyNamespace("model_list", model_list)
  cat(paste0(
    "Normal model type '", name, "' can now be accessed using other bayesTPC functions. ",
    "Reload the package to reset back to defaults.\n"
  ))
  return(x)
}

## Methods

# internal
#' @export
validate.btpc_normal_model <- function(x) {
  x <- validate.btpc_model(x)
  var <- attr(x, "sigma.sq")
  if (length(var) == 0) {
    cat("Using default prior for model variance.\n")
    attr(x, "sigma.sq") <- "T(dt(mu = 0, tau = 1/10, df = 1), 0, )"
  }

  return(x)
}

#' @export
print.btpc_normal_model <- function(x, ...) {
  print.btpc_model(x)
  cat(paste0("\nPrior for Variance:\n  ", attr(x, "sigma.sq")))
}

#' @export
.loop_string.btpc_normal_model <- function(model) {
  model_string <- paste0(
    "{\n    for (i in 1:N){\n            ",
    "Trait[i] ~ T(dnorm(mean = ",
    gsub("Temp", "Temp[i]", attr(model, "formula")),
    ", var = sigma.sq), 0, )\n    }\n"
  )
}

#' @export
.priors_string.btpc_normal_model <- function(model) {
  priors <- attr(model, "parameters")
  sig <- attr(model, "sigma.sq")
  num_params <- length(priors)

  priors_vec <- paste0(names(priors), " ~ ", priors)
  priors_string <- paste0("    ", paste0(priors_vec, collapse = "\n    "), "\n    sigma.sq ~ ", sig)
}
