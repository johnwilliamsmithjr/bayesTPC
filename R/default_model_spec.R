## Model Creation
new_btpc_model <- function(name = character(),
                           parameters = character(), # names are parameters, values are priors
                           formula = expression(),
                           constants = double(), # names are constant names, values are default values
                           class = character(), # for subclassing support
                           ...) {
  # forcing explicit types
  stopifnot(is.character(name))
  stopifnot(is.character(parameters))
  stopifnot(is.expression(formula))
  stopifnot(is.double(constants))
  stopifnot(is.character(class))

  structure(name,
            class = c(class, "btpc_model"),
            parameters = parameters,
            formula = formula,
            constants = constants,
            ...
  )
}

## Model Validation
#' @export
validate.btpc_model <- function(x) {
  name <- unclass(x)
  parameters <- attr(x, "parameters")
  formula <- attr(x, "formula")
  constants <- attr(x, "constants")

  # This is gonna be the workhorse for input validation, since we want user model specification support.
  # S3 has no built in input validation, so this mostly just covers obvious edge cases.

  # name
  if (length(name) == 0) {
    stop("Model specification must have a name.")
  }
  if (length(name) != 1) {
    stop("Model specification must only have one name.")
  }
  # could the model list be stored as an environment? or is that too overcomplicated?

  if (name %in% model_list) {
    stop("Model must have unique name. To remove all user-defined models, reload 'bayesTPC'.")
  }
  # parameters
  if (length(parameters) == 0) {
    stop("Model specification must have parameters.")
  }
  par_names <- names(parameters)
  if (is.null(par_names)) {
    stop("'parameters' vector must be named.")
  }
  if (any(vapply(par_names, function(x) {
    x == ""
  }, TRUE))) {
    stop("All model parameters must be named.")
  }
  if (length(par_names) != length(unique(par_names))) {
    stop("Model parameters must have unique names.")
  }

  # how do u check if the priors are correctly written????
  # constants
  if (length(constants) > 0) {
    const_names <- names(constants)

    if (is.null(const_names)) {
      stop("'constants' vector must be named.")
    }
    if (any(vapply(const_names, function(x) {
      x == ""
    }, TRUE))) {
      stop("All model constants must be named.")
    }
    if (length(const_names) != length(unique(const_names))) {
      stop("Model constants must have unique names.")
    }
  }

  # formula
  if (length(formula) == 0) {
    stop("Model specification must have a formula.")
  }
  if (length(formula) > 1) {
    stop("Model specification can only have one formula.")
  }
  formula_vars <- all.vars(formula) # extract all variables from formula
  if (!"Temp" %in% formula_vars) {
    stop("Model specification formula must contain variable 'Temp'.")
  }
  if (!all(par_names %in% formula_vars)) {
    # this could be a warning but I want to be strict here.
    stop("One or more parameters are not included in the model formula.")
  }
  if (!all(formula_vars %in% c("Temp", par_names, names(constants)))) {
    stop("One or more variables in the model formula is not named as a parameter or a constant.")
  }

  return(x)
}

validate <- function(x) {
  UseMethod("validate")
}

#' @export
validate.default <- function(x) {
  stop("Misconfigured Model Specification.")
}

## Changing Models ======================================================

#' Change priors of pre-specified model
#'
#' Intended to be used to change the priors of an already existing model.
#'
#' @export
#' @details `bayesTPC` does not verify if priors specified are compatible with NIMBLE's dialect of BUGS.
#'   All available distributions and formatting are provided on the
#'  \href{https://r-nimble.org/html_manual/cha-writing-models.html#subsec:dists-and-functions}{NIMBLE user manual}.
#' @param model Object of type `btpc_model`. The specification to be changed.
#' @param priors named character. The names should correspond to the parameters to change, and the values should be the new desired priors.
#' @returns Returns the modified model. Does not change the default values of any registered model type.
change_priors <- function(model, priors) {
  if (!("btpc_model" %in% class(model))) {
    stop("Invalid type for model.")
  }
  if (!is.character(priors)) {
    stop("Invalid type for new priors.")
  }
  params_to_change <- names(priors)
  current_priors <- attr(model, "parameters")
  if ("sigma.sq" %in% params_to_change) {
    if (!all(params_to_change %in% c(names(current_priors), "sigma.sq"))) {
      stop("Attempting to change prior of non-existent parameter.")
    }
    model_priors <- priors[names(priors) != "sigma.sq"]
    current_priors[names(model_priors)] <- unlist(model_priors)
    attr(model, "parameters") <- current_priors
    attr(model, "sigma.sq") <- priors["sigma.sq"]
    return(model)
  } else {
    if (!all(params_to_change %in% names(current_priors))) {
      stop("Attempting to change prior of non-existent parameter.")
    }
    current_priors[params_to_change] <- unlist(priors)
    attr(model, "parameters") <- current_priors
    return(model)
  }

}

#' Change constants of pre-specified model
#'
#' Intended to be used to change the constants of an already existing model.
#'
#' @export
#' @details `bayesTPC` does not verify if constants specified are compatible with NIMBLE's dialect of BUGS.
#'   All available distributions and formatting are provided on the
#'  \href{https://r-nimble.org/html_manual/cha-writing-models.html#subsec:dists-and-functions}{NIMBLE user manual}.
#' @param model Object of type `btpc_model`. The specification to be changed.
#' @param constants named character. The names should correspond to the constants to change, and the values should be the new desired constants.
#' @returns Returns the modified model. Does not change the default values of any registered model type.
change_constants <- function(model, constants) {
  if (!("btpc_model" %in% class(model))) {
    stop("Invalid type for model.")
  }
  if (!is.double(constants)) {
    stop("Invalid type for new constants.")
  }

  constants_to_change <- names(constants)
  current_constants <- attr(model, "constants")

  if (length(current_constants) == 0) {
    stop("Attempting to change constants for a model without constants.")
  }
  if (!all(constants_to_change %in% names(current_constants))) {
    stop("Attempting to change non-existent constant.")
  }
  current_constants[constants_to_change] <- unlist(constants)
  attr(model, "constants") <- current_constants
  return(model)
}


## Methods

#' @export
print.btpc_model <- function(x, ...) {
  cat(paste0("bayesTPC Model Specification of Type: ", c(x)))
  cat(paste0("\nModel Formula:\n  ", attr(x, "formula")))
  cat(paste0("\nModel Parameters and Priors:"))
  params <- attr(x, "parameters")
  cat(paste0("\n  ", names(params), ": ", params))
  consts <- attr(x, "constants")
  if (length(consts) > 0) {
    cat(paste0("\nModel Constants:"))
    cat(paste0("\n  ", names(consts), ": ", consts))
  }
}

#' @export
.loop_string.btpc_model <- function(model) {
  model_string <- paste0(
    "{\n    for (i in 1:N){\n            ",
    "Trait[i] ~ dnorm(Temp[i],1) ",
    "\n    }\n"
  )

  return(model_string)
}

#' @export
.priors_string.btpc_model <- function(model) {
  priors <- attr(model, "parameters")
  num_params <- length(priors)

  priors_vec <- paste0(names(priors), " ~ ", priors)
  priors_string <- paste0("    ", paste0(priors_vec, collapse = "\n    "))
  return(priors_string)
}
