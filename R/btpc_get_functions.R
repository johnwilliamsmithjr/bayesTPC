#' Get Model Formula
#'
#' Returns the formula for a specified model.
#'
#' @export
#' @details This function is best used to investigate specifics of a model before using it.
#'  It can be used to evaluate models, but it is better practice to use the output of [get_model_function()] for direct model evaluation.
#' @param model A string specifying the model name, or a btpc_model object.
#' If a string is provided, the default model formula is provided if the model is implemented. If the model is not implemented, an error occurs.
#' Use [get_models()] to view all options.
#' @returns `get_formula` returns the formula for the provided model as an expression.
#' @seealso [get_model_function()]
#' @examples
#' get_formula("stinner")
#'
#' # If used to evaluate, variables must be set beforehand.
#' q <- .75
#' T_max <- 35
#' T_min <- 10
#' Temp <- c(15, 20, 25, 30)
#' ans <- eval(get_formula("quadratic"))
#' ans
#'
get_formula <- function(model) {
  if(!("btpc_model" %in% class(model))){
    if (!(model %in% model_list)){
      stop("Unsupported model, use get_models() to view implemented models.")
    }
    model <- model_list[[model]]
  }
  return(attr(model, "formula"))
}

#' Get Model Parameters
#'
#' Returns the parameters for a specified model.
#'
#' @export
#' @details `get_model_params` only returns the parameters changed during model training.
#'  Depending on the model chosen, there may be constants not included in the output. Use [get_model_constants()] to obtain these if needed.
#' @inheritParams get_formula
#' @returns A character vector containing all required parameters for the model provided.
#' @examples
#' get_model_params("ratkowsky")
#'
get_model_params <- function(model) {
  if(!("btpc_model" %in% class(model))){
    if (!(model %in% model_list)){
      stop("Unsupported model, use get_models() to view implemented models.")
    }
    model <- model_list[[model]]
  }
  return(names(attr(model, "parameters")))
}

#' Get Default Prior Distributions
#'
#' Returns the default distributions for all parameters trained by [b_TPC()].
#' These are used if no priors are provided.
#'
#' @export
#' @details Prior distributions are written in the format required by NIMBLE's BUGS implementation.
#'  All available distributions and formatting are provided on the
#'  \href{https://r-nimble.org/html_manual/cha-writing-models.html#subsec:dists-and-functions}{NIMBLE user manual}.
#' @inheritParams get_formula
#' @returns A named list of all parameters and their default prior distribution.
#' @examples
#' get_default_priors("briere")
get_default_priors <- function(model) {
  if(!("btpc_model" %in% class(model))){
    if (!(model %in% model_list)){
      stop("Unsupported model, use get_models() to view implemented models.")
    }
    model <- model_list[[model]]
  }
  return(attr(model, "parameters"))
}

#' Get Model Constants
#'
#' Returns the constants of a specified model, if they exist.
#'
#' @export
#' @details Using this function is only necessary if the model being fit has constant values.
#'  Currently, the only implemented model with constants is 'pawar-shsch'.
#' @inheritParams get_formula
#' @returns If the specified model has constants, a character vector containing all required constants for the model provided.
#'  Otherwise, only outputs a message and returns nothing.
#' @examples
#'
#' # Model without constants
#' get_model_constants("gaussian")
#'
#' # Model with constants
#' get_model_constants("pawar_shsch")
get_model_constants <- function(model) {
  if(!("btpc_model" %in% class(model))){
    if (!(model %in% model_list)){
      stop("Unsupported model, use get_models() to view implemented models.")
    }
    model <- model_list[[model]]
  }
  consts <- names(attr(model, "constants"))

  if (length(consts) > 0) {
    return(consts)
  } else {
    simpleMessage("Specified model has no associated constants.")
  }
}

#' Get Default Model Constants
#'
#' Returns the default values for all constants of a specified model.
#' These are used if no constant values are provided to [b_TPC()].
#'
#' @export
#' @details Using this function is only necessary if the model being fit has constant values.
#'  Currently, the only implemented model with constants is 'pawar-shsch'.
#' @inheritParams get_formula
#' @returns If the specified model has constants, `get_default_constants` returns a named numeric vector
#' of all default constant values used when none are provided. Otherwise, only outputs a message and returns nothing.
#' @examples
#'
#' #' # Model without constants
#' get_default_constants("gaussian")
#'
#' # Model with constants
#' get_default_constants("pawar_shsch")
get_default_constants <- function(model) {
  if(!("btpc_model" %in% class(model))){
    if (!(model %in% model_list)){
      stop("Unsupported model, use get_models() to view implemented models.")
    }
    model <- model_list[[model]]
  }
  consts <- attr(model, "constants")

  if (length(consts) > 0) {
    return(consts)
  } else {
    simpleMessage("Specified model has no associated constants.")
  }
}

#' Get Models and Formulas
#'
#' Returns all implemented models and their respective formulas.
#'
#' @export
#' @returns `get_formulas()` returns a named list containing all models and their respective formulas as expressions.
#' @examples
#'
#' get_models()
get_formulas <- function() {
  return(lapply(model_list, get_formula))
}

#' Get Model Names
#'
#' Returns the names of all implemented models in `bayesTPC`.
#'
#' @export
#' @returns `get_models()` returns a character vector of all implemented models in `bayesTPC`.
#' @examples
#'
#' get_models()
get_models <- function() {
  nm <- vapply(model_list, c, character(1))
  names(nm) <- NULL
  return(nm)
}

#' Get Model as a Function
#'
#' Builds and provides a function that evaluates an implemented model.
#'
#' @export
#' @details This function provides a more accessible way to directly evaluate implemented models.
#'  Manual evaluation is available through [get_formula()].
#' @inheritParams get_formula
#' @returns `get_model_function` returns a function that evaluates the implemented formula of the specified model.
#' The parameters of the specified model along with constants (if present) are used as the arguments to the returned function.
#' @examples
#'
#' stinner_function <- get_model_function("stinner")
#' stinner_function
#'
#' # compare to example in get_formula()
#' quadratic_function <- get_model_function("quadratic")
#' quadratic_function(q = .75, T_max = 35, T_min = 10, Temp = c(15, 20, 25, 30))
get_model_function <- function(model) {
  if(!("btpc_model" %in% class(model))){
    if (!(model %in% model_list)){
      stop("Unsupported model, use get_models() to view implemented models.")
    }
    model <- model_list[[model]]
  }

  sorted_pars <- sort(names(attr(model, "parameters")))
  sorted_consts <- sort(names(attr(model, "constants")))
  formula_string <- as.character(attr(model, "formula"))

  params_string <- paste0(sorted_pars, ", ", collapse = "")
  if (is.null(sorted_consts)) {
    function_string <- paste0("function(", params_string, "Temp){return(", formula_string, ")}")
  } else {
    constant_string <- paste0(sorted_consts, ", ", collapse = "")
    function_string <- paste0("function(", params_string, constant_string, "Temp){return(", formula_string, ")}")
  }
  return(eval(str2lang(function_string)))
}


#' Full Model Specification
#'
#' Returns the full details of an implemented model.
#'
#' @export
#' @param model A string, naming either a default or user-implemented model in `bayesTPC`.
#'   Use [get_models()] to view all options.
#' @returns A `btpc_model` object containing the model type, formula, parameters and respective priors,
#'   constants and respective default values if applicable, and variance prior if applicable.
get_default_model_specification <- function(model){
  if (!(model %in% model_list)){
    stop("Unsupported model, use get_models() to view implemented models.")
  }

  return(model_list[[model]])
}
