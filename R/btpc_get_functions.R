#' Get Model Formula
#'
#' Returns the formula for a specified model.
#'
#' @export
#' @details
#'  Since the model formulas are stored and returned as strings, one of [str2lang()] or
#'   [str2expression()] can be used in conjunction with [eval()] for dynamic evaluation.
#'  However, it is better practice to use the output of [get_model_function()] for direct model evaluation.
#' @param model A string specifying the model name. Use [get_model_names()] to view all options.
#' @returns `get_formula` returns the formula for the provided model as a string.
#' @examples
#' get_formula("stinner")
#'
#' # If used to evaluate, variables must be set beforehand.
#' q <- .75
#' T_max <- 35
#' T_min <- 10
#' Temp <- c(15, 20, 25, 30)
#' ans <- eval(str2lang(get_formula("quadratic")))
#' ans
#'
get_formula <- function(model) {
  if (!(model %in% model_master_list[model_master_list$name == model, ][[1]])) {
    stop("Unsupported model, use get_models() to view implemented models.")
  }
  return(model_master_list[model_master_list$name == model, ]$formula[[1]])
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
  if (!(model %in% model_master_list[model_master_list$name == model, ][[1]])) {
    stop("Unsupported model, use get_models() to view implemented models.")
  }
  return(unlist(model_master_list[model_master_list$name == model, ]$params[[1]]))
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
  if (!(model %in% model_master_list[model_master_list$name == model, ][[1]])) {
    stop("Unsupported model, use get_models() to view implemented models.")
  }
  dp <- model_master_list[model_master_list$name == model, ]$default_priors[[1]]
  names(dp) <- model_master_list[model_master_list$name == model, ]$params[[1]]
  return(dp)
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
#' get_model_constants("pawar-shsch")
get_model_constants <- function(model) {
  if (!(model %in% model_master_list[model_master_list$name == model, ][[1]])) {
    stop("Unsupported model, use get_models() to view implemented models.")
  }
  consts <- model_master_list[model_master_list$name == model, ]$constants[[1]]

  if (!is.null(consts)){
    return(consts)
  }
  else{
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
#' get_default_constants("pawar-shsch")
get_default_constants <- function(model) {
  if (!(model %in% model_master_list[model_master_list$name == model, ][[1]])) {
    stop("Unsupported model, use get_models() to view implemented models.")
  }
  if (is.null(model_master_list[model_master_list$name == model, ]$constants[[1]])) {
    simpleMessage("Specified model has no associated constants.")
    return(NULL)
  } else {
    dc <- model_master_list[model_master_list$name == model, ]$default_constants[[1]]
    names(dc) <- model_master_list[model_master_list$name == model, ]$constants[[1]]
    return(dc)
  }
}

#' Get Models and Formulas
#'
#' Returns all implemented models and their respective formulas.
#'
#' @export
#' @returns `get_models()` returns a data frame with two columns containing the names and formulas for all implemented models in `bayesTPC`.
#' @examples
#'
#' get_models()
get_models <- function() {
  return(model_master_list[, c("name", "formula")])
}

#' Get Model Names
#'
#' Returns the names of all implemented models in `bayesTPC`.
#'
#' @export
#' @returns `get_model_names()` returns a character vector of all implemented models in `bayesTPC`.
#' @examples
#'
#' get_model_names()
get_model_names <- function() {
  return(unlist(model_master_list[, c("name")]))
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
#' #compare to example in get_formula()
#' quadratic_function <- get_model_function("quadratic")
#' quadratic_function(q = .75, T_max = 35, T_min = 10, Temp = c(15,20,25,30))
get_model_function <- function(model) {
  if (!(model %in% model_master_list[model_master_list$name == model, ][[1]])) {
    stop("Unsupported model, use get_models() to view implemented models.")
  }
  model_info <-
    model_master_list[model_master_list$name == model, ]

  sorted_pars <- sort(unlist(model_info$params))
  sorted_consts <- sort(unlist(model_info$constants))
  formula_string <- model_info$formula[[1]]

  params_string <- paste0(sorted_pars, ", ", collapse = "")
  if (is.null(sorted_consts)){
    function_string <- paste0("function(", params_string, "Temp){return(",formula_string,")}")
  }
  else{
    constant_string <- paste0(sorted_consts, ", ", collapse = "")
    function_string <- paste0("function(", params_string,constant_string, "Temp){return(",formula_string,")}")
  }
  return(eval(str2lang(function_string)))
}
