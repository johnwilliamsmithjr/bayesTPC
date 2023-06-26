#' Get Model Formula
#'
#' Returns the formula for a specified model.
#'
#' @export
#' @details
#'  Since the model formulas are stored and returned as strings, one of [str2lang()] or
#'   [str2expression()] can be used in conjunction with [eval()] for dynamic evaluation.
#'  However, it is better practice to use the output of [get_model_function()] for direct model evaluation.
#' @param model A string specifying the model name. Use [get_models()] to view all options.
#' @returns `get_formula` Returns the formula for the provided model as a string.
#' @examples
#' get_formula("stinner")
#'
#' # If used to evaluate, variables must be set beforehand.
#' q <- .75
#' T_max <- 35
#' T_min <- 10
#' Temps <- c(15, 20, 25, 30)
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
#' TODO
#'
#' @details
#' @inheritParams get_formula
#' @returns
#' @examples
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
#' TODO
#'
#' @details
#' @inheritParams get_formula
#' @returns
#' @examples
get_default_constants <- function(model) {
  if (!(model %in% model_master_list[model_master_list$name == model, ][[1]])) {
    stop("Unsupported model, use get_models() to view implemented models.")
  }
  if (is.null(model_master_list[model_master_list$name == model, ]$constants[[1]])) {
    warning("Specified model has no constants.")
    return(NULL)
  } else {
    dc <- model_master_list[model_master_list$name == model, ]$default_constants[[1]]
    names(dc) <- model_master_list[model_master_list$name == model, ]$constants[[1]]
    return(dc)
  }
}

#' Get Models and Formulas
#'
#' TODO
#'
#' @details
#' @param
#' @returns
#' @examples
get_models <- function() {
  return(model_master_list[, c("name", "formula")])
}

#' Get Model Names
#'
#' TODO
#'
#' @details
#' @param
#' @returns
#' @examples
get_model_names <- function() {
  return(unlist(model_master_list[, c("name")]))
}

#' Get Model as a Function
#'
#' TODO
#'
#' @details
#' @inheritParams get_formula
#' @returns
#' @examples
get_model_function <- function(model) {

}
