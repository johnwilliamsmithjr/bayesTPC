# Sean Sorek 06/04/2024
# Issue 36 on github

## Likelihood Creation ==================================================
new_btpc_likelihood <- function(name = character(),
                                formula = expression(),
                                llh_parameters = character(), #names are parameter names, body are default priors
                                llh_constants = double(), #names are constant names, body are default values
                                class = character(),
                                ...
                                ) {
  stopifnot("Likelihood name must be a string" = is.character(name))
  stopifnot("Likelihood priors must be written as strings" = is.character(llh_parameters))
  stopifnot("Likelihood constants must be numeric" = is.numeric(llh_constants))
  stopifnot("Likelihood formula must be an expression" = is.expression(formula))

  structure(name,
            class = "btpc_likelihood",
            formula = formula,
            llh_parameters = llh_parameters,
            llh_constants = llh_constants,
            ...)
  }


#' Specify a Trait Data Likelihood
#'
#' Creates and registers a new likelihood for trait data. Useful for custom parameterizations of trait likelihoods, like using precision instead of variance in a normal distribution.
#'
#' @param name character, the name of the new likelihood. Must not match any existing likelihood in bayesTPC.
#' @param formula expression, the likelihood code for trait data. The inferential parameter must be named 'm'.
#' @param llh_parameters optional named character, additional parameters used in the likelihood calculation (names) and their respective priors (value).
#' @param llh_constants optional named double, additional constants used in the likelihood calculation (names) and their respective default values (value).
#'
#' @return Returns a `btpc_likelihood` object, which can be modified or used in [specify_model()] to create model specifications. Additionally, the name is registered so that the default specification can be referenced in the same fashion as the pre-included likelihoods.
#' @export
specify_likelihood <- function(name = character(),
                               formula = expression(),
                               llh_parameters = character(), #names are parameter names, body are default priors
                               llh_constants = double()) {

  x <- new_btpc_likelihood(name, formula, llh_parameters, llh_constants)
  x <- validate(x)

  llh_list[[name]] <- x
  utils::assignInMyNamespace("llh_list", llh_list)
  cat(paste0(
    "Likelihood type '", name, "' can now be accessed using other bayesTPC functions. ",
    "Use `reset_likelihoods()` to reset back to defaults.\n"
  ))
  x
}

## Likelihood Validation =============================================

#' @export
validate.btpc_likelihood <- function(x) {
  name <- unclass(x)
  parameters <- attr(x, "llh_parameters")
  formula <- attr(x, "formula")
  constants <- attr(x, "llh_constants")

  # name
  if (length(name) == 0) stop("Likelihood specification must have a name.")
  if (length(name) != 1) stop("Likelihood specification must only have one name.")
  if (name %in% llh_list) stop("Likelihood must have unique name. To remove all user-defined models, use reset_likelihoods().")

  #formula
  if (length(formula) == 0) stop("Likelihood specification must have a formula.")
  if (length(formula) > 1)  stop("Likelihood specification can only have one formula.")
  formula_vars <- all.vars(formula) # extract all variables from formula
  if (!"m" %in% formula_vars) stop("Inferential parameter must be named 'm'.")

  # parameters
  if (length(parameters) > 0) {
    par_names <- names(parameters)
    if (is.null(par_names)) stop("'llh_parameters' vector must be named.")
    if (any(vapply(par_names, function(x) x == "", TRUE))) stop("All likelihood parameters must be named.")
    if (length(par_names) != length(unique(par_names))) stop("Likelihood parameters must have unique names.")
    if (!all(par_names %in% formula_vars)) stop("One or more parameters are not included in the model formula.")
  }

  # constants
  if (length(constants) > 0) {
    const_names <- names(constants)

    if (is.null(const_names)) stop("'constants' vector must be named.")
    if (any(vapply(const_names, function(x) x == "", TRUE))) stop("All model constants must be named.")
    if (length(const_names) != length(unique(const_names))) stop("Model constants must have unique names.")
    if (!all(const_names %in% formula_vars)) stop("One or more constants are not included in the model formula.")
  }

  return(x)
}

## Modifying Likelihoods =============================================

#' @rdname change_priors
#' @export
change_priors.btpc_likelihood <- function(x, priors = character()) {
  if (!("btpc_likelihood" %in% class(x))) {
    stop("Invalid type for likelihood.")
  }

  if (is.null(priors)) return(x)
  if (length(priors) == 0) return(x)

  priors_error_check(priors)
  params_to_change <- names(priors)
  current_priors <- attr(x, "llh_parameters")

  if(!all(params_to_change %in% names(current_priors))) {
    stop("Attempting to change prior of non-existent parameter.")
  }

  current_priors[names(priors)] <- unlist(priors)
  attr(x, "llh_parameters") <- current_priors
  return(x)
}

#' @rdname change_constants
#' @export
change_constants.btpc_likelihood <- function(x, constants) {
  if (!("btpc_likelihood" %in% class(x))) {
    stop("Invalid type for likelihood.")
  }

  if (length(constants) == 0) {
    return(x)
  }

  constants_error_check(constants)
  params_to_change <- names(constants)
  current_constants <- attr(x, "llh_constants")

  if(!all(params_to_change %in% names(current_constants))) {
    stop("Attempting to change prior of non-existent constant.")
  }

  current_constants[names(constants)] <- unlist(constants)
  attr(x, "llh_constants") <- current_constants
  return(x)
}

## Removing Likelihoods ==============================================

#' Removing User-Defined Likelihoods
#'
#' Removes one or all user-defined likelihoods accessible to other `bayesTPC` functions. `remove_likelihood()` only removes one specific likelihood, while `reset_likelihoods()` removes **all** user-defined likelihoods.
#' @param name character, the name of the likelihood to be removed
#' @export
remove_likelihood <- function(name) {
  if(!name %in% llh_list) {
    stop("Attempting to remove non-existent likelihood.")
  }

  if (name %in% immutable_llh_list) {
    stop("Only user-defined likelihoods can be removed.")
  }

  ## REMOVE IN FUNCTION ENVIRONMENT
  llh_list[[name]] <- NULL

  ## UPDATE PACKAGE ENVIRONMENT
  utils::assignInMyNamespace("llh_list", llh_list)
  cat("Likelihood '",name,"' has been removed and can no longer be accessed by other bayesTPC functions.", sep = "")
}

#' @rdname remove_likelihood
reset_likelihoods <- function() {
  utils::assignInMyNamespace("llh_list", immutable_llh_list)
  cat("All user-defined likelihoods have been removed.")
}

## Methods ========================================================

#' Get implemented likelihoods.
#'
#' Returns the names of all implemented likelihoods in `bayesTPC`.
#'
#' @export
#'
get_likelihoods <- function() {
  names(llh_list)
}
