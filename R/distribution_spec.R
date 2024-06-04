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

validate.btpc_likelihood <- function(x) {
  name <- unclass(x)
  parameters <- attr(x, "llh_parameters")
  formula <- attr(x, "formula")
  constants <- attr(x, "llh_constants")

  # name
  if (length(name) == 0) stop("Likelihood specification must have a name.")
  if (length(name) != 1) stop("Likelihood specification must only have one name.")
  if (name %in% model_list) stop("Likelihood must have unique name. To remove all user-defined models, use reset_likelihoods().")

  #formula
  if (length(formula) == 0) stop("Likelihood specification must have a formula.")
  if (length(formula) > 1)  stop("Likelihood specification can only have one formula.")
  formula_vars <- all.vars(formula) # extract all variables from formula
  if (!"m" %in% formula_vars) stop("Inferential parameter must be named 'm'.")

  # parameters
  if (length(parameters) > 0) {
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
change_priors.btpc_likelihood <- function(x, priors) {
  if (!("btpc_likelihood" %in% class(x))) {
    stop("Invalid type for model.")
  }

  if (length(priors) == 0) {
    return(model)
  }

  if (!is.character(priors)) {
    stop("Invalid type for new priors.")
  }

  params_to_change <- names(priors)
  if(is.null(params_to_change)) {
    stop("New priors must be named.")
  }

  if (any(vapply(params_to_change, function(x) {
    x == ""
  }, TRUE))) {
    stop("All new priors must be named.")
  }
  if (length(params_to_change) != length(unique(params_to_change))) {
    stop("New priors must have unique names.")
  }
  current_priors <- attr(model, "parameters")
  if ("sigma.sq" %in% params_to_change) {
    if (!all(params_to_change %in% c(names(current_priors), "sigma.sq"))) {
      stop("Attempting to change prior of non-existent parameter.")
    }
    model_priors <- priors[names(priors) != "sigma.sq"]
    current_priors[names(model_priors)] <- unlist(model_priors)
    attr(model, "parameters") <- current_priors
    sig <- priors["sigma.sq"]; attributes(sig) <- NULL
    attr(model, "sigma.sq") <- sig
    return(model)
  } else if ("shape_par" %in% params_to_change) {
    if (!all(params_to_change %in% c(names(current_priors), "shape_par"))) {
      stop("Attempting to change prior of non-existent parameter.")
    }
    model_priors <- priors[names(priors) != "shape_par"]
    current_priors[names(model_priors)] <- unlist(model_priors)
    attr(model, "parameters") <- current_priors
    sp <- priors["shape_par"]; attributes(sp) <- NULL
    attr(model, "shape_par") <- sp
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

## Removing Likelihoods ==============================================

## Methods
