## Model Creation ================================
new_btpc_model <- function(name = character(),
                           parameters = character(), # names are parameters, values are priors
                           formula = expression(),
                           constants = double(), # names are constant names, values are default values
                           link = character(),
                           distribution = character(),
                           class = character(), # for subclassing support
                           ...) {
  # forcing explicit types
  stopifnot("Model name must be a string" = is.character(name))
  stopifnot("Model priors must be written as strings" = is.character(parameters))
  stopifnot("Model formula must be an expression" = is.expression(formula))
  stopifnot("Model constants must be numeric" = is.double(constants))



  structure(name,
    class = c(
      paste0("btpc_", distribution),
      paste0("btpc_", link),
      "btpc_model"
    ),
    parameters = parameters,
    formula = formula,
    constants = constants,
    link = link,
    distribution = distribution,
    ...
  )
}

#' Specify a model
#'
#' Creates an object with the required formatting to be fit using other `bayesTPC` functions.
#' @export
#' @details `bayesTPC` does not verify if the priors specified are compatible with NIMBLE's dialect of BUGS.
#'   All available distributions and formatting are provided on the
#'  \href{https://r-nimble.org/html_manual/cha-writing-models.html#subsec:dists-and-functions}{NIMBLE user manual}.
#'
#'  This model type should be used for counts of binary results.
#' @param name character, The desired name of the model specification.
#' @param parameters named character vector. The names should correspond to the parameters being fit,
#'   and the values should be the prior distributions to be drawn from for each respective parameter.
#'   Uniform distributions should be used unless there is good reason to draw from another.
#' @param formula expression. The actual formula being fit.
#'   Must include 'Temp' to represent temperature and all specified parameters and constants
#' @param constants optional double. Represents any terms in the formula that should not be fit.
#' @param link character. A link function between the model fit and the trait values.
#'   Currently supported options are: 'identity', 'log', 'logit', and 'reciprocal'. Default is 'identity'.
#' @param distribution character. The distribution used to calculate likelihoods.
#'   Currently supported options are: 'normal', 'poisson', 'bernoulli', 'binomial', 'exponential', and 'gamma'. Default is 'normal'.
#' @param ... Additional model specification attributes.
#'   If distribution is 'normal' or 'gamma', one can include an attribute named
#'   'sigma.sq' or 'shape_par' respectively to choose a non-default prior.
#' @returns Returns an object of type `btpc_model`, which can then be used in other `bayesTPC` functions.
#'   The model name is also registered, and so can be accessed using by passing only the name into functions.
#'   However, user-defined models are not saved between sessions, and will be reset whenever the package is reloaded.
#' @examples
#' my_name <- "my_model"
#' my_formula <- expression(a * Temp^c + b)
#' my_parameters <- c(a = "dunif(0,1)", b = "dnorm(0,1)")
#' my_constants <- c(c = 1.5)
#'
#' \dontrun{
#' my_model <- specify_model(
#'   name = my_name,
#'   parameters = my_parameters,
#'   formula = my_formula,
#'   constants = my_constants,
#'   link = "logit",
#'   distribution = "binomial"
#' )
#' }
specify_model <- function(name = character(),
                          parameters = character(),
                          formula = expression(),
                          constants = double(),
                          link = "identity",
                          distribution = "normal",
                          ...) {


  x <- new_btpc_model(name, parameters, formula, constants, link, distribution, ...)
  x <- validate(x)

  # add to model list. lets us check that model has been input validated
  model_list[[name]] <- x
  utils::assignInMyNamespace("model_list", model_list)
  cat(paste0(
    "Model type '", name, "' can now be accessed using other bayesTPC functions. ",
    "Restart R to reset back to defaults.\n"
  ))
  x
}

## Model Validation ================================================
validate <- function(x) {
  UseMethod("validate")
}

#' @export
validate.btpc_model <- function(x) {
  name <- unclass(x)
  parameters <- attr(x, "parameters")
  formula <- attr(x, "formula")
  constants <- attr(x, "constants")
  link <- attr(x, "link")
  distribution <- attr(x, "distribution")
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
    stop("Model must have unique name. To remove all user-defined models, restart R.")
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
  # it will break once nimble tries to run, but i'd prefer it checked here

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

  # constants
  if (length(constants) > 0) {
    const_names <- names(constants)

    if (is.null(const_names)) {
      stop("'constants' vector must be named.")
    }
    if (any(vapply(const_names, function(x) {
      x == "" #idk why i wrote this like this
    }, TRUE))) {
      stop("All model constants must be named.")
    }
    if (length(const_names) != length(unique(const_names))) {
      stop("Model constants must have unique names.")
    }
    if (!all(const_names %in% formula_vars)) {
      # this could be a warning but I want to be strict here.
      stop("One or more constants are not included in the model formula.")
    }
  }

  if (length(link) != 1) {
    stop("Model must have one and only one link function")
  }
  if (length(distribution) != 1) {
    stop("Model must have one and only one distribution")
  }
  supported_links <- c("identity", "log", "logit", "reciprocal")
  supported_dist <- c("normal", "poisson", "bernoulli", "binomial", "exponential", "gamma")

  if (!link %in% supported_links) {
    stop("Unsupported link function.")
  }
  if (!distribution %in% supported_dist) {
    stop("Unsupported distribution.")
  }

  return(x)
}

#' @export
validate.btpc_normal <- function(x) {
  x <- validate.btpc_model(x)
  var <- attr(x, "sigma.sq")
  if (length(var) == 0) {
    cat("Using default prior for model variance.\n")
    attr(x, "sigma.sq") <- "dexp(1)"
  }

  return(x)
}

#' @export
validate.btpc_gamma <- function(x) {
  x <- validate.btpc_model(x)
  var <- attr(x, "shape_par")
  if (length(var) == 0) {
    cat("Using default prior for shape parameter.\n")
    attr(x, "shape_par") <- "dexp(1)"
  }

  return(x)
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
    attr(model, "sigma.sq") <- priors["sigma.sq"]
    return(model)
  } else if ("shape_par" %in% params_to_change) {
    if (!all(params_to_change %in% c(names(current_priors), "shape_par"))) {
      stop("Attempting to change prior of non-existent parameter.")
    }
    model_priors <- priors[names(priors) != "shape_par"]
    current_priors[names(model_priors)] <- unlist(model_priors)
    attr(model, "parameters") <- current_priors
    attr(model, "shape_par") <- priors["shape_par"]
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
  if(is.null(constants_to_change)) {
    stop("New constants must be named.")
  }

  if (any(vapply(constants_to_change, function(x) {
    x == ""
  }, TRUE))) {
    stop("All new constants must be named.")
  }
  if (length(constants_to_change) != length(unique(constants_to_change))) {
    stop("New constants must have unique names.")
  }

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
  cat(paste0(cli::style_underline(cli::col_cyan("bayesTPC Model Specification of Type:\n")), "  ", c(x)))
  cat(paste0(cli::style_underline(cli::col_cyan("\n\nModel Formula:\n")), "  ", .link_string(x), attr(x, "formula"), " )"))
  cat(paste0(cli::style_underline(cli::col_cyan("\n\nModel Distribution:\n")), "  Trait[i] ~ ", .distribution_string(x)))
  cat(paste0(cli::style_underline(cli::col_cyan("\n\nModel Parameters and Priors:"))))
  params <- attr(x, "parameters")
  cat(paste0("\n  ", names(params), " ~ ", params))
  consts <- attr(x, "constants")
  if (length(consts) > 0) {
    cat(paste0(cli::style_underline(cli::col_cyan("\n\nModel Constants:"))))
    cat(paste0("\n  ", names(consts), " = ", consts))
  }
}

#' @export
print.btpc_normal <- function(x, ...) {
  print.btpc_model(x)
  cat(paste0(cli::style_underline(cli::col_cyan("\n\nPrior for Variance:")), "\n  sigma.sq ~ ", attr(x, "sigma.sq")))
}

#' @export
print.btpc_gamma <- function(x, ...) {
  print.btpc_model(x)
  cat(paste0(cli::style_underline(cli::col_cyan("\n\nPrior for Shape:")), "\n  shape_par ~ ", attr(x, "shape_par")))
}
