# Sean Sorek

## Model Creation ================================
new_btpc_model <- function(name = character(),
                           parameters = character(), # names are parameters, values are priors
                           formula = expression(),
                           constants = double(), # names are constant names, values are default values
                           link = character(),
                           distribution = character(), #character or btpc_likelihood
                           class = character(), # for subclassing support
                           ...) {
  # forcing explicit types
  stopifnot("Model name must be a string" = is.character(name))
  stopifnot("Model priors must be written as strings" = is.character(parameters))
  stopifnot("Model formula must be an expression" = is.expression(formula))
  stopifnot("Model constants must be numeric" = is.double(constants))



  structure(name,
    class = c("btpc_model",
              paste0("btpc_", link),
              paste0("btpc_", c(distribution))),
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
#' @param parameters named character, the names should correspond to the parameters being fit,
#'   and the values should be the prior distributions to be drawn from for each respective parameter.
#'   Uniform distributions should be used unless there is good reason to draw from another.
#' @param formula expression, The actual formula being fit.
#'   Must include 'Temp' to represent temperature and all specified parameters and constants
#' @param constants optional named double, Represents any terms in the formula that should not be fit.
#' @param link character, A link function between the model fit and the trait values.
#'   Currently supported options are: 'identity', 'log', 'logit', and 'reciprocal'. Default is 'identity'.
#' @param distribution character, The distribution used to calculate likelihoods.
#'   Currently supported options are: 'normal', 'poisson', 'bernoulli', 'binomial', 'exponential', and 'gamma'. Default is 'normal'.
#' @param llh_parameters optional named character, additional parameters used in the likelihood calculation (names) and their respective priors (value).
#' @param llh_constants optional named double, additional constants used in the likelihood calculation (names) and their respective default values (value).
#' @param ... Additional model specification attributes.
#' @returns Returns an object of type `btpc_model`, which can then be used in other `bayesTPC` functions.
#'   The model name is also registered, and so can be accessed using by passing only the name into functions.
#'   However, user-defined models are not saved between sessions, and will be reset whenever the package is reloaded.
#' @seealso [remove_model()], [reset_models()], [specify_bernoulli_model()], [specify_normal_model()], [specify_binomial_model()]
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
                          llh_parameters = character(),
                          llh_constants = double(),
                          ...) {


  x <- new_btpc_model(name, parameters, formula, constants, link, distribution, ...)
  x <- validate(x) #adds the default llh priors
  x <- change_priors(x, llh_parameters) #set to user llh priors :)
  x <- change_constants(x, llh_constants)

  # add to model list. lets us check that model has been input validated
  model_list[[name]] <- x
  utils::assignInMyNamespace("model_list", model_list)
  cat(paste0(
    "Model type '", name, "' can now be accessed using other bayesTPC functions. ",
    "Use `reset_models()` to reset back to defaults.\n"
  ))
  x
}

## Wrappers ========================================================

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
#' @seealso [remove_model()], [reset_models()], [specify_model()], [specify_normal_model()], [specify_binomial_model()]
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
#' @seealso [remove_model()], [reset_models()], [specify_bernoulli_model()], [specify_normal_model()], [specify_model()]
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
#' @param dist_parameters optional named character, additional parameters used in the likelihood calculation (names) and their respective priors (value).
#'  Default value is 'c(sigma.sq = "dexp(1)")'.
#'   If not provided, an exponential distribution with rate = 1 is used.
#' @returns Returns an object of type `btpc_normal_model`, which can then be used in other `bayesTPC` functions.
#'   The model name is also registered, and so can be accessed using by passing only the name into functions.
#'   However, user-defined models are not saved between sessions, and will be reset whenever the package is reloaded.
#' @seealso [remove_model()], [reset_models()], [specify_bernoulli_model()], [specify_model()], [specify_binomial_model()]
#' @examples
#' my_name <- "my_model"
#' my_formula <- expression(a * Temp^c + b)
#' my_parameters <- c(a = "dunif(0,1)", b = "dnorm(0,1)")
#' my_constants <- c(c = 1.5)
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
                                 dist_parameters = c(sigma.sq = "dexp(1)"),
                                 ...) {
  specify_model(
    name = name,
    parameters = parameters,
    formula = formula,
    constants = constants,
    link = "identity",
    distribution = "normal",
    dist_parameters = dist_parameters,
    ...
  )
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
  if (length(name) == 0) stop("Model specification must have a name.")
  if (length(name) != 1) stop("Model specification must only have one name.")
  if (name %in% model_list) stop("Model must have unique name. To remove all user-defined models, use reset_models().")

  # parameters
  if (length(parameters) == 0) stop("Model specification must have parameters.")
  par_names <- names(parameters)
  if (is.null(par_names)) stop("'parameters' vector must be named.")
  if (any(vapply(par_names, function(x) x == "", TRUE))) stop("All model parameters must be named.")
  if (length(par_names) != length(unique(par_names))) stop("Model parameters must have unique names.")

  # formula
  if (length(formula) == 0) stop("Model specification must have a formula.")
  if (length(formula) > 1)  stop("Model specification can only have one formula.")
  formula_vars <- all.vars(formula) # extract all variables from formula
  if (!"Temp" %in% formula_vars) stop("Model specification formula must contain variable 'Temp'.")
  if (!all(par_names %in% formula_vars)) stop("One or more parameters are not included in the model formula.")
  if (!all(formula_vars %in% c("Temp", par_names, names(constants)))) stop("One or more variables in the model formula is not named as a parameter or a constant.")

  # constants
  if (length(constants) > 0) {
    const_names <- names(constants)

    if (is.null(const_names)) stop("'constants' vector must be named.")
    if (any(vapply(const_names, function(x) x == "", TRUE))) stop("All model constants must be named.")
    if (length(const_names) != length(unique(const_names))) stop("Model constants must have unique names.")
    if (!all(const_names %in% formula_vars)) stop("One or more constants are not included in the model formula.")
  }

  # link/dist
  if (length(link) != 1) stop("Model must have one and only one link function")
  if (length(distribution) != 1) stop("Model must have one and only one distribution")

  supported_links <- c("identity", "log", "logit", "reciprocal")
  if (!link %in% supported_links) stop("Unsupported link function.")
  if (!distribution %in% llh_list) stop("Unsupported distribution.")

  if (!"btpc_likelihood" %in% class(distribution)) {
    if (!distribution %in% llh_list) stop("Unsupported likelihood function. ")
    distribution <- llh_list[[distribution]]
    attr(x, "distribution") <- distribution
  }

  return(x)
}

#' @export
validate.default <- function(x) {
  stop("Misconfigured Model Specification.")
}

## Changing Models ======================================================

priors_error_check <- function(priors) {
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
}

#' Change priors of pre-specified model
#'
#' Intended to be used to change the priors of an already existing model.
#'
#' @export
#' @details `bayesTPC` does not verify if priors specified are compatible with NIMBLE's dialect of BUGS.
#'   All available distributions and formatting are provided on the
#'  \href{https://r-nimble.org/html_manual/cha-writing-models.html#subsec:dists-and-functions}{NIMBLE user manual}.
#' @param x `btpc_model` or `btpc_likelihood`, The specification to be changed.
#' @param priors named character, The names should correspond to the parameters to change, and the values should be the new desired priors.
#' @returns Returns the modified model/likelihood. Does not change the default values of any registered model/likelihood type.
change_priors <- function(x, priors) {
  UseMethod("change_priors")
}

#' @rdname change_priors
#' @export
change_priors.btpc_model <- function(x, priors = character()) {
  if (is.null(priors)) return(x)
  if (length(priors) == 0) return(x)

  # separate the pieces we need
  priors_error_check(priors)
  params_to_change <- names(priors)
  current_priors <- attr(x, "parameters")
  llh <- attr(x, "distribution")
  llh_current_priors <- attr(llh, "llh_parameters")

  # clear bad inputs before splitting btw model and llh
  if (!all(params_to_change %in% c(names(current_priors), names(llh_current_priors)))) {
    stop("Attempting to change prior of non-existent parameter.")
  }

  # split input btw model and llh priors
  model_new_priors <- priors[params_to_change %in% names(current_priors)]
  llh_new_priors <- priors[params_to_change %in% names(llh_current_priors)]

  #change both llh and model priors
  modified_llh <- change_priors(llh, llh_new_priors)
  current_priors[names(model_new_priors)] <- unlist(model_new_priors)

  # attach back into model object
  attr(x, "distribution") <- modified_llh
  attr(x, "parameters") <- current_priors

  return(x)
}

#' @export
change_priors.default <- function(x, priors) {
  stop("Invalid type of specification.")
}

#' Change constants of pre-specified model
#'
#' Intended to be used to change the constants of an already existing model.
#'
#' @export
#' @details `bayesTPC` does not verify if constants specified are compatible with NIMBLE's dialect of BUGS.
#'   All available distributions and formatting are provided on the
#'  \href{https://r-nimble.org/html_manual/cha-writing-models.html#subsec:dists-and-functions}{NIMBLE user manual}.
#' @param x `btpc_model` or `btpc_likelihood`, The specification to be changed.
#' @param constants named character, The names should correspond to the constants to change, and the values should be the new desired constants.
#' @returns Returns the modified model. Does not change the default values of any registered model type.
change_constants <- function(x, constants) {
  UseMethod("change_constants")
}


constants_error_check <- function(constants) {
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
}

#' @rdname change_constants
#' @export
change_constants.btpc_model <- function(x, constants) {
  if (is.null(constants)) return(x)
  if (length(constants) == 0) return(x)


  llh <- attr(x, "distribution")
  llh_current_constants <- attr(llh, "llh_constants")

  current_constants <- attr(x, "constants")
  if (length(c(current_constants, llh_current_constants)) == 0) {
    stop("Attempting to change constants for a model without constants.")
  }

  constants_error_check(constants)
  constants_to_change <- names(constants)

  if (!all(constants_to_change %in% names(c(current_constants, llh_current_constants)))) {
    stop("Attempting to change non-existent constant.")
  }


  # split input btw model and llh constants
  model_new_constants <- constants[constants_to_change %in% names(current_constants)]
  llh_new_constants <- constants[constants_to_change %in% names(llh_current_constants)]

  #change both llh and model constants
  modified_llh <- change_constants(llh, llh_new_constants)
  current_constants[names(model_new_constants)] <- unlist(model_new_constants)

  # attach back into model object
  attr(x, "distribution") <- modified_llh
  attr(x, "constants") <- current_constants

  return(x)
}

#' @export
change_constants.default <- function(x, constants) {
  stop("Invalid type of specification.")
}
## Removing Models ==================================================


#' Removing User-defined models
#'
#' Removes one or all user-defined models accessible to other `bayesTPC` functions. `remove_model()` only removes one specific model, while `reset_models()` removes **all** user-defined models.
#' @param name character, the name of the model to be removed
#' @seealso [specify_model()], [specify_bernoulli_model()], [specify_normal_model()], [specify_binomial_model()]
#' @export
remove_model <- function(name) {
  if(!name %in% model_list) {
    stop("Attempting to remove non-existent model.")
  }

  if (name %in% immutable_model_list) {
    stop("Only user-defined models can be removed.")
  }

  ## REMOVE IN FUNCTION ENVIRONMENT
  model_list[[name]] <- NULL

  ## UPDATE PACKAGE ENVIRONMENT
  utils::assignInMyNamespace("model_list", model_list)
  cat("Model '",name,"' has been removed and can no longer be accessed by other bayesTPC functions.", sep = "")
}

#' @rdname remove_model
#' @export
reset_models <- function() {
  utils::assignInMyNamespace("model_list", immutable_model_list)
  cat("All user-defined models have been removed.")
}
## Methods ==========================================================

#' @export
print.btpc_model <- function(x, ...) {
  cat(paste0(cli::style_underline(cli::col_cyan("bayesTPC Model Specification of Type:\n")), "  ", c(x)))
  formula_string_wrapped <- paste(strwrap(paste0(.link_string(x), attr(x, "formula")), width = options()$width, simplify = F)[[1]], collapse = "\n")
  cat(paste0(cli::style_underline(cli::col_cyan("\n\nModel Formula:\n")), "  ",formula_string_wrapped, " )"))
  dist_string_wrapped <- paste(strwrap(paste0("  Trait[i] ~ ",.distribution_string(x)), width = options()$width, simplify = F)[[1]], collapse = "\n")
  cat(paste0(cli::style_underline(cli::col_cyan("\n\nModel Distribution:\n")), "  ",dist_string_wrapped))
  cat(paste0(cli::style_underline(cli::col_cyan("\n\nModel Parameters and Priors:"))))
  cat("", .priors_string(x), sep = "\n  ")
  consts <- c(attr(x, "constants"),attr(attr(x,"distribution"),"llh_constants"))
  if (length(consts) > 0) {
    cat(paste0(cli::style_underline(cli::col_cyan("\nModel Constants:"))))
    cat(paste0("\n  ", names(consts), " = ", consts))
  }
}
