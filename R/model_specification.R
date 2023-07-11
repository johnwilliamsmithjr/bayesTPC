## Model Creation ========================================================

#highest level class, lacks density function
#internal
new_btpc_model <- function(name = character(),
                           parameters = character(), #names are parameters, values are priors
                           formula = expression(),
                           constants = double(), #names are constant names, values are default values
                           class = character(), #for subclassing support
                           ...){

  #forcing explicit types
  stopifnot(is.character(name))
  stopifnot(is.character(parameters))
  stopifnot(is.expression(formula))
  stopifnot(is.double(constants))
  stopifnot(is.character(class))

  structure(name,
            class = c(class,"btpc_model"),
            parameters = parameters,
            formula = formula,
            constants = constants,
            ...)
}

#internal
new_btpc_normal_model <- function(name = character(),
                                  parameters = character(), #names are parameters, values are priors
                                  formula = character(),
                                  constants = double(),
                                  sigma.sq = character(),
                                  ...){  #names are constant names, values are default values

  stopifnot(is.character(sigma.sq))

  new_btpc_model(name = name,
                 parameters = parameters,
                 formula = formula,
                 constants = constants,
                 sigma.sq = sigma.sq,
                 class = "btpc_normal_model",
                 ...)
}

#internal
new_btpc_binomial_model <- function(name = character(),
                                    parameters = character(), #names are parameters, values are priors
                                    formula = character(),
                                    constants = double(),
                                    ...){ #names are constant names, values are default values

  new_btpc_model(name = name,
                 parameters = parameters,
                 formula = formula,
                 constants = constants,
                 class = "btpc_binomial_model",
                 ...)
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
#' my_model <- specify_normal_model(name = my_name,
#'                                  parameters = my_parameters,
#'                                  formula = my_formula,
#'                                  constants = my_constants)
#' }
specify_normal_model <- function(name = character(),
                                 parameters = character(), #names are parameters, values are priors
                                 formula = expression(),
                                 constants = double(), #names are constant names, values are default values
                                 sigma.sq = character(),
                                 ...){
  x <- new_btpc_normal_model(name, parameters, formula, constants, ...)
  x <- validate(x)
  model_list[[name]] <- x
  utils::assignInMyNamespace("model_list", model_list)
  print(paste0("Normal model type '", name,"' can now be accessed using other bayesTPC functions. ",
               "Reload the package to reset back to defaults.\n"))
  return(x)
}

#' Specify a binomial model
#'
#' Creates an object with the required formatting to be fit using other `bayesTPC` functions.
#' @export
#' @details `bayesTPC` does not verify if the priors specified are compatible with NIMBLE's dialect of BUGS.
#'   All available distributions and formatting are provided on the
#'  \href{https://r-nimble.org/html_manual/cha-writing-models.html#subsec:dists-and-functions}{NIMBLE user manual}.
#' @param name character, The desired name of the model specification.
#' @param parameters named character vector. The names should correspond to the parameters being fit,
#'   and the values should be the prior distributions to be drawn from for each respective parameter.
#'   Uniform distributions should be used unless there is good reason to draw from another.
#' @param formula expression. The actual formula being fit.
#'   Must include 'Temp' to represent temperature and all specified parameters and constants
#' @param constants optional double. Represents any terms in the formula that should not be fit.
#' @param ... Additional model specification attributes. Unused by other functions.
#' @returns Returns an object of type `btpc_binomial_model`, which can then be used in other `bayesTPC` functions.
#'   The model name is also registered, and so can be accessed using by passing only the name into functions.
#'   However, user-defined models are not saved between sessions, and will be reset whenever the package is reloaded.
#' @examples
#' my_name <- "my_model"
#' my_formula <- expression(a * Temp^c + b)
#' my_parameters <- c(a = "dunif(0,1)", b = "dnorm(0,1)")
#' my_constants <- c(c = 1.5)
#'
#' \dontrun{
#' my_model <- specify_binomial_model(name = my_name,
#'                                  parameters = my_parameters,
#'                                  formula = my_formula,
#'                                  constants = my_constants)
#' }
specify_binomial_model <- function(name = character(),
                                   parameters = character(), #names are parameters, values are priors
                                   formula = expression(),
                                   constants = double(), #names are constant names, values are default values
                                   ...){
  x <- new_btpc_binomial_model(name, parameters, formula, constants, ...)
  x <- validate(x)
  model_list[[name]] <- x
  utils::assignInMyNamespace("model_list", model_list)
  print(paste0("Binomial model type '", name,"' can now be accessed using other bayesTPC functions. ",
               "Reload the package to reset back to defaults.\n"))
}


## Model Validation ========================================================

#' @export
validate.btpc_model <- function(x){
 name <- unclass(x)
 parameters <- attr(x, "parameters")
 formula <- attr(x, "formula")
 constants <- attr(x, "constants")

 # This is gonna be the workhorse for input validation, since we want user model specification support.
 #S3 has no built in input validation, so this mostly just covers obvious edge cases.

 #name
 if (length(name) == 0){
   stop("Model specification must have a name.")
 }
 if (length(name) != 1){
   stop("Model specification must only have one name.")
 }
 #could the model list be stored as an environment? or is that too overcomplicated?

 if (name %in% model_list){
   stop("Model must have unique name. To remove all user-defined models, reload 'bayesTPC'.")
 }
 #parameters
 if (length(parameters) == 0){
   stop("Model specification must have parameters.")
 }
 par_names <- names(parameters)
 if (par_names |> is.null()){
   stop("'parameters' vector must be named.")
 }
 if (vapply(par_names,function(x) {x == ""}, TRUE) |> any()){
   stop("All model parameters must be named.")
 }
 if (length(par_names) != length(unique(par_names))){
   stop("Model parameters must have unique names.")
 }

 # how do u check if the priors are correctly written????
 #constants
 if (length(constants) > 0){
   const_names <- names(constants)

   if (const_names |> is.null()){
     stop("'constants' vector must be named.")
   }
   if (vapply(const_names,function(x) {x == ""}, TRUE) |> any()){
     stop("All model constants must be named.")
   }
   if (length(const_names) != length(unique(const_names))){
     stop("Model constants must have unique names.")
   }
 }

 #formula
 if(length(formula) == 0){
   stop("Model specification must have a formula.")
 }
 if(length(formula) > 1){
   stop("Model specification can only have one formula.")
 }
 formula_vars <- formula  |> all.vars() #extract all variables from formula
 if (!"Temp" %in% formula_vars){
   stop("Model specification formula must contain variable 'Temp'.")
 }
 if(!all(par_names %in% formula_vars)){
   #this could be a warning but I want to be strict here.
   stop("One or more parameters are not included in the model formula.")
 }
 if (!all(formula_vars %in% c("Temp", par_names, names(constants)))){
   stop("One or more variables in the model formula is not named as a parameter or a constant.")
 }

 return(x)
}

validate <- function(x){
  UseMethod("validate")
}
#internal
#' @export
validate.btpc_normal_model <- function(x){
  x <- validate.btpc_model(x)
  var <- attr(x, "sigma.sq")
  if (length(var) == 0){
    cat("Using default prior for model variance.\n")
    attr(x, "sigma.sq") <- "T(dt(mu = 0, tau = 1/10, df = 1), 0, )"
  }

  return(x)
}

#' @export
validate.default <- function(x){
  stop("Misconfigured Model Specification.")
}


## Changing Models ======================================================

#' Change priors of pre-specified model
#'
#' Intended to be used to change the priors of an already existing model.
#'
#' @export
#' @param model Object of type `btpc_model`. The specification to be changed.
#' @param priors named character. The names should correspond to the parameters to change, and the values should be the new desired priors.
#' @returns Returns the modified model. Does not change the default values of any registered model type.
change_priors <- function(model, priors){
  if (!("btpc_model" %in% class(model))){
    stop("Invalid type for model.")
  }
  if (!is.character(priors)){
    stop("Invalid type for new priors.")
  }
  params_to_change <- names(priors)
  current_priors <- attr(model, "parameters")
  if (!all(params_to_change %in% names(current_priors))){
    stop("Attempting to change prior of non-existent parameter.")
  }
  current_priors[params_to_change] <- unlist(priors)
  attr(model, "parameters") <- current_priors
  return(model)
}

#' Change constants of pre-specified model
#'
#' Intended to be used to change the constants of an already existing model.
#'
#' @export
#' @param model Object of type `btpc_model`. The specification to be changed.
#' @param constants named character. The names should correspond to the constants to change, and the values should be the new desired constants.
#' @returns Returns the modified model. Does not change the default values of any registered model type.
change_constants <- function(model, constants){
  if (!("btpc_model" %in% class(model))){
    stop("Invalid type for model.")
  }
  if (!is.double(constants)){
    stop("Invalid type for new constants.")
  }

  constants_to_change <- names(constants)
  current_constants <- attr(model, "constants")

  if (length(current_constants) == 0){
    stop("Attempting to change constants for a model without constants.")
  }
  if (!all(constants_to_change %in% names(current_constants))){
    stop("Attempting to change non-existent constant.")
  }
  current_constants[constants_to_change] <- unlist(constants)
  attr(model, "constants") <- current_constants
  return(model)
}


#' @export
print.btpc_model <- function(x, ...){
  cat(paste0("bayesTPC Model Specification of Type: ",c(x)))
  cat(paste0("\nModel Formula:\n  ",attr(x, "formula")))
  cat(paste0("\nModel Parameters and Priors:"))
  params <- attr(x, "parameters")
  cat(paste0("\n  ",names(params),": ", params))
  consts <- attr(x, "constants")
  if (length(consts) > 0){
    cat(paste0("\nModel Constants:"))
    cat(paste0("\n  ",names(consts),": ", consts))
  }
}

#' @export
print.btpc_normal_model <- function(x, ...){
  print.btpc_model(x)
  cat(paste0("\nPrior for Variance:\n  ", attr(x, "sigma.sq")))
}
