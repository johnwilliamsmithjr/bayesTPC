#this is my first time using S3, forgive me if its a little unoptimized :).
#highest level class, lacks density function and constant support
new_btpc_model <- function(name = character(),
                           parameters = character(), #names are parameters, values are priors
                           formula = character(),
                           constants = double(), #names are constant names, values are default values
                           class = character(), #for subclassing support
                           ...){

  #forcing explicit types
  stopifnot(is.character(name))
  stopifnot(is.character(parameters))
  stopifnot(is.character(formula)) #might store the formula as an expression
  stopifnot(is.double(constants))
  stopifnot(is.character(class))

  structure(name,
            class = c(class,"btpc_model"),
            parameters = parameters,
            formula = formula,
            constants = constants,
            ...)
}

validate_btpc_model <- function(x){
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
 #TODO add check if model already exists in master_list, once master_list is created
 #could the master_list be stored as an environment? or is that too overcomplicated?

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
 formula_vars <- formula |> str2lang() |> all.vars() #extract all variables from formula
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
}


new_btpc_normal_model <- function(name = character(),
                                  parameters = character(), #names are parameters, values are priors
                                  formula = character(),
                                  constants = double()){  #names are constant names, values are default values

  new_btpc_model(name = name,
                 parameters = parameters,
                 formula = formula,
                 constants = constants,
                 class = "btpc_normal_model",
                 ...)
}

new_btpc_binomial_model <- function(name = character(),
                                    parameters = character(), #names are parameters, values are priors
                                    formula = character(),
                                    constants = double()){ #names are constant names, values are default values

  new_btpc_model(name = name,
                 parameters = parameters,
                 formula = formula,
                 constants = constants,
                 class = "btpc_binomial_model",
                 ...)
}
