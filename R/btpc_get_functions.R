#' Get Model Formula
#'
#' Returns the formula for a specified model.
#'
#' @details
#'  Since the model formulas are stored and returned as strings, one of `parse()`
#'  or its wrappers `str2lang()` and `str2expression()` can be used in conjunction with `eval()` for dynamic evaluation.
#'  However, it is better practice to use the output of `get_model_function()` for direct model evaluation.
#' @param model A string specifying the model name. Use `get_models()` to view all options.
#' @returns `get_formula` Returns the formula for the provided model as a string.
#' @examples
#' get_formula("stinner")
#'
#' # If used to evaluate, variables must be set beforehand.
#' q = .75
#' T_max = 35
#' T_min = 10
#' Temps = c(15, 20, 25, 30)
#' ans <- eval(str2lang(get_formula("quadratic")))
#'
get_formula <- function(model){
  if (!(model %in% model_master_list[model_master_list$name == model,][[1]])){
    stop("Unsupported model, use get_models() to view implemented models.")
  }
  return(model_master_list[model_master_list$name == model,]$formula[[1]])
}

#' Get Model Parameters
#'
#' TODO
#'
#' @details
#' @inheritParams get_formula
#' @returns
#' @examples
#'
#'
get_model_params <- function(model){
  if (!(model %in% model_master_list[model_master_list$name == model,][[1]])){
    stop("Unsupported model, use get_models() to view implemented models.")
  }
  return(model_master_list[model_master_list$name == model,]$params[[1]])
}

#' Get Default Prior Distributions
#'
#' TODO
#'
#' @details
#' @inheritParams get_formula
#' @returns
#' @examples
#'
#'
get_default_priors <- function(model){
  if (!(model %in% model_master_list[model_master_list$name == model,][[1]])){
    stop("Unsupported model, use get_models() to view implemented models.")
  }
  dp <- model_master_list[model_master_list$name == model,]$default_priors[[1]]
  names(dp) <- model_master_list[model_master_list$name == model,]$params[[1]]
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
#'
#'
get_model_constants <- function(model){
  if (!(model %in% model_master_list[model_master_list$name == model,][[1]])){
    stop("Unsupported model, use get_models() to view implemented models.")
  }
  return(model_master_list[model_master_list$name == model,]$constants[[1]])
}

#' Get Default Model Constants
#'
#' TODO
#'
#' @details
#' @inheritParams get_formula
#' @returns
#' @examples
#'
#'
get_default_constants <- function(model){
  if (!(model %in% model_master_list[model_master_list$name == model,][[1]])){
    stop("Unsupported model, use get_models() to view implemented models.")
  }
  if (is.null(model_master_list[model_master_list$name == model,]$constants[[1]])){
    warning("Specified model has no constants.")
    return(NULL)
  }
  else{
    dc <- model_master_list[model_master_list$name == model,]$default_constants[[1]]
    names(dc) <- model_master_list[model_master_list$name == model,]$constants[[1]]
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
#'
#'
get_models <- function(){
  return(model_master_list[,c("name", "formula")])
}

#' Get Model Names
#'
#' TODO
#'
#' @details
#' @param
#' @returns
#' @examples
#'
#'
get_model_names <- function(){
  return(unlist(model_master_list[,c("name")]))
}

#' Get Model as a Function
#'
#' TODO
#'
#' @details
#' @inheritParams get_formula
#' @returns
#' @examples
#'
#'
get_model_function <- function(model){

}
