model_evaluation_function <- function(model){
  model_info <-
    model_master_list[model_master_list$name == model,]
  model_function <- function(params,
                             Temp,
                             constants = NULL){

    #assign parameters into individual variables
    params <- checkParams(model, params)
    for (i in 1:length(params)){
      assign(names(params[i]), unlist(as.vector(params[i])))
    }

    #assign constants
    if (!is.null(constants)){
      for (i in 1:length(sorted_consts)){
        assign(names(constants[i]), unlist(as.vector(constants[i])))
      }
    }

    #get formula from dataframe and parse
    evaluated_model <-
      eval(str2expression(model_info$formula[[1]]))

    return(evaluated_model)
  }

  return(model_function)
}

#for INTERNAL USE ONLY
.model_eval <- function(model){
  model_info <-
    model_master_list[model_master_list$name == model,]

  #assume params and constants are sorted lexicographically
  sorted_pars <- sort(unlist(model_info$params))
  sorted_consts <- sort(unlist(model_info$constants))

  #A concerted effort to optimize this function will have high reward
  # It's run thousands of times, and R calls are expensive.
  model_function <- function(params,
                             Temp,
                             constants = NULL){

    #assign parameters into individual variables
    for (i in 1:length(sorted_pars)){
      assign(sorted_pars[[i]], params[[i]])
    }

    if (length(sorted_consts) > 0){
      for (i in 1:(length(sorted_consts))){
        assign(sorted_consts[[i]], constants[[i]])
      }
    }


    #get formula from dataframe and parse
    evaluated_model <-
      eval(str2expression(model_info$formula[[1]]))

    return(evaluated_model)
  }

  return(model_function)
}

#functioning
.direct_nimble <- function(model){
  model_info <-
    model_master_list[model_master_list$name == model,]

  #assume params and constants are sorted lexicographically
  sorted_pars <- sort(unlist(model_info$params))
  sorted_consts <- sort(unlist(model_info$constants))
  formula_string <- model_info$formula[[1]]

  par_string <- vector("character", length(sorted_pars))
  for (i in 1:length(sorted_pars)){
    par_string[i] <- paste0(sorted_pars[i], " <- params[",i,"]\n")
  }
  par_exp <- str2expression(par_string)

  const_string <- vector("character", length(sorted_consts))
  if (length(sorted_consts) > 0){
    for (i in 1:length(sorted_consts)){
      const_string[i] <- paste0(sorted_consts[i], " <- constants[",i,"]\n")
    }
  }
  const_exp <- str2expression(const_string)

  function_string <- paste0('
nimble_mod_function <- nimbleFunction(
    run = function(params = double(1),
                   Temp = double(0),
                   constants = double(1)){
  ', paste0(par_string, collapse = ""),
paste0(const_string, collapse = ""),
"return(", formula_string,")\n returnType(double(0))\n}\n)",collapse = "")

  return(str2expression(function_string))
}


