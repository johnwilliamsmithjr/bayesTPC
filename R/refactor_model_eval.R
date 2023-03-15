model_evaluation_function <- function(model){
  model_info <-
    model_master_list[model_master_list$name == model,]
  model_function <- function(params,
                             Temp){

    #assign parameters into individual variables
    params <- checkParams(model, params, F)
    for (i in 1:length(params)){
      assign(names(params[i]), unlist(as.vector(params[i])))
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
  model_function <- function(params,
                             Temp){

    #assign parameters into individual variables
    #assume params is sorted lexicographically
    sorted_vars <- sort(unlist(model_info$params))

    #assign parameters into individual variables
    for (i in 1:length(sorted_vars)){
      assign(sorted_vars[[i]], unlist(as.vector(params[i])))
    }

    #get formula from dataframe and parse
    evaluated_model <-
      eval(str2expression(model_info$formula[[1]]))

    return(evaluated_model)
  }

  return(model_function)
}
