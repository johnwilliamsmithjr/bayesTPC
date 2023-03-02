model_evaluation_function <- function(model){
  model_info <-
    model_master_list[model_master_list$name == model,]
  model_function <- function(params,
                             Temp,
                             posteriorPredictive = F,
                             sigma.sq = 0){

    #assign parameters into individual variables
    params <- checkParams(model, params, F)
    for (i in 1:length(params)){
      assign(names(params[i]), unlist(as.vector(params[i])))
    }

    #get formula from dataframe and parse
    evaluated_model <-
      eval(str2expression(model_info$formula[[1]]))

    #determine output
    if (posteriorPredictive == F){
      return(evaluated_model)
    }
    else{
      #this means the binomial functions dont work
      curve <- rtruncnorm(length(Temp), a = 0, b = Inf,
                          mean = evaluated_model,
                          sd = sqrt(sigma.sq))
      return(curve)
    }
  }

  return(model_function)
}

#for INTERNAL USE ONLY
.model_eval <- function(model){
  model_info <-
    model_master_list[model_master_list$name == model,]
  model_function <- function(params,
                             Temp,
                             posteriorPredictive = F,
                             sigma.sq = 0){

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

    #determine output
    if (posteriorPredictive == F){
      return(evaluated_model)
    }
    else{
      #this means the binomial functions dont work
      curve <- rtruncnorm(length(Temp), a = 0, b = Inf,
                          mean = evaluated_model,
                          sd = sqrt(sigma.sq))
      return(curve)
    }
  }

  return(model_function)
}
