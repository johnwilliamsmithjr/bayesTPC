model_evaluation_function <- function(model){
  if (!(model %in% model_master_list[model_master_list$name == model,][[1]])){
    stop("Unsupported model, use get_models() to view implemented models.")
  }
  model_info <-
    model_master_list[model_master_list$name == model,]

  #assume params and constants are sorted lexicographically
  sorted_pars <- sort(unlist(model_info$params))
  sorted_consts <- sort(unlist(model_info$constants))
  formula_string <- model_info$formula[[1]]

  par_string <- vector("character", length(sorted_pars))
  for (i in 1:length(sorted_pars)){
    par_string[i] <- paste0(sorted_pars[i], " <- params['",sorted_pars[i],"']\n")
  }

  const_string <- vector("character", length(sorted_consts))
  if (length(sorted_consts) > 0){
    for (i in 1:length(sorted_consts)){
      const_string[i] <- paste0(sorted_consts[i], " <- constants['",sorted_consts[i],"']\n")
    }
  }

  function_string <- paste0('function(params,Temp,constants){
  ', paste0(par_string, collapse = ""),
paste0(const_string, collapse = ""),
"return(", formula_string,")}",collapse = "")

  return(eval(str2expression(function_string)))
}

#functioning
.direct_nimble <- function(model){
  if (!(model %in% model_master_list[model_master_list$name == model,][[1]])){
    stop("Unsupported model, use get_models() to view implemented models.")
  }
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

  const_string <- vector("character", length(sorted_consts))
  if (length(sorted_consts) > 0){
    for (i in 1:length(sorted_consts)){
      const_string[i] <- paste0(sorted_consts[i], " <- constants[",i,"]\n")
    }
  }

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


