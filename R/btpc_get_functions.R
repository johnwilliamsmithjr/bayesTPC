get_formula <- function(model){
  if (!(model %in% model_master_list[model_master_list$name == model,][[1]])){
    stop("Unsupported model, use get_models() to view implemented models.")
  }
  return(model_master_list[model_master_list$name == model,]$formula[[1]])
}

get_model_params <- function(model){
  if (!(model %in% model_master_list[model_master_list$name == model,][[1]])){
    stop("Unsupported model, use get_models() to view implemented models.")
  }
  return(model_master_list[model_master_list$name == model,]$params[[1]])
}

get_default_priors <- function(model){
  if (!(model %in% model_master_list[model_master_list$name == model,][[1]])){
    stop("Unsupported model, use get_models() to view implemented models.")
  }
  dp <- model_master_list[model_master_list$name == model,]$default_priors[[1]]
  names(dp) <- model_master_list[model_master_list$name == model,]$params[[1]]
  return(dp)
}

get_model_constants <- function(model){
  if (!(model %in% model_master_list[model_master_list$name == model,][[1]])){
    stop("Unsupported model, use get_models() to view implemented models.")
  }
  return(model_master_list[model_master_list$name == model,]$constants[[1]])
}

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
get_models <- function(){
  return(model_master_list[,c("name", "formula")])
}

get_model_names <- function(){
  return(unlist(model_master_list[,c("name")]))
}
