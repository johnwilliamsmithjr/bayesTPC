get_formula <- function(model){
  if (!(model %in% model_master_list[model_master_list$name == model,][[1]])){
    stop("Unsupported model, use get_models() to view implemented models.")
  }
  return(model_master_list[model_master_list$name == model,]$formula[[1]])
}

get_model_params <- function(model){
  return(model_master_list[model_master_list$name == model,]$params[[1]])
}

get_default_priors <- function(model){
  dp <- model_master_list[model_master_list$name == model,]$default_priors[[1]]
  names(dp) <- model_master_list[model_master_list$name == model,]$params[[1]]
  return(dp)
}

get_model_constants <- function(model){
  return(model_master_list[model_master_list$name == model,]$constants[[1]])
}

get_models <- function(){
  return(model_master_list[,c("name", "formula")])
}
