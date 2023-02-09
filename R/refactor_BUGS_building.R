loop_string <- function(model){
  model_info <-
    model_master_list[model_master_list$name == model,]
  model_string = paste0('{\n    for (i in 1:N){\n    ',
                        '        Trait[i] ~ nimble_model_function(params[1:',
                        length(model_info[[2]][[1]]),
                        "], Temp[i], T, sigma.sq)\n    }\n")
  return(model_string)
}

priors_string <- function(model, priors = NULL, verbose = TRUE){
  model_info <-
    model_master_list[model_master_list$name == model,]
  num_params <- length(model_info[[2]][[1]])

  if (!is.null(priors)){
    if (!is.list(priors)) stop("Unexpected type for argument 'priors'. Priors must be given as a list.")
    if (is.null(names(priors))){
      stop('Prior list cannot be empty. To use default priors use priors = NULL.')
    }
  }
  ## check for proper names in prior list
  param_names <- model_info[,2]
  if (any(!(names(priors) %in% param_names))){
    stop('One or more priors do not have names that correspond to model parameters.')
  }
  param_string <- vector("character", num_params+1)
  for (i in 1:num_params){
    if (param_names[i] %in% names(priors)){
      if (verbose){
        simpleMessage(paste0('Not using default prior for parameter ',
                   param_names[i],
                   '. To suppress these messagess use verbose = FALSE\n'))
      }
      param_string[i] <- paste0(
        "params[",i,"] ~ ",priors[[param_names[i]]])
    }
    else{
      param_string[i] <- paste0(
        "params[",i,"] ~ ",model_info[[4]][[1]][i])
    }
  }
  param_string[num_params + 1] <-
    'sigma.sq ~ T(dt(mu = 0, tau = 10, df = 1), 0, )'

  return(param_string)
}

configure_model <- function(model, priors = NULL, verbose = TRUE){
  model_info <-
    model_master_list[model_master_list$name == model,]
  num_params <- length(model_info[[2]][[1]])

  if (!is.null(priors)){
    if (!is.list(priors)) stop("Unexpected type for argument 'priors'. Priors must be given as a list.")
    if (is.null(names(priors))){
      stop('Prior list cannot be empty. To use default priors use priors = NULL.')
    }
  }
  ## check for proper names in prior list
  param_names <- model_info[,2]
  if (any(!(names(priors) %in% param_names))){
    stop('One or more priors do not have names that correspond to model parameters.')
  }

  loop <- loop_string(model)
  pri <- priors_string(model,priors,verbose)

  pri_string <- paste0(pri, collapse = "\n    ")

  nimble_string = paste0(loop,"    ",pri_string, '\n}')
  return(nimble_string)
}

b_TPC <- function(data, model, priors = NULL, samplerType = 'RW',
                  niter = 10000, inits = NULL, burn = 0, constant_list = NULL, ...){
  if (!(samplerType %in% c('RW', 'RW_block', 'AF_slice', 'slice'))) stop('Unsupported option for input samplerType. Currently only RW, RW_block, slice, and AF_slice are supported.')
  if (model %in% c('binomial_glm_lin', 'binomial_glm_quad')){
    if (is.null(unlist(data['n']))) stop("For a Binomial GLM, data list must have a variable called 'n'. Perhaps check spelling and capitalization?")
    #const.list$n = unlist(data['n'])
  }

  if (is.null(constant_list)){
    if (model == 'pawar-shsch'){
      warning("Constant list not found for model = 'pawar-shsch'. Setting default value for parameter T.ref to 20.\n")
      constant_list = list(T.ref = 20)
    }
  }
  else{
    if (!is.list(constant_list)){
      stop('If constants are provided, they must be formatted as a list\n')
    }
    if (model == 'pawar-shsch'){
      if (!('T.ref' %in% names(constant_list))){
        warning("Constant list not found for model = 'pawar-shsch'. Setting default value for parameter T.ref to 20.\n")
        constant_list$T.ref = 20
      }
    }
    for (i in names(constant_list)){
      const.list[i] = constant_list[i]
    }
  }

  model_info <-
    model_master_list[model_master_list$name == model,]
  model_params <- model_info[[2]][[1]]
  data.nimble = checkData(data)
  modelStr = configure_model(model = model, priors = priors, ...)

  const.list = vector('list', 0)
  const.list$N = data.nimble$N

  model_function <- .model_eval(model)
  nimble_model_function <-
    nimbleRcall(prototype = function(params = double(1),
                                     Temp = double(0),
                                     posteriorPredictive = logical(0),
                                     sigma.sq = double(0)){},
                Rfun = 'model_function',
                returnType = double(0))

  nimTPCmod = nimbleModel(str2expression(modelStr), constants = const.list,
                          data = data.nimble$data, inits = inits)
  #nimTPCmod = initializeModel(nimTPCmod)
  nimTPCmod_compiled = compileNimble(nimTPCmod)

  mcmcConfig <- configureMCMC(nimTPCmod)

  if (samplerType == 'slice'){
    for (i in model_params){
      mcmcConfig$removeSamplers(i)
      mcmcConfig$addSampler(i, type = samplerType)
    }
  }
  else if (samplerType != 'RW'){
    mcmcConfig$removeSamplers(model_params)
    mcmcConfig$addSampler(model_params, type = samplerType)
  }

  mcmcConfig$enableWAIC = TRUE
  mcmc <- buildMCMC(mcmcConfig)
  tpc_mcmc <- compileNimble(mcmc, project = nimTPCmod)
  tpc_mcmc$run(niter, ...)
  samples = as.mcmc(as.matrix(tpc_mcmc$mvSamples))

  default_priors <- model_info[[4]][[1]]
  prior_list = list()
  for (i in 1:length(model_params)){
    if (model_params[i] %in% names(priors)){
      prior_list[model_params[i]] = priors[model_params[i]]
    } else{
      prior_list[model_params[i]] = default_priors[model_params[i]]
    }
  }
  #tpc_mcmc = nimbleMCMC(model = nimTPCmod_compiled, niter = 10000)
  return(list(samples = samples, model = tpc_mcmc, data = data.nimble, modelType = model, priors = prior_list))
}
