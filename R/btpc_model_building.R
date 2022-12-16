#' Extract function from model string
#'
#' Extract thermal performance curve function from model string used to specify type of model to fit
#'
#' @details This function returns a function that can be used to evaluate a thermal performance for a model. Designed to be called internally by `posteriorPredTPC()` and `bayesTPC_summary()`. Not recommended to be called directly, though it is possible (see examples)
#' @param model character, name of thermal performance curve model. Currently, supported options include "quadratic", "briere", "gaussian", "weibull", "pawar-shsch", "lactin2", "kamykowski", "ratkowsky", "binomial_glm_lin", "binomial_glm_quad",  and "stinner".
#' @return function handle, corresponding to function used to evaluate the thermal performance curve for a given `model` input
#' @examples
#' ## set params and reference temperature set
#' myfun = str2tpc_fun(model = 'gaussian')
#' param_set = c(T.opt = 36, a = 6.5, rmax = 2.75)
#' Temp_ref = seq(from = 5, to = 50, length.out = 1000)
#' plot(Temp_ref, myfun(params = param_set, Temp = Temp_ref), type = 'l')

str2tpc_fun = function(model){
  ## check if model type is supported
  if (!(model %in% c('quadratic', 'briere', 'weibull', 'gaussian', 'pawar-shsch', 'lactin2', 'kamykowski', 'ratkowsky', 'stinner', "binomial_glm_lin", "binomial_glm_quad"))) stop('Model choice not currently supported')
  ## if it is, assign the appropriate function
  if (model == 'lactin2') stop('lactin2 is currently under construction')
  if (model == 'quadratic') fun = quadratic_tpc
  if (model == 'briere') fun = briere_tpc
  if (model == 'weibull') fun = weibull_tpc
  if (model == 'gaussian') fun = gauss_tpc
  if (model == 'pawar-shsch') fun = pawar_shsch_tpc
  if (model == 'kamykowski') fun = kamykowski_tpc
  if (model == 'ratkowsky') fun = ratkowsky_tpc
  if (model == 'stinner') fun = stinner_tpc
  return(fun)
}


#' Create model string
#'
#' Create model string for thermal performance curve model to be passed to nimble
#'
#' @details This function returns a character string of the full `nimble` model for a user-specified thermal performance curve and prior distributions
#' @param model character, name of thermal performance curve model. Currently, supported options include "quadratic", "briere", "gaussian", "weibull", "pawar-shsch", "lactin2", "kamykowski","ratkowsky", "stinner", "binomial_glm_lin", "binomial_glm_quad".
#' @param priors list, optional input specifying prior distributions for parameters (default = NULL). Elements of the list should correspond to model parameters, and written using nimble logic. For parameters not specified in the list, default priors are used.
#' @param verbose logical, optional input. If verbose = TRUE, messages are printed when for priors that deviate from the defaultPriors(model) (see ?defaultPriors for additional information). Default = TRUE
#' @return character, character string specifying the default model formulation to be passed to `nimble`.
#' @examples
#' ## Print default model for briere
#' cat(defaultModel(model = 'briere'))
#'
#' ## Use custom prior for 'q' parameter in quadratic curve
#' my_prior = list(q = 'q~beta(.5, .5)')
#' cat(defaultModel(model = 'quadratic', priors = my_prior))


configureModel <- function(model, priors = NULL, verbose = TRUE){
  ## checks if model type is supported
  if (!(model %in% c('binomial_glm_lin', 'binomial_glm_quad', 'quadratic', 'briere', 'weibull', 'gaussian', 'pawar-shsch', 'lactin2', 'kamykowski', 'ratkowsky', 'stinner', "binomial_glm_lin", "binomial_glm_quad"))) stop('Model choice not currently supported')
  ## makes sure prior information has correct types and classes
  if (!is.null(priors)){
    if (!is.list(priors)) stop("Unexpected type for argument 'priors'. Priors must be given as a list.")
    if (is.null(names(priors))){
      stop('Prior list cannot be empty. To use default priors use priors = NULL.')
    }
  }
  ## check for proper names in prior list
  dp <- defaultPriors(model)
  if (any(!(names(priors) %in% names(dp)))){
    stop('One or more priors do not have names that correspond to model parameters.')
  }
  prior_string = '    '
  for (i in names(dp)){
    if (i %in% names(priors)){
      if (verbose) cat(paste0('Not using default prior for parameter ', i, '. To supress these messagess use verbose = FALSE\n'))
      prior_string = paste0(prior_string, priors[i], sep = '\n    ')
    } else{
      prior_string = paste0(prior_string, dp[i], sep = '\n    ')
    }
  }
  #} #else{
  #prior_string = paste(sapply(defaultPriors(model), paste, collapse=":"), collapse="\n")
  #  }
  nimble_string = paste0(defaultModel(model), prior_string, '\n}')
  return(nimble_string)
}

#' Perform MCMC
#'
#' Generate `nimble` model, perform sampling using MCMC
#'
#' @details This function returns a list, containing entries: `samples` - object of class `mcmc.list` containing posterior samples of parameters for corresponding model; `model` - object of class `nimbleModel` containing `nimble` model object corresponding to model being fit; `data` - object of class `list` containing trait and temperature data and number of observations (N); `modelType` - object of class `character` containing the type of thermal performance curve being fit.
#' @param data list, with expected entries "Trait" (corresponding to the trait being modeled by the thermal performance curve) and "Temp" (corresponding to the temperature in Celcius that the trait is being measured at).
#' @param model character, name of thermal performance curve model. Currently, supported options include "quadratic", "briere", "gaussian", "weibull", "pawar-shsch", "lactin2", "kamykowski", "ratkowsky", "binomial_glm_lin", "binomial_glm_quad", and "stinner".
#' @param priors list, optional input specifying prior distributions for parameters (default = NULL). Elements of the list should correspond to model parameters, and written using nimble logic. For parameters not specified in the list, default priors are used.
#' @param samplerType character string, specifying the sampling method used during the MCMC. Currently, supported options are Random Walk Metropolis (samplerType = 'RW'), Blocked Random Walk Metropolis (samplerType = 'RW_block'), Automated Factor Slice Sampling (samplerType = 'AF_slice'), and Slice sampling (samplerType = 'slice')
#' @param niter integer, number of MCMC iterations to perform (default is niter = 10000)
#' @param inits optional list, initial parameter values to be provided to nimble MCMC
#' @param burn optional integer, number of initial MCMC iterations to be discarded as burn-in. Default is burn = 0
#' @param constant_list optional list, constants to be provided to model. Currently only used for model = 'pawar-shsch'
#' @param ... Additional parameters to be passed to nimble during MCMC configuration and sampling
#' @return list, with entries "data" (object of class "list", data to be passed to `nimble` model) and "N" (integer number of data points, to be passed `nimble` model as a constant)
#' @examples
#' ## generate data
#' test_data = list(Temp = rep(c(10, 20, 30, 40), 5), Trait = rgamma(20, 5, rep(c(10, 20, 30, 40), 5)))
#' checkData(test_data)

bTPC <- function(data, model, priors = NULL, samplerType = 'RW',
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

  data.nimble = checkData(data)
  modelStr = configureModel(model = model, priors = priors, ...)

  const.list = vector('list', 0)
  const.list$N = data.nimble$N


  nimTPCmod = nimbleModel(str2expression(modelStr), constants = const.list,
                          data = data.nimble$data, inits = inits)
  #nimTPCmod = initializeModel(nimTPCmod)
  nimTPCmod_compiled = compileNimble(nimTPCmod)

  mcmcConfig <- configureMCMC(nimTPCmod)

  if (samplerType == 'slice'){ #why is this written like this?
    for (i in names(defaultPriors(model))){
      mcmcConfig$removeSamplers(i)
      mcmcConfig$addSampler(i, type = samplerType)
    }
  }
  else if (samplerType != 'RW'){
    mcmcConfig$removeSamplers(names(defaultPriors(model)))
    mcmcConfig$addSampler(names(defaultPriors(model)), type = samplerType)
  }

  mcmcConfig$enableWAIC = TRUE
  mcmc <- buildMCMC(mcmcConfig)
  tpc_mcmc <- compileNimble(mcmc, project = nimTPCmod)
  tpc_mcmc$run(niter, ...)
  samples = as.mcmc(as.matrix(tpc_mcmc$mvSamples))

  dp <- defaultPriors(model)
  prior_list = list()
  for (i in 1:length(names(dp))){
    if (names(dp)[i] %in% names(priors)){
      prior_list[names(dp)[i]] = priors[names(dp)[i]]
    } else{
      prior_list[names(dp)[i]] = dp[names(dp)[i]]
    }
  }
  #tpc_mcmc = nimbleMCMC(model = nimTPCmod_compiled, niter = 10000)
  return(list(samples = samples, model = tpc_mcmc, data = data.nimble, modelType = model, priors = prior_list))
}
