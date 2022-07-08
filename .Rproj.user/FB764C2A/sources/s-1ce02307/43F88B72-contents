#' Retrieve default prior choices
#'
#' Retrieve default prior choices for a given thermal performance curve model
#'
#' @details This function returns the default prior choices for the parameters of the thermal performance curve model that is passed as an input argument
#' @param model character, name of thermal performance curve model. Currently, supported options include "quadratic", "briere", "gaussian", "weibull", "pawar-shsch", and "lactin2".
#'
#' @return list, named list detailing the default prior distributions for each parameter of the model
#' @examples
#' defaultPriors(model = 'quadratic')
#'
defaultPriors <- function(model){
  ## quadratic model default priors for q, T.min, T.max, sigma.sq
  if (model == 'quadratic' || model == 'briere'){
    p <- list(q = 'q ~ dunif(0, 1)',
              T.min = 'T.min ~ dunif(0, 24)',
              T.max = 'T.max ~ dunif(25, 60)',
              sigma.sq = 'sigma.sq ~ T(dt(mu = 0, tau = 10, df = 1), 0, )')
  } else if (model == 'weibull'){
    p <- list(a = 'a ~ dunif(0, 10)',
              T.opt = 'T.opt ~ dunif(0, 60)',
              b = 'b ~ dunif(0, 1e10)',
              c = 'c ~ dunif(0, 1e10)',
              sigma.sq = 'sigma.sq ~ T(dt(mu = 0, tau = 10, df = 1), 0, )')
  } else if (model == 'gaussian'){
    p <- list(a = 'a ~ T(dt(mu = 0, tau = 10, df = 1), 0, )',
              T.opt = 'T.opt ~ dunif(0, 60)',
              rmax = 'rmax ~ dunif(0, 1000)',
              sigma.sq = 'sigma.sq ~ T(dt(mu = 0, tau = 10, df = 1), 0, )')
  } else if (model == 'pawar-shsch'){
    p <- list(r_tref = 'r_tref ~ dunif(0, 10)',
              e = 'e ~ dunif(0, 1)',
              T.opt = 'T.opt ~ dunif(0, 50)',
              e_h = 'e_h ~ dunif(0, 30)',
              sigma.sq = 'sigma.sq ~ T(dt(mu = 0, tau = 10, df = 1), 0, )')
  } else if (model == 'lactin2'){
    p <- list(a = 'a ~ dunif(0, 100)',
              b = 'b ~ dunif(-10, 10)',
              T.max = 'T.max ~ dunif(0, 70)',
              delta_t = 'delta_t ~ T(dt(mu = 0, tau = 100, df = 1), 0, )',
              sigma.sq = 'sigma.sq ~ T(dt(mu = 0, tau = 10, df = 1), 0, )')
  } else{
    stop('Model type not currently supported. Options include "quadratic", "briere", "gaussian", "weibull", "pawar-shsch", and "lactin2".')
  }
  return(p)
}

#' Retrieve default model
#'
#' Retrieve default model formulation for a given thermal performance curve model
#'
#' @details This function returns a character string of the `nimble` likelihood model for a user-specified thermal performance curve
#' @param model character, name of thermal performance curve model. Currently, supported options include "quadratic", "briere", "gaussian", "weibull", "pawar-shsch", and "lactin2".
#'
#' @return character, character string specifying the default likelihood model formulation to be passed to `nimble`
#' @examples
#' cat(defaultModel(model = 'briere'))
#'

defaultModel <- function(model){
  ## if model type is quadratic, creates a string of model text for the quadratic model
  ## that will be readable when evaluated as an expression later
  if (model == 'quadratic'){
    model_string = paste0('{\n    for (i in 1:N){\n    ', '        Trait[i] ~ T(dnorm(mean = -q*(Temp[i] - T.min)*(Temp[i] - T.max) * step(T.max - Temp[i]) * step(Temp[i] - T.min), var = sigma.sq), 0, )\n    }\n')
  } else if (model == 'briere'){
    model_string = paste0('{\n    for (i in 1:N){\n    ', '        Trait[i] ~ T(dnorm(mean = q*(Temp[i] - T.min)*(pow(abs(T.max-Temp[i]), .5)) * step(T.max - Temp[i]) * step(Temp[i] - T.min), var = sigma.sq), 0, )\n    }\n')
  } else if (model == 'weibull'){
    #model_string = paste0('{\n    for (i in 1:N){\n    ', '        Trait[i] ~ T(dnorm(mean = a*(((c-1) / (c))^((1-c)/c))*(((Temp[i] - T.opt)/b + ((1 - (1/c))^(1/c)))^ (c-1))*(exp(-((((Temp[i] - T.opt)/b + ((1 - (1/c))^(1/c)))^ c))) + 1 - (1/c)), sd = sigma.sq), 0, )\n    }\n')
    model_string = paste0('{\n    for (i in 1:N){\n    ', '        Trait[i] ~ T(dnorm(mean = ((a*(((c-1)/c)^((1-c)/c))*((((Temp[i]-T.opt)/b)+(((c-1)/c)^(1/c)))^(c-1))*(exp(-((((Temp[i]-T.opt)/b)+(((c-1)/c)^(1/c)))^c)+((c-1)/c))))), var = sigma.sq), 0, )\n    }\n')
  } else if (model == 'gaussian'){
    #model_string = paste0('{\n    for (i in 1:N){\n    ', '        Trait[i] ~ T(dnorm(mean = a*(((c-1) / (c))^((1-c)/c))*(((Temp[i] - T.opt)/b + ((1 - (1/c))^(1/c)))^ (c-1))*(exp(-((((Temp[i] - T.opt)/b + ((1 - (1/c))^(1/c)))^ c))) + 1 - (1/c)), sd = sigma.sq), 0, )\n    }\n')
    model_string = paste0('{\n    for (i in 1:N){\n    ', '        Trait[i] ~ T(dnorm(mean = rmax*exp(-0.5*(abs(Temp[i] - T.opt)/a)^2), var = sigma.sq), 0, )\n    }\n')
  } else if (model == 'pawar-shsch'){
    model_string = paste0('{\n    for (i in 1:N){\n    ', '        Trait[i] ~ T(dnorm(mean = step(e_h - e)*r_tref*exp((e/(8.62e-05))*((1/(T.ref+273.15)) - (1/(Temp[i] + 273.15)))) / (1 + (e / (e_h-e)) * exp((e_h/(8.62e-05))*(1/(T.opt + 273.15) - 1/(Temp[i] + 273.15)))), var = sigma.sq), 0, )\n    }\n')
  } else if (model == 'lactin2'){
    model_string = paste0('{\n    for (i in 1:N){\n    ', '        Trait[i] ~ T(dnorm(mean = step(T.max - Temp[i])*step(exp(a*Temp[i]) - exp(a*T.max - ((T.max - Temp[i]) / delta_t)) + b)*(exp(a*Temp[i]) - exp(a*T.max - ((T.max - Temp[i]) / delta_t)) + b), var = sigma.sq), 0, )\n    }\n')
  } else{
    stop("Argument for 'model' not currently supported. Current options include: quadratic, briere, weibull, gaussian, pawar-shsch")
  }
  return(model_string)
}

#' Create model string
#'
#' Create model string for thermal performance curve model to be passed to nimble
#'
#' @details This function returns a character string of the full `nimble` model for a user-specified thermal performance curve and prior distributions
#' @param model character, name of thermal performance curve model. Currently, supported options include "quadratic", "briere", "gaussian", "weibull", "pawar-shsch", and "lactin2".
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
  if (!(model %in% c('quadratic', 'briere', 'weibull', 'gaussian', 'pawar-shsch', 'lactin2'))) stop('Model choice not currently supported')
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

#' Check TPC data format
#'
#' Check if data is correctly formatted
#'
#' @details This function returns a list of data and constants to be passed to a `nimble` model to perform MCMC
#' @param data list, with expected entries "Trait" (corresponding to the trait being modeled by the thermal performance curve) and "Temp" (corresponding to the temperature in Celcius that the trait is being measured at).
#' @return list, with entries "data" (object of class "list", data to be passed to `nimble` model) and "N" (integer number of data points, to be passed `nimble` model as a constant)
#' @examples
#' ## generate data
#' test_data = list(Temp = rep(c(10, 20, 30, 40), 5), Trait = rgamma(20, 5, rep(c(10, 20, 30, 40), 5)))
#' checkData(test_data)

checkData <- function(data){
  ## data checks to make sure there are values for Temp and Trait
  if (!is.list(data)) stop("Unexpected class for argument 'data'. Data must be input as a list.")
  if (is.null(unlist(data['Temp']))) stop("Data list must have a variable called 'Temp'. Perhaps check spelling and capitalization?")
  if (is.null(unlist(data['Trait']))) stop("Data list must have a variable called 'Trait'. Perhaps check spelling and capitalization?")
  if (!is.vector(unlist(data['Trait']))) stop('List elements of data must be numeric vectors')
  if (!is.vector(unlist(data['Temp']))) stop('List elements of data must be numeric vectors')
  if (length(data['Trait']) != length(data['Temp'])) stop("'Temp' and 'Trait' must have the same length.")
  ## warnings for when temperature may be in F instead of C
  if (any(unlist(data['Temp']) > 50)) warning('Unusual (Temp>50) temperature values detected (are Temps given in Celcius?)')
  if (any(unlist(data['Temp']) < 0)) warning('Unusual (Temp<0) temperature values detected (are Temps given in Celcius?)')
  if (any(is.na(data['Temp']))) warning('Temperature data contains missing values. This may lead to unexpected results')
  if (any(is.na(data['Trait']))) warning('Trait data contains missing values. This may lead to unexpected results')
  return(list(data = data, N = length(unlist(data['Trait']))))
}

#' Perform MCMC
#'
#' Generate `nimble` model, perform sampling using MCMC
#'
#' @details This function returns a list, containing entries: `samples` - object of class `mcmc.list` containing posterior samples of parameters for corresponding model; `model` - object of class `nimbleModel` containing `nimble` model object corresponding to model being fit; `data` - object of class `list` containing trait and temperature data and number of observations (N); `modelType` - object of class `character` containing the type of thermal performance curve being fit.
#' @param data list, with expected entries "Trait" (corresponding to the trait being modeled by the thermal performance curve) and "Temp" (corresponding to the temperature in Celcius that the trait is being measured at).
#' @param model character, name of thermal performance curve model. Currently, supported options include "quadratic", "briere", "gaussian", "weibull", "pawar-shsch", and "lactin2".
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
  data.nimble = checkData(data)
  modelStr = configureModel(model = model, priors = priors, ...)

  const.list = vector('list', 0)
  const.list$N = data.nimble$N

  if(!is.null(constant_list)){
    if (!is.list(constant_list)){
      stop('If constants are provided, they must be formatted as a list')
    }
    for (i in names(constant_list)){
      const.list[i] = constant_list[i]
    }
  }
  #print(const.list)
  #print(inits)
  #print('Starting nimble comp')
  nimTPCmod = nimbleModel(str2expression(modelStr), constants = const.list,
                          data = data.nimble$data, inits = inits)
  #nimTPCmod = initializeModel(nimTPCmod)
  nimTPCmod_compiled = compileNimble(nimTPCmod)

  mcmcConfig <- configureMCMC(nimTPCmod)
  if (samplerType != 'RW'){
    mcmcConfig$removeSamplers(names(defaultPriors(model)))
    mcmcConfig$addSampler(names(defaultPriors(model)), type = samplerType)
  }
  mcmc <- buildMCMC(mcmcConfig)
  tpc_mcmc <- compileNimble(mcmc, project = nimTPCmod)
  tpc_mcmc$run(niter, ...)
  samples = as.mcmc(as.matrix(tpc_mcmc$mvSamples))


  #tpc_mcmc = nimbleMCMC(model = nimTPCmod_compiled, niter = 10000)
  return(list(samples = samples, model = nimTPCmod_compiled, data = data.nimble, modelType = model))
}
