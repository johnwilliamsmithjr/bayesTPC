#' Retrieve default prior choices
#'
#' Retrieve default prior choices for a given thermal performance curve model
#'
#' @details This function returns the default prior choices for the parameters of the thermal performance curve model that is passed as an input argument
#' @param model character, name of thermal performance curve model. Currently, supported options include "quadratic", "briere", "gaussian", "weibull", "pawar-shsch", "lactin2", "kamykowski", "ratkowsky", "binomial_glm_lin", "binomial_glm_quad",  and "stinner".
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
  } else if (model == 'kamykowski'){
    p <- list(a = 'a ~ dunif(0, 50)',
              b = 'b ~ dunif(0, 1)',
              T.min = 'T.min ~ dunif(0, 25)',
              T.max = 'T.max ~ dunif(25, 70)',
              c = 'c ~ dunif(0, 5)',
              sigma.sq = 'sigma.sq ~ T(dt(mu = 0, tau = 10, df = 1), 0, )')
  } else if (model == 'ratkowsky'){
    p <- list(a = 'a ~ dunif(0, 5)',
              b = 'b ~ dunif(0, 5)',
              T.min = 'T.min ~ dunif(0, 25)',
              T.max = 'T.max ~ dunif(25, 70)',
              sigma.sq = 'sigma.sq ~ T(dt(mu = 0, tau = 10, df = 1), 0, )')
  } else if (model == 'stinner'){
    p <- list(C = 'C ~ dunif(0, 1000)',
              k1 = 'k1 ~ dunif(-100, 100)',
              k2 = 'k2 ~ dunif(-10, 10)',
              T.opt = 'T.opt ~ dunif(15, 70)',
              sigma.sq = 'sigma.sq ~ T(dt(mu = 0, tau = 10, df = 1), 0, )')
  } else if (model == 'binomial_glm_lin'){
    p <- list(B0 = 'B0 ~ dnorm(0, var = 250000)',
              B1 = 'B1 ~ dnorm(0, var = 250000)')
  } else if (model == 'binomial_glm_quad'){
    p <- list(B0 = 'B0 ~ dnorm(0, var = 250000)',
              B1 = 'B1 ~ dnorm(0, var = 250000)',
              B2 = 'B2 ~ dnorm(0, var = 250000)')
  } else{
    stop('Model type not currently supported. Options include "quadratic", "briere", "gaussian", "weibull", "pawar-shsch", "lactin2", "ratkowsky", "kamykowski", "binomial_glm_lin", "binomial_glm_quad",  and "stinner".')
  }
  return(p)
}

#' Retrieve default model
#'
#' Retrieve default model formulation for a given thermal performance curve model
#'
#' @details This function returns a character string of the `nimble` likelihood model for a user-specified thermal performance curve
#' @param model character, name of thermal performance curve model. Currently, supported options include "quadratic", "briere", "gaussian", "weibull", "pawar-shsch", "lactin2", "kamykowski", "ratkowsky", "binomial_glm_lin", "binomial_glm_quad", and "stinner".
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
  } else if (model == 'kamykowski'){
    model_string = paste0('{\n    for (i in 1:N){\n    ', '        Trait[i] ~ T(dnorm(mean = step(Temp[i] - T.min)*step(T.max - Temp[i])*a*(1 - exp(-b*(Temp[i] - T.min)))*(1 - exp(-c*(T.max - Temp[i]))), var = sigma.sq), 0, )\n    }\n')
  } else if (model == 'ratkowsky'){
    model_string = paste0('{\n    for (i in 1:N){\n    ', '        Trait[i] ~ T(dnorm(mean = step(Temp[i] - T.min)*step(T.max - Temp[i])*((a*(Temp[i] - T.min))*(1 - exp(b*(Temp[i] - T.max))))^2, var = sigma.sq), 0, )\n    }\n')
  } else if (model == 'stinner'){
    model_string = paste0('{\n    for (i in 1:N){\n    ', '        Trait[i] ~ T(dnorm(mean = C / (1 + exp(k1 + k2*(T.opt - abs(T.opt - Temp[i])))), var = sigma.sq), 0, )\n    }\n')
  } else if (model == 'binomial_glm_lin'){
    model_string = paste0('{\n    for (i in 1:N){\n    ', '        Trait[i] ~ dbinom(p[i], n[i])\n            logit(p[i]) <- B0 + B1*Temp[i] \n    }\n')
  } else if (model == 'binomial_glm_quad'){
    model_string = paste0('{\n    for (i in 1:N){\n    ', '        Trait[i] ~ dbinom(p[i], n[i])\n            logit(p[i]) <- B0 + B1*Temp[i] + B2*(Temp[i])^2 \n    }\n')
  } else{
    stop("Argument for 'model' not currently supported. Current options include: quadratic, briere, weibull, gaussian, pawar-shsch, lactin2, ratkowsky, stinner, binomial_glm_lin, binomial_glm_quad")
  }
  return(model_string)
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
  data.nimble = checkData(data)
  modelStr = configureModel(model = model, priors = priors, ...)

  const.list = vector('list', 0)
  const.list$N = data.nimble$N

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

  if(!is.null(constant_list)){
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
  nimTPCmod = nimbleModel(str2expression(modelStr), constants = const.list,
                          data = data.nimble$data, inits = inits)
  #nimTPCmod = initializeModel(nimTPCmod)
  nimTPCmod_compiled = compileNimble(nimTPCmod)

  mcmcConfig <- configureMCMC(nimTPCmod)
  if (samplerType != 'RW' & samplerType != 'slice'){
    mcmcConfig$removeSamplers(names(defaultPriors(model)))
    mcmcConfig$addSampler(names(defaultPriors(model)), type = samplerType)
  }
  if (samplerType == 'slice'){
    for (i in names(defaultPriors(model))){
      mcmcConfig$removeSamplers(i)
      mcmcConfig$addSampler(i, type = samplerType)
    }
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

#' Evaluate Ratkowsky model
#'
#' Evaluate Ratkowsky model for thermal performance for a given parameter set
#'
#' @details This function returns a numeric vector of evaluations of the Ratkowsky thermal performance curve for a reference set `Temp`
#' @param params named vector, with entries `a`, `b`, `T.min`, `T.max`. If posteriorPredictive = TRUE, `sigma.sq` must also be present.
#' @param Temp numeric vector, set of temperature values to evaluate the Ratkowsky model at
#' @param posteriorPredictive logical, should posterior predictive samples be generated instead of evaluating the deterministic Ratkowsky model. default = FALSE.
#' @return numeric vector, where entry `k` represents either the value of the Ratkowsky model at `Temp[k]` for the given vector `params` (posteriorPredictive = FALSE), or a sigle draw from the posterior predictive distribution of the Ratkowsky model at `Temp[k]` for the given vector `params` (posteriorPredictive = TRUE)
#' @examples
#' ## set params and reference temperature set
#' param_set = c(T.max = 47, T.min = 10, a = .06, b = .5)
#' Temp_ref = seq(from = 5, to = 50, length.out = 1000)
#' plot(Temp_ref, ratkowsky_tpc(params = param_set, Temp = Temp_ref), type = 'l')


ratkowsky_tpc <- function(params, Temp, posteriorPredictive = FALSE){
  if (!is.logical(posteriorPredictive)) stop('posteriorPredictive argument must be supplied as a logical (= TRUE or = FALSE)')
  if (is.null(names(params))) stop('Error in call to function ratkowsky. param input must be named.')
  if (!is.vector(params)){
    warning('Expected params input to be a vector. Attempting to convert to vector')
    params = as.vector(params)
  }
  if (!('a' %in% names(params))){
    stop('Parameter vector is expected to be a named vector with element "a"')
  } else{
    if (!is.numeric(params['a'])) stop('Value for params["a"] is non-numeric')
    a = params['a']
  }
  if (!('b' %in% names(params))){
    stop('Parameter vector is expected to be a named vector with element "b"')
  } else{
    if (!is.numeric(params['b'])) stop('Value for params["b"] is non-numeric')
    b = params['b']
  }
  if (!('T.max' %in% names(params))){
    stop('Parameter vector is expected to be a named vector with element "T.max"')
  } else{
    if (!is.numeric(params['T.max'])) stop('Value for params["T.max"] is non-numeric')
    T.max = params['T.max']
  }
  if (!('T.min' %in% names(params))){
    stop('Parameter vector is expected to be a named vector with element "T.min"')
  } else{
    if (!is.numeric(params['T.min'])) stop('Value for params["T.min"] is non-numeric')
    T.min = params['T.min']
  }
  if (!('sigma.sq' %in% names(params)) && posteriorPredictive == TRUE){
    stop('Parameter vector is expected to be a named vector with element "sigma.sq"')
  } else{
    if (!is.numeric(params['sigma.sq'])) stop('Value for params["sigma.sq"] is non-numeric')
    sigma.sq = params['sigma.sq']
  }
  if (posteriorPredictive == FALSE){
    curve = (Temp > T.min)*(T.max > Temp)*((a*(Temp - T.min))*(1 - exp(b*(Temp - T.max))))^2
  } else{
    truncmeans = (Temp > T.min)*(T.max > Temp)*((a*(Temp - T.min))*(1 - exp(b*(Temp - T.max))))^2
    curve = rtruncnorm(length(Temp), a = 0, b = Inf, mean = truncmeans, sd = sqrt(sigma.sq))
  }
  return(curve)
}

#' Evaluate Stinner model
#'
#' Evaluate Stinner model for thermal performance for a given parameter set
#'
#' @details This function returns a numeric vector of evaluations of the Stinner thermal performance curve for a reference set `Temp`
#' @param params named vector, with entries `C`, `k1`, `k2`, `T.opt`. If posteriorPredictive = TRUE, `sigma.sq` must also be present.
#' @param Temp numeric vector, set of temperature values to evaluate the Stinner model at
#' @param posteriorPredictive logical, should posterior predictive samples be generated instead of evaluating the deterministic Stinner model. default = FALSE.
#' @return numeric vector, where entry `k` represents either the value of the Stinner model at `Temp[k]` for the given vector `params` (posteriorPredictive = FALSE), or a sigle draw from the posterior predictive distribution of the Stinner model at `Temp[k]` for the given vector `params` (posteriorPredictive = TRUE)
#' @examples
#' ## set params and reference temperature set
#' param_set = c(T.max = 47, T.min = 10, a = .06, b = .5)
#' Temp_ref = seq(from = 5, to = 50, length.out = 1000)
#' plot(Temp_ref, ratkowsky_tpc(params = param_set, Temp = Temp_ref), type = 'l')

stinner_tpc <- function(params, Temp, posteriorPredictive = FALSE){
  if (!is.logical(posteriorPredictive)) stop('posteriorPredictive argument must be supplied as a logical (= TRUE or = FALSE)')
  if (is.null(names(params))) stop('Error in call to function stinner_tpc. param input must be named.')
  if (!is.vector(params)){
    warning('Expected params input to be a vector. Attempting to convert to vector')
    params = as.vector(params)
  }
  if (!('C' %in% names(params))){
    stop('Parameter vector is expected to be a named vector with element "C"')
  } else{
    if (!is.numeric(params['C'])) stop('Value for params["C"] is non-numeric')
    C = params['C']
  }
  if (!('k1' %in% names(params))){
    stop('Parameter vector is expected to be a named vector with element "k1"')
  } else{
    if (!is.numeric(params['k1'])) stop('Value for params["k1"] is non-numeric')
    k1 = params['k1']
  }
  if (!('T.opt' %in% names(params))){
    stop('Parameter vector is expected to be a named vector with element "T.opt"')
  } else{
    if (!is.numeric(params['T.opt'])) stop('Value for params["T.opt"] is non-numeric')
    T.opt = params['T.opt']
  }
  if (!('k2' %in% names(params))){
    stop('Parameter vector is expected to be a named vector with element "k2"')
  } else{
    if (!is.numeric(params['k2'])) stop('Value for params["k2"] is non-numeric')
    k2 = params['k2']
  }
  if (!('sigma.sq' %in% names(params)) && posteriorPredictive == TRUE){
    stop('Parameter vector is expected to be a named vector with element "sigma.sq"')
  } else{
    if (!is.numeric(params['sigma.sq'])) stop('Value for params["sigma.sq"] is non-numeric')
    sigma.sq = params['sigma.sq']
  }
  if (posteriorPredictive == FALSE){
    curve = C / (1 + exp(k1 + k2*(T.opt - abs(T.opt - Temp))))
  } else{
    truncmeans = C / (1 + exp(k1 + k2*(T.opt - abs(T.opt - Temp))))
    curve = rtruncnorm(length(Temp), a = 0, b = Inf, mean = truncmeans, sd = sqrt(sigma.sq))
  }
  return(curve)
}

#' Evaluate Kamykowski model
#'
#' Evaluate Kamykowski model for thermal performance for a given parameter set
#'
#' @details This function returns a numeric vector of evaluations of the Kamykowski thermal performance curve for a reference set `Temp`
#' @param params named vector, with entries `a`, `b`, `c`, `T.min`, `T.max`. If posteriorPredictive = TRUE, `sigma.sq` must also be present.
#' @param Temp numeric vector, set of temperature values to evaluate the Kamykowski model at
#' @param posteriorPredictive logical, should posterior predictive samples be generated instead of evaluating the deterministic Ratkowsky model. default = FALSE.
#' @return numeric vector, where entry `k` represents either the value of the Kamykowski model at `Temp[k]` for the given vector `params` (posteriorPredictive = FALSE), or a sigle draw from the posterior predictive distribution of the Kamykowski model at `Temp[k]` for the given vector `params` (posteriorPredictive = TRUE)
#' @examples
#' ## set params and reference temperature set
#' param_set = c(T.max = 45, T.min = 18, a = 3, b = .1, c = 2)
#' Temp_ref = seq(from = 5, to = 50, length.out = 1000)
#' plot(Temp_ref, kamykowski_tpc(params = param_set, Temp = Temp_ref), type = 'l')


kamykowski_tpc <- function(params, Temp, posteriorPredictive = FALSE){
  if (!is.logical(posteriorPredictive)) stop('posteriorPredictive argument must be supplied as a logical (= TRUE or = FALSE)')
  if (is.null(names(params))) stop('Error in call to function ratkowsky. param input must be named.')
  if (!is.vector(params)){
    warning('Expected params input to be a vector. Attempting to convert to vector')
    params = as.vector(params)
  }
  if (!('a' %in% names(params))){
    stop('Parameter vector is expected to be a named vector with element "a"')
  } else{
    if (!is.numeric(params['a'])) stop('Value for params["a"] is non-numeric')
    a = params['a']
  }
  if (!('b' %in% names(params))){
    stop('Parameter vector is expected to be a named vector with element "b"')
  } else{
    if (!is.numeric(params['b'])) stop('Value for params["b"] is non-numeric')
    b = params['b']
  }
  if (!('c' %in% names(params))){
    stop('Parameter vector is expected to be a named vector with element "b"')
  } else{
    if (!is.numeric(params['c'])) stop('Value for params["c"] is non-numeric')
    c = params['c']
  }
  if (!('T.max' %in% names(params))){
    stop('Parameter vector is expected to be a named vector with element "T.max"')
  } else{
    if (!is.numeric(params['T.max'])) stop('Value for params["T.max"] is non-numeric')
    T.max = params['T.max']
  }
  if (!('T.min' %in% names(params))){
    stop('Parameter vector is expected to be a named vector with element "T.min"')
  } else{
    if (!is.numeric(params['T.min'])) stop('Value for params["T.min"] is non-numeric')
    T.min = params['T.min']
  }
  if (!('sigma.sq' %in% names(params)) && posteriorPredictive == TRUE){
    stop('Parameter vector is expected to be a named vector with element "sigma.sq"')
  } else{
    if (!is.numeric(params['sigma.sq'])) stop('Value for params["sigma.sq"] is non-numeric')
    sigma.sq = params['sigma.sq']
  }
  if (posteriorPredictive == FALSE){
    curve = (Temp > T.min)*(T.max > Temp)*a*(1 - exp(-b*(Temp - T.min)))*(1 - exp(-c*(T.max - Temp)))
  } else{
    truncmeans = (Temp > T.min)*(T.max > Temp)*a*(1 - exp(-b*(Temp - T.min)))*(1 - exp(-c*(T.max - Temp)))
    curve = rtruncnorm(length(Temp), a = 0, b = Inf, mean = truncmeans, sd = sqrt(sigma.sq))
  }
  return(curve)
}

#' Evaluate Schoolfield-Sharpe model
#'
#' Evaluate Schoolfield-Sharpe model for thermal performance for a given parameter set using the Schoolfield-Sharpe formulation from Kontopoulos et al, 2018
#'
#' @details This function returns a numeric vector of evaluations of the Schoolfield-Sharpe thermal performance curve for a reference set `Temp`
#' @param params named vector, with entries `a`, `b`, `c`, `T.min`, `T.max`. If posteriorPredictive = TRUE, `sigma.sq` must also be present.
#' @param Temp numeric vector, set of temperature values to evaluate the Schoolfield-Sharpe model at
#' @param T.ref numeric, value for fixed parameter T.ref  used in the formulation of the Schoolfield-Sharpe model from Kontopoulos et al, 2018
#' @param posteriorPredictive logical, should posterior predictive samples be generated instead of evaluating the deterministic Schoolfield-Sharpe model. default = FALSE.
#' @return numeric vector, where entry `k` represents either the value of the Schoolfield-Sharpe model at `Temp[k]` for the given vector `params` (posteriorPredictive = FALSE), or a sigle draw from the posterior predictive distribution of the Schoolfield-Sharpe model at `Temp[k]` for the given vector `params` (posteriorPredictive = TRUE)
#' @examples
#' ## set params and reference temperature set
#' param_set = c(T.opt = 39, e = .7, e_h = 15, r_tref = 1)
#' Temp_ref = seq(from = 5, to = 50, length.out = 1000)
#' plot(Temp_ref, pawar_shsch_tpc(params = param_set, Temp = Temp_ref, T.ref = 20), type = 'l')


pawar_shsch_tpc <- function(params, Temp, T.ref, posteriorPredictive = FALSE){
  if (!is.logical(posteriorPredictive)) stop('posteriorPredictive argument must be supplied as a logical (= TRUE or = FALSE)')
  if (is.null(names(params))) stop('Error in call to function ratkowsky. param input must be named.')
  if (!is.vector(params)){
    warning('Expected params input to be a vector. Attempting to convert to vector')
    params = as.vector(params)
  }
  if (!('r_tref' %in% names(params))){
    stop('Parameter vector is expected to be a named vector with element "r_tref"')
  } else{
    if (!is.numeric(params['r_tref'])) stop('Value for params["r_tref"] is non-numeric')
    r_tref = params['r_tref']
  }
  if (!('e' %in% names(params))){
    stop('Parameter vector is expected to be a named vector with element "e"')
  } else{
    if (!is.numeric(params['e'])) stop('Value for params["e"] is non-numeric')
    e = params['e']
  }
  if (!('e_h' %in% names(params))){
    stop('Parameter vector is expected to be a named vector with element "e_h"')
  } else{
    if (!is.numeric(params['e_h'])) stop('Value for params["e_h"] is non-numeric')
    e_h = params['e_h']
  }
  if (!('T.opt' %in% names(params))){
    stop('Parameter vector is expected to be a named vector with element "T.opt"')
  } else{
    if (!is.numeric(params['T.opt'])) stop('Value for params["T.opt"] is non-numeric')
    T.opt = params['T.opt']
  }
  if (!('sigma.sq' %in% names(params)) && posteriorPredictive == TRUE){
    stop('Parameter vector is expected to be a named vector with element "sigma.sq"')
  } else{
    if (!is.numeric(params['sigma.sq'])) stop('Value for params["sigma.sq"] is non-numeric')
    sigma.sq = params['sigma.sq']
  }
  if (posteriorPredictive == FALSE){
    curve = (e_h > e)*r_tref*exp((e/(8.62e-05))*((1/(T.ref+273.15)) - (1/(Temp + 273.15)))) / (1 + (e / (e_h-e)) * exp((e_h/(8.62e-05))*(1/(T.opt + 273.15) - 1/(Temp + 273.15))))
  } else{
    truncmeans = (e_h > e)*r_tref*exp((e/(8.62e-05))*((1/(T.ref+273.15)) - (1/(Temp + 273.15)))) / (1 + (e / (e_h-e)) * exp((e_h/(8.62e-05))*(1/(T.opt + 273.15) - 1/(Temp + 273.15))))
    curve = rtruncnorm(length(Temp), a = 0, b = Inf, mean = truncmeans, sd = sqrt(sigma.sq))
  }
  return(curve)
}

#' Evaluate Quadratic TPC model
#'
#' Evaluate Quadratic model for thermal performance for a given parameter set.
#'
#' @details This function returns a numeric vector of evaluations of the Quadratic thermal performance curve for a reference set `Temp`
#' @param params named vector, with entries `q`, `T.max`, `T.min`. If posteriorPredictive = TRUE, `sigma.sq` must also be present.
#' @param Temp numeric vector, set of temperature values to evaluate the Quadratic model at
#' @param posteriorPredictive logical, should posterior predictive samples be generated instead of evaluating the deterministic Quadratic model. default = FALSE.
#' @return numeric vector, where entry `k` represents either the value of the Quadratic model at `Temp[k]` for the given vector `params` (posteriorPredictive = FALSE), or a sigle draw from the posterior predictive distribution of the Schoolfield-Sharpe model at `Temp[k]` for the given vector `params` (posteriorPredictive = TRUE)
#' @examples
#' ## set params and reference temperature set
#' param_set = c(T.max = 47, T.min = 23, q = .02)
#' Temp_ref = seq(from = 5, to = 50, length.out = 1000)
#' plot(Temp_ref, quadratic_tpc(params = param_set, Temp = Temp_ref), type = 'l')

quadratic_tpc <- function(params, Temp, posteriorPredictive = FALSE){
  if (!is.logical(posteriorPredictive)) stop('posteriorPredictive argument must be supplied as a logical (= TRUE or = FALSE)')
  if (is.null(names(params))) stop('Error in call to function ratkowsky. param input must be named.')
  if (!is.vector(params)){
    warning('Expected params input to be a vector. Attempting to convert to vector')
    params = as.vector(params)
  }
  if (!('q' %in% names(params))){
    stop('Parameter vector is expected to be a named vector with element "q"')
  } else{
    if (!is.numeric(params['q'])) stop('Value for params["q"] is non-numeric')
    q = params['q']
  }
  if (!('T.max' %in% names(params))){
    stop('Parameter vector is expected to be a named vector with element "T.max"')
  } else{
    if (!is.numeric(params['T.max'])) stop('Value for params["T.max"] is non-numeric')
    T.max = params['T.max']
  }
  if (!('T.min' %in% names(params))){
    stop('Parameter vector is expected to be a named vector with element "T.min"')
  } else{
    if (!is.numeric(params['T.min'])) stop('Value for params["T.min"] is non-numeric')
    T.min = params['T.min']
  }
  if (!('sigma.sq' %in% names(params)) && posteriorPredictive == TRUE){
    stop('Parameter vector is expected to be a named vector with element "sigma.sq"')
  } else{
    if (!is.numeric(params['sigma.sq'])) stop('Value for params["sigma.sq"] is non-numeric')
    sigma.sq = params['sigma.sq']
  }
  if (posteriorPredictive == FALSE){
    curve = -q*(Temp - T.min)*(Temp - T.max) * (T.max > Temp) * (Temp > T.min)
  } else{
    truncmeans = -q*(Temp - T.min)*(Temp - T.max) * (T.max > Temp) * (Temp > T.min)
    curve = rtruncnorm(length(Temp), a = 0, b = Inf, mean = truncmeans, sd = sqrt(sigma.sq))
  }
  return(curve)
}

#' Evaluate Briere TPC model
#'
#' Evaluate Briere model for thermal performance for a given parameter set.
#'
#' @details This function returns a numeric vector of evaluations of the Briere thermal performance curve for a reference set `Temp`
#' @param params named vector, with entries `q`, `T.max`, `T.min`. If posteriorPredictive = TRUE, `sigma.sq` must also be present.
#' @param Temp numeric vector, set of temperature values to evaluate the Briere model at
#' @param posteriorPredictive logical, should posterior predictive samples be generated instead of evaluating the deterministic Briere model. default = FALSE.
#' @return numeric vector, where entry `k` represents either the value of the Briere model at `Temp[k]` for the given vector `params` (posteriorPredictive = FALSE), or a sigle draw from the posterior predictive distribution of the Schoolfield-Sharpe model at `Temp[k]` for the given vector `params` (posteriorPredictive = TRUE)
#' @examples
#' ## set params and reference temperature set
#' param_set = c(T.max = 45, T.min = 20, q = .05)
#' Temp_ref = seq(from = 5, to = 50, length.out = 1000)
#' plot(Temp_ref, briere_tpc(params = param_set, Temp = Temp_ref), type = 'l')

briere_tpc <- function(params, Temp, posteriorPredictive = FALSE){
  if (!is.logical(posteriorPredictive)) stop('posteriorPredictive argument must be supplied as a logical (= TRUE or = FALSE)')
  if (is.null(names(params))) stop('Error in call to function ratkowsky. param input must be named.')
  if (!is.vector(params)){
    warning('Expected params input to be a vector. Attempting to convert to vector')
    params = as.vector(params)
  }
  if (!('q' %in% names(params))){
    stop('Parameter vector is expected to be a named vector with element "q"')
  } else{
    if (!is.numeric(params['q'])) stop('Value for params["q"] is non-numeric')
    q = params['q']
  }
  if (!('T.max' %in% names(params))){
    stop('Parameter vector is expected to be a named vector with element "T.max"')
  } else{
    if (!is.numeric(params['T.max'])) stop('Value for params["T.max"] is non-numeric')
    T.max = params['T.max']
  }
  if (!('T.min' %in% names(params))){
    stop('Parameter vector is expected to be a named vector with element "T.min"')
  } else{
    if (!is.numeric(params['T.min'])) stop('Value for params["T.min"] is non-numeric')
    T.min = params['T.min']
  }
  if (!('sigma.sq' %in% names(params)) && posteriorPredictive == TRUE){
    stop('Parameter vector is expected to be a named vector with element "sigma.sq"')
  } else{
    if (!is.numeric(params['sigma.sq'])) stop('Value for params["sigma.sq"] is non-numeric')
    sigma.sq = params['sigma.sq']
  }
  if (posteriorPredictive == FALSE){
    curve = q*(Temp - T.min)*sqrt((T.max>Temp)*abs(T.max-Temp)) * (T.max > Temp) * (Temp > T.min)
  } else{
    truncmeans = q*(Temp - T.min)*sqrt((T.max>Temp)*abs(T.max-Temp)) * (T.max > Temp) * (Temp > T.min)
    curve = rtruncnorm(length(Temp), a = 0, b = Inf, mean = truncmeans, sd = sqrt(sigma.sq))
  }
  return(curve)
}

#' Evaluate Weibull TPC model
#'
#' Evaluate Weibull model for thermal performance for a given parameter set.
#'
#' @details This function returns a numeric vector of evaluations of the Weibull thermal performance curve for a reference set `Temp`
#' @param params named vector, with entries `a`, `b`, `c`, `T.opt`. If posteriorPredictive = TRUE, `sigma.sq` must also be present.
#' @param Temp numeric vector, set of temperature values to evaluate the Weibull model at
#' @param posteriorPredictive logical, should posterior predictive samples be generated instead of evaluating the deterministic Weibull model. default = FALSE.
#' @return numeric vector, where entry `k` represents either the value of the Weibull model at `Temp[k]` for the given vector `params` (posteriorPredictive = FALSE), or a sigle draw from the posterior predictive distribution of the Schoolfield-Sharpe model at `Temp[k]` for the given vector `params` (posteriorPredictive = TRUE)
#' @examples
#' ## set params and reference temperature set
#' param_set = c(T.opt = 38, a = 3, b = 7e9, c = 1e9)
#' Temp_ref = seq(from = 5, to = 50, length.out = 1000)
#' plot(Temp_ref, weibull_tpc(params = param_set, Temp = Temp_ref), type = 'l')

weibull_tpc <- function(params, Temp, posteriorPredictive = FALSE){
  if (!is.logical(posteriorPredictive)) stop('posteriorPredictive argument must be supplied as a logical (= TRUE or = FALSE)')
  if (is.null(names(params))) stop('Error in call to function ratkowsky. param input must be named.')
  if (!is.vector(params)){
    warning('Expected params input to be a vector. Attempting to convert to vector')
    params = as.vector(params)
  }
  if (!('a' %in% names(params))){
    stop('Parameter vector is expected to be a named vector with element "a"')
  } else{
    if (!is.numeric(params['a'])) stop('Value for params["a"] is non-numeric')
    a = params['a']
  }
  if (!('b' %in% names(params))){
    stop('Parameter vector is expected to be a named vector with element "b"')
  } else{
    if (!is.numeric(params['b'])) stop('Value for params["b"] is non-numeric')
    b = params['b']
  }
  if (!('c' %in% names(params))){
    stop('Parameter vector is expected to be a named vector with element "b"')
  } else{
    if (!is.numeric(params['c'])) stop('Value for params["c"] is non-numeric')
    c = params['c']
  }
  if (!('T.opt' %in% names(params))){
    stop('Parameter vector is expected to be a named vector with element "T.opt"')
  } else{
    if (!is.numeric(params['T.opt'])) stop('Value for params["T.opt"] is non-numeric')
    T.opt = params['T.opt']
  }
  if (!('sigma.sq' %in% names(params)) && posteriorPredictive == TRUE){
    stop('Parameter vector is expected to be a named vector with element "sigma.sq"')
  } else{
    if (!is.numeric(params['sigma.sq'])) stop('Value for params["sigma.sq"] is non-numeric')
    sigma.sq = params['sigma.sq']
  }
  if (posteriorPredictive == FALSE){
    curve = ((a*(((c-1)/c)^((1-c)/c))*((((Temp-T.opt)/b)+(((c-1)/c)^(1/c)))^(c-1))*(exp(-((((Temp-T.opt)/b)+(((c-1)/c)^(1/c)))^c)+((c-1)/c)))))

  } else{
    truncmeans = ((a*(((c-1)/c)^((1-c)/c))*((((Temp-T.opt)/b)+(((c-1)/c)^(1/c)))^(c-1))*(exp(-((((Temp-T.opt)/b)+(((c-1)/c)^(1/c)))^c)+((c-1)/c)))))
    curve = rtruncnorm(length(Temp), a = 0, b = Inf, mean = truncmeans, sd = sqrt(sigma.sq))
  }
  return(curve)
}


#' Evaluate Gaussian TPC model
#'
#' Evaluate Gaussian model for thermal performance for a given parameter set.
#'
#' @details This function returns a numeric vector of evaluations of the Gaussian thermal performance curve for a reference set `Temp`
#' @param params named vector, with entries `a`, `rmax`, `T.opt`. If posteriorPredictive = TRUE, `sigma.sq` must also be present.
#' @param Temp numeric vector, set of temperature values to evaluate the Gaussian model at
#' @param posteriorPredictive logical, should posterior predictive samples be generated instead of evaluating the deterministic Gaussian model. default = FALSE.
#' @return numeric vector, where entry `k` represents either the value of the Gaussian model at `Temp[k]` for the given vector `params` (posteriorPredictive = FALSE), or a sigle draw from the posterior predictive distribution of the Schoolfield-Sharpe model at `Temp[k]` for the given vector `params` (posteriorPredictive = TRUE)
#' @examples
#' ## set params and reference temperature set
#' param_set = c(T.opt = 36, a = 6.5, rmax = 2.75)
#' Temp_ref = seq(from = 5, to = 50, length.out = 1000)
#' plot(Temp_ref, gauss_tpc(params = param_set, Temp = Temp_ref), type = 'l')


gauss_tpc <- function(params, Temp, posteriorPredictive = FALSE){
  if (!is.logical(posteriorPredictive)) stop('posteriorPredictive argument must be supplied as a logical (= TRUE or = FALSE)')
  if (is.null(names(params))) stop('Error in call to function ratkowsky. param input must be named.')
  if (!is.vector(params)){
    warning('Expected params input to be a vector. Attempting to convert to vector')
    params = as.vector(params)
  }
  if (!('a' %in% names(params))){
    stop('Parameter vector is expected to be a named vector with element "a"')
  } else{
    if (!is.numeric(params['a'])) stop('Value for params["a"] is non-numeric')
    a = params['a']
  }
  if (!('rmax' %in% names(params))){
    stop('Parameter vector is expected to be a named vector with element "rmax"')
  } else{
    if (!is.numeric(params['rmax'])) stop('Value for params["rmax"] is non-numeric')
    rmax = params['rmax']
  }
  if (!('T.opt' %in% names(params))){
    stop('Parameter vector is expected to be a named vector with element "T.opt"')
  } else{
    if (!is.numeric(params['T.opt'])) stop('Value for params["T.opt"] is non-numeric')
    T.opt = params['T.opt']
  }
  if (!('sigma.sq' %in% names(params)) && posteriorPredictive == TRUE){
    stop('Parameter vector is expected to be a named vector with element "sigma.sq"')
  } else{
    if (!is.numeric(params['sigma.sq'])) stop('Value for params["sigma.sq"] is non-numeric')
    sigma.sq = params['sigma.sq']
  }
  if (posteriorPredictive == FALSE){
    curve = rmax*exp(-0.5*(abs(Temp - T.opt)/a)^2)
  } else{
    truncmeans = rmax*exp(-0.5*(abs(Temp - T.opt)/a)^2)
    curve = rtruncnorm(length(Temp), a = 0, b = Inf, mean = truncmeans, sd = sqrt(sigma.sq))
  }
  return(curve)
}

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

#' Provide thermal performance curve summaries
#'
#' Provide thermal performance curve summaries using output from `bTPC`'s nimble MCMC
#'
#' @details This function returns various summaries of the output of the thermal performance curve model, generarted using MCMC samples from the object returned by `bTPC`
#' @param TPC list, object output from performing MCMC using the `bTPC` function.
#' @param Temp_interval vector, reference values to use to compute values of the thermal performance curve. If no vector is provided, Temp_interval is set as a sequence from the lowest observed temperature in the data to the highest observed temperature in the data, with 1000 equally spaced points
#' @param summaryOnly logical, should the function return only summaries, or the entire matrix of thermal performance curve evaluations AND the summaries? default = TRUE
#' @param summaryType character, type of summary used. Currently supported options are "quantile" and "hdi" (default option). Users that wish to use alternatives may consider summaryOnly = FALSE, and using their desired method on the raw TPC evaluations.
#' @param centralSummary character, central summary measure used. Currently supported options are "median" (default) and "mean". Users that wish to use alternatives may consider summaryOnly = FALSE, and using their desired method on the raw TPC evaluations.
#' @param plot logical, should a plot be created? default = TRUE
#' @param probs numeric, represents either the quantiles to be used during summarization (in the form c(lower, upper)) or the credible mass used to compute the highest density intervals
#' @param burn numeric, initial number of iterations to be discarded as burn-in (default = 0)
#' @param plotOnly logical, should a plot be generated without returning a list of summaries? if plotOnly = TRUE, an invisible NULL is returned and only plot is generated. Default = FALSE
#' @param traitName string, name of Trait to be used as y-axis if a plot is generated. default = "Trait"
#' @param ... additional parameters to be passed as arguments
#' @return return types vary by argument. For plotOnly = TRUE, invisible NULL is returned. For summaryOnly = TRUE, a list of summaries is returned. For summaryOnly = FALSE, a list is returned with summaries as well as the entire set of thermal performance curve evaluations
#' @examples
#' ## need data to set up example here. placeholder for now
#' ## set params and reference temperature set
#' myfun = str2tpc_fun(model = 'gaussian')
#' param_set = c(T.opt = 36, a = 6.5, rmax = 2.75)
#' Temp_ref = seq(from = 5, to = 50, length.out = 1000)
#' plot(Temp_ref, myfun(params = param_set, Temp = Temp_ref), type = 'l')


bayesTPC_summary <- function(TPC, Temp_interval = NULL, summaryOnly = TRUE,
                             summaryType = 'hdi', centralSummary = 'median',
                             plot = TRUE, probs = c(.05, .95),
                             burn = 0, plotOnly = FALSE, traitName = 'Trait', ...){
  if (!(summaryType %in% c('hdi', 'quantile')) & summaryOnly) stop('Unsupported argument for "summaryType". Currently only "quantile" and "hdi" are supported. To use other summary functions, try using summaryOnly = FALSE and working with the entire matrix')
  if (!(centralSummary %in% c('mean', 'median')) & summaryOnly) stop('Unsupported argument for "centralSummary". Currently only "median" and "mean" are supported. To use other summary functions, try using summaryOnly = FALSE and working with the entire matrix')

  if (length(probs) == 2 & (probs[2] < probs[1])){
    stop('Incorrect specification of tail probabilities for quantile method. Tail probs must be provided in the form c(lower, upper)')
  }
  if (!(is.logical(summaryOnly))){
    stop('Argument summaryOnly must be provided as a logical (= TRUE or = FALSE)')
  }
  if ((length(probs) > 1) & summaryType == 'hdi'){
    if (all(probs == c(.05,.95))){
      probs = .9
      warning('Currently using summaryType = "hdi". Default credible interval mass is credMass = .9)')
    } else{
      stop('For summaryType = "hdi", only one value is accepted for argument probs')
    }

  }

  if (plot == FALSE & plotOnly == TRUE){
    stop('Argument plotOnly = TRUE requires argument plot to also be TRUE')
  }

  if (is.null(Temp_interval)) Temp_interval = seq(from = min(TPC$data$data$Temp), to = max(TPC$data$data$Temp), length.out = 1000)
  tpc_fun = str2tpc_fun(TPC$modelType)
  max.ind = nrow(TPC$samples)
  tpc_evals = apply(tpc_fun, X = TPC$samples[(burn+1):max.ind,], Temp = Temp_interval, MARGIN = 1, posteriorPredictive = FALSE, ...)

  if (centralSummary == 'median'){
    if (summaryType == 'quantile'){
      upper_bounds = rowQuantiles(tpc_evals, probs = probs[2])
      lower_bounds = rowQuantiles(tpc_evals, probs = probs[1])
      medians = rowMedians(tpc_evals)
    }
    if (summaryType == 'hdi'){
      hdi_mat = apply(FUN = hdi, X = tpc_evals, MARGIN = 1, credMass = probs)
      upper_bounds = hdi_mat[2,]
      lower_bounds = hdi_mat[1,]
      medians = rowMedians(tpc_evals)
    }
    if (plot){
      plot(Temp_interval, upper_bounds, type = 'l', lty = 2, col = 'blue', xlab = 'Temperature (C)', ylab = traitName, ylim = c(0, max(upper_bounds)))
      points(Temp_interval, lower_bounds, type = 'l', col = 'blue', lty = 2)
      points(Temp_interval, medians, type = 'l', col = 'blue')
      #points(TPC$data$data$Temp, TPC$data$data$Trait)
      if (plotOnly){
        return(invisible(NULL))
      } else{
        if (summaryOnly == FALSE){
          return(list(TPC_vals = tpc_evals,
                      Temp_interval = Temp_interval,
                      Upper_bounds = upper_bounds,
                      Lower_bounds = lower_bounds,
                      Medians = medians))
        } else{
          return(list(Temp_interval = Temp_interval,
                      Upper_bounds = upper_bounds,
                      Lower_bounds = lower_bounds,
                      Medians = medians))
        }
      }
    } else{
      if (summaryOnly == FALSE){
        return(list(TPC_vals = tpc_evals,
                    Temp_interval = Temp_interval,
                    Upper_bounds = upper_bounds,
                    Lower_bounds = lower_bounds,
                    Medians = medians))
      } else{
        return(list(Temp_interval = Temp_interval,
                    Upper_bounds = upper_bounds,
                    Lower_bounds = lower_bounds,
                    Medians = medians))
      }
    }
  }
  if (centralSummary == 'mean'){
    if (summaryType == 'quantile'){
      upper_bounds = rowQuantiles(tpc_evals, probs = tail.probs[2])
      lower_bounds = rowQuantiles(tpc_evals, probs = tail.probs[1])
      means = rowMeans2(tpc_evals)
    }
    if (summaryType == 'hdi'){
      hdi_mat = apply(FUN = hdi, X = tpc_evals, MARGIN = 1, credMass = probs)
      upper_bounds = hdi_mat[2,]
      lower_bounds = hdi_mat[1,]
      means = rowMeans2(tpc_evals)
    }
    if (plot){
      plot(Temp_interval, upper_bounds, type = 'l', col = 'blue', lty = 2, xlab = 'Temperature (C)', ylab = traitName, ylim = c(0, max(upper_bounds)))
      points(Temp_interval, lower_bounds, type = 'l', col = 'blue', lty = 2)
      points(Temp_interval, means, type = 'l', col = 'blue')
      if (plotOnly){
        return(invisible(NULL))
      } else{
        if (summaryOnly == FALSE){
          return(list(TPC_vals = tpc_evals,
                      Temp_interval = Temp_interval,
                      Upper_bounds = upper_bounds,
                      Lower_bounds = lower_bounds,
                      Means = means))
        } else{
          return(list(Temp_interval = Temp_interval,
                      Upper_bounds = upper_bounds,
                      Lower_bounds = lower_bounds,
                      Means = means))
        }
      }
    } else{
      if (summaryOnly == FALSE){
        return(list(TPC_vals = tpc_evals,
                    Temp_interval = Temp_interval,
                    Upper_bounds = upper_bounds,
                    Lower_bounds = lower_bounds,
                    Means = means))
      } else{
        return(list(Temp_interval = Temp_interval,
                    Upper_bounds = upper_bounds,
                    Lower_bounds = lower_bounds,
                    Means = means))
      }
    }
  }
}

#' Provide thermal performance curve posterior predictive summaries
#'
#' Provide thermal performance curve posterior predictive summaries using output from `bTPC`'s nimble MCMC
#'
#' @details This function returns various summaries of the output of the thermal performance curve posterior predictive model samples, generarted using MCMC samples from the object returned by `bTPC`
#' @param TPC list, object output from performing MCMC using the `bTPC` function.
#' @param Temp_interval vector, reference values to use to compute values of the thermal performance curve. If no vector is provided, Temp_interval is set as a sequence from the lowest observed temperature in the data to the highest observed temperature in the data, with 1000 equally spaced points
#' @param summaryOnly logical, should the function return only summaries, or the entire matrix of thermal performance curve posterior predictive samples AND the summaries? default = TRUE
#' @param summaryType character, type of summary used. Currently supported options are "quantile" and "hdi" (default option). Users that wish to use alternatives may consider summaryOnly = FALSE, and using their desired method on the raw TPC evaluations.
#' @param centralSummary character, central summary measure used. Currently supported options are "median" (default) and "mean". Users that wish to use alternatives may consider summaryOnly = FALSE, and using their desired method on the raw TPC evaluations.
#' @param plot logical, should a plot be created? default = TRUE
#' @param probs numeric, represents either the quantiles to be used during summarization (in the form c(lower, upper)) or the credible mass used to compute the highest density intervals
#' @param burn numeric, initial number of iterations to be discarded as burn-in (default = 0)
#' @param plotOnly logical, should a plot be generated without returning a list of summaries? if plotOnly = TRUE, an invisible NULL is returned and only plot is generated. Default = FALSE
#' @param traitName string, name of Trait to be used as y-axis if a plot is generated. default = "Trait"
#' @param seed integer, seed value to be used. Useful for ensuring that results are reproducible. default = NULL
#' @param ... additional parameters to be passed as arguments
#' @return return types vary by argument. For plotOnly = TRUE, invisible NULL is returned. For summaryOnly = TRUE, a list of summaries is returned. For summaryOnly = FALSE, a list is returned with summaries as well as the entire set of thermal performance curve posterior predictive samples
#' @examples
#' ## need data to set up example here. placeholder for now
#' ## set params and reference temperature set
#' myfun = str2tpc_fun(model = 'gaussian')
#' param_set = c(T.opt = 36, a = 6.5, rmax = 2.75)
#' Temp_ref = seq(from = 5, to = 50, length.out = 1000)
#' plot(Temp_ref, myfun(params = param_set, Temp = Temp_ref), type = 'l')


posteriorPredTPC <- function(TPC, Temp_interval = NULL, summaryOnly = TRUE,
                             summaryType = 'hdi', centralSummary = 'median',
                             plot = TRUE, probs = c(.05, .95),
                             burn = 0, plotOnly = FALSE, traitName = 'Trait', seed = NULL, ...){
  if (!(summaryType %in% c('hdi', 'quantile')) & summaryOnly) stop('Unsupported argument for "summaryType". Currently only "quantile" and "hdi" are supported. To use other summary functions, try using summaryOnly = FALSE and working with the entire matrix')
  if (!(centralSummary %in% c('mean', 'median')) & summaryOnly) stop('Unsupported argument for "centralSummary". Currently only "median" and "mean" are supported. To use other summary functions, try using summaryOnly = FALSE and working with the entire matrix')
  if (!(is.null(seed))){
    if (!(is.integer(seed))) stop('Argument "seed" must be integer valued')
  }
  if (length(probs) == 2 & (probs[2] < probs[1])){
    stop('Incorrect specification of tail probabilities for quantile method. Tail probs must be provided in the form c(lower, upper)')
  }
  if (!(is.logical(summaryOnly))){
    stop('Argument summaryOnly must be provided as a logical (= TRUE or = FALSE)')
  }
  if ((length(probs) > 1) & summaryType == 'hdi'){
    if (all(probs == c(.05,.95))){
      probs = .9
      warning('Currently using summaryType = "hdi". Default credible interval mass is credMass = .9)')
    } else{
      stop('For summaryType = "hdi", only one value is accepted for argument probs')
    }

  }

  if (plot == FALSE & plotOnly == TRUE){
    stop('Argument plotOnly = TRUE requires argument plot to also be TRUE')
  }

  if (is.null(Temp_interval)) Temp_interval = seq(from = min(TPC$data$data$Temp), to = max(TPC$data$data$Temp), length.out = 1000)
  tpc_fun = str2tpc_fun(TPC$modelType)
  max.ind = nrow(TPC$samples)
  if (!is.null(seed)) set.seed(seed)
  post_pred_samples = apply(tpc_fun, X = TPC$samples[(burn+1):max.ind,], Temp = Temp_interval, MARGIN = 1, posteriorPredictive = TRUE, ...)
  tpc_ev = rowMeans2(apply(tpc_fun, X = TPC$samples[(burn+1):max.ind,], Temp = Temp_interval, MARGIN = 1, posteriorPredictive = FALSE, ...))
  if (centralSummary == 'median'){
    if (summaryType == 'quantile'){
      upper_bounds = rowQuantiles(post_pred_samples, probs = probs[2])
      lower_bounds = rowQuantiles(post_pred_samples, probs = probs[1])
      medians = rowMedians(post_pred_samples)
    }
    if (summaryType == 'hdi'){
      hdi_mat = apply(FUN = hdi, X = post_pred_samples, MARGIN = 1, credMass = probs)
      upper_bounds = hdi_mat[2,]
      lower_bounds = hdi_mat[1,]
      medians = rowMedians(post_pred_samples)
    }
    if (plot){
      plot(Temp_interval, upper_bounds, type = 'l', lty = 3, col = 'blue', xlab = 'Temperature (C)', ylab = traitName, ylim = c(0, max(max(upper_bounds), max(TPC$data$data$Trait))))
      points(Temp_interval, tpc_ev, col = 'red', type = 'l', lty = 2, lwd = 1.1)
      points(Temp_interval, lower_bounds, type = 'l', col = 'blue', lty = 3)
      points(Temp_interval, medians, type = 'l', col = 'blue')
      points(TPC$data$data$Temp, TPC$data$data$Trait)
      if (plotOnly){
        return(invisible(NULL))
      } else{
        if (summaryOnly == FALSE){
          return(list(Posterior_predictive_samples = post_pred_samples,
                      Temp_interval = Temp_interval,
                      Upper_bounds = upper_bounds,
                      Lower_bounds = lower_bounds,
                      Medians = medians,
                      TPC_mean = tpc_ev))
        } else{
          return(list(Temp_interval = Temp_interval,
                      Upper_bounds = upper_bounds,
                      Lower_bounds = lower_bounds,
                      Medians = medians,
                      TPC_mean = tpc_ev))
        }
      }
    } else{
      if (summaryOnly == FALSE){
        return(list(Posterior_predictive_samples = post_pred_samples,
                    Temp_interval = Temp_interval,
                    Upper_bounds = upper_bounds,
                    Lower_bounds = lower_bounds,
                    Medians = medians,
                    TPC_mean = tpc_ev))
      } else{
        return(list(Temp_interval = Temp_interval,
                    Upper_bounds = upper_bounds,
                    Lower_bounds = lower_bounds,
                    Medians = medians,
                    TPC_mean = tpc_ev))
      }
    }
  }
  if (centralSummary == 'mean'){
    if (summaryType == 'quantile'){
      upper_bounds = rowQuantiles(post_pred_samples, probs = tail.probs[2])
      lower_bounds = rowQuantiles(post_pred_samples, probs = tail.probs[1])
      means = rowMeans2(post_pred_samples)
    }
    if (summaryType == 'hdi'){
      hdi_mat = apply(FUN = hdi, X = post_pred_samples, MARGIN = 1, credMass = probs)
      upper_bounds = hdi_mat[2,]
      lower_bounds = hdi_mat[1,]
      means = rowMeans2(post_pred_samples)
    }
    if (plot){
      plot(Temp_interval, upper_bounds, type = 'l', col = 'blue', lty = 3, xlab = 'Temperature (C)', ylab = traitName, ylim = c(0, max(max(upper_bounds), max(TPC$data$data$Trait))))
      points(Temp_interval, tpc_ev, col = 'red', type = 'l', lty = 2, lwd = 1.1)
      points(Temp_interval, lower_bounds, type = 'l', col = 'blue', lty = 3)
      points(Temp_interval, means, type = 'l', col = 'blue')
      points(TPC$data$data$Temp, TPC$data$data$Trait)

      if (plotOnly){
        return(invisible(NULL))
      } else{
        if (summaryOnly == FALSE){
          return(list(Posterior_predictive_samples = post_pred_samples,
                      Temp_interval = Temp_interval,
                      Upper_bounds = upper_bounds,
                      Lower_bounds = lower_bounds,
                      Means = means,
                      TPC_mean = tpc_ev))
        } else{
          return(list(Temp_interval = Temp_interval,
                      Upper_bounds = upper_bounds,
                      Lower_bounds = lower_bounds,
                      Means = means,
                      TPC_mean = tpc_ev))
        }
      }
    } else{
      if (summaryOnly == FALSE){
        return(list(Posterior_predictive_samples = post_pred_samples,
                    Temp_interval = Temp_interval,
                    Upper_bounds = upper_bounds,
                    Lower_bounds = lower_bounds,
                    Means = means,
                    TPC_mean = tpc_ev))
      } else{
        return(list(Temp_interval = Temp_interval,
                    Upper_bounds = upper_bounds,
                    Lower_bounds = lower_bounds,
                    Means = means,
                    TPC_mean = tpc_ev))
      }
    }
  }
}

#' Wrapper for coda::traceplot()
#'
#' Wrapper for coda::traceplot() that can both directly accept samples of object type `mcmc` or `mcmc.list` as well as an object of class `list` with an element `samples` that is of class `mcmc` or `mcmc.list`
#'
#' @details This is a wrapper to create trace plots using coda's `traceplot` function.
#' @param object Either an object of class `mcmc` or `mcmc.list` OR an object of class `list` which contains an element called `samples` that is of class `mcmc` or `mcmc.list`
#' @param burn Integer, number of samples to discard as burn-in before creating traceplot (default = 0)
#' @param thin Integer, thinning interval used to generate traceplots (default = 1)
#' @param ... additional graphical parameters to be passed as arguments to coda::traceplot
#' @return Returns invisible(NULL) and creates trace plots for MCMC parameters
#' @examples
#' ## need data to set up example here. placeholder for now
#' ## set params and reference temperature set
#' myfun = str2tpc_fun(model = 'gaussian')
#' param_set = c(T.opt = 36, a = 6.5, rmax = 2.75)
#' Temp_ref = seq(from = 5, to = 50, length.out = 1000)
#' plot(Temp_ref, myfun(params = param_set, Temp = Temp_ref), type = 'l')


traceplot <- function(object, burn = 0, thin = 1, ...){
  if (is.list(object)) if (is.null(object$samples)) stop('Object of class list must have element "samples"')
  if (is.list(object)){
    if (is.mcmc(object$samples)){
      N = nrow(object$samples)
      thinned = seq(from = burn+1, to = N, by = thin)
      coda::traceplot(x = as.mcmc(object$samples[thinned,]), ...)
      return(invisible(NULL))
    }
  } else{
    N = nrow(object)
    thinned = seq(from = burn+1, to = N, by = thin)
    coda::traceplot(x = as.mcmc(object[thinned,]), ...)
    return(invisible(NULL))
  }
}

#' Wrapper for IDPmisc::ipairs()
#'
#' Wrapper for IDPmisc() that can both directly accept samples of object type `mcmc` or `matrix` as well as an object of class `list` with an element `samples` that is of class `mcmc` or `matrix`
#'
#' @details This is a wrapper to create trace plots using coda's `traceplot` function.
#' @param object Either an object of class `mcmc` or `matrix` OR an object of class `list` which contains an element called `samples` that is of class `mcmc` or `matrix`
#' @param burn Integer, number of samples to discard as burn-in before creating pairs plot (default = 0)
#' @param thin Integer, thinning interval used to generate pairs plots (default = 1)
#' @param ztransf Function, used to transform the counts per pixel in IDPmisc::ipairs.
#' @param ... additional graphical parameters to be passed as arguments to IDPmisc::ipairs(). For additional information, try ?IDPmisc::ipairs
#' @return Returns invisible(NULL) and creates pairs plots for MCMC parameters
#' @examples
#' ## need data to set up example here. placeholder for now
#' ## set params and reference temperature set
#' myfun = str2tpc_fun(model = 'gaussian')
#' param_set = c(T.opt = 36, a = 6.5, rmax = 2.75)
#' Temp_ref = seq(from = 5, to = 50, length.out = 1000)
#' plot(Temp_ref, myfun(params = param_set, Temp = Temp_ref), type = 'l')

bayesTPC.ipairs <- function(x, burn = 0, thin = 1,
                            ztransf = function(x){x[x<1] <- 1; log2(x)}, ...){
  if (is.list(x)) if(is.null(x$samples)) stop('Expected list "x" to have an element called samples')
  if (!(is.mcmc(x)) & !(is.matrix(x)) & !(is.list(x))) stop('Input "x" is expected as a list, matrix, or mcmc object. See ?bayesTPC.ipairs')
  if (is.list(x)){
    N = nrow(x$samples)
    thinned = seq(from = burn+1, to = N, by = thin)
    if (is.mcmc(x$samples)){
      samples = as.matrix(x$samples)
    } else{
      samples = x$samples
    }
    ipairs(samples[thinned,], ztransf = ztransf, ...)
  } else{
    N = nrow(x)
    thinned = seq(from = burn+1, to = N, by = thin)
    if (is.mcmc(x)){
      samples = as.matrix(x)
    } else{
      samples = x
    }
    ipairs(samples[thinned,], ztransf = ztransf, ...)
  }
  return(invisible(NULL))
}

#' Prior-Posterior Overlap Plot
#'
#' Create a prior-posterior overlap plot from output of the bTPC function
#'
#' @details This is a wrapper to create trace plots using coda's `traceplot` function.
#' @param bTPC.object List, usually output from the `bTPC` function. `ppo_plot` expects this list to have entries "samples", of class `mcmc` or `numeric` and "priors", with class `list` and entries that match the corresponding model parameters
#' @param burn Integer, number of samples to discard as burn-in before creating prior-posterior overlap plot (default = 0)
#' @return Returns invisible(NULL) and creates a prior posterior overlap plot
#' @examples
#' ## need data to set up example here. placeholder for now
#' ## set params and reference temperature set
#' myfun = str2tpc_fun(model = 'gaussian')
#' param_set = c(T.opt = 36, a = 6.5, rmax = 2.75)
#' Temp_ref = seq(from = 5, to = 50, length.out = 1000)
#' plot(Temp_ref, myfun(params = param_set, Temp = Temp_ref), type = 'l')

ppo_plot <- function(bTPC.object, burn = 0){
  if (!is.list(bTPC.object)) stop('Expected input bTPC.object to be a list')
  if (is.null(bTPC.object$samples)) stop('Expected input bTPC.object to have an entry called "samples"')
  if (is.null(bTPC.object$priors)) stop('Expected input bTPC.object to have an entry called "priors"')
  for (i in colnames(bTPC.object$samples)){
    par_samples <- as.numeric(bTPC.object$samples[(burn+1):(nrow(bTPC.object$samples)),i])
    #print(strsplit(bTPC.object$priors[i], '~')[[1]][2])
    prior_exp = strsplit(strsplit(as.character(bTPC.object$priors[i]), '~')[[1]][2], '\\(')
    prior_exp = paste0(prior_exp[[1]][1], '(', 'sort(par_samples),', prior_exp[[1]][2])
    plot(density(par_samples), type = 'l', col = 'red', lwd = 2, xlab = i,
         main = paste0('Prior-Posterior Overlap for ', i))
    points(sort(par_samples), eval(str2expression(prior_exp)), type = 'l', col = 'blue', lwd = 2, lty = 2)
  }
}

#' Thermal performance data on fecundity for
#'
#' Data from a QTL experiment on gravitropism in
#' Arabidopsis, with data on 162 recombinant inbred lines (Ler x
#' Cvi). The outcome is the root tip angle (in degrees) at two-minute
#' increments over eight hours.
#'
#' @docType data
#'
#' @usage data("fecundity_tpc")
#'
#' @format An object of class \code{"cross"}; see \code{\link[qtl]{read.cross}}.
#'
#' @keywords datasets
#'
#' @references Reference here
#'
#'
#' @source reference here
#'
#' @examples
#' data(grav)

#' Thermal performance data on longevity for
#'
#' Data from a QTL experiment on gravitropism in
#' Arabidopsis, with data on 162 recombinant inbred lines (Ler x
#' Cvi). The outcome is the root tip angle (in degrees) at two-minute
#' increments over eight hours.
#'
#' @docType data
#'
#' @usage data('longevity_tpc')
#'
#' @format An object of class \code{"cross"}; see \code{\link[qtl]{read.cross}}.
#'
#' @keywords datasets
#'
#' @references Reference here
#'
#'
#' @source reference here
#'
#' @examples
#' data(grav)
