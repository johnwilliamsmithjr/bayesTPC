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
