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
