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


alt_ratkowsky_tpc <- function(a,b,c,T.min,T.max, Temp, posteriorPredictive = FALSE){

  if (posteriorPredictive == FALSE){
    curve = (Temp > T.min)*(T.max > Temp)*((a*(Temp - T.min))*(1 - exp(b*(Temp - T.max))))^2
  } else{
    sigma.sq = params['sigma.sq']

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

alt_stinner_tpc <- function(C,k1,k2,T.opt, Temp, posteriorPredictive = FALSE){

  if (posteriorPredictive == FALSE){
    curve = C / (1 + exp(k1 + k2*(T.opt - abs(T.opt - Temp))))
  } else{
    sigma.sq = params['sigma.sq']

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


alt_kamykowski_tpc <- function(a,b,c,T.min,T.max, Temp, posteriorPredictive = FALSE){

  if (posteriorPredictive == FALSE){
    curve = (Temp > T.min)*(T.max > Temp)*a*(1 - exp(-b*(Temp - T.min)))*(1 - exp(-c*(T.max - Temp)))
  } else{
    sigma.sq = params['sigma.sq']

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


alt_pawar_shsch_tpc <- function(T.opt,e,e_h,r_tref, Temp, T.ref, posteriorPredictive = FALSE){

  if (posteriorPredictive == FALSE){
    curve = (e_h > e)*r_tref*exp((e/(8.62e-05))*((1/(T.ref+273.15)) - (1/(Temp + 273.15)))) / (1 + (e / (e_h-e)) * exp((e_h/(8.62e-05))*(1/(T.opt + 273.15) - 1/(Temp + 273.15))))
  } else{
    sigma.sq = params['sigma.sq']

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

alt_quadratic_tpc <- function(q,T.min,T.max, Temp, posteriorPredictive = FALSE){

  if (posteriorPredictive == FALSE){
    curve = -1*q*(Temp - T.min)*(Temp - T.max) * (T.max > Temp) * (Temp > T.min)
  } else{
    sigma.sq = params['sigma.sq']

    truncmeans = -1*q*(Temp - T.min)*(Temp - T.max) * (T.max > Temp) * (Temp > T.min)
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

alt_briere_tpc <- function(q,T.min,T.max, Temp, posteriorPredictive = FALSE){

  if (posteriorPredictive == FALSE){
    curve = q*(Temp - T.min)*sqrt((T.max>Temp)*abs(T.max-Temp)) * (T.max > Temp) * (Temp > T.min)
  } else{
    sigma.sq = params['sigma.sq']

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

alt_weibull_tpc <- function(a,b,c,T.opt, Temp, posteriorPredictive = FALSE){

  if (posteriorPredictive == FALSE){
    curve = ((a*(((c-1)/c)^((1-c)/c))*((((Temp-T.opt)/b)+(((c-1)/c)^(1/c)))^(c-1))*(exp(-((((Temp-T.opt)/b)+(((c-1)/c)^(1/c)))^c)+((c-1)/c)))))

  } else{
    sigma.sq = params['sigma.sq']

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


alt_gauss_tpc <- function(a,rmax,T.opt, Temp, posteriorPredictive = FALSE){

  if (posteriorPredictive == FALSE){
    curve = rmax*exp(-0.5*(abs(Temp - T.opt)/a)^2)
  } else{
    sigma.sq = params['sigma.sq']

    truncmeans = rmax*exp(-0.5*(abs(Temp - T.opt)/a)^2)
    curve = rtruncnorm(length(Temp), a = 0, b = Inf, mean = truncmeans, sd = sqrt(sigma.sq))
  }
  return(curve)
}

