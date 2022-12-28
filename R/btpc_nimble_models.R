#' Evaluate Ratkowsky model in Nimble
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

ratkowsky_tpc_nimble <-
  nimbleRcall(prototype = function(params = double(1),
                                   Temp = double(0),
                                   posteriorPredictive = logical(0)){},
              Rfun = 'ratkowsky_tpc',
              returnType = double(0))

#' Evaluate Stinner model in Nimble
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

stinner_tpc_nimble <-
  nimbleRcall(prototype = function(params = double(1),
                                   Temp = double(0),
                                   posteriorPredictive = logical(0)){},
              Rfun = 'stinner_tpc',
              returnType = double(0))

#' Evaluate Kamykowski model in Nimble
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

kamykowski_tpc_nimble <-
  nimbleRcall(prototype = function(params = double(1),
                                   Temp = double(0),
                                   posteriorPredictive = logical(0)){},
              Rfun = 'kamykowski_tpc',
              returnType = double(0))

#' Evaluate Schoolfield-Sharpe model in Nimble
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

pawar_shsch_tpc_nimble <-
  nimbleRcall(prototype = function(params = double(1),
                                   Temp = double(0),
                                   posteriorPredictive = logical(0)){},
              Rfun = 'pawar_shsch_tpc',
              returnType = double(0))

#' Evaluate Quadratic TPC model in Nimble
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

quadratic_tpc_nimble <-
  nimbleRcall(prototype = function(params = double(1),
                                   Temp = double(0),
                                   posteriorPredictive = logical(0)){},
              Rfun = 'quadratic_tpc',
              returnType = double(0))

#' Evaluate Briere TPC model in Nimble
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

briere_tpc_nimble <-
  nimbleRcall(prototype = function(params = double(1),
                                   Temp = double(0),
                                   posteriorPredictive = logical(0)){},
              Rfun = 'briere_tpc',
              returnType = double(0))

#' Evaluate Weibull TPC model in Nimble
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

weibull_tpc_nimble <-
  nimbleRcall(prototype = function(params = double(1),
                                   Temp = double(0),
                                   posteriorPredictive = logical(0)){},
              Rfun = 'weibull_tpc',
              returnType = double(0))

#' Evaluate Gaussian TPC model in Nimble
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

gauss_tpc_nimble <-
  nimbleRcall(prototype = function(params = double(1),
                                   Temp = double(0),
                                   posteriorPredictive = logical(0)){},
              Rfun = 'gauss_tpc',
              returnType = double(0))

