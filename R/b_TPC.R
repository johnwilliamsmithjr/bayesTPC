
# Helper for b_TPC
.check_inits <- function(inits) {
  if (!is.null(inits)) {
    if (length(inits) == 0) stop("inits list cannot be empty. Use 'inits = NULL' to sample initial values from the priors.")
    if (!is.list(inits)) stop("Unexpected type for argument 'inits'. Initial values must be given as a list.")
    if (is.null(names(inits))) stop("'inits' list must be named.")

    return(inits)
  } else {
    return(list())
  }
}

# Helper for b_TPC
.configure_constants <- function(model, N) {
  constants <- attr(model, "constants")
  const.list <- vector("list", 0)
  const.list$N <- N

  if (length(constants) == 0) {
    return(const.list)
  } else {
    const.list[names(constants)] <- constants
    return(const.list)
  }
}


#' Perform MCMC
#'
#' Generates NIMBLE model, and performs MCMC.
#'
#' @export
#' @details Behind the scenes, this function configures the necessary components to generate a BUGS model using NIMBLE.
#' The default priors and constant values are chosen to be as flexible as possible.
#'
#' Both the model specification and the MCMC object are compiled by NIMBLE. Progress is printed for clarity's sake.
#' @param data list, with expected entries "Trait" (corresponding to the trait being modeled by the thermal performance curve)
#'  and "Temp" (corresponding to the temperature in Celsius that the trait is being measured at).
#' @param model A string specifying the model name, or a btpc_model object.
#'  If a string is provided, the default model formula is provided if the model is implemented. If the model is not implemented, an error occurs.
#'  Use [get_models()] to view all options.
#' @param priors list, optional input specifying prior distributions for parameters (default = NULL).
#'  Elements of the list should correspond to model parameters,
#'  and written using NIMBLE logic. For parameters not specified in the list, default priors are used.
#'  Use [get_default_priors()] to view default values.
#' @param samplerType character string, specifying the sampling method used during the MCMC.
#'  Currently, supported options are:
#'  * Random Walk Metropolis ('RW')
#'  * Blocked Random Walk Metropolis ('RW_block')
#'  * Automated Factor Slice Sampling ('AF_slice')
#'  * Slice sampling ('slice')
#' @param niter integer, number of MCMC iterations to perform (default is niter = 10000).
#' @param inits optional list, initial parameter values to be provided to nimble MCMC.
#' @param burn optional integer, number of initial MCMC iterations to be discarded as burn-in. Default is burn = 0.
#' @param constants optional list, constants to be provided to model. If constants are needed and not provided, constant values are used.
#'  Currently only used for model = 'pawar-shsch'.
#' @param verbose logical, determines whether to print additional information, Default is FALSE.
#' @param ... Additional parameters to be passed to nimble during MCMC configuration and sampling.
#' @return `b_TPC` returns a list containing entries:
#'  * `samples` - `mcmc.list` containing posterior samples of parameters for corresponding model.
#'  * `mcmc` -  `nimbleModel` object corresponding to model being fit.
#'  * `data` -  `list` containing trait and temperature data and number of observations (N).
#'  * `model_type` -  `character` containing the type of thermal performance curve being fit.
#'  * `constants` - A named vector containing the constant values used, if the model includes constants. Otherwise, returns NULL.
#'  * `uncomp_model` - Uncompiled version of the NIMBLE model. For internal use.
#' @examples
#' library(nimble)
#' # simulate data
#' N <- 16
#' q <- .75
#' T_min <- 10
#' T_max <- 35
#' Temps <- rep(c(15, 20, 25, 30), N / 4)
#' Traits <- rep(0, N)
#' for (i in 1:N) {
#'   while (Traits[i] <= 0) {
#'     Traits[i] <- rnorm(
#'       1,
#'       -1 * q * (Temps[i] - T_max) * (Temps[i] - T_min) * (Temps[i] > T_min) * (Temps[i] < T_max), 2
#'     )
#'   }
#' }
#' trait_list <- list(Trait = Traits, Temp = Temps)
#'
#' # create model
#' \dontrun{
#' quadratic_model <- b_TPC(data = trait_list, model = "quadratic")
#' quadratic_model <- b_TPC(
#'   data = trait_list, model = "quadratic", niter = 8000,
#'   inits = list(T_min = 15, T_max = 30),
#'   priors = list(q = "dunif(0, .5)", sigma.sq = "dexp(1)")
#' )
#' }
b_TPC <- function(data, model, priors = NULL, samplerType = "RW",
                  niter = 10000, inits = NULL, burn = 0, constants = NULL, verbose = FALSE, ...) {
  # exception handling and variable setup
  if (is.null(model) || ((!(model %in% model_list)) && !("btpc_model" %in% class(model)))) {
    stop("Unsupported model, use get_models() to view implemented models.")
  }

  data.nimble <- check_data(data)
  inits.list <- .check_inits(inits)
  const.list <- .configure_constants(model, data.nimble$N)

  # no change of state! we are functional programmers!!!!!!
  original_environmental_objects <- force(ls(envir = .GlobalEnv))
  original_verbose_option <- nimble::getNimbleOption("verbose")
  user_verbose <- verbose # avoid quoting mishaps
  nimble::nimbleOptions(verbose = user_verbose)


  if (!(samplerType %in% c("RW", "RW_block", "AF_slice", "slice"))) stop("Unsupported option for input samplerType. Currently only RW, RW_block, slice, and AF_slice are supported.")
  if ("btpc_binomial_model" %in% class(model)) {
    if (is.null(unlist(data["n"]))) stop("For a Binomial GLM, data list must have a variable called 'n'. Perhaps check spelling and capitalization?")
    # const.list$n = unlist(data['n'])
  }

  # create model specification
  if (!("btpc_model" %in% class(model))) {
    model <- model_list[[model]]
  }

  # change priors if necessary
  if (!is.null(priors)) {
    if (!is.list(priors)) stop("Unexpected type for argument 'priors'. Priors must be given as a list.")
    if (is.null(names(priors))) {
      stop("Prior list cannot be empty. To use default priors, use priors = NULL.")
    }

    model <- change_priors(model, unlist(priors))
  }

  # change constants if necessary
  if (!is.null(constants)) {
    if (!is.list(constants)) stop("Unexpected type for argument 'constants'. Contantss must be given as a list.")
    if (is.null(names(constants))) {
      stop("Constant list cannot be empty. To use default constants, use constants = NULL.")
    }

    model <- change_constants(model, unlist(constants))
  }

  # configure model, handles the density funciton, priors, and constants
  modelStr <- configure_model(model)



  # create uncompiled nimble model
  # TODO odd things with importing functions from nimble ???? fix at some point
  cat("Creating NIMBLE model.\n")
  nimTPCmod <- nimble::nimbleModel(str2expression(modelStr),
    constants = const.list,
    data = data.nimble$data, inits = inits.list,
    check = nimble::getNimbleOption("checkModel"),
    buildDerivs = nimble::getNimbleOption("buildModelDerivs")
  )

  nimTPCmod_compiled <- nimble::compileNimble(nimTPCmod)

  # no printing because sampler hasn't changed yet, can be confusing
  mcmcConfig <- nimble::configureMCMC(nimTPCmod, print = F)

  # set correct sampler type
  if ("btpc_normal_model" %in% class(model)) {
    bugs_params <- c(names(attr(model, "parameters")), "sigma.sq")
  } else if ("btpc_binomial_model" %in% class(model)) {
    bugs_params <- names(attr(model, "parameters"))
  }
  if (samplerType == "slice") { # TODO make this less weird. There was a reason it was set up this way i just cannot remember.
    for (i in bugs_params) {
      mcmcConfig$removeSamplers(i)
      mcmcConfig$addSampler(i, type = samplerType)
    }
  } else if (samplerType != "RW") {
    mcmcConfig$removeSamplers(bugs_params)
    mcmcConfig$addSampler(bugs_params, type = samplerType)
  }

  if (verbose) {
    # Manually Printing, see mcmcConfig
    cat("\n===== Monitors ===== \n")
    mcmcConfig$printMonitors()
    cat("===== Samplers ===== \n")
    mcmcConfig$printSamplers(byType = T)
    cat("\n")
  }

  cat("Running MCMC.\n")
  mcmcConfig$enableWAIC <- TRUE
  mcmc <- nimble::buildMCMC(mcmcConfig)
  tpc_mcmc <- nimble::compileNimble(mcmc, project = nimTPCmod_compiled)
  tpc_mcmc$run(niter, nburnin = burn, ...)
  samples <- coda::as.mcmc(as.matrix(tpc_mcmc$mvSamples))

  prior_out <- attr(model, "parameters")

  if ("btpc_normal_model" %in% class(model)) {
    prior_out["sigma.sq"] <- attr(model, "sigma.sq")
  }


  # remove random objects from global environment
  rm(list = base::setdiff(ls(envir = .GlobalEnv), original_environmental_objects), envir = .GlobalEnv)
  # set verbose option back to what nimble had
  nimble::nimbleOptions(verbose = original_verbose_option)
  out <- list(
    samples = samples, mcmc = tpc_mcmc, data = data.nimble$data,
    model_type = c(model), priors = prior_out, constants = attr(model, "constants"), uncomp_model = nimTPCmod
  )

  class(out) <- "btpc_MCMC"
  return(out)
}
