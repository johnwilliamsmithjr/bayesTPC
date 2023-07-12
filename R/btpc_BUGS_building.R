.loop_string <- function(model) {
  UseMethod(".loop_string")
}

#' @export
.loop_string.btpc_binomial_model <- function(model) {
  model_string <- paste0(
    "{\n    for (i in 1:N){\n            ",
    "Trait[i] ~ dbinom(p[i], n[i])\n            ",
    "logit(p[i]) <- ", gsub("Temp", "Temp[i]", attr(model, "formula")),
    "\n    }\n"
  )

  return(model_string)
}

#' @export
.loop_string.btpc_normal_model <- function(model) {
  model_string <- paste0(
    "{\n    for (i in 1:N){\n            ",
    "Trait[i] ~ T(dnorm(mean = ",
    gsub("Temp", "Temp[i]", attr(model, "formula")),
    ", var = sigma.sq), 0, )\n    }\n"
  )
}
#' @export
.loop_string.default <- function(model) {
  stop("Misconfigured Model Specification.")
}



.priors_string <- function(model) {
  UseMethod(".priors_string")
}

#' @export
.priors_string.btpc_binomial_model <- function(model) {
  priors <- attr(model, "parameters")
  num_params <- length(priors)

  priors_vec <- paste0(names(priors), " ~ ", priors)
  priors_string <- paste0("    ", paste0(priors_vec, collapse = "\n    "))
  return(priors_string)
}

#' @export
.priors_string.btpc_normal_model <- function(model) {
  priors <- attr(model, "parameters")
  sig <- attr(model, "sigma.sq")
  num_params <- length(priors)

  priors_vec <- paste0(names(priors), " ~ ", priors)
  priors_string <- paste0("    ", paste0(priors_vec, collapse = "\n    "), "\n    sigma.sq ~ ", sig)
}

#' @export
.priors_string.default <- function(model) {
  stop("Misconfigured Model Specification.")
}

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

.configure_constants <- function(model, N) {
  constants <- attr(model, "constants")
  const.list <- vector("list", 0)
  const.list$N <- N

  if (length(constants) == 0) {
    return(const.list)
  } else {
    eval(str2expression(paste0("const.list$", names(constants), " <- ", constants)))
    return(const.list)
  }
}

#' Create model string
#'
#' Create model string for thermal performance curve model to be passed to nimble.
#'
#' @export
#' @details This function returns a character string of the full `nimble` model for a user-specified thermal performance curve and prior distributions.
#' @param model A string specifying the model name, or a btpc_model object.
#'  If a string is provided, the default model formula is provided if the model is implemented. If the model is not implemented, an error occurs.
#'  Use [get_models()] to view all options.
#' @param priors list, optional input specifying prior distributions for parameters (default = NULL).
#'  Elements of the list should correspond to model parameters, and written using nimble logic.
#'  For parameters not specified in the list, default priors are used.
#' @param constants list, optional input specifying model constants.
#'  If model requires constant values and none are provided, default values are used.
#' @param verbose optional logical. If verbose = TRUE, messages are printed when user-end priors are used, rather than the default values. Default is TRUE.
#' @return character, character string specifying the default model formulation to be passed to `nimble`.
#' @examples
#' ## Print default model for briere
#' cat(configure_model(model = "briere"))
#'
#' ## Use custom prior for 'q' parameter in quadratic curve
#' my_prior <- list(q = "q~beta(.5, .5)")
#' cat(configure_model(model = "quadratic", priors = my_prior))
configure_model <- function(model, priors = NULL, constants = NULL, verbose = T) {
  if (!("btpc_model" %in% class(model))) {
    if (!(model %in% model_list)) {
      stop("Unsupported model, use get_models() to view implemented models.")
    }
    model <- model_list[[model]]
  }

  # change priors if necessary
  if (!is.null(priors)) {
    if (!is.list(priors)) stop("Unexpected type for argument 'priors'. Priors must be given as a list.")
    if (is.null(names(priors))) {
      stop("Prior list cannot be empty. To use default priors, use priors = NULL.")
    }
    if (verbose) {
      warning("At least one user-defined prior being used.")
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

  if ("btpc_model" %in% class(model)) {
    loop <- .loop_string(model)
    pri <- .priors_string(model)
  } else {
    model <- model_list[[model]]
    loop <- .loop_string(model)
    pri <- .priors_string(model)
  }


  nimble_string <- paste0(loop, pri, "\n}")
  return(nimble_string)
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
#' @param verbose logical, determines whether to print additional information, Default is TRUE.
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
                  niter = 10000, inits = NULL, burn = 0, constants = NULL, verbose = TRUE, ...) {
  # exception handling and variable setup
  if (is.null(model) || ((!(model %in% model_list)) && !("btpc_model" %in% class(model)))) {
    stop("Unsupported model, use get_models() to view implemented models.")
  }

  original_environmental_objects <- force(ls(envir = .GlobalEnv))
  data.nimble <- check_data(data)

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
  inits.list <- .check_inits(inits)
  const.list <- .configure_constants(model, data.nimble$N)
  modelStr <- configure_model(model)



  # create uncompiled nimble model
  # TODO get verbose to interact with this.
  cat("Creating NIMBLE model:\n")
  nimTPCmod <- nimble::nimbleModel(str2expression(modelStr),
    constants = const.list,
    data = data.nimble$data, inits = inits.list,
    where = environment(),
    calculate = T
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
    cat("===== Monitors ===== \n")
    mcmcConfig$printMonitors()
    cat("===== Samplers ===== \n")
    mcmcConfig$printSamplers(byType = T)
  }

  cat("\nRunning MCMC:\n")
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
  # TODO create an S3 class for a finished MCMC, so we can do fun front end stuff with it
  return(list(
    samples = samples, mcmc = tpc_mcmc, data = data.nimble$data,
    model_type = c(model), priors = prior_out, constants = attr(model, "constants"), uncomp_model = nimTPCmod
  ))
}
