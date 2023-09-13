# Helper for b_TPC
check_data <- function(data) {
  ## data checks to make sure there are values for Temp and Trait
  if (!is.list(data)) stop("Unexpected class for argument 'data'. Data must be input as a list.")
  if (is.null(unlist(data["Temp"]))) stop("Data list must have a variable called 'Temp'. Perhaps check spelling and capitalization?")
  if (is.null(unlist(data["Trait"]))) stop("Data list must have a variable called 'Trait'. Perhaps check spelling and capitalization?")
  if (!is.numeric(unlist(data["Trait"]))) stop("List elements of data must be numeric vectors.")
  if (!is.numeric(unlist(data["Temp"]))) stop("List elements of data must be numeric vectors.")
  if (length(data$Trait) != length(data$Temp)) stop("'Temp' and 'Trait' must have the same length.")
  if (any(is.na(data$Temp))) stop("Temperature data contains missing values.")
  if (any(is.na(data$Trait))) stop("Trait data contains missing values.")
  ## warnings for when temperature may be in F instead of C
  if (any(data$Temp > 50)) warning("Unusual (Temp>50) temperature values detected (are Temps given in Celcius?)")
  if (any(data$Temp < 0)) warning("Unusual (Temp<0) temperature values detected (are Temps given in Celcius?)")

  return(list(data = data, N = length(unlist(data["Trait"]))))
}

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
#'  The default priors and constant values are chosen to be as flexible as possible.
#'
#'  Both the model interface and the MCMC object are compiled by NIMBLE. Progress is printed for clarity's sake.
#'  Users seeking to interact with the fitted model should use `comp_model`.
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
#'  * `model_spec` -  `btpc_model` containing the model specification being fit.
#'  * `constants` - A named vector containing the constant values used, if the model includes constants. Otherwise, returns NULL.
#'  * `uncomp_model` - Uncompiled version of the NIMBLE model. For internal use.
#'  * `comp_model` - Compiled version of the NIMBLE model.
#'  * `MAP_parameters` - The parameters of the highest density MCMC sample.
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
  if (is.null(model) || !(model %in% model_list)) {
    if ("btpc_model" %in% class(model)) {
      stop("Model has been specified incorrectly. Please use specify_*_model() to create custom models.")
    } else {
      stop("Unsupported model. Use get_models() to view implemented models.")
    }
  }

  if (!(samplerType %in% c("RW", "RW_block", "AF_slice", "slice"))) {
    stop("Unsupported option for input samplerType. Currently only RW, RW_block, slice, and AF_slice are supported.")
  }

  data.nimble <- check_data(data)


  # prep for cleanup
  original_environmental_objects <- force(ls(envir = .GlobalEnv))
  original_verbose_option <- nimble::getNimbleOption("verbose")
  user_verbose <- verbose # avoid quoting mishaps
  nimble::nimbleOptions(verbose = user_verbose)

  # removes superflous environmental objects
  on.exit({
    # remove random objects from global environment
    rm(list = base::setdiff(ls(envir = .GlobalEnv), original_environmental_objects), envir = .GlobalEnv)
    # set verbose option back to what nimble had
    nimble::nimbleOptions(verbose = original_verbose_option)
  })

  # create model specification
  if (!("btpc_model" %in% class(model))) {
    model <- model_list[[model]]
  }

  # misc error check (will refactor this when possible)
  if ("btpc_binomial" %in% class(model)) {
    if (is.null(data$n)) stop("For a Binomial GLM, data list must have a variable called 'n'. Perhaps check spelling and capitalization?")
    # const.list$n = unlist(data['n'])
  }

  # change priors if necessary - this will become a helper function for b_TPC and configure_model
  if (!is.null(priors)) {
    if (!is.list(priors)) stop("Unexpected type for argument 'priors'. Priors must be given as a list.")
    if (length(priors) == 0) {
      stop("Prior list cannot be empty. To use default priors, use priors = NULL.")
    }
    if (is.null(names(priors))) {
      stop("Prior list must be named.")
    }

    model <- change_priors(model, unlist(priors))
  }

  # change constants if necessary
  if (!is.null(constants)) {
    if (!is.list(constants)) stop("Unexpected type for argument 'constants'. Contants must be given as a list.")
    if (length(constants) == 0) {
      stop("Constant list cannot be empty. To use default priors, use priors = NULL.")
    }
    if (is.null(names(constants))) {
      stop("Constant list must be named.")
    }

    model <- change_constants(model, unlist(constants))
  }

  cat(cli::style_underline(cli::col_cyan("Creating NIMBLE model:\n")))
  if (!verbose) {
    cat(" - Configuring model.\n")
  }
  # handles the density function, priors, and constants
  inits.list <- .check_inits(inits)
  const.list <- .configure_constants(model, data.nimble$N)
  modelStr <- configure_model(model)

  # create uncompiled nimble model
  # TODO odd things with importing functions from nimble ???? fix at some point

  nimTPCmod <- nimble::nimbleModel(str2expression(modelStr),
    constants = const.list,
    data = data.nimble$data, inits = inits.list
  )

  if (!verbose) {
    cat(" - Compiling model.\n")
  }
  nimTPCmod_compiled <- nimble::compileNimble(nimTPCmod)

  cat(cli::style_underline(cli::col_cyan("\nCreating MCMC:\n")))

  if (!verbose) {
    cat(" - Configuring MCMC.\n")
  }
  # no printing because sampler hasn't changed yet, can be confusing
  mcmcConfig <- nimble::configureMCMC(nimTPCmod, print = F)

  # set correct sampler type (will also refactor this)
  if ("btpc_normal" %in% class(model)) { # TODO refactor this with generics
    bugs_params <- c(names(attr(model, "parameters")), "sigma.sq")
  } else if ("btpc_gamma" %in% class(model)) {
    bugs_params <- c(names(attr(model, "parameters")), "shape_par")
  } else {
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


  mcmcConfig$enableWAIC <- TRUE
  mcmc <- nimble::buildMCMC(mcmcConfig)
  if (!verbose) {
    cat(" - Compiling MCMC.\n")
  }
  tpc_mcmc <- nimble::compileNimble(mcmc, project = nimTPCmod_compiled)
  if (!verbose) {
    cat(" - Running MCMC.\n")
    Sys.sleep(.25)
    cat(cli::style_underline(cli::col_cyan("\nProgress:\n")))
  }
  tpc_mcmc$run(niter, nburnin = burn, ...)
  samples <- coda::as.mcmc(as.matrix(tpc_mcmc$mvSamples)) # TODO theres a way to do this in nimble

  prior_out <- attr(model, "parameters")

  if ("btpc_normal" %in% class(model)) { # TODO remove hard-coding
    prior_out["sigma.sq"] <- attr(model, "sigma.sq")
  }
  if ("btpc_gamma" %in% class(model)) {
    prior_out["shape_par"] <- attr(model, "shape_par")
  }

  cat(cli::style_underline(cli::col_cyan("\nConfiguring Output:\n")))

  out <- list(
    samples = samples, mcmc = tpc_mcmc, data = data.nimble$data,
    model_spec = model, priors = prior_out, constants = attr(model, "constants"),
    uncomp_model = nimTPCmod, comp_model = nimTPCmod_compiled
  )
  class(out) <- "btpc_MCMC"

  cat(" - Finding Max. a Post. parameters.\n")
  out$MAP_parameters <- do_map(out)

  return(out)
}

#' @import nimble
#' @import methods
nimMAP <- nimble::nimbleFunction(
  setup = function(fit) {
    # I want to do some crazy hackery to compile this on startup
    # rather than every time it's run, but I do not know how nimble does this well enough
    # maybe won't make the 1.0.0 release but maybe a 1.1 or 1.2
    # maybe contact the nimble developers
    model <- fit$uncomp_model # has to be uncompiled for some reason
    spl <- as.matrix(fit$samples)
    pars <- colnames(spl)
    b_pars <- numeric(length(pars))
    b_lp <- -Inf
    n <- nrow(spl)
  },
  run = function() {
    for (i in 1:n) {
      values(model, pars) <<- spl[i, ]
      lp <- model$calculate()
      if (lp > b_lp) { # naive algorithm
        b_lp <<- lp
        b_pars <<- spl[i, ]
      }
    }
    returnType(double(1))
    return(c(b_pars, b_lp))
  }
)

do_map <- function(fit) {
  # wrapper for nimMAP()
  com <- nimble::compileNimble(nimMAP(fit))
  out <- com$run()
  names(out) <- c(colnames(fit$samples), "log_prob")
  out
}

#' Find Maximum A Posteriori Estimate.
#'
#' @param fit `btpc_MCMC`, output from [b_TPC()]
#'
#' @return `MAP_estimate` returns the parameters of the sample with the highest posterior density.
#' @export
MAP_estimate <- function(fit) {
  if (!"btpc_MCMC" %in% class(fit)) stop("Unexpected type for parameter 'fit'. Only use this method with the output of b_TPC().")
  fit$MAP_parameters
}
