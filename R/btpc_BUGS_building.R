.loop_string <- function(model) {
  model_info <-
    model_master_list[model_master_list$name == model, ]
  # TODO clean this, possible into helper functions
  if (is.null(model_info$constants[[1]]) | length(model_info$constants[[1]]) <= 1) {
    if (model_info$density_function == "normal") {
      model_string <- paste0(
        "{\n    for (i in 1:N){\n            ",
        "Trait[i] ~ T(dnorm(mean = nimble_mod_function(params[1:",
        length(model_info[[2]][[1]]),
        "], Temp[i], constants[1:2]), var = sigma.sq), 0, )\n    }\n"
      )
    } else if (model_info$density_function == "binomial") {
      model_string <- paste0(
        "{\n    for (i in 1:N){\n            ",
        "Trait[i] ~ dbinom(p[i], n[i])\n            ",
        "logit(p[i]) <- nimble_mod_function(params[1:",
        length(model_info[[2]][[1]]),
        "], Temp[i], constants[1:2])\n    }\n"
      )
    } else {
      stop("Unexpected density Function in model specification")
    }
  } else {
    if (model_info$density_function == "normal") {
      model_string <- paste0(
        "{\n    for (i in 1:N){\n            ",
        "Trait[i] ~ T(dnorm(mean = nimble_mod_function(params[1:",
        length(model_info[[2]][[1]]),
        "], Temp[i], constants[1:", length(model_info[[5]][[1]]),
        "]), var = sigma.sq), 0, )\n    }\n"
      )
    } else if (model_info$density_function == "binomial") {
      model_string <- paste0(
        "{\n    for (i in 1:N){\n            ",
        "Trait[i] ~ dbinom(p[i], n[i])\n            ",
        "logit(p[i]) <- nimble_mod_function(params[1:",
        length(model_info[[2]][[1]]),
        "], Temp[i], constants[1:", length(model_info[[5]][[1]]), "])\n    }\n"
      )
    } else {
      stop("Unexpected density Function in model specification")
    }
  }
  return(model_string)
}

.priors_string <- function(model,
                          priors = NULL,
                          verbose = TRUE) {
  model_info <-
    model_master_list[model_master_list$name == model, ]
  num_params <- length(model_info$params[[1]])

  if (!is.null(priors)) {
    if (!is.list(priors)) stop("Unexpected type for argument 'priors'. Priors must be given as a list.")
    if (is.null(names(priors))) {
      stop("Prior list cannot be empty. To use default priors use priors = NULL.")
    }
  }

  ## check for proper names in lists
  param_names <- model_info$params[[1]]


  param_string <- vector("character", num_params + 1)
  for (i in 1:num_params) {
    if (param_names[i] %in% names(priors)) {
      if (verbose) {
        simpleMessage(paste0(
          "Not using default prior for parameter '",
          param_names[i],
          "'. To suppress these messages use verbose = FALSE\n"
        ))
      }
      param_string[i] <- paste0(
        "params[", i, "] ~ ", priors[[param_names[[i]]]]
      )
    } else {
      param_string[i] <- paste0(
        "params[", i, "] ~ ", model_info$default_priors[[1]][i]
      )
    }
  }

  if ("sigma.sq" %in% names(priors)) {
    if (verbose) {
      simpleMessage(paste0("Not using default prior for parameter 'sigma.sq'. To suppress these messages use verbose = FALSE\n"))
    }
    param_string[num_params + 1] <-
      paste0("sigma.sq ~ ", priors["sigma.sq"])
  } else {
    param_string[num_params + 1] <-
      "sigma.sq ~ T(dt(mu = 0, tau = 1/10, df = 1), 0, )"
  }


  return(param_string)
}

.configure_inits <- function(inits, model_params) {
  inits.list <- vector("list", 2)

  if (!is.null(inits)) {
    if (length(inits) == 0) stop("inits list cannot be empty. Use 'inits = NULL' to sample initial values from the priors.")
    if (!is.list(inits)) stop("Unexpected type for argument 'inits'. Initial values must be given as a list.")
    if (is.null(names(inits))) stop("inits list must be named.")

    init_params <- vector("double", length(model_params))
    for (i in 1:length(model_params)) {
      if (model_params[[i]] %in% names(inits)) {
        init_params[i] <- inits[[model_params[[i]]]]
      } else {
        init_params[i] <- NA
      }
    }

    inits.list[[1]] <- init_params
    if ("sigma.sq" %in% names(inits)) {
      inits.list[[2]] <- inits[["sigma.sq"]]
      names(inits.list) <- c("params", "sigma.sq")
    } else {
      inits.list[[2]] <- NULL
      names(inits.list) <- c("params")
    }
  } else {
    inits.list <- list()
  }

  return(inits.list)
}

.configure_constants <- function(model_info, N, model_constants, constant_list) {
  model <- model_info$name[[1]]
  constants <- vector("double", 0)
  const.list <- vector("list", 0)
  const.list$N <- N

  if (length(model_constants) > 0) {
    # possibly turn this into a helper function
    if (is.null(constant_list)) {
      warning(paste0(
        "Constant list not found for model = '", model,
        "'. Setting default value for all constants.\n"
      ))
      constants <- c(model_info$default_constants[[1]], -1)
      const.list$constants <- constants
    } else {
      if (!is.list(constant_list)) {
        stop("If constants are provided, they must be formatted as a list\n")
      }

      sorted_constants <- sort(model_constants)
      for (i in 1:length(model_constants)) {
        if (sorted_constants[i] %in% names(constant_list)) {
          constants[i] <- constant_list[[sorted_constants[i]]]
        } else {
          warning(
            "Constant '", sorted_constants[i],
            "' not provided. Using default value '",
            sorted_constants[i], "' = ", model_info$default_constants[[1]][i], "."
          )
          constants[i] <- model_info$default_constants[[1]][[i]]
        }
      }

      if (length(constant_list) == 1) {
        # to ensure nimble sees this as a vector and not a scalar
        constants[length(constant_list) + 1] <- -1
        const.list$constants <- constants
      }
    }
  } else {
    constants[1:2] <- -1
    const.list$constants <- constants
  }

  return(const.list)
}

#' Create model string
#'
#' Create model string for thermal performance curve model to be passed to nimble
#'
#' @export
#' @details This function returns a character string of the full `nimble` model for a user-specified thermal performance curve and prior distributions
#' @param model A string specifying the model name. Use [get_model_names()] to view all options.
#' @param priors list, optional input specifying prior distributions for parameters (default = NULL). Elements of the list should correspond to model parameters, and written using nimble logic. For parameters not specified in the list, default priors are used.
#' @param constants list, optional input specifying model constants. If model requires constant values and none are provided, default values are used.
#' @param verbose logical, optional input. If verbose = TRUE, messages are printed when for priors that deviate from the defaultPriors(model) (see ?defaultPriors for additional information). Default = TRUE
#' @return character, character string specifying the default model formulation to be passed to `nimble`.
#' @examples
#' ## Print default model for briere
#' cat(configure_model(model = "briere"))
#'
#' ## Use custom prior for 'q' parameter in quadratic curve
#' my_prior <- list(q = "q~beta(.5, .5)")
#' cat(configure_model(model = "quadratic", priors = my_prior))
configure_model <- function(model, priors = NULL, constants = NULL, verbose = TRUE) {
  model_info <-
    model_master_list[model_master_list$name == model, ]
  num_params <- length(model_info[[2]][[1]])

  if (!is.null(priors)) {
    if (!is.list(priors)) stop("Unexpected type for argument 'priors'. Priors must be given as a list.")
    if (is.null(names(priors))) {
      stop("Prior list cannot be empty. To use default priors use priors = NULL.")
    }
  }
  ## check for proper names in prior list
  # should move this error check to .priors_string
  param_names <- model_info$params[[1]]
  if (any(!(names(priors) %in% c(param_names, "sigma.sq")))) {
    stop("One or more priors do not have names that correspond to model parameters.")
  }

  loop <- .loop_string(model)
  pri <- .priors_string(model, priors, verbose)

  pri_string <- paste0(pri, collapse = "\n    ")

  nimble_string <- paste0(loop, "    ", pri_string, "\n}")
  return(nimble_string)
}

#' Perform MCMC
#'
#' Generate `nimble` model, perform sampling using MCMC
#'
#' @details This function returns a list, containing entries: `samples` - object of class `mcmc.list` containing posterior samples of parameters for corresponding model; `model` - object of class `nimbleModel` containing `nimble` model object corresponding to model being fit; `data` - object of class `list` containing trait and temperature data and number of observations (N); `modelType` - object of class `character` containing the type of thermal performance curve being fit, `uncomp_model` - uncompiled model used during prior evaluation.
#' @export
#' @param data list, with expected entries "Trait" (corresponding to the trait being modeled by the thermal performance curve) and "Temp" (corresponding to the temperature in Celcius that the trait is being measured at).
#' @param model character, name of thermal performance curve model. Currently, supported options include "quadratic", "briere", "gaussian", "weibull", "pawar-shsch", "lactin2", "kamykowski", "ratkowsky", "binomial_glm_lin", "binomial_glm_quad", and "stinner".
#' @param priors list, optional input specifying prior distributions for parameters (default = NULL). Elements of the list should correspond to model parameters, and written using nimble logic. For parameters not specified in the list, default priors are used.
#' @param samplerType character string, specifying the sampling method used during the MCMC. Currently, supported options are Random Walk Metropolis (samplerType = 'RW'), Blocked Random Walk Metropolis (samplerType = 'RW_block'), Automated Factor Slice Sampling (samplerType = 'AF_slice'), and Slice sampling (samplerType = 'slice')
#' @param niter integer, number of MCMC iterations to perform (default is niter = 10000)
#' @param inits optional list, initial parameter values to be provided to nimble MCMC
#' @param burn optional integer, number of initial MCMC iterations to be discarded as burn-in. Default is burn = 0
#' @param constant_list optional list, constants to be provided to model. Currently only used for model = 'pawar-shsch'
#' @param verbose logical, should sampling progress be output?
#' @param ... Additional parameters to be passed to nimble during MCMC configuration and sampling
#' @return list, with entries "data" (object of class "list", data to be passed to `nimble` model) and "N" (integer number of data points, to be passed `nimble` model as a constant)
#' @examples
#' ## generate data
#' test_data = list(Temp = rep(c(10, 20, 30, 40), 5), Trait = rgamma(20, 5, rep(c(10, 20, 30, 40), 5)))
#' checkData(test_data)

b_TPC <- function(data, model, priors = NULL, samplerType = "RW",
                  niter = 10000, inits = NULL, burn = 0, constant_list = NULL, verbose = FALSE, ...) {
  # exception handling and variable setup
  data.nimble <- checkData(data)
  nimble_mod_function <- NULL
  model_info <-
    model_master_list[model_master_list$name == model, ]
  model_params <- model_info$params[[1]]
  model_constants <- model_info$constants[[1]]

  if (!(samplerType %in% c("RW", "RW_block", "AF_slice", "slice"))) stop("Unsupported option for input samplerType. Currently only RW, RW_block, slice, and AF_slice are supported.")
  if (model_info$density_function == "binomial") {
    if (is.null(unlist(data["n"]))) stop("For a Binomial GLM, data list must have a variable called 'n'. Perhaps check spelling and capitalization?")
    # const.list$n = unlist(data['n'])
  }



  # configure model, handles the density funciton, priors, and constants
  inits.list <- .configure_inits(inits, model_params)
  const.list <- .configure_constants(model_info, data.nimble$N, model_constants, constant_list)
  modelStr <- configure_model(model = model, priors = priors, ...)

  # create the model evaluation function
  eval(.direct_nimble(model))
  assign("nimble_mod_function", nimble_mod_function, envir = .GlobalEnv)
  nimTPCmod <- nimble::nimbleModel(str2expression(modelStr),
    constants = const.list,
    data = data.nimble$data, inits = inits.list,
    where = environment()
  )

  nimTPCmod_compiled <- nimble::compileNimble(nimTPCmod)

  # no printing because sampler hasn't changed yet, can be confusing
  mcmcConfig <- nimble::configureMCMC(nimTPCmod, print = F)

  # set correct sampler type
  if (model_info$density_function[[1]] == "normal") {
    bugs_params <- c(paste0("params[", 1:length(model_params), "]"), "sigma.sq")
  } else if (model_info$density_function[[1]] == "binomial") {
    bugs_params <- paste0("params[", 1:length(model_params), "]")
  }
  if (samplerType == "slice") {
    for (i in bugs_params) {
      mcmcConfig$removeSamplers(i)
      mcmcConfig$addSampler(i, type = samplerType)
    }
  } else if (samplerType != "RW") {
    mcmcConfig$removeSamplers(bugs_params)
    mcmcConfig$addSampler(bugs_params, type = samplerType)
  }

  if (verbose) {
    # Manually Printing
    cat("===== Monitors ===== \n")
    mcmcConfig$printMonitors()
    cat("===== Samplers ===== \n")
    mcmcConfig$printSamplers(byType = T)
  }


  mcmcConfig$enableWAIC <- TRUE
  mcmc <- nimble::buildMCMC(mcmcConfig)
  tpc_mcmc <- nimble::compileNimble(mcmc, project = nimTPCmod_compiled)
  tpc_mcmc$run(niter, nburnin = burn, ...)
  samples <- coda::as.mcmc(as.matrix(tpc_mcmc$mvSamples))

  default_priors <- model_info$default_priors[[1]]
  prior_list <- list()
  for (i in 1:length(model_params)) { # somehow works
    colnames(samples)[i] <- model_params[i]
    if (model_params[i] %in% names(priors)) {
      prior_list[model_params[[i]]] <- priors[model_params[[i]]]
    } else {
      prior_list[model_params[[i]]] <- default_priors[i]
    }
  }

  if ("sigma.sq" %in% names(priors)) {
    prior_list["sigma.sq"] <- priors["sigma.sq"]
  } else {
    prior_list["sigma.sq"] <- "T(dt(mu = 0, tau = 1/10, df = 1), 0, )"
  }

  if (!is.null(model_constants)) {
    constants <- const.list$constants[1:(length(const.list$constants) - 1)]
    names(constants) <- model_constants
  } else {
    constants <- NULL
  }


  rm(nimble_mod_function, envir = .GlobalEnv)
  # tpc_mcmc = nimbleMCMC(model = nimTPCmod_compiled, niter = 10000)
  return(list(
    samples = samples, model = tpc_mcmc, data = data.nimble$data,
    modelType = model, priors = prior_list, constants = constants, uncomp_model = nimTPCmod
  ))
}
