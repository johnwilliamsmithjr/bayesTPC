.link_string <- function(model) {
  UseMethod(".link_string")
}

#' @export
.link_string.btpc_identity <- function(model) {
  "m[i] <- ( "
}

#' @export
.link_string.btpc_log <- function(model) {
  "log(m[i]) <- ( "
}

#' @export
.link_string.btpc_logit <- function(model) {
  "logit(m[i]) <- ( "
}

#' @export
.link_string.btpc_reciprocal <- function(model) {
  "m[i] <- 1 / ( "
}

#' @export
.link_string.default <- function(model) {
  stop("Misconfigured Model Specification.")
}

.distribution_string <- function(model) {
  UseMethod(".distribution_string")
}

#' @export
.distribution_string.btpc_normal <- function(model) {
  "T(dnorm(mean = m[i], tau = 1/sigma.sq), 0, )"
}

#' @export
.distribution_string.btpc_poisson <- function(model) {
  "dpois(m[i])"
}

#' @export
.distribution_string.btpc_bernoulli <- function(model) {
  "dbern(m[i])"
}

#' @export
.distribution_string.btpc_binomial <- function(model) {
  "dbinom(m[i], n[i])"
}

#' @export
.distribution_string.btpc_exponential <- function(model) {
  "dexp(m[i])"
}

#' @export
.distribution_string.btpc_gamma <- function(model) {
  "dgamma(shape_par, shape_par * m[i])"
}

#' @export
.distribution_string.default <- function(model) {
  stop("Misconfigured Model Specification.")
}

.priors_string <- function(model) {
  UseMethod(".priors_string")
}

#' @export
.priors_string.btpc_normal <- function(model) {
  c(
    .priors_string.btpc_model(model),
    paste0("sigma.sq ~ ", attr(model, "sigma.sq"))
  )
}

#' @export
.priors_string.btpc_gamma <- function(model) {
  c(
    .priors_string.btpc_model(model),
    paste0("shape_par ~ ", attr(model, "shape_par"))
  )
}

#' @export
.priors_string.btpc_model <- function(model) {
  priors <- attr(model, "parameters")
  paste0(names(priors), " ~ ", priors)
}

#' @export
.priors_string.default <- function(model) {
  stop("Misconfigured Model Specification.")
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
  if (is.null(model) || !(model %in% model_list)) {
    if ("btpc_model" %in% class(model)) {
      stop("Model has been specified incorrectly. Please use specify_*_model() to create custom models.")
    } else {
      stop("Unsupported model. Use get_models() to view implemented models.")
    }
  }
  if (!("btpc_model" %in% class(model))) {
    model <- model_list[[model]]
  }

  # change priors if necessary
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

  loop <- paste0(
    "{\n    for (i in 1:N){\n        ",
    .link_string(model), gsub("Temp", "Temp[i]", attr(model, "formula")),
    " )\n        Trait[i] ~ ", .distribution_string(model), "\n    }\n"
  )
  pri <- paste0(.priors_string(model), collapse = "\n    ")
  nimble_string <- paste0(loop, "    ", pri, "\n}")
  return(nimble_string)
}
