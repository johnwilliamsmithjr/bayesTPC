# Sean Sorek & John W. Smith

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
  stopifnot("Invalid input" = "btpc_model" %in% class(model))
  as.character(attr(attr(model, "distribution"), "formula"))
}

.priors_string <- function(model) {
  stopifnot("Invalid input" = "btpc_model" %in% class(model))
  model_priors <- attr(model, "parameters")
  llh_priors <- attr(attr(model, "distribution"), "llh_parameters")
  priors <- c(model_priors, llh_priors)
  paste0(names(priors), " ~ ", priors)
}


#' Create model string
#'
#' Create model string for thermal performance curve model to be passed to nimble.
#'
#' @export
#' @details This function returns a character string of the full `nimble` model for a user-specified thermal performance curve and prior distributions.
#' @param model character or `btpc_model`. If a character, a string specifying the model name. Otherwise, a model specification.
#' If a string is provided, the default values are used if the model is implemented. Use [get_models()] to view all options.
#' @param priors list or character, optional input specifying prior distributions for parameters (default = NULL).
#'  Elements of the list should correspond to model parameters, and written using nimble logic.
#'  For parameters not specified in the list, default priors are used.
#' @param constants list or character, optional input specifying model constants.
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
      stop("Model has been specified incorrectly. Please use specify_model() to create custom models.")
    } else {
      stop("Unsupported model. Use get_models() to view implemented models.")
    }
  }
  if (!("btpc_model" %in% class(model))) {
    model <- model_list[[model]]
  }

  # change priors if necessary
  model <- change_priors(model, unlist(priors))
  model <- change_constants(model, unlist(constants))

  loop <- paste0(
    "{\n    for (i in 1:N){\n        ",
    .link_string(model), gsub("Temp", "Temp[i]", attr(model, "formula")),
    " )\n        Trait[i] ~ ", .distribution_string(model), "\n    }\n"
  )
  pri <- paste0(.priors_string(model), collapse = "\n    ")
  nimble_string <- paste0(loop, "    ", pri, "\n}")
  return(nimble_string)
}
