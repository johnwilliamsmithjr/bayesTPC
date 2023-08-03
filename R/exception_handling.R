#' Checks Prior data format
#'
#' //This function may not be needed.
#' Check if data is correctly formatted. For interal use.
#'
#' @details Helper function to help error correction.
#' @param model character, the model type
#' @param params named vector, contains the actual params from the function call
#' @return list, input params coerced as a vector, if necessary

check_params <- function(model, params) {
  # having an external function call in our model evaluation
  # might cause issues, but we can use an error handling wrapper
  # that calls the nimble function, probably
  if (is.null(names(params))) stop("Error in call to tpc evaluation function. 'param' input must be named.")
  if (!all(sapply(params, is.numeric))) stop("All parameter values must be numeric.")
  priorNames <- model_master_list[model_master_list$name == model, ]$params[[1]]
  priorNames <- priorNames[priorNames != "sigma.sq"]
  if (!all(priorNames %in% names(params))) {
    missingpriors <- paste(priorNames[!(priorNames %in% names(params))], collapse = " ")
    stop(paste("Missing model parameters:", missingpriors))
  }
  if (!is.vector(params)) {
    warning("Expected params input to be a named vector. Attempting to convert to vector")
    params <- tryCatch(
      {
        as.vector(params)
      },
      error = function(e) { # custom error message
        stop("Params input unable to be coerced to a vector.")
      }
    )
  }

  return(params)
}
