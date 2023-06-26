#' Checks Prior data format
#'
#' Check if data is correctly formatted
#'
#' @details TODO
#' @param model character, the model type
#' @param params named vector, contains the actual params from the function call
#' @return list, input params coerced as a vector, if necessary

checkParams <- function(model, params) {
  # having an external function call in our model evaluation
  # might cause issues, but we can use an error handling wrapper
  # that calls the nimble function, probably
  if (is.null(names(params))) stop("Error in call to tpc evaluation function. param input must be named.")
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


#' Check TPC data format
#'
#' Check if data is correctly formatted
#'
#' @details This function returns a list of data and constants to be passed to a `nimble` model to perform MCMC
#' @param data list, with expected entries "Trait" (corresponding to the trait being modeled by the thermal performance curve) and "Temp" (corresponding to the temperature in Celcius that the trait is being measured at).
#' @return list, with entries "data" (object of class "list", data to be passed to `nimble` model) and "N" (integer number of data points, to be passed `nimble` model as a constant)
#' @examples
#' ## generate data
#' test_data <- list(Temp = rep(c(10, 20, 30, 40), 5), Trait = rgamma(20, 5, rep(c(10, 20, 30, 40), 5)))
#' checkData(test_data)
checkData <- function(data) {
  ## data checks to make sure there are values for Temp and Trait
  if (!is.list(data)) stop("Unexpected class for argument 'data'. Data must be input as a list.")
  if (is.null(unlist(data["Temp"]))) stop("Data list must have a variable called 'Temp'. Perhaps check spelling and capitalization?")
  if (is.null(unlist(data["Trait"]))) stop("Data list must have a variable called 'Trait'. Perhaps check spelling and capitalization?")
  if (!is.vector(unlist(data["Trait"]))) stop("List elements of data must be numeric vectors")
  if (!is.vector(unlist(data["Temp"]))) stop("List elements of data must be numeric vectors")
  if (length(data["Trait"]) != length(data["Temp"])) stop("'Temp' and 'Trait' must have the same length.")
  ## warnings for when temperature may be in F instead of C
  if (any(unlist(data["Temp"]) > 50)) warning("Unusual (Temp>50) temperature values detected (are Temps given in Celcius?)")
  if (any(unlist(data["Temp"]) < 0)) warning("Unusual (Temp<0) temperature values detected (are Temps given in Celcius?)")
  if (any(is.na(data["Temp"]))) warning("Temperature data contains missing values. This may lead to unexpected results")
  if (any(is.na(data["Trait"]))) warning("Trait data contains missing values. This may lead to unexpected results")
  return(list(data = data, N = length(unlist(data["Trait"]))))
}
