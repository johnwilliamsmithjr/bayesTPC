#' Checks Prior data format
#'
#' Cgeck if data is correctly formatted
#'
#' @details
#' @param params named vector, contains the actual params from the function call
#' @param expectedParams char vector, contains the names of the expected params for the called model
#' @param posteriorPredictive logical, should posterior predictive be generated insread of evaluating the deterministic model. default = FALSE

priorConds <- function(params, expectedParams, posteriorPredictive){
  if (!is.logical(posteriorPredictive)) stop('posteriorPredictive argument must be supplied as a logical (= TRUE or = FALSE)')
  if (is.null(names(params))) stop('Error in call to tpc evaluation function. param input must be named.')
}

#' Retrieve expected parameters
#'
#' Retrieve expected parameters for a given thermal performance curve model
#' @param model character, name of model type
#'
expectedParams <- function(model){

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
#' test_data = list(Temp = rep(c(10, 20, 30, 40), 5), Trait = rgamma(20, 5, rep(c(10, 20, 30, 40), 5)))
#' checkData(test_data)

checkData <- function(data){
  ## data checks to make sure there are values for Temp and Trait
  if (!is.list(data)) stop("Unexpected class for argument 'data'. Data must be input as a list.")
  if (is.null(unlist(data['Temp']))) stop("Data list must have a variable called 'Temp'. Perhaps check spelling and capitalization?")
  if (is.null(unlist(data['Trait']))) stop("Data list must have a variable called 'Trait'. Perhaps check spelling and capitalization?")
  if (!is.vector(unlist(data['Trait']))) stop('List elements of data must be numeric vectors')
  if (!is.vector(unlist(data['Temp']))) stop('List elements of data must be numeric vectors')
  if (length(data['Trait']) != length(data['Temp'])) stop("'Temp' and 'Trait' must have the same length.")
  ## warnings for when temperature may be in F instead of C
  if (any(unlist(data['Temp']) > 50)) warning('Unusual (Temp>50) temperature values detected (are Temps given in Celcius?)')
  if (any(unlist(data['Temp']) < 0)) warning('Unusual (Temp<0) temperature values detected (are Temps given in Celcius?)')
  if (any(is.na(data['Temp']))) warning('Temperature data contains missing values. This may lead to unexpected results')
  if (any(is.na(data['Trait']))) warning('Trait data contains missing values. This may lead to unexpected results')
  return(list(data = data, N = length(unlist(data['Trait']))))
}
