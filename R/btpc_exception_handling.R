#' Checks Prior Function Inputs
#'
#' Helper function to verify correct input formatting for default prior functions
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
