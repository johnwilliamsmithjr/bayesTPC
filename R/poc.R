#' Goals
#' 1. User-end functionality of individual performance curves
#' with arbitrary parameters
#'
#' 2. Easy user-end addition of arbitrary distributions
#'
#' 3. Nimble/BUGS usable individual performance curve functions
#'  - Because of how Nimble works, they must work w/o named params
#'  - May or may not be implemented individually
#'
#' Current Idea:
#' Store all available distributions as a data frame
#' when functions are run, we should be able to reference
#' all the appropriate information from the dataframe
#'  - We can then hard code our default ones somewhere
#'  in the source code
#'

model_names <- c("quad", "briere")
model_params <- list(list("q","T.max", "T.min"), list("q","T.max", "T.min"))

quad_tpc <- function(params,
                     Temps,
                     posteriorPredictive = FALSE){
  q = params[[1]] #parameter checking is done previously
  T.max = params[[2]]
  T.min = params[[3]]

  if (posteriorPredictive == FALSE){
    curve = -1*q*(Temp - T.min)*(Temp - T.max) * (T.max > Temp) * (Temp > T.min)
  } else{
    sigma.sq = params[[4]]

    truncmeans = -1*q*(Temp - T.min)*(Temp - T.max) * (T.max > Temp) * (Temp > T.min)
    curve = rtruncnorm(length(Temp), a = 0, b = Inf, mean = truncmeans, sd = sqrt(sigma.sq))
  }
  return(curve)
}

briere_tpc <- function(params,
                     Temps,
                     posteriorPredictive = FALSE){
  q = params[[1]] #parameter checking is done previously
  T.max = params[[2]]
  T.min = params[[3]]

  if (posteriorPredictive == FALSE){
    curve = q*(Temp - T.min)*sqrt((T.max>Temp)*abs(T.max-Temp)) * (T.max > Temp) * (Temp > T.min)
  } else{
    sigma.sq = params[['sigma.sq']]

    truncmeans = q*(Temp - T.min)*sqrt((T.max>Temp)*abs(T.max-Temp)) * (T.max > Temp) * (Temp > T.min)
    curve = rtruncnorm(length(Temp), a = 0, b = Inf, mean = truncmeans, sd = sqrt(sigma.sq))
  }
  return(curve)
}

quad_formula <- "-1*q*(Temp - T.min)*(Temp - T.max) * (T.max > Temp) * (Temp > T.min)"
briere_formula <- "q*(Temp - T.min)*sqrt((T.max>Temp)*abs(T.max-Temp)) * (T.max > Temp) * (Temp > T.min)"
master_list <- data.frame("name" = model_names)
master_list$params <- model_params
master_list$formula <- list(quad_formula, briere_formula)

