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

model_names <- c("quadratic", "briere")
model_params <- list(list("q","T.max", "T.min"), list("q","T.max", "T.min"))


quad_formula <- "-1*q*(Temp - T.min)*(Temp - T.max) * (T.max > Temp) * (Temp > T.min)"
briere_formula <- "q*(Temp - T.min)*sqrt((T.max>Temp)*abs(T.max-Temp)) * (T.max > Temp) * (Temp > T.min)"
master_list <- data.frame("name" = model_names)
master_list$params <- model_params
master_list$formula <- list(quad_formula, briere_formula)
master_list$default_priors <- list(defaultPriors("quadratic"), defaultPriors("briere"))

build_Rfunction <- function(model){
  model_info <- master_list[master_list$name == model,]
  model_function <- function(params, Temp){
    params <- checkParams(model, params, F)
    for (i in 1:length(params)){
      assign(names(params[i]), unlist(as.vector(params[i])))
    }
    curve <- eval(str2expression(model_info$formula[[1]]))
    return(curve)
  }

  return(model_function)
}

test_quadR <- build_Rfunction("quadratic")
test_params <- list(q = .75,
                                 T.max = 35,
                                 T.min = 10)
test_temp <- 20:25

test_quadR(test_params, test_temp)
quadratic_tpc(test_params, test_temp) #looks good!
