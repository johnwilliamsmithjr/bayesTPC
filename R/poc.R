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

    #assign parameters into individual variables
    for (i in 1:length(params)){
      assign(names(params[i]), unlist(as.vector(params[i])))
    }

    #get formula from dataframe and parse
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

build_unprotected <- function(model){
  #we should add custom error handling at some point
  #R's default message would be confusing here
  model_info <- master_list[master_list$name == model,]

  model_function <- function(params, Temp){
    #assume params is sorted lexicographically
    sorted_vars <- sort(unlist(model_info$params))

    #assign parameters into individual variables
    for (i in 1:length(sorted_vars)){
      assign(sorted_vars[[i]], unlist(as.vector(params[i])))
    }

    #get formula from dataframe and parse
    curve <- eval(str2expression(model_info$formula[[1]]))
    return(curve)
  }

  return(model_function)
}

test_unprotected_quad <- build_unprotected("quadratic")

test_unprotected_quad(test_params, test_temp)
#works when parameters are in the correct order

params_badorder <- list(T.max = 35,
                                       T.min = 10,
                                       q = .75
                                       )

quadratic_tpc(params_badorder, test_temp)
test_quadR(params_badorder, test_temp)
test_unprotected_quad(params_badorder, test_temp)
#as expected, the protected version was correct
#but the unprotected version failed!
test_unprotected_nimble <-
  nimbleRcall(prototype = function(params = double(1),
                                   Temp = double(0)){},
              Rfun = 'test_unprotected_quad',
              returnType = double(0))

test_unprotected_nimble(test_params, test_temp)
test_unprotected_nimble(params_badorder, test_temp)
#Wow! it is that easy.
#I am actually surprised nimble is smart enough to do this
#R is a lovely language sometimes
