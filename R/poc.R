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
test_params <- list(q = .75, T.max = 35, T.min = 10)
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

params_badorder <- list(T.max = 35, T.min = 10, q = .75)
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
#R is a lovely language sometimes.

build_nimble_model <- function(model){
  R_model <- build_unprotected(model)

  nimble_model <-
    nimbleRcall(prototype = function(params = double(1),
                                     Temp = double(0)){},
                Rfun = "R_model",
                returnType = double(0))

  return(nimble_model)
}

test_nimble_quad <- build_nimble_model("quadratic")
# test_nimble_quad(test_params, test_temp)
# test_nimble_quad(params_badorder, test_temp)
#it seems the R function has to be in the same environment
#as the nimble function. This isn't a problem, as long as
#we are careful how we call the functions

## test this, by simulating data
set.seed(12345)
N = 16
params <- list('q' = .75, 'T.max' = 35, 'T.min' = 10)
sd_trait = 2

Temps = rep(c(15, 20, 25, 30), N/4)
Traits = rep(0, N)

for (i in 1:N){
  while(Traits[i] <= 0){
    Traits[i] = rnorm(1, quadratic_tpc(params, Temps[i], F),
                      sd=2)
  }
}


bTPC_q_test <- list(Trait = Traits, Temp = Temps)
## plot simulated data
plot(bTPC_q_test$Temp, bTPC_q_test$Trait,
     col = 'red', xlab = 'Temperature (C)', ylab = 'Trait Value')

quadConsts <- list(N = N,
                   Temp = bTPC_q_test$Temp)

quadData <- list(Trait = bTPC_q_test$Trait)

alt_quadCode <- nimbleCode({

  pars[1] ~ dunif(0,1)
  pars[2] ~ dunif(25, 60)
  pars[3] ~ dunif(0, 24)


  for (i in 1:N){
    m[i] <- test_unprotected_nimble(pars[1:3], Temp[i])
    Trait[i] ~ T(dnorm(mean = m[i],  var = sigma.sq), 0, )
  }

  sigma.sq ~ T(dt(mu = 0, tau = 10, df = 1), 0, )
})

## This block is commented out so the package doesnt run it when loaded
# alt_quadInits <- list(pars = c(.50,50,5), #unnamed parameters!
#                       sigma.sq=2)
#
#
# alt_quad <- nimbleModel(code = alt_quadCode, name = "alt_quad",
#                         constants = quadConsts,
#                         data = quadData, inits = alt_quadInits)
#
# alt_quad$getNodeNames()
#
# alt_Cquad<- compileNimble(alt_quad)
#
# alt_mcmc.out <- nimbleMCMC(code = alt_quadCode, constants = quadConsts,
#                            data = quadData, inits = alt_quadInits,
#                            nchains = 2, niter = 10000,
#                            #summary = TRUE, WAIC = TRUE,
#                            monitors = c('pars', 'sigma.sq'))
#
# dim(alt_mcmc.out$chain1)
# head(alt_mcmc.out$chain1)
#
# MCMCsummary(object = alt_mcmc.out, round = 2)

#NICE!
