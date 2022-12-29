require(coda)
require(nimble)
require(HDInterval)
require(MCMCvis)
## Define quadratic function
myquad<-function(Temp, Tmax=40, Tmin=10, q=0.1){
  -1 * q * (Temp - Tmax) * (Temp-Tmin)*(Temp > Tmin)*(Temp < Tmax)
}

quadNimble <- nimbleFunction(
  run = function(Temp= double(0),
                 Tmax= double(0),
                 Tmin= double(0),
                 q = double(0)){
    out<- -1 * q * (Temp - Tmax) * (Temp-Tmin)*(Temp > Tmin)*(Temp < Tmax) #step(Tmax - Temp) * step(Temp - Tmin)
    return(out)
    returnType(double(0))
  }
)

alt_Rquad <- nimbleFunction(
  run = function(params = double(1),
                 Temp = double(0),
                 posteriorPredictive = logical(0)){
    return(1)
    returnType(double(0))
  }
)
Rquadratic_tpc <-
  nimbleRcall(prototype = function(params = double(1),
                                   Temp = double(0),
                                   posteriorPredictive = logical(0)){},
              Rfun = 'quadratic_tpc',
              returnType = double(0))

model_qtpc <- nimbleFunction(
  run = function(params = double(1),
                 Temp = double(0)){
    q <- params[1]
    T.min <- params[2]
    T.max <- params[3]

    curve <- -1*q*(Temp - T.min)*(Temp - T.max) * (T.max > Temp) * (Temp > T.min)
    return(curve)
    returnType(double(0))
  }
)

## this might be another way, but I haven't tested it
Rmyquad <-
  nimbleRcall(prototype = function(Temp= double(0),
                                   Tmax= double(0),
                                   Tmin= double(0),
                                   q = double(0)){},
              Rfun = 'myquad',
              returnType = double(0))

## test this, by simulating data
set.seed(12345)
N = 16
q = .75
Tmin = 10
Tmax = 35
sd_trait = 2

Temps = rep(c(15, 20, 25, 30), N/4)
params = list("q" = q, "T.min" = Tmin, "T.max" = Tmax)
Traits = rep(0, N)

for (i in 1:N){
  while(Traits[i] <= 0){
    Traits[i] = rnorm(1, Rmyquad(Temps[i], Tmax, Tmin, q),
                      sd=2)
  }
}

bTPC_q_test <- list(Trait = Traits, Temp = Temps)
## plot simulated data
plot(bTPC_q_test$Temp, bTPC_q_test$Trait,
     col = 'red', xlab = 'Temperature (C)', ylab = 'Trait Value')

## Starting the Nimble code
quadCode <- nimbleCode({
  q ~ dunif(0, 1)
  Tmin ~ dunif(0, 24)
  Tmax ~ dunif(25, 60)
  for (i in 1:N){
    m[i] <- quadNimble(Temp=Temp[i], Tmax, Tmin, q)
    Trait[i] ~ T(dnorm(mean = m[i],  var = sigma.sq), 0, )
  }
  sigma.sq ~ T(dt(mu = 0, tau = 10, df = 1), 0, )
})



quadConsts <- list(N = N,
                   Temp = bTPC_q_test$Temp)

quadData <- list(Trait = bTPC_q_test$Trait)

quadInits <- list(q = .75,
                  Tmin = 10,
                  Tmax = 35,
                  sigma.sq=2)

quad <- nimbleModel(code = quadCode, name = "quad",
                    constants = quadConsts,
                    data = quadData, inits = quadInits)

quad$getNodeNames()

## just checking it can compile
Cquad<- compileNimble(quad)

mcmc.out <- nimbleMCMC(code = quadCode, constants = quadConsts,
                       data = quadData, inits = quadInits,
                       nchains = 2, niter = 10000,
                       #summary = TRUE, WAIC = TRUE,
                       monitors = c('Tmax','Tmin','q', 'sigma.sq'))

dim(mcmc.out$chain1)
head(mcmc.out$chain1)

MCMCsummary(object = mcmc.out, round = 2)

par(mfrow=c(1,1), bty="n")
# ## these draw to pdf
# MCMCtrace(object = mcmc.out,
#           params = 'Tmax')
# MCMCtrace(object = mcmc.out,
#           params = 'Tmin')
# MCMCtrace(object = mcmc.out,
#           params = 'q')
# MCMCtrace(object = mcmc.out,
#           params = 'sigma.sq')

temps<-seq(5, 40, length=100)

plot(temps, quadNimble(temps,
                       Tmax = mcmc.out$chain1[[1,1]],
                       Tmin = mcmc.out$chain1[[1,2]],
                       q = mcmc.out$chain1[1,3]),
     type="l")
for(i in 1:50){

}


## same thing, but with a parameter vector, pars

alt_quadCode <- nimbleCode({

  pars[1] ~ dunif(0, 1)
  pars[2] ~ dunif(0, 24)
  pars[3] ~ dunif(25, 60)

  for (i in 1:N){
    m[i] <- model_qtpc(pars[1:3],
                          Temp[i])
    Trait[i] ~ T(dnorm(mean = m[i],  var = sigma.sq), 0, )
  }

  sigma.sq ~ T(dt(mu = 0, tau = 10, df = 1), 0, )
})


alt_quadInits <- list(pars = c('q' = .75, #names get erased by BUGS
                               'T.min' = 10,
                               'T.max'= 35),
                      sigma.sq=2)


alt_quad <- nimbleModel(code = alt_quadCode, name = "alt_quad",
                    constants = quadConsts,
                    data = quadData, inits = alt_quadInits)

alt_quad$getNodeNames()

alt_Cquad<- compileNimble(alt_quad)

alt_mcmc.out <- nimbleMCMC(code = alt_quadCode, constants = quadConsts,
                       data = quadData, inits = alt_quadInits,
                       nchains = 2, niter = 10000,
                       #summary = TRUE, WAIC = TRUE,
                       monitors = c('pars', 'sigma.sq'))

dim(alt_mcmc.out$chain1)
head(alt_mcmc.out$chain1)

MCMCsummary(object = alt_mcmc.out, round = 2)

par(mfrow=c(1,1), bty="n")
# ## these draw to pdf
# MCMCtrace(object = mcmc.out,
#           params = 'Tmax')
# MCMCtrace(object = mcmc.out,
#           params = 'Tmin')
# MCMCtrace(object = mcmc.out,
#           params = 'q')
# MCMCtrace(object = mcmc.out,
#           params = 'sigma.sq')

temps<-seq(5, 40, length=100)

plot(temps)
