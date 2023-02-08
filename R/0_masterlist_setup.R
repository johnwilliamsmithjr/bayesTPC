#' im a little worried about binomial / multinomial models
#' i think another column for density function would work
#' with sufficient string manipulation
#' For the time being, im gonna implement the other ones
model_names <- c("ratkowsky",
                 "stinner",
                 "kamykowski",
                 "pawar-shsch",
                 "quadratic",
                 "briere",
                 "weibull",
                 "gaussian",
                 "binomial_glm_lin",
                 "binomial_glm_quad"
                 )
model_priors <- lapply(model_names,
                       defaultPriors)
model_params <-
  lapply(lapply(lapply(model_names,
                       defaultPriors),
                names), as.list)

model_formulas <- c(
  #ratkowsky
  "(Temp > T.min)*(T.max > Temp)*((a*(Temp - T.min))*(1 - exp(b*(Temp - T.max))))^2",
  #stinner
  "C / (1 + exp(k1 + k2*(T.opt - abs(T.opt - Temp))))",
  #kamykowski
  "(Temp > T.min)*(T.max > Temp)*a*(1 - exp(-b*(Temp - T.min)))*(1 - exp(-c*(T.max - Temp)))",
  #pawar-shsch
  "(e_h > e)*r_tref*exp((e/(8.62e-05))*((1/(T.ref+273.15)) - (1/(Temp + 273.15)))) / (1 + (e / (e_h-e)) * exp((e_h/(8.62e-05))*(1/(T.opt + 273.15) - 1/(Temp + 273.15))))",
  #quadratic
  "-1*q*(Temp - T.min)*(Temp - T.max) * (T.max > Temp) * (Temp > T.min)",
  #briere
  "q*(Temp - T.min)*sqrt((T.max>Temp)*abs(T.max-Temp)) * (T.max > Temp) * (Temp > T.min)",
  #weibull
  "((a*(((c-1)/c)^((1-c)/c))*((((Temp-T.opt)/b)+(((c-1)/c)^(1/c)))^(c-1))*(exp(-((((Temp-T.opt)/b)+(((c-1)/c)^(1/c)))^c)+((c-1)/c)))))",
  #gaussian
  "rmax*exp(-0.5*(abs(Temp - T.opt)/a)^2)",
  #linear binomial
  "B0 + B1*Temp",
  #quadratic binomial,
  "B0 + B1*Temp[i] + B2*(Temp[i])^2"
)

model_density_functions <- c(
  rep("normal", 8), rep("binomial", 2)
)

model_master_list <- data.frame(
  "name" = I(as.list(model_names)),
  "params" = I(model_params),
  "formula" = I(as.list(model_formulas)),
  "default_priors" = I(model_priors),
  "density_function" = I(as.list(model_density_functions))
)
