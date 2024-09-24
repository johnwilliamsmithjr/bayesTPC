
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bayesTPC: An accessible interface to fit Bayesian thermal performance curves in R.

<!-- badges: start -->

[![R-CMD-check](https://github.com/johnwilliamsmithjr/bayesTPC/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/johnwilliamsmithjr/bayesTPC/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/johnwilliamsmithjr/bayesTPC/branch/master/graph/badge.svg)](https://app.codecov.io/gh/johnwilliamsmithjr/bayesTPC?branch=master)
<!-- badges: end -->

**bayesTPC** is an R package to help conveniently fit common thermal
performance models using Bayesian MCMC. We provide functionality to
substitute for or supplement BUGS modelling workflows, especially for
those without experience in Bayesian methods. To this end, **bayesTPC**
contains a simple model specification format, BUGS configuration
helpers, end-to-end model fitting methods, and model diagnostics.

For a detailed introduction, visit
[this](https://seansorek.github.io/bayesTPC/pages/intro.html) tutorial.

## Installation

You can install the development version of **bayesTPC** from
[GitHub](https://github.com/) with:

``` r
remotes::install_github("johnwilliamsmithjr/bayesTPC")
```

## Functionality

### Models

`bayesTPC` offers many pre-configured TPC models. We can use
`get_models()` to view all implemented models.

``` r
get_models()
#>  [1] "poisson_glm_lin"    "poisson_glm_quad"   "binomial_glm_lin"  
#>  [4] "binomial_glm_quad"  "bernoulli_glm_lin"  "bernoulli_glm_quad"
#>  [7] "briere"             "gaussian"           "kamykowski"        
#> [10] "pawar_shsch"        "quadratic"          "ratkowsky"         
#> [13] "stinner"            "weibull"
```

To see the exact specification of a particular model, we can use

``` r
get_default_model_specification("briere")
#> bayesTPC Model Specification of Type:
#>   briere
#> 
#> Model Formula:
#>   m[i] <- ( q * Temp * (Temp - T_min) * sqrt((T_max > Temp) * abs(T_max - Temp))
#> * (T_max > Temp) * (Temp > T_min) )
#> 
#> Model Distribution:
#>   Trait[i] ~ T(dnorm(mean = m[i], tau = 1/sigma.sq), 0, )
#> 
#> Model Parameters and Priors:
#>   q ~ dexp(1)
#>   T_max ~ dunif(25, 60)
#>   T_min ~ dunif(0, 20)
#>   sigma.sq ~ dexp(1)
```

Use `get_formula()`, `get_default_priors()`, and
`get_default_constants()` to display the individual components of a any
model included in `bayesTPC`.

For those with experience in fitting models in BUGS, `configure_model()`
provides pre-formatted code.

### Fitting

The fitting workhorse of `bayesTPC` is the `b_TPC()` function. With
`b_TPC()`, a user only needs to specify the dataset and desired model
and `bayesTPC` handles configuring and running an MCMC using `nimble`.
The user can also change the model priors, the sampler used, and other
hyperparameters of the MCMC.

### Changing and Specifying models

One can also create their own models by specifying a formula and priors
using `specify_model()`, along with its wrappers
`specify_normal_model()` and `specify_binomial_model()`.

`bayesTPC` accepts both the name of a model and the model object itself
as inputs to its workhorse functions. This provides two methods for
users to customize existing models: providing customized priors and
constants in the options of the functions themselves, or by storing the
model object in the local environment and using the `change_priors()`
and `change_constants()` functions. Note that since R objects are
immutable, the modified models must be stored again.

### Summaries

The fit object returned by `b_TPC()` can be summarized in multiple ways.
`summary()` gives a detailed summary of the MCMC results, and `plot()`
shows the fit given by the center (mean or median) and bounding (95%
quantiles or hdi) MCMC samples. Similarly, `posterior_predictive()` and
`plot_prediction()` take samples of the posterior to predict new values.

### Diagnostics

`bayesTPC` provides multiple MCMC diagnostic plots. `traceplot()` wraps
`coda::traceplot()` and shows the sampled values by sequential
iteration. `bayesTPC_ipairs()` wraps `IDPmisc::ipairs()` and shows the
pairwise joint posterior distributions of all model parameters. These
wrappers are provided for convenience.

The `ppo_plot()` function shows the overlap between the priors specified
versus a kernel density estimation of the posterior sample.

### Vectraits API

`bayesTPC` also includes a basic API to access the Vectraits database of
disease vector trait data. One can either retrieve a pre-known dataset
using `get_VB_dataset()` or `get_VB_datasets()`, or search specific
keywords using `find_VB_datasets()`.
