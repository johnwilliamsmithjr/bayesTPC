
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

## Installation

You can install the development version of **bayesTPC** from
[GitHub](https://github.com/) with:

``` r
remotes::install_github("johnwilliamsmithjr/bayesTPC")
```
