# John W. Smith

#' Wrapper for coda::traceplot()
#'
#' Wrapper for coda::traceplot() that can both directly accept samples of object type `mcmc` or `mcmc.list` as well as an object of class `list` with an element `samples` that is of class `mcmc` or `mcmc.list`.
#'
#' @export
#' @details This is a wrapper to create trace plots using coda's `traceplot()` function.
#' @param object Either an object of class `mcmc` or `mcmc.list` OR an object of class `list` which contains an element called `samples` that is of class `mcmc` or `mcmc.list`.
#' @param burn Integer, number of samples to discard as burn-in before creating traceplot. Default is 0.
#' @param thin Integer, thinning interval used to generate traceplots.  Default is 1.
#' @param ... additional graphical parameters to be passed as arguments to coda::traceplot.
#' @return Returns invisible(NULL) and creates trace plots for MCMC parameters.
traceplot <- function(object, burn = 0, thin = 1, ...) {
  ## checks to see if samples are contained in list object
  if (is.list(object)) if (is.null(object$samples)) stop('Object of class list must have element "samples"')
  if (is.list(object)) {
    if (coda::is.mcmc(object$samples)) {
      ## if samples is an mcmc list, extract number of rows
      N <- nrow(object$samples)
      ## thin samples
      thinned <- seq(from = burn + 1, to = N, by = thin)
      ## create traceplots
      coda::traceplot(x = coda::as.mcmc(object$samples[thinned, ]), ...)
      return(invisible(NULL))
    }
  } else { ## if samples is not class coda mcmc list
    ## extract number of rows
    N <- nrow(object)
    ## thin samples
    thinned <- seq(from = burn + 1, to = N, by = thin)
    ## create traceplots
    coda::traceplot(x = coda::as.mcmc(object[thinned, ]), ...)
    ## return invisible NULL
    return(invisible(NULL))
  }
}

#' Wrapper for IDPmisc::ipairs() to create pairs plots of bTPC output
#'
#' Wrapper for IDPmisc() that can both directly accept samples of object type `mcmc` or `matrix` as well as an object of class `list` with an element `samples` that is of class `mcmc` or `matrix`.
#'
#' @export
#' @details This is a wrapper to create pairs plots using IDPmisc's `ipairs()` function.
#' @param x Either an object of class `mcmc` or `matrix` OR an object of class `list` which contains an element called `samples` that is of class `mcmc` or `matrix`.
#' @param burn Integer, number of samples to discard as burn-in before creating pairs plot. Default is 0.
#' @param thin Integer, thinning interval used to generate pairs plots. Default is 1.
#' @param ztransf Function, used to transform the counts per pixel in [IDPmisc::ipairs()].
#' @param ... additional graphical parameters to be passed as arguments to [IDPmisc::ipairs()].
#'  For additional information, try `?IDPmisc::ipairs`.
#' @return Returns invisible(NULL) and creates pairs plots for MCMC parameters.
bayesTPC_ipairs <- function(x, burn = 0, thin = 1,
                            ztransf = function(x) {
                              x[x < 1] <- 1
                              log2(x)
                            }, ...) {
  ## checks to see if list object contains entry called samples
  if (is.list(x)) if (is.null(x$samples)) stop('Expected list "x" to have an element called samples')
  ## checks that x is input in proper format
  if (!(coda::is.mcmc(x)) & !(is.matrix(x)) & !(is.list(x))) stop('Input "x" is expected as a list, matrix, or mcmc object. See ?bayesTPC_ipairs')
  if (is.list(x)) {
    ## extract number of rows
    N <- nrow(x$samples)
    ## thin samples
    thinned <- seq(from = burn + 1, to = N, by = thin)
    ## converts samples to matrix to pass to IDPmisc::ipairs()
    if (coda::is.mcmc(x$samples)) {
      samples <- as.matrix(x$samples)
    } else {
      samples <- x$samples
    }
    ## create pairs plot of thinned samples
    IDPmisc::ipairs(samples[thinned, ], ztransf = ztransf, ...)
  } else {
    ## extract number of rows
    N <- nrow(x)
    ## thin samples
    thinned <- seq(from = burn + 1, to = N, by = thin)
    ## convert to matrix
    if (coda::is.mcmc(x)) {
      samples <- as.matrix(x)
    } else {
      samples <- x
    }
    ## create pairs plot
    IDPmisc::ipairs(samples[thinned, ], ztransf = ztransf, ...)
  }
  ## return invisible NULL
  return(invisible(NULL))
}

#' Prior-Posterior Overlap Plot
#'
#' Create a prior-posterior overlap plot from output of [b_TPC()].
#'
#' @export
#' @details Function to create a prior-posterior overlap plot. This is useful in determining the impact that the choice of prior distribution has on the analyses.
#' @param model List, usually output from `b_TPC()`. `ppo_plot` expects this list to have entries "samples", of class `mcmc` or `numeric` and "priors", with class `list` and entries that match the corresponding model parameters.
#' @param burn Integer, number of samples to discard as burn-in before creating prior-posterior overlap plot. Default is 0.
#' @param seq.length Integer, length of sequence used to evaluate prior density. Default is 100.
#' @return Returns invisible(NULL) and creates a prior posterior overlap plot, with the prior density shown using a red line and the posterior density shown using a blue dashed line.
ppo_plot <- function(model, burn = 0, seq.length = 100) {
  ## extract model parameters and sort alphabetically
  ppo_parameters <- sort(names(model$priors)[names(model$priors) != "sigma.sq"])
  ## if sigma.sq is in the mcmc sample list, add it as the
  ## last entry of the param_list vector
  if ("sigma.sq" %in% colnames(model$samples)) {
    param_list <- c(ppo_parameters, "sigma.sq")
  } else {
    param_list <- ppo_parameters
  }

  get_prior_eval <- function(x, name) {
    init_list <- list()
    init_list[name] <- x
    model$uncomp_model$setInits(init_list)
    return(exp(model$uncomp_model$calculate(name)))
  }

  for (i in 1:length(param_list)) {
    param_string <- param_list[i]
    ## extract sequence lower bound to evaluate prior density on. if the bound of a particular
    ## parameter is infinite, one half of the smallest observed posterior sample is used.
    ## otherwise, the lower bound of the prior is used.
    seq_lower <- ifelse(
      test = is.infinite(nimble::getBound(model$uncomp_model, param_string, "lower")),
      yes = .5 * min(model$samples[(burn + 1):nrow(model$samples), param_list[i]]),
      no = nimble::getBound(model$uncomp_model, param_string, "lower")
    )
    ## extract sequence upper bound to evaluate prior density on. if the bound of a particular
    ## parameter is infinite, twice the largest observed posterior sample is used.
    ## otherwise, the upper bound of the prior is used.
    seq_upper <- ifelse(
      test = is.infinite(nimble::getBound(model$uncomp_model, param_string, "upper")),
      yes = 2 * max(model$samples[(burn + 1):nrow(model$samples), param_list[i]]),
      no = nimble::getBound(model$uncomp_model, param_string, "upper")
    )
    ## create evaluation sequence
    eval_seq <- seq(from = seq_lower, to = seq_upper, length.out = seq.length)


    ## generate prior evaluations
    prior_evals <- sapply(X = eval_seq, FUN = get_prior_eval, name = param_string)
    ## create density of posterior MCMC samples
    posterior_approx <- stats::density(model$samples[(burn + 1):nrow(model$samples), param_list[i]])

    ## create ylimits
    ylim_ppo <- c(0, 1.05 * max(c(max(posterior_approx$y), max(prior_evals))))
    ## generate plot
    plot(eval_seq, prior_evals,
      ylim = ylim_ppo, type = "l", col = "red",
      ylab = "Density", xlab = param_list[i], lwd = 2,
      main = paste0("Prior-Posterior Overlap Plot for ", param_list[i])
    )
    graphics::points(posterior_approx, type = "l", col = "blue", lwd = 2, lty = 2)
    graphics::legend("topleft",
      legend = c("Prior", "Posterior"), col = c("red", "blue"),
      lty = c(1, 2), lwd = c(2, 2)
    )
  }
}
