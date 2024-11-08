# John W. Smith

#' Wrapper for coda::traceplot()
#'
#' Wrapper for coda::traceplot() that can both directly accept samples of object type `mcmc` or `mcmc.list` as well as an object of class `list` with an element `samples` that is of class `mcmc` or `mcmc.list`.
#'
#' @export
#' @details This is a wrapper to create trace plots using coda's `traceplot()` function.
#' @param object `mcmc`, `mcmc.list`, or `list`. If a list, must contain an element called `samples` that is of class `mcmc` or `mcmc.list`.
#' @param burn integer, number of samples to discard as burn-in before creating traceplot. Default is 0.
#' @param thin integer, thinning interval used to generate traceplots.  Default is 1.
#' @param ... additional graphical parameters to be passed as arguments to coda::traceplot.
#' @return Returns invisible(NULL) and creates trace plots for MCMC parameters.
traceplot <- function(object, burn = 0, thin = 1, ...) {
  ## checks to see if samples are contained in list object
  if (coda::is.mcmc.list(object)) { #this needs to be checked before #is.list

    if (length(object) == 0) stop("Sample list must have at least one element.")
    thinned <- seq(from = burn + 1, to = nrow(object[[1]]), by = thin)
    for (s in object) coda::traceplot(x = coda::as.mcmc(s[thinned, ]), ...)

  } else if (coda::is.mcmc(object)) {

    thinned <- seq(from = burn + 1, to = nrow(object), by = thin)
    coda::traceplot(x = coda::as.mcmc(object[thinned, ]), ...)

  } else if (is.list(object)) {

    if (is.null(object$samples)) stop('Object of class list must have element "samples".')
    if (coda::is.mcmc(object$samples)) {

      thinned <- seq(from = burn + 1, to = nrow(object$samples), by = thin)
      coda::traceplot(x = coda::as.mcmc(object$samples[thinned, ]), ...)

    } else if (coda::is.mcmc.list(object$samples)) {

      if (length(object$samples) == 0) stop("Sample list must have at least one element.")
      thinned <- seq(from = burn + 1, to = nrow(object$samples[[1]]), by = thin)
      for (s in object$samples) coda::traceplot(x = coda::as.mcmc(s[thinned, ]), ...)
    } else {
      stop("Misconfigured MCMC.")
    }

  } else {
    stop("Invalid input. Parameter 'object' must be an mcmc, mcmc.list, or a list containing an element named 'samples' which is an mcmc or mcmc.list.")
  }

  return(invisible(NULL))
}

#' Wrapper for IDPmisc::ipairs() to create pairs plots of bTPC output
#'
#' Wrapper for IDPmisc() that can both directly accept samples of object type `mcmc` or `matrix` as well as an object of class `list` with an element `samples` that is of class `mcmc` or `matrix`.
#'
#' @export
#' @details This is a wrapper to create pairs plots using IDPmisc's `ipairs()` function.
#' @param x `mcmc`, `matrix`, or `list`. If a list, must contain an element called `samples` that is of class `mcmc` or `matrix`.
#' @inheritParams traceplot
#' @param ztransf Function, used to transform the counts per pixel in [IDPmisc::ipairs()].
#' @param ... additional graphical parameters to be passed as arguments to [IDPmisc::ipairs()].
#'  For additional information, try `?IDPmisc::ipairs`.
#' @return Returns invisible(NULL) and creates pairs plots for MCMC parameters.
ipairs <- function(x, burn = 0, thin = 1,
                            ztransf = function(x) {
                              x[x < 1] <- 1
                              log2(x)
                            }, ...) {
  ## checks to see if samples are contained in list object
  if (coda::is.mcmc.list(x)) { #this needs to be checked before #is.list

    if (length(x) == 0) stop("Sample list must have at least one element.")
    thinned <- seq(from = burn + 1, to = nrow(x[[1]]), by = thin)
    for (s in x) IDPmisc::ipairs(s[thinned, ], ztransf = ztransf, ...)

  } else if (coda::is.mcmc(x)) {

    thinned <- seq(from = burn + 1, to = nrow(x), by = thin)
    IDPmisc::ipairs(x[thinned, ], ztransf = ztransf, ...)

  } else if (is.list(x)) {

    if (is.null(x$samples)) stop('Object of class list must have element "samples".')
    if (coda::is.mcmc(x$samples)) {

      thinned <- seq(from = burn + 1, to = nrow(x$samples), by = thin)
      IDPmisc::ipairs(x$samples[thinned, ], ztransf = ztransf, ...)

    } else if (coda::is.mcmc.list(x$samples)) {

      if (length(x$samples) == 0) stop("Sample list must have at least one element.")
      thinned <- seq(from = burn + 1, to = nrow(x$samples[[1]]), by = thin)
      for (s in x$samples) IDPmisc::ipairs(s[thinned, ], ztransf = ztransf, ...)
    } else {
      stop("Misconfigured MCMC.")
    }

  } else {
    stop("Invalid input. Parameter 'object' must be an mcmc, mcmc.list, or a list containing an element named 'samples' which is an mcmc or mcmc.list.")
  }

  return(invisible(NULL))
}

#' Prior-Posterior Overlap Plot
#'
#' Create a prior-posterior overlap plot from output of [b_TPC()].
#'
#' @export
#' @details Creates a prior-posterior overlap plot. This is useful in determining the impact that the choice of prior distribution has on the analyses.
#' @param model list, usually output from `b_TPC()`. `ppo_plot` expects this list to have entries "samples", of class `mcmc` or `numeric` and "priors", with class `list` and entries that match the corresponding model parameters.
#' @param burn integer, number of samples to discard as burn-in before creating prior-posterior overlap plot. Default is 0.
#' @param seq.length integer, length of sequence used to evaluate prior density. Default is 100.
#' @param legend logical, should a legend be included? Default is TRUE.
#' @param legend_position character, position of the legend. Only used if legend = TRUE. Default is "bottomright".
#' @details
#' Priors are evaluated using the `setInits()` and `calculate()` methods of `model$uncomp_model()`. Posteriors are approximated using `stats::density()` on the posterior sample of each parameter.
#'
#'
#' @return Returns invisible(NULL) and creates a prior posterior overlap plot, with the prior density shown using a red line and the posterior density shown using a blue dashed line.
ppo_plot <- function(model, burn = 0, seq.length = 100, legend = TRUE, legend_position = "topleft") {

  if (!"btpc_MCMC" %in% class(model)) stop("Unexpected type for parameter 'model'. Only use this method with the output of b_TPC().")
  if (is.null(model$uncomp_model)) stop("The NIMBLE model object must exist to generate the prior posterior overlap. Make sure store_nimble_models = TRUE in b_TPC()")
  # This fails with gamma likelihoods but i dont have the patience to change it right now
  # I also dont think its the most necessary part of this code
  ## extract model parameters and sort alphabetically
  param_list <- sort(names(model$priors))

  get_prior_eval <- function(x, name) {
    init_list <- list()
    init_list[name] <- x
    model$uncomp_model$setInits(init_list)
    return(exp(model$uncomp_model$calculate(name)))
  }

  if (is(model$samples, "mcmc")) s_list <- list(model$samples)
  else s_list <- model$samples

  for (j in 1:length(s_list)) {
    s <- s_list[[j]]
    for (i in 1:length(param_list)) {
      param_string <- param_list[i]
      ## extract sequence lower bound to evaluate prior density on. if the bound of a particular
      ## parameter is infinite, one half of the smallest observed posterior sample is used.
      ## otherwise, the lower bound of the prior is used.
      seq_lower <- ifelse(
        test = is.infinite(nimble::getBound(model$uncomp_model, param_string, "lower")),
        yes = .5 * min(s[(burn + 1):nrow(s), param_list[i]]),
        no = nimble::getBound(model$uncomp_model, param_string, "lower")
      )
      ## extract sequence upper bound to evaluate prior density on. if the bound of a particular
      ## parameter is infinite, twice the largest observed posterior sample is used.
      ## otherwise, the upper bound of the prior is used.
      seq_upper <- ifelse(
        test = is.infinite(nimble::getBound(model$uncomp_model, param_string, "upper")),
        yes = 2 * max(s[(burn + 1):nrow(s), param_list[i]]),
        no = nimble::getBound(model$uncomp_model, param_string, "upper")
      )
      ## create evaluation sequence
      eval_seq <- seq(from = seq_lower, to = seq_upper, length.out = seq.length)

      ## generate prior evaluations
      prior_evals <- sapply(X = eval_seq, FUN = get_prior_eval, name = param_string)
      ## create density of posterior MCMC samples
      posterior_approx <- stats::density(s[(burn + 1):nrow(s), param_list[i]])

      ## create ylimits
      ylim_ppo <- c(0, 1.05 * max(c(max(posterior_approx$y), max(prior_evals))))
      ## generate plot
      plot(eval_seq, prior_evals,
           ylim = ylim_ppo, type = "l", col = "red",
           ylab = "Density", xlab = param_list[i], lwd = 2,
           main = paste0("Chain ",j, ": Prior-Posterior Overlap Plot for ", param_list[i])
      )
      graphics::points(posterior_approx, type = "l", col = "blue", lwd = 2, lty = 2)
      if (legend) {
        graphics::legend(legend_position,
                         legend = c("Prior", "Posterior"), col = c("red", "blue"),
                         lty = c(1, 2), lwd = c(2, 2)
        )
      }

    }
  }

}
