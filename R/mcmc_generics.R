# Sean Sorek & John W. Smith

# helper for reused code
print_MCMC_metadata <- function(x) {
  cat(paste0(cli::style_underline(cli::col_cyan("bayesTPC MCMC of Type:\n")), "  ", c(x$model_spec)))
  formula_string_wrapped <- paste(strwrap(paste0(.link_string(x$model_spec), attr(x$model_spec, "formula")), width = options()$width, simplify = F)[[1]], collapse = "\n")
  cat(paste0(cli::style_underline(cli::col_cyan("\n\nFormula:\n")), "  ",formula_string_wrapped, " )"))
  dist_string_wrapped <- paste(strwrap(paste0("  Trait[i] ~ ",.distribution_string(x$model_spec)), width = options()$width, simplify = F)[[1]], collapse = "\n")
  cat(paste0(cli::style_underline(cli::col_cyan("\n\nDistribution:\n")),"  ",dist_string_wrapped))
  if (length(x$constants) > 0) {
    cat(cli::style_underline(cli::col_cyan("\n\nConstants:")))
    cat("\n  ", names(x$constants), " = ", x$constants, sep = "")
  }
}

#' @export
print.btpc_MCMC <- function(x, digits = 3, ...) {
  if (!"btpc_MCMC" %in% class(x)) stop("Unexpected type for parameter 'object'. Only use this method with the output of b_TPC().")

  print_MCMC_metadata(x)
  if (is(x$samples, "mcmc")) s_list <- list(x$samples)
  else s_list <- x$samples

  cat(cli::style_underline(cli::col_cyan("\n\nParameters:\n")))

  for (i in 1:length(s_list)) {
    cat(cli::col_br_cyan("\nChain ",i,":\n"), sep = "")
    s <- s_list[[i]]
    means <- round(matrixStats::colMeans2(s), digits)
    medians <- round(matrixStats::colMedians(s), digits)
    tbl <- cbind.data.frame(round(x$MAP_parameters[1:ncol(s)], digits), means, medians, x$priors[colnames(s)])
    rownames(tbl) <- colnames(s)
    colnames(tbl) <- c("MAP", "Mean", "Median", "Priors")
    print(tbl)
  }


}

#' Summary statistics for MCMCs
#'
#' Provide thermal performance curve summaries using output from [b_TPC()]'s nimble MCMC.
#' @export
#' @details This function returns various summaries of the output of the thermal performance curve model, generated using MCMC samples from the object returned by `b_TPC()`.
#' @param object `btpc_MCMC`, object output from performing MCMC using the `bTPC` function.
#' @param quantiles numeric, calculated quantiles for each variable.
#' @param ... additional parameters, passed to [summary.mcmc]
#' @returns A list (invisible) containing the information about and summary statistics of the MCMC chain(s).
summary.btpc_MCMC <- function(object,
                              quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975),
                              ...) {

  if (!"btpc_MCMC" %in% class(object)) stop("Unexpected type for parameter 'object'. Only use this method with the output of b_TPC().")

  print_MCMC_metadata(object)
  cat(cli::style_underline(cli::col_cyan("\n\nPriors:")))
  cat(paste0("\n  ", names(object$priors), " ~ ", object$priors))
  cat(cli::style_underline(cli::col_cyan("\n\nMax. A Post. Parameters:")), "\n")
  print(round(object$MAP_parameters, 4))
  cat(cli::style_underline(cli::col_cyan("\nMCMC Results:")))
  s <- summary(object$samples, quantiles = quantiles, ...)
  print(s)
  invisible(s)
}

#' Evaluate thermal performance curve MCMCs
#'
#' Provide thermal performance curve evaluations using output from [b_TPC()]'s nimble MCMC.
#' @param object `btpc_MCMC`, object output from performing MCMC using the `bTPC` function.
#' @param temp_interval numeric, reference values to use to compute values of the thermal performance curve.
#'  If no vector is provided, temp_interval is set as a sequence from the lowest observed temperature in the data to the highest observed temperature in the data, with 1,000 equally spaced points.
#' @param summaryType character, Determines what method is used to create credible intervals. Currently supported options are "quantile" and "hdi" (default).
#' @param centralSummary character, central summary measure used. Currently supported options are "median" (default), "mean", and "MAP" (maximum a posteriori).
#' @param prob numeric, the credible mass used to compute the highest density interval. Used if summaryType = "hdi".
#' @param quantiles numeric, quantiles used for a credible interval. Used if summaryType = "quantile".
#' @param type character, should the summaries be calculated for the link or the response?
#'  Supported inputs are "response" and "link". Default is "response".
#' @param burn numeric, initial number of iterations to be discarded as burn-in. Default is 0.
#' @param ... additional parameters, unused.
#' @export
#' @returns A list (invisible) containing the central summary and the bounds of the credible interval generated.
predict.btpc_MCMC <- function(object,
                              temp_interval = NULL,
                              summaryType = "hdi",
                              centralSummary = "median",
                              prob = .9,
                              quantiles = c(.05, .95),
                              burn = 0,
                              type = "response",
                              ...) {
  if (!"btpc_MCMC" %in% class(object)) stop("Unexpected type for parameter 'object'. Only use this method with the output of b_TPC().")
  if (!"btpc_model" %in% class(object$model_spec)) stop("Misconfigured Model Specification.")
  if (!(summaryType %in% c("hdi", "quantile"))) stop('Unsupported argument for "summaryType". Currently only "quantile" and "hdi" are supported.')
  if (!(centralSummary %in% c("mean", "median", "MAP"))) stop('Unsupported argument for "centralSummary". Currently only "median" and "mean" are supported.')
  if (is.null(temp_interval)) temp_interval <- seq(from = min(object$data$Temp), to = max(object$data$Temp), length.out = 1000)
  if (!is.numeric(temp_interval)) stop('Parameter `temp_interval` must be numeric.')
  if (!is.numeric(burn) || length(burn) != 1) {
    warning("Unsupported argument for parameter `burn`. Default value of 0 is used.")
    burn <- 0
  }

  llh <- attr(object$model_spec, "distribution")
  llh_params <- attr(llh, "llh_parameters")
  if (is(object$samples, "mcmc")) s_list <- list(object$samples)
  else s_list <- object$samples

  samp_params <- colnames(s_list[[1]])
  formula_params <- samp_params[!(samp_params %in% names(llh_params))]
  tpc_fun <- get_model_function(object$model_spec, type = type)

  # assign constants
  MA <- list(Temp = temp_interval)
  if (length(object$constants) > 0) {
    for (i in 1:length(object$constants)) {
      MA[names(object$constants)[i]] <- object$constants[i]
    }
  }



  if (length(s_list) > 1 && centralSummary == "MAP") {
    #ive already spent too much time refactoring do_map, add support for this laaater
    stop("centralSummary = 'MAP' can only be used if the fit object has one chain.")
  }

  out_list <- list()
  for (i in 1:length(s_list)) {
    s <- s_list[[i]]
    max.ind <- nrow(s)

    #evaluate the TPC
    tpc_evals <- simplify2array(.mapply(
      FUN = tpc_fun, dots = data.frame(s[(burn + 1):max.ind, formula_params]),
      MoreArgs = MA
    ))

    #calculate location statistic
    if (centralSummary == "median") {
      centers <- matrixStats::rowMedians(tpc_evals)
    } else if (centralSummary == "mean") {
      centers <- matrixStats::rowMeans2(tpc_evals)
    } else if (centralSummary == "MAP") {
      map_row <- which(apply(s, 1, function(x) all(x == object$MAP_parameters[1:ncol(s)])))
      centers <- tpc_evals[,map_row]
    }

    #calculate CI bounds
    if (summaryType == "hdi") {
      if (length(prob) < 1) stop("Probability must be provided for summaryType = 'hdi'.")
      if (length(prob) > 1) warning("Only the first value in parameter `prob` is used to calculate the credible interval.")
      hdi_mat <- apply(FUN = HDInterval::hdi, X = tpc_evals, MARGIN = 1, credMass = prob[1])
      upper_bounds <- hdi_mat[2, ]
      lower_bounds <- hdi_mat[1, ]
    } else if (summaryType == "quantile") {
      if (length(quantiles) < 2) stop("Two quantiles must be provided to calculate credible interval.")
      if (length(quantiles) > 2) warning("Only the first two values in parameter `quantiles` are used to calculate the credible interval.")
      if (!all(quantiles < 1 & quantiles > 0)) stop("Quantiles must be between 0 and 1.")
      upper_bounds <- matrixStats::rowQuantiles(tpc_evals, probs = quantiles[2])
      lower_bounds <- matrixStats::rowQuantiles(tpc_evals, probs = quantiles[1])
    }

    output <- list(
      temp_interval,
      centers,
      upper_bounds,
      lower_bounds
    )

    names(output) <- c(
      "temp_interval",
      paste0(centralSummary, "s"),
      "upper_bounds",
      "lower_bounds"
    )

    out_list[[paste0("Chain",i)]] <- output
  }

  out_list
}

#' Plot an model fit by `b_TPC()`.
#'
#' Plots thermal performance curve summaries using output from [b_TPC()]'s nimble MCMC.
#' @export
#' @inheritParams predict.btpc_MCMC
#' @param x `btpc_MCMC`, object output from performing MCMC using the `bTPC` function.
#' @param main character, the title of the plot(s). Must be length 1 or the same length as the number of chains
#' @param ylab character, a title for the y axis. Default is "Trait".
#' @param xlab character, a title for the x-axis. Default is "Temperature (C)"
#' @param ylim numeric, the limits for the y-axis.
#' @param legend logical, should a legend be added to the plot? Default is TRUE.
#' @param legend_position character, position of the legend. Only used if legend = TRUE. Default is "bottomright".
#' @param ... additional parameters passed to `plot.default()`.
plot.btpc_MCMC <- function(x,
                           temp_interval = NULL,
                           summaryType = "hdi",
                           centralSummary = "median",
                           prob = .9,
                           quantiles = c(.05, .95),
                           burn = 0,
                           type = "response",
                           main = NULL,
                           ylab = "Trait",
                           xlab = "Temperature (C)",
                           ylim = NULL,
                           legend = TRUE, legend_position = "bottomright",
                           ...) {
  sms <- predict.btpc_MCMC(
    object = x,
    temp_interval = temp_interval,
    summaryType = summaryType,
    centralSummary = centralSummary,
    prob = prob,
    quantiles = quantiles,
    burn = burn,
    type = type
  )

  if (is.null(main) || (length(main) != 1 && length(main) != length(sms))) {
    warning("Using default title for plots. Either one title or individual titles for each plot may be provided.")
    main <- paste0("Chain ", 1:length(sms))
  }

  for (i in 1:length(sms)) {
    sm <- sms[[i]]
    if ("btpc_binomial" %in% class(x$model_spec)) {
      yl <- if (is.null(ylim)) c(0, 1.2) else ylim

      plot(sm$temp_interval, sm$upper_bounds,
           type = "l", col = "blue", lty = 2, main = main[i],
           ylab = paste0(ylab, " / n"), xlab = xlab, ylim = yl, ...
      )

    } else {
      yl <- if (is.null(ylim)) c(0, max(sm$upper_bounds, x$data$Trait)) else ylim

      plot(sm$temp_interval, sm$upper_bounds,
           type = "l", col = "blue", lty = 2, main = main[i],
           ylab = ylab, xlab = xlab, ylim = yl, ...
      )
    }

    graphics::points(sm$temp_interval, sm$lower_bounds, type = "l", col = "blue", lty = 2)
    graphics::points(sm$temp_interval, sm[[paste0(centralSummary, "s")]], type = "l", col = "red")


    if ("btpc_binomial" %in% class(x$model_spec)) {
      graphics::points(x$data$Temp, x$data$Trait / x$data$n, pch = 16, cex = .75)
    } else {
      graphics::points(x$data$Temp, x$data$Trait, pch = 16, cex = .75)
    }

    if (legend) {
      graphics::legend(legend_position,
                       legend = c("Bounds", tools::toTitleCase(paste0(centralSummary, "s"))),
                       lty = c(2, 1), col = c("blue", "red")
      )
    }
  }

}

#' Provide thermal performance curve posterior predictive summaries
#'
#' Provide thermal performance curve posterior predictive summaries using output from [b_TPC()]'s nimble MCMC.
#'
#' @export
#' @details This function returns various summaries of the output of the thermal performance curve posterior predictive model samples, generated using MCMC samples from the object returned by `b_TPC()`.
#' @param TPC `btpc_MCMC`, object output from performing MCMC using the `b_TPC` function.
#' @param centralSummary character, central summary measure used. Currently supported options are "median" (default) and "mean".
#' @inheritParams predict.btpc_MCMC
#' @param seed integer, seed value to be used. Useful for ensuring that results are reproducible. Default is NULL.
#' @returns A list containing the central summary and the bounds of the credible interval generated by the posterior samples.
posterior_predictive <- function(TPC,
                                 temp_interval = NULL,
                                 summaryType = "hdi",
                                 centralSummary = "median",
                                 prob = .9,
                                 quantiles = c(.05, .95),
                                 burn = 0,
                                 type = "response",
                                 seed = NULL) {
  if (!"btpc_MCMC" %in% class(TPC)) stop("Unexpected type for parameter 'TPC'. Only use this method with the output of b_TPC().")
  if (!(is.null(seed))) {
    if (!(is.integer(seed))) stop('Argument "seed" must be integer valued')
    set.seed(seed)
  }


  if (is.null(temp_interval)) {
    temp_interval <- seq(from = min(TPC$data$Temp), to = max(TPC$data$Temp), length.out = 1000)
  } else if (length(temp_interval) < 1000) {
    warning("Taking posterior predictive samples at less than 1000 points may lead to innaccurate results.")
  }
  llh <- attr(TPC$model_spec, "distribution")

  if (is.null(llh)) stop("Misconfigured Model Specification.")
  if (!llh %in% immutable_llh_list) {
    stop("posterior_predictive is only available for non-custom likelihood specifications.")
  }

  llh_params <- attr(llh, "llh_parameters")
  if (is(TPC$samples, "mcmc")) s_list <- list(TPC$samples)
  else s_list <- TPC$samples

  samp_params <- colnames(s_list[[1]])
  formula_params <- samp_params[!(samp_params %in% names(llh_params))]
  tpc_fun <- get_model_function(TPC$model_spec, type = type)

  # assign constants
  MA <- list(Temp = temp_interval)
  if (length(TPC$constants) > 0) {
    for (i in 1:length(TPC$constants)) {
      MA[names(TPC$constants)[i]] <- TPC$constants[i]
    }
  }

  out_list <- list()
  for (i in 1:length(s_list)) {
    s <- s_list[[i]]
    max.ind <- nrow(s)

    #evaluate the TPC
    tpc_evals <- simplify2array(.mapply(
      FUN = tpc_fun, dots = data.frame(s[(burn + 1):max.ind, formula_params]),
      MoreArgs = MA
    ))

    tpc_ev <- matrixStats::rowMeans2(tpc_evals)

    # get one sample at each temperature from each posterior sample

    # I REALLLY hate how this is written
    # I would love to simulate directly in the NIMBLE model but this cannot be done at
    # points not included in the original training dataset, because ?????
    # prevents issue #38 on github from being fixed
    if ("btpc_normal" %in% class(TPC$model_spec)) {
      post_pred_draw <- function(X) { # this can be optimized i think. a lot of overhead
        return(truncnorm::rtruncnorm(
          n = length(X) - 1, mean = X[1:(length(X) - 1)], sd = sqrt(X[length(X)]),
          a = 0
        ))
      }
      post_pred_samples <- apply(
        FUN = post_pred_draw, X = rbind(tpc_evals, s[(burn + 1):max.ind, "sigma.sq"]),
        MARGIN = 2
      )
    } else if ("btpc_gamma" %in% class(TPC$model_spec)) {
      post_pred_draw <- function(X) { # this can be optimized i think. a lot of overhead
        return(stats::rgamma(
          n = length(X) - 1, rate = X[1:(length(X) - 1)], shape = X[length(X)]
        )) # TODO verify if this is parameterized correctly
      }
      post_pred_samples <- apply(
        FUN = post_pred_draw, X = rbind(tpc_evals, s[(burn + 1):max.ind, "shape_var"]),
        MARGIN = 2
      )
    } else {
      if ("btpc_poisson" %in% class(TPC$model_spec)) {
        post_pred_draw <- function(X) { # this can be optimized i think. a lot of overhead
          return(stats::rpois(
            n = length(X), lambda = X
          )) # TODO verify if this is parameterized correctly
        }
      } else if ("btpc_bernoulli" %in% class(TPC$model_spec)) {
        post_pred_draw <- function(X) { # this can be optimized i think. a lot of overhead
          return(stats::rbinom(
            n = length(X), size = 1, prob = X
          )) # TODO verify if this is parameterized correctly
        }
      } else if ("btpc_binomial" %in% class(TPC$model_spec)) {
        post_pred_draw <- function(X) { # this can be optimized i think. a lot of overhead
          return(stats::rbinom(
            n = length(X), size = floor(mean(TPC$data$n)), prob = X
          )) # TODO verify if this is parameterized correctly
        }
      } else if ("btpc_exponential" %in% class(TPC$model_spec)) {
        post_pred_draw <- function(X) { # this can be optimized i think. a lot of overhead
          return(stats::rexp(
            n = length(X), rate = 1 / X # R parameterizes the exponential with mean = 1 / rate
          )) # TODO verify if this is parameterized correctly
        }
      } else {
        # not sure this would be caught elsewhere
        stop("Misconfigured Model Specification. If you see this error, please contact the package developers.")
      }
      post_pred_samples <- apply(
        FUN = post_pred_draw, X = tpc_evals,
        MARGIN = 2
      )
    }

    #calculate location statistic
    if (centralSummary == "median") {
      centers <- matrixStats::rowMedians(post_pred_samples)
    } else if (centralSummary == "mean") {
      centers <- matrixStats::rowMeans2(post_pred_samples)
    } else {
      stop("Unsupported argument for 'centralSummary'. Currently only 'median' and 'mean' are supported.")
    }


    #calculate bounds
    if (summaryType == "hdi") { #later remove dependency on HDInterval with optimized sorting algorithm
      if (length(prob) < 1) stop("Probability must be provided for summaryType = 'hdi'.")
      if (length(prob) > 1) warning("Only the first value in parameter `prob` is used to calculate the credible interval.")
      hdi_mat <- apply(FUN = HDInterval::hdi, X = post_pred_samples, MARGIN = 1, credMass = prob[1])
      upper_bounds <- hdi_mat[2, ]
      lower_bounds <- hdi_mat[1, ]
    } else if (summaryType == "quantile") {
      if (length(quantiles) < 2) stop("Two quantiles must be provided to calculate credible interval.")
      if (length(quantiles) > 2) warning("Only the first two values in parameter `quantiles` are used to calculate the credible interval.")
      if (!all(quantiles < 1 & quantiles > 0)) stop("Quantiles must be between 0 and 1.")
      upper_bounds <- matrixStats::rowQuantiles(post_pred_samples, probs = quantiles[2])
      lower_bounds <- matrixStats::rowQuantiles(post_pred_samples, probs = quantiles[1])
    } else {
      stop("Unsupported argument for 'summaryType'. Currently only 'quantile' and 'hdi' are supported.")
    }

    output <- list(
      temp_interval,
      centers,
      upper_bounds,
      lower_bounds,
      tpc_ev,
      TPC$data,
      TPC$model_spec
    )

    names(output) <- c(
      "temp_interval",
      paste0(centralSummary, "s"),
      "upper_bounds",
      "lower_bounds",
      "TPC_means",
      "data",
      "model_spec"
    )

    out_list[[paste0("Chain",i)]] <- output
  }

  class(out_list) <- "btpc_prediction"
  return(out_list)
}

#' Plots posterior predictive samples
#'
#' Plots the output of [posterior_predictive()].
#'
#' @export
#' @param x `btpc_prediction` or `btpc_MCMC`, output from [posterior_predictive()] or [b_TPC()], respectively.
#' @param ylab character, a title for the y-axis. Default is "Trait".
#' @param ylim length 2 numeric, The limits of the y-axis of the plot. Defaults to the range of the data if unspecified.
#' @param main character, the title of the plot(s). Must be length 1 or the same length as the number of chains.
#' @param legend logical, should a legend be added to the plot? Default is TRUE.
#' @param legend_position character, position of the legend. Only used if legend = TRUE. Default is "bottomright".
#' @param ... additional parameters passed to [plot.default()].
plot_prediction <- function(x, ...) {
  UseMethod("plot_prediction")
}

#' @export
plot_prediction.default <- function(x, ...) {
  stop("Invalid type for parameter 'x'. Input should be the output of 'posterior_predictive()' or `b_TPC()`.")
}

#' @export
#' @rdname plot_prediction
plot_prediction.btpc_prediction <- function(x, ylab = "Trait", ylim = NULL, main = NULL,
                            legend = TRUE, legend_position = "bottomright", ...) {
  if (!"btpc_prediction" %in% class(x)) {
    stop("Invalid type for parameter 'x'. Input should be the output of 'posterior_predictive()'.")
  }


  if (is.null(main) || (length(main) != 1 && length(main) != length(x))) {
    warning("Using default title for plots. Either one title or individual titles for each plot may be provided.")
    main <- paste0("Chain ", 1:length(x))
  }

  for (i in 1:length(x)) {
    s <- x[[i]]

    if (!is.null(s$medians)) {
      center_name <- "Medians"
      s$centers <- s$medians
    } else if (!is.null(s$means)) {
      center_name <- "Means"
      s$centers <- s$means
    } else {
      stop("Misconfigured posterior predictive. Input must contain either posterior means or medians.")
    }

    if ("btpc_binomial" %in% class(x$model_spec)) {
      if (missing(ylim)) {
        yl <- c(0,1.2)
      } else {
        yl <- ylim
      }

      # make average N in data for sample_n
      sample_n <- floor(mean(s$data$n))
      plot(s$temp_interval, s$upper_bounds / sample_n,
        type = "l", lty = 3, col = "blue", xlab = "Temperature (C)",
        ylab = paste0(ylab, " / n"), ylim = yl, ...
      )

      graphics::points(s$temp_interval, s$TPC_means, col = "red", type = "l", lty = 2, lwd = 1.1)
      graphics::points(s$temp_interval, s$lower_bounds / sample_n, type = "l", col = "blue", lty = 3)
      graphics::points(s$temp_interval, s$centers / sample_n, type = "l", col = "blue")
    } else {


      if (missing(ylim)) {
        yl <- c(0, max(max(s$upper_bounds), max(s$data$Trait)))
      } else {
        yl <- ylim
      }
      plot(s$temp_interval, s$upper_bounds, main = main[i],
           type = "l", lty = 3, col = "blue", xlab = "Temperature (C)",
           ylab = ylab, ylim = yl, ...

      )
      graphics::points(s$temp_interval, s$TPC_means, col = "red", type = "l", lty = 2, lwd = 1.1)
      graphics::points(s$temp_interval, s$lower_bounds, type = "l", col = "blue", lty = 3)
      graphics::points(s$temp_interval, s$centers, type = "l", col = "blue")
    }


    if ("btpc_binomial" %in% class(x$model_spec)) {
      graphics::points(s$data$Temp, s$data$Trait / s$data$n, pch = 16, cex = .75)
    } else {
      graphics::points(s$data$Temp, s$data$Trait, pch = 16, cex = .75)
    }

    if (legend) {
      graphics::legend(legend_position,
                       legend = c("PP Bounds", "TPC Means", paste0("PP ",center_name)),
                       lty = c(3, 2, 1), col = c("blue", "red", "blue")
      )
    }
  }

}

#' @export
#' @inheritParams plot_prediction
#' @param ... additional parameters passed to `plot()`.
#' @rdname posterior_predictive
plot_prediction.btpc_MCMC <- function(x, ylab = "Trait",
                                      legend = TRUE, legend_position = "bottomright",
                                      temp_interval = NULL,
                                      summaryType = "hdi",
                                      centralSummary = "median",
                                      prob = .9,
                                      quantiles = c(.05, .95),
                                      burn = 0,
                                      seed = NULL,
                                      ...) {
  if (!"btpc_MCMC" %in% class(x)) {
    stop("Invalid type for parameter 'x'. Input should be the output of 'b_TPC()'.")
  }
  pred <- posterior_predictive(x, temp_interval = temp_interval,
                               summaryType = summaryType,
                               centralSummary = centralSummary,
                               prob = prob,
                               quantiles = quantiles,
                               burn = burn,
                               seed = seed)

  plot_prediction(x = pred, ylab = ylab, legend = legend, legend_position = legend_position, ...)
}

#' Plots Posterior Sample Histogram
#'
#' @importFrom graphics hist.default
#' @param x `btpc_MCMC`, object output from performing MCMC using the `bTPC` function.
#' @param plot logical, should histograms be plotted? Default is TRUE.
#' @param burn numeric, initial number of iterations to be discarded as burn-in. Default is 0.
#' @param ... parameters passed to [hist.default].
#'
#' @return Returns a named list of the `histogram` objects for each model parameter. Returned invisibly unless plot = F.
#' @export
hist.btpc_MCMC <- function(x, plot = T, burn = 0, ...) {
  stopifnot("Unexpected type for parameter 'x'. Only use this method with the output of b_TPC()." = "btpc_MCMC" %in% class(x))
  stopifnot("Parameter 'burn' must be numeric." = is.numeric(burn))

  burn <- floor(burn)
  smp <- x$samples
  N <- nrow(smp)
  names <- colnames(smp)
  out <- list()

  if (burn >= N) stop("Parameter 'burn' must be smaller than the total number of samples.")
  if (plot) {
    for (i in 1:ncol(smp)) {
      out[[names[i]]] <- hist.default(smp[(burn+1):N,i], main = paste("Posterior Histogram of", names[i]), xlab = names[i], ...)
    }
    invisible(out)
  } else {
    for (i in 1:ncol(smp)) {
      out[[names[i]]] <- hist.default(smp[(burn+1):N,i], plot = F, ...)
    }
    out
  }
}
