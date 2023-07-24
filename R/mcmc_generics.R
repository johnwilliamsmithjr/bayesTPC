#' @export
print.btpc_MCMC <- function(x, digits = 3, ...) {
  cat("bayesTPC MCMC of type:", x$model_type)
  cat("\n\nModel Formula:\n", as.character(get_formula(x$model_type)), "\n\n", sep = "")
  s <- x$samples
  means <- round(matrixStats::colMeans2(s), digits)
  medians <- round(matrixStats::colMedians(s), digits)
  tbl <- cbind.data.frame(means, medians, x$priors[colnames(s)])
  rownames(tbl) <- colnames(s)
  colnames(tbl) <- c("Mean", "Median", "Priors")
  cat("Model Parameters:\n")
  print(tbl)
}

#' Provide thermal performance curve summaries
#'
#' Provide thermal performance curve summaries using output from [b_TPC()]'s nimble MCMC.
#' @export
#' @details This function returns various summaries of the output of the thermal performance curve model, generated using MCMC samples from the object returned by `b_TPC()`.
#' @param object `btpc_MCMC`, object output from performing MCMC using the `bTPC` function.
#' @param temp_interval numeric vector, reference values to use to compute values of the thermal performance curve.
#'  If no vector is provided, temp_interval is set as a sequence from the lowest observed temperature in the data to the highest observed temperature in the data, with 1,000 equally spaced points.
#' @param summaryType character, type of summary used. Currently supported options are "quantile" and "hdi" (default).
#' @param centralSummary character, central summary measure used. Currently supported options are "median" (default) and "mean".
#' @param prob numeric, the credible mass used to compute the highest density interval. Used if summaryType = "hdi".
#' @param quantiles length 2 numeric, quantiles used for a credible interval. Used if summaryType = "quantile".
#' @param burn numeric, initial number of iterations to be discarded as burn-in. Default is 0.
#' @param ... additonal parameters, unused.
#' @returns A list (invisible) containing the central summary and the bounds of the credible interval generated.
summary.btpc_MCMC <- function(object,
                              temp_interval = NULL,
                              summaryType = "hdi",
                              centralSummary = "median",
                              prob = .9,
                              quantiles = c(.05, .95),
                              burn = 0,
                              ...) {
  if (!(summaryType %in% c("hdi", "quantile"))) stop('Unsupported argument for "summaryType". Currently only "quantile" and "hdi" are supported.')
  if (!(centralSummary %in% c("mean", "median"))) stop('Unsupported argument for "centralSummary". Currently only "median" and "mean" are supported.')

  cat("bayesTPC MCMC of type:", object$model_type)
  cat("\n\nModel Formula:\n", as.character(get_formula(object$model_type)), sep = "")
  cat("\n\nModel Priors:\n")
  print(object$priors)
  if (length(object$constants) > 0) {
    cat("\n\nModel Constants:")
    cat("\n  ", names(object$constants), ": ", object$constants, sep = "")
  }
  cat("\nMCMC Results:")
  print(summary(object$samples))

  # assign constants
  MA <- list(Temp = temp_interval)
  if (length(object$constants) > 0) {
    for (i in 1:length(object$constants)) {
      MA[names(object$constants)[i]] <- object$constants[i]
    }
  }

  if (is.null(temp_interval)) temp_interval <- seq(from = min(object$data$Temp), to = max(object$data$Temp), length.out = 1000)
  tpc_fun <- get_model_function(object$model_type)
  max.ind <- nrow(object$samples)

  MA <- list(Temp = temp_interval)
  if (length(object$constants) > 0) {
    for (i in 1:length(object$constants)) {
      MA[names(object$constants)[i]] <- object$constants[i]
    }
  }

  tpc_evals <- simplify2array(.mapply(
    FUN = tpc_fun, dots = data.frame(object$samples[(burn + 1):max.ind, !colnames(object$samples) %in% "sigma.sq"]),
    MoreArgs = MA
  ))

  if (centralSummary == "median") {
    centers <- matrixStats::rowMedians(tpc_evals)
  } else if (centralSummary == "mean") {
    centers <- matrixStats::rowMeans2(tpc_evals)
  } else {
    stop("Unsupported argument for 'centralSummary'. Currently only 'median' and 'mean' are supported.")
  }

  if (summaryType == "hdi") {
    hdi_mat <- apply(FUN = HDInterval::hdi, X = tpc_evals, MARGIN = 1, credMass = prob)
    upper_bounds <- hdi_mat[2, ]
    lower_bounds <- hdi_mat[1, ]
  } else if (summaryType == "quantile") {
    upper_bounds <- matrixStats::rowQuantiles(tpc_evals, probs = quantiles[2])
    lower_bounds <- matrixStats::rowQuantiles(tpc_evals, probs = quantiles[1])
  } else {
    stop("Unsupported argument for 'summaryType'. Currently only 'quantile' and 'hdi' are supported.")
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
  invisible(output)
}

#' Plot an model fit by `b_TPC()`.
#'
#' Plots thermal performance curve summaries using output from [b_TPC()]'s nimble MCMC.
#' @export
#' @param x `btpc_MCMC`, object output from performing MCMC using the `bTPC` function.
#' @param temp_interval numeric vector, reference values to use to compute values of the thermal performance curve.
#'  If no vector is provided, temp_interval is set as a sequence from the lowest observed temperature in the data to the highest observed temperature in the data, with 1,000 equally spaced points.
#' @param summaryType character, type of summary used. Currently supported options are "quantile" and "hdi" (default).
#' @param centralSummary character, central summary measure used. Currently supported options are "median" (default) and "mean".
#' @param prob numeric, the credible mass used to compute the highest density interval. Used if summaryType = "hdi".
#' @param quantiles length 2 numeric, quantiles used for a credible interval. Used if summaryType = "quantile".
#' @param burn numeric, initial number of iterations to be discarded as burn-in. Default is 0.
#' @param ylab a title for the y axis.
#' @param ... additional parameters passed to `plot.default()`.
plot.btpc_MCMC <- function(x,
                           temp_interval = NULL,
                           summaryType = "hdi",
                           centralSummary = "median",
                           prob = .9,
                           quantiles = c(.05, .95),
                           burn = 0,
                           ylab = "Trait", ...) {
  if (!(summaryType %in% c("hdi", "quantile"))) stop('Unsupported argument for "summaryType". Currently only "quantile" and "hdi" are supported.')
  if (!(centralSummary %in% c("mean", "median"))) stop('Unsupported argument for "centralSummary". Currently only "median" and "mean" are supported.')
  if (is.null(temp_interval)) temp_interval <- seq(from = min(x$data$Temp), to = max(x$data$Temp), length.out = 1000)
  tpc_fun <- get_model_function(x$model_type)
  max.ind <- nrow(x$samples)

  # assign constants
  MA <- list(Temp = temp_interval)
  if (length(x$constants) > 0) {
    for (i in 1:length(x$constants)) {
      MA[names(x$constants)[i]] <- x$constants[i]
    }
  }

  tpc_evals <- simplify2array(.mapply(
    FUN = tpc_fun, dots = data.frame(x$samples[(burn + 1):max.ind, !colnames(x$samples) %in% "sigma.sq"]),
    MoreArgs = MA
  ))

  if (centralSummary == "median") {
    centers <- matrixStats::rowMedians(tpc_evals)
  } else if (centralSummary == "mean") {
    centers <- matrixStats::rowMeans2(tpc_evals)
  } else {
    stop("Unsupported argument for 'centralSummary'. Currently only 'median' and 'mean' are supported.")
  }

  if (summaryType == "hdi") {
    hdi_mat <- apply(FUN = HDInterval::hdi, X = tpc_evals, MARGIN = 1, credMass = prob)
    upper_bounds <- hdi_mat[2, ]
    lower_bounds <- hdi_mat[1, ]
  } else if (summaryType == "quantile") {
    upper_bounds <- matrixStats::rowQuantiles(tpc_evals, probs = quantiles[2])
    lower_bounds <- matrixStats::rowQuantiles(tpc_evals, probs = quantiles[1])
  } else {
    stop("Unsupported argument for 'summaryType'. Currently only 'quantile' and 'hdi' are supported.")
  }

  plot(temp_interval, upper_bounds, type = "l", col = "blue", lty = 2, ylab = ylab, xlab = "Temperature (C)", ylim = c(0, max(upper_bounds, x$data$Trait)), ...)
  graphics::points(temp_interval, lower_bounds, type = "l", col = "blue", lty = 2)
  graphics::points(temp_interval, centers, type = "l", col = "red")
  graphics::points(x$data$Temp, x$data$Trait, pch = 16, cex = .75)
}

#' Provide thermal performance curve posterior predictive summaries
#'
#' Provide thermal performance curve posterior predictive summaries using output from [b_TPC()]'s nimble MCMC.
#'
#' @export
#' @details This function returns various summaries of the output of the thermal performance curve posterior predictive model samples, generated using MCMC samples from the object returned by `b_TPC()`.
#' @param TPC `btpc_MCMC`, object output from performing MCMC using the `bTPC` function.
#' @param temp_interval vector, reference values to use to compute values of the thermal performance curve.
#'  If no vector is provided, temp_interval is set as a sequence from the lowest observed temperature in the data to the highest observed temperature in the data, with 1,000 equally spaced points.
#' @param summaryType character, type of summary used. Currently supported options are "quantile" and "hdi" (default option).
#' @param centralSummary character, central summary measure used. Currently supported options are "median" (default) and "mean".
#' @param prob numeric, the credible mass used to compute the highest density interval. Used if summaryType = "hdi".
#' @param quantiles length 2 numeric, quantiles used for a credible interval. Used if summaryType = "quantile".
#' @param burn numeric, initial number of iterations to be discarded as burn-in. Default is 0.
#' @param seed integer, seed value to be used. Useful for ensuring that results are reproducible. Default is NULL.
#' @returns A list containing the central summary and the bounds of the credible interval generated by the posterior samples.
#' @examples
#' ## need data to set up example here. placeholder for now
posterior_predictive <- function(TPC,
                                 temp_interval = NULL,
                                 summaryType = "hdi",
                                 centralSummary = "median",
                                 prob = .9,
                                 quantiles = c(.05, .95),
                                 burn = 0,
                                 seed = NULL) {
  if (!(is.null(seed))) {
    if (!(is.integer(seed))) stop('Argument "seed" must be integer valued')
  }

  if (is.null(temp_interval)) temp_interval <- seq(from = min(TPC$data$Temp), to = max(TPC$data$Temp), length.out = 1000)
  tpc_fun <- get_model_function(TPC$model_type)
  max.ind <- nrow(TPC$samples)

  # assign constants
  MA <- list(Temp = temp_interval)
  if (length(TPC$constants) > 0) {
    MA[names(TPC$constants)] <- TPC$constants
  }

  if (!is.null(seed)) set.seed(seed)
  truncmeans <- simplify2array(.mapply(
    FUN = tpc_fun, dots = data.frame(TPC$samples[(burn + 1):max.ind, !colnames(TPC$samples) %in% "sigma.sq"]),
    MoreArgs = MA
  ))

  post_pred_draw <- function(X) { # this can be optimized i think. a lot of overhead
    return(truncnorm::rtruncnorm(
      n = length(X) - 1, mean = X[1:(length(X) - 1)], sd = sqrt(X[length(X)]),
      a = 0
    ))
  }
  post_pred_samples <- apply(
    FUN = post_pred_draw, X = rbind(truncmeans, TPC$samples[(burn + 1):max.ind, "sigma.sq"]),
    MARGIN = 2
  )
  tpc_ev <- matrixStats::rowMeans2(truncmeans)

  if (centralSummary == "median") {
    centers <- matrixStats::rowMedians(post_pred_samples)
  } else if (centralSummary == "mean") {
    centers <- matrixStats::rowMeans2(post_pred_samples)
  } else {
    stop("Unsupported argument for 'centralSummary'. Currently only 'median' and 'mean' are supported.")
  }

  if (summaryType == "hdi") { # also can be optimized. god this function is slow
    hdi_mat <- apply(FUN = HDInterval::hdi, X = post_pred_samples, MARGIN = 1, credMass = prob)
    upper_bounds <- hdi_mat[2, ]
    lower_bounds <- hdi_mat[1, ]
  } else if (summaryType == "quantile") {
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
    TPC$data
  )

  names(output) <- c(
    "temp_interval",
    paste0(centralSummary, "s"),
    "upper_bounds",
    "lower_bounds",
    "TPC_means",
    "data"
  )

  class(out) <- "btpc_prediction"
  return(out)
}


#' Plots posterior predictive samples
#'
#' Plots the output of `posterior_predictive()`.
#'
#' @export
#' @param prediction `btpc_prediction`, output from `posterior_predictive()`.
#' @param ylab a title for the y-axis. Default is "Trait".
#' @param ... additional parameters passed to `plot.default()`.
plot_prediction <- function(prediction, ylab = "Trait", ...) {
  if (!"btpc_prediction" %in% class(prediction)) {
    stop("Input should be the output of 'posterior_predictive'.")
  }

  plot(prediction$temp_interval, prediction$upper_bounds, type = "l", lty = 3, col = "blue", xlab = "Temperature (C)", ylab = ylab, ylim = c(0, max(max(prediction$upper_bounds), max(prediction$data$Trait))), ...)
  graphics::points(prediction$temp_interval, prediction$tpc_ev, col = "red", type = "l", lty = 2, lwd = 1.1)
  graphics::points(prediction$temp_interval, prediction$lower_bounds, type = "l", col = "blue", lty = 3)
  graphics::points(prediction$temp_interval, prediction$medians, type = "l", col = "blue")
  graphics::points(prediction$data$Temp, prediction$data$Trait)
}
