#' Provide thermal performance curve summaries
#'
#' Provide thermal performance curve summaries using output from `bTPC`'s nimble MCMC
#'
#' @details This function returns various summaries of the output of the thermal performance curve model, generarted using MCMC samples from the object returned by `bTPC`
#' @param TPC list, object output from performing MCMC using the `bTPC` function.
#' @param Temp_interval vector, reference values to use to compute values of the thermal performance curve. If no vector is provided, Temp_interval is set as a sequence from the lowest observed temperature in the data to the highest observed temperature in the data, with 1000 equally spaced points
#' @param summaryOnly logical, should the function return only summaries, or the entire matrix of thermal performance curve evaluations AND the summaries? default = TRUE
#' @param summaryType character, type of summary used. Currently supported options are "quantile" and "hdi" (default option). Users that wish to use alternatives may consider summaryOnly = FALSE, and using their desired method on the raw TPC evaluations.
#' @param centralSummary character, central summary measure used. Currently supported options are "median" (default) and "mean". Users that wish to use alternatives may consider summaryOnly = FALSE, and using their desired method on the raw TPC evaluations.
#' @param plot logical, should a plot be created? default = TRUE
#' @param probs numeric, represents either the quantiles to be used during summarization (in the form c(lower, upper)) or the credible mass used to compute the highest density intervals
#' @param burn numeric, initial number of iterations to be discarded as burn-in (default = 0)
#' @param plotOnly logical, should a plot be generated without returning a list of summaries? if plotOnly = TRUE, an invisible NULL is returned and only plot is generated. Default = FALSE
#' @param traitName string, name of Trait to be used as y-axis if a plot is generated. default = "Trait"
#' @param ... additional parameters to be passed as arguments
#' @return return types vary by argument. For plotOnly = TRUE, invisible NULL is returned. For summaryOnly = TRUE, a list of summaries is returned. For summaryOnly = FALSE, a list is returned with summaries as well as the entire set of thermal performance curve evaluations
#' @examples
#' ## need data to set up example here. placeholder for now
#' ## set params and reference temperature set
#' myfun = str2tpc_fun(model = 'gaussian')
#' param_set = c(T.opt = 36, a = 6.5, rmax = 2.75)
#' Temp_ref = seq(from = 5, to = 50, length.out = 1000)
#' plot(Temp_ref, myfun(params = param_set, Temp = Temp_ref), type = 'l')


bayesTPC_summary <- function(TPC, Temp_interval = NULL, summaryOnly = TRUE,
                             summaryType = 'hdi', centralSummary = 'median',
                             plot = TRUE, probs = c(.05, .95),
                             burn = 0, plotOnly = FALSE, traitName = 'Trait', ...){
  if (!(summaryType %in% c('hdi', 'quantile')) & summaryOnly) stop('Unsupported argument for "summaryType". Currently only "quantile" and "hdi" are supported. To use other summary functions, try using summaryOnly = FALSE and working with the entire matrix')
  if (!(centralSummary %in% c('mean', 'median')) & summaryOnly) stop('Unsupported argument for "centralSummary". Currently only "median" and "mean" are supported. To use other summary functions, try using summaryOnly = FALSE and working with the entire matrix')

  if (length(probs) == 2 & (probs[2] < probs[1])){
    stop('Incorrect specification of tail probabilities for quantile method. Tail probs must be provided in the form c(lower, upper)')
  }
  if (!(is.logical(summaryOnly))){
    stop('Argument summaryOnly must be provided as a logical (= TRUE or = FALSE)')
  }
  if ((length(probs) > 1) & summaryType == 'hdi'){
    if (all(probs == c(.05,.95))){
      probs = .9
      warning('Currently using summaryType = "hdi". Default credible interval mass is credMass = .9)')
    } else{
      stop('For summaryType = "hdi", only one value is accepted for argument probs')
    }

  }

  if (plot == FALSE & plotOnly == TRUE){
    stop('Argument plotOnly = TRUE requires argument plot to also be TRUE')
  }

  if (is.null(Temp_interval)) Temp_interval = seq(from = min(TPC$data$Temp), to = max(TPC$data$Temp), length.out = 1000)
  tpc_fun = bayesTPC:::model_evaluation_function(TPC$modelType)
  max.ind = nrow(TPC$samples)
  tpc_evals = apply(tpc_fun, X = TPC$samples[(burn+1):max.ind,], Temp = Temp_interval, MARGIN = 1, ...)

  if (centralSummary == 'median'){
    if (summaryType == 'quantile'){
      upper_bounds = rowQuantiles(tpc_evals, probs = probs[2])
      lower_bounds = rowQuantiles(tpc_evals, probs = probs[1])
      medians = rowMedians(tpc_evals)
    }
    if (summaryType == 'hdi'){
      hdi_mat = apply(FUN = hdi, X = tpc_evals, MARGIN = 1, credMass = probs)
      upper_bounds = hdi_mat[2,]
      lower_bounds = hdi_mat[1,]
      medians = rowMedians(tpc_evals)
    }
    if (plot){
      plot(Temp_interval, upper_bounds, type = 'l', lty = 2, col = 'blue', xlab = 'Temperature (C)', ylab = traitName, ylim = c(0, max(upper_bounds)))
      points(Temp_interval, lower_bounds, type = 'l', col = 'blue', lty = 2)
      points(Temp_interval, medians, type = 'l', col = 'blue')
      if (plotOnly){
        return(invisible(NULL))
      } else{
        if (summaryOnly == FALSE){
          return(list(TPC_vals = tpc_evals,
                      Temp_interval = Temp_interval,
                      Upper_bounds = upper_bounds,
                      Lower_bounds = lower_bounds,
                      Medians = medians))
        } else{
          return(list(Temp_interval = Temp_interval,
                      Upper_bounds = upper_bounds,
                      Lower_bounds = lower_bounds,
                      Medians = medians))
        }
      }
    } else{
      if (summaryOnly == FALSE){
        return(list(TPC_vals = tpc_evals,
                    Temp_interval = Temp_interval,
                    Upper_bounds = upper_bounds,
                    Lower_bounds = lower_bounds,
                    Medians = medians))
      } else{
        return(list(Temp_interval = Temp_interval,
                    Upper_bounds = upper_bounds,
                    Lower_bounds = lower_bounds,
                    Medians = medians))
      }
    }
  }
  if (centralSummary == 'mean'){
    if (summaryType == 'quantile'){
      upper_bounds = rowQuantiles(tpc_evals, probs = tail.probs[2])
      lower_bounds = rowQuantiles(tpc_evals, probs = tail.probs[1])
      means = rowMeans2(tpc_evals)
    }
    if (summaryType == 'hdi'){
      hdi_mat = apply(FUN = hdi, X = tpc_evals, MARGIN = 1, credMass = probs)
      upper_bounds = hdi_mat[2,]
      lower_bounds = hdi_mat[1,]
      means = rowMeans2(tpc_evals)
    }
    if (plot){
      plot(Temp_interval, upper_bounds, type = 'l', col = 'blue', lty = 2, xlab = 'Temperature (C)', ylab = traitName, ylim = c(0, max(upper_bounds)))
      points(Temp_interval, lower_bounds, type = 'l', col = 'blue', lty = 2)
      points(Temp_interval, means, type = 'l', col = 'blue')
      if (plotOnly){
        return(invisible(NULL))
      } else{
        if (summaryOnly == FALSE){
          return(list(TPC_vals = tpc_evals,
                      Temp_interval = Temp_interval,
                      Upper_bounds = upper_bounds,
                      Lower_bounds = lower_bounds,
                      Means = means))
        } else{
          return(list(Temp_interval = Temp_interval,
                      Upper_bounds = upper_bounds,
                      Lower_bounds = lower_bounds,
                      Means = means))
        }
      }
    } else{
      if (summaryOnly == FALSE){
        return(list(TPC_vals = tpc_evals,
                    Temp_interval = Temp_interval,
                    Upper_bounds = upper_bounds,
                    Lower_bounds = lower_bounds,
                    Means = means))
      } else{
        return(list(Temp_interval = Temp_interval,
                    Upper_bounds = upper_bounds,
                    Lower_bounds = lower_bounds,
                    Means = means))
      }
    }
  }
}

#' Provide thermal performance curve posterior predictive summaries
#'
#' Provide thermal performance curve posterior predictive summaries using output from `bTPC`'s nimble MCMC
#'
#' @details This function returns various summaries of the output of the thermal performance curve posterior predictive model samples, generarted using MCMC samples from the object returned by `bTPC`
#' @param TPC list, object output from performing MCMC using the `bTPC` function.
#' @param Temp_interval vector, reference values to use to compute values of the thermal performance curve. If no vector is provided, Temp_interval is set as a sequence from the lowest observed temperature in the data to the highest observed temperature in the data, with 1000 equally spaced points
#' @param summaryOnly logical, should the function return only summaries, or the entire matrix of thermal performance curve posterior predictive samples AND the summaries? default = TRUE
#' @param summaryType character, type of summary used. Currently supported options are "quantile" and "hdi" (default option). Users that wish to use alternatives may consider summaryOnly = FALSE, and using their desired method on the raw TPC evaluations.
#' @param centralSummary character, central summary measure used. Currently supported options are "median" (default) and "mean". Users that wish to use alternatives may consider summaryOnly = FALSE, and using their desired method on the raw TPC evaluations.
#' @param plot logical, should a plot be created? default = TRUE
#' @param probs numeric, represents either the quantiles to be used during summarization (in the form c(lower, upper)) or the credible mass used to compute the highest density intervals
#' @param burn numeric, initial number of iterations to be discarded as burn-in (default = 0)
#' @param plotOnly logical, should a plot be generated without returning a list of summaries? if plotOnly = TRUE, an invisible NULL is returned and only plot is generated. Default = FALSE
#' @param traitName string, name of Trait to be used as y-axis if a plot is generated. default = "Trait"
#' @param seed integer, seed value to be used. Useful for ensuring that results are reproducible. default = NULL
#' @param ... additional parameters to be passed as arguments
#' @return return types vary by argument. For plotOnly = TRUE, invisible NULL is returned. For summaryOnly = TRUE, a list of summaries is returned. For summaryOnly = FALSE, a list is returned with summaries as well as the entire set of thermal performance curve posterior predictive samples
#' @examples
#' ## need data to set up example here. placeholder for now
#' ## set params and reference temperature set
#' myfun = str2tpc_fun(model = 'gaussian')
#' param_set = c(T.opt = 36, a = 6.5, rmax = 2.75)
#' Temp_ref = seq(from = 5, to = 50, length.out = 1000)
#' plot(Temp_ref, myfun(params = param_set, Temp = Temp_ref), type = 'l')


posteriorPredTPC <- function(TPC, Temp_interval = NULL, summaryOnly = TRUE,
                             summaryType = 'hdi', centralSummary = 'median',
                             plot = TRUE, probs = c(.05, .95),
                             burn = 0, plotOnly = FALSE, traitName = 'Trait', seed = NULL, ...){
  if (!(summaryType %in% c('hdi', 'quantile')) & summaryOnly) stop('Unsupported argument for "summaryType". Currently only "quantile" and "hdi" are supported. To use other summary functions, try using summaryOnly = FALSE and working with the entire matrix')
  if (!(centralSummary %in% c('mean', 'median')) & summaryOnly) stop('Unsupported argument for "centralSummary". Currently only "median" and "mean" are supported. To use other summary functions, try using summaryOnly = FALSE and working with the entire matrix')
  if (!(is.null(seed))){
    if (!(is.integer(seed))) stop('Argument "seed" must be integer valued')
  }
  if (length(probs) == 2 & (probs[2] < probs[1])){
    stop('Incorrect specification of tail probabilities for quantile method. Tail probs must be provided in the form c(lower, upper)')
  }
  if (!(is.logical(summaryOnly))){
    stop('Argument summaryOnly must be provided as a logical (= TRUE or = FALSE)')
  }
  if ((length(probs) > 1) & summaryType == 'hdi'){
    if (all(probs == c(.05,.95))){
      probs = .9
      warning('Currently using summaryType = "hdi". Default credible interval mass is credMass = .9)')
    } else{
      stop('For summaryType = "hdi", only one value is accepted for argument probs')
    }

  }

  if (plot == FALSE & plotOnly == TRUE){
    stop('Argument plotOnly = TRUE requires argument plot to also be TRUE')
  }

  if (is.null(Temp_interval)) Temp_interval = seq(from = min(TPC$data$Temp), to = max(TPC$data$Temp), length.out = 1000)
  tpc_fun = bayesTPC:::model_evaluation_function(TPC$modelType)
  max.ind = nrow(TPC$samples)
  if (!is.null(seed)) set.seed(seed)
  truncmeans = apply(tpc_fun, X = TPC$samples[(burn+1):max.ind,], Temp = Temp_interval, MARGIN = 1, ...)
  ## checking logic here...
  post_pred_draw <- function(X){
    return(rtruncnorm(n = length(X) - 1, mean = X[1:(length(X)-1)], sd = sqrt(X[length(X)]),
                      a = 0))
  }
  post_pred_samples = apply(FUN = post_pred_draw, X = rbind(truncmeans, TPC$samples[(burn+1):max.ind,'sigma.sq']),
                            MARGIN = 2)
  tpc_ev = rowMeans2(apply(tpc_fun, X = TPC$samples[(burn+1):max.ind,], Temp = Temp_interval, MARGIN = 1, ...))
  if (centralSummary == 'median'){
    if (summaryType == 'quantile'){
      upper_bounds = rowQuantiles(post_pred_samples, probs = probs[2])
      lower_bounds = rowQuantiles(post_pred_samples, probs = probs[1])
      medians = rowMedians(post_pred_samples)
    }
    if (summaryType == 'hdi'){
      hdi_mat = apply(FUN = hdi, X = post_pred_samples, MARGIN = 1, credMass = probs)
      upper_bounds = hdi_mat[2,]
      lower_bounds = hdi_mat[1,]
      medians = rowMedians(post_pred_samples)
    }
    if (plot){
      plot(Temp_interval, upper_bounds, type = 'l', lty = 3, col = 'blue', xlab = 'Temperature (C)', ylab = traitName, ylim = c(0, max(max(upper_bounds), max(TPC$data$Trait))))
      points(Temp_interval, tpc_ev, col = 'red', type = 'l', lty = 2, lwd = 1.1)
      points(Temp_interval, lower_bounds, type = 'l', col = 'blue', lty = 3)
      points(Temp_interval, medians, type = 'l', col = 'blue')
      points(TPC$data$Temp, TPC$data$Trait)
      if (plotOnly){
        return(invisible(NULL))
      } else{
        if (summaryOnly == FALSE){
          return(list(Posterior_predictive_samples = post_pred_samples,
                      Temp_interval = Temp_interval,
                      Upper_bounds = upper_bounds,
                      Lower_bounds = lower_bounds,
                      Medians = medians,
                      TPC_mean = tpc_ev))
        } else{
          return(list(Temp_interval = Temp_interval,
                      Upper_bounds = upper_bounds,
                      Lower_bounds = lower_bounds,
                      Medians = medians,
                      TPC_mean = tpc_ev))
        }
      }
    } else{
      if (summaryOnly == FALSE){
        return(list(Posterior_predictive_samples = post_pred_samples,
                    Temp_interval = Temp_interval,
                    Upper_bounds = upper_bounds,
                    Lower_bounds = lower_bounds,
                    Medians = medians,
                    TPC_mean = tpc_ev))
      } else{
        return(list(Temp_interval = Temp_interval,
                    Upper_bounds = upper_bounds,
                    Lower_bounds = lower_bounds,
                    Medians = medians,
                    TPC_mean = tpc_ev))
      }
    }
  }
  if (centralSummary == 'mean'){
    if (summaryType == 'quantile'){
      upper_bounds = rowQuantiles(post_pred_samples, probs = tail.probs[2])
      lower_bounds = rowQuantiles(post_pred_samples, probs = tail.probs[1])
      means = rowMeans2(post_pred_samples)
    }
    if (summaryType == 'hdi'){
      hdi_mat = apply(FUN = hdi, X = post_pred_samples, MARGIN = 1, credMass = probs)
      upper_bounds = hdi_mat[2,]
      lower_bounds = hdi_mat[1,]
      means = rowMeans2(post_pred_samples)
    }
    if (plot){
      plot(Temp_interval, upper_bounds, type = 'l', col = 'blue', lty = 3, xlab = 'Temperature (C)', ylab = traitName, ylim = c(0, max(max(upper_bounds), max(TPC$data$Trait))))
      points(Temp_interval, tpc_ev, col = 'red', type = 'l', lty = 2, lwd = 1.1)
      points(Temp_interval, lower_bounds, type = 'l', col = 'blue', lty = 3)
      points(Temp_interval, means, type = 'l', col = 'blue')
      points(TPC$data$Temp, TPC$data$Trait)

      if (plotOnly){
        return(invisible(NULL))
      } else{
        if (summaryOnly == FALSE){
          return(list(Posterior_predictive_samples = post_pred_samples,
                      Temp_interval = Temp_interval,
                      Upper_bounds = upper_bounds,
                      Lower_bounds = lower_bounds,
                      Means = means,
                      TPC_mean = tpc_ev))
        } else{
          return(list(Temp_interval = Temp_interval,
                      Upper_bounds = upper_bounds,
                      Lower_bounds = lower_bounds,
                      Means = means,
                      TPC_mean = tpc_ev))
        }
      }
    } else{
      if (summaryOnly == FALSE){
        return(list(Posterior_predictive_samples = post_pred_samples,
                    Temp_interval = Temp_interval,
                    Upper_bounds = upper_bounds,
                    Lower_bounds = lower_bounds,
                    Means = means,
                    TPC_mean = tpc_ev))
      } else{
        return(list(Temp_interval = Temp_interval,
                    Upper_bounds = upper_bounds,
                    Lower_bounds = lower_bounds,
                    Means = means,
                    TPC_mean = tpc_ev))
      }
    }
  }
}
