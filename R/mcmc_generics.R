#' @export
print.btpc_MCMC <- function(x, digits = 3, ...){
  cat("bayesTPC MCMC of type:", x$model_type)
  cat("\n\nModel Formula:\n", as.character(get_formula(x$model_type)), "\n\n", sep = "")
  s <- x$samples
  means <- round(matrixStats::colMeans2(s), digits)
  medians <- round(matrixStats::colMedians(s), digits)
  tbl <- cbind.data.frame(means,medians,x$priors[colnames(s)])
  rownames(tbl) <- colnames(s)
  colnames(tbl) <- c("Mean", "Median", "Priors")
  cat("Model Parameters:\n")
  print(tbl)
}

#' @export
summary.btpc_MCMC <- function(object,
                              Temp_interval = NULL,
                              summaryType = "quantile",
                              centralSummary = "median",
                              prob = .9,
                              quantiles = c(.05,.95),
                              burn = 0,...){
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
  MA <- list(Temp = Temp_interval)
  if (length(object$constants) > 0) {
    for (i in 1:length(object$constants)) {
      MA[names(object$constants)[i]] <- object$constants[i]
    }
  }

  if (is.null(Temp_interval)) Temp_interval <- seq(from = min(object$data$Temp), to = max(object$data$Temp), length.out = 1000)
  tpc_fun <- get_model_function(object$model_type)
  max.ind <- nrow(object$samples)

  MA <- list(Temp = Temp_interval)
  if (length(object$constants) > 0) {
    for (i in 1:length(object$constants)) {
      MA[names(object$constants)[i]] <- object$constants[i]
    }
  }

  tpc_evals <- simplify2array(.mapply(
    FUN = tpc_fun, dots = data.frame(object$samples[(burn + 1):max.ind, !colnames(object$samples) %in% "sigma.sq"]),
    MoreArgs = MA
  ))

  if (centralSummary == "median"){
    centers <- matrixStats::rowMedians(tpc_evals)
  } else if (centralSummary == "mean") {
    centers <- matrixStats::rowMeans2(tpc_evals)
  } else {
    stop("Unsupported argument for 'centralSummary'. Currently only 'median' and 'mean' are supported.")
  }

  if (summaryType == "hdi"){
    hdi_mat <- apply(FUN = HDInterval::hdi, X = tpc_evals, MARGIN = 1, credMass = prob)
    upper_bounds <- hdi_mat[2, ]
    lower_bounds <- hdi_mat[1, ]
  } else if (summaryType == "quantile"){
    upper_bounds <- matrixStats::rowQuantiles(tpc_evals, probs = quantiles[2])
    lower_bounds <- matrixStats::rowQuantiles(tpc_evals, probs = quantiles[1])
  } else {
    stop("Unsupported argument for 'summaryType'. Currently only 'quantile' and 'hdi' are supported.")
  }

  output <- list(
    tpc_evals,
    Temp_interval,
    centers,
    upper_bounds,
    lower_bounds
  )

  names(output) <- c(
    "TPC_Evaluations",
    "Temp_Interval",
    tools::toTitleCase(paste0(centralSummary, "s")),
    "Upper_Bounds",
    "Lower_Bounds"
  )
  invisible(output)
}

#' @export
plot.btpc_MCMC <- function(x,y = NULL,
                           Temp_interval = NULL,
                           summaryType = "quantile",
                           centralSummary = "median",
                           prob = .9,
                           quantiles = c(.05,.95),
                           burn = 0,
                           ylab = "Trait", ...){
  if (is.null(Temp_interval)) Temp_interval <- seq(from = min(x$data$Temp), to = max(x$data$Temp), length.out = 1000)
  tpc_fun <- get_model_function(x$model_type)
  max.ind <- nrow(x$samples)

  # assign constants
  MA <- list(Temp = Temp_interval)
  if (length(x$constants) > 0) {
    for (i in 1:length(x$constants)) {
      MA[names(x$constants)[i]] <- x$constants[i]
    }
  }

  tpc_evals <- simplify2array(.mapply(
    FUN = tpc_fun, dots = data.frame(x$samples[(burn + 1):max.ind, !colnames(x$samples) %in% "sigma.sq"]),
    MoreArgs = MA
  ))

  if (centralSummary == "median"){
    centers <- matrixStats::rowMedians(tpc_evals)
  } else if (centralSummary == "mean") {
    centers <- matrixStats::rowMeans2(tpc_evals)
  } else {
    stop("Unsupported argument for 'centralSummary'. Currently only 'median' and 'mean' are supported.")
  }

  if (summaryType == "hdi"){
    hdi_mat <- apply(FUN = HDInterval::hdi, X = tpc_evals, MARGIN = 1, credMass = prob)
    upper_bounds <- hdi_mat[2, ]
    lower_bounds <- hdi_mat[1, ]
  } else if (summaryType == "quantile"){
    upper_bounds <- matrixStats::rowQuantiles(tpc_evals, probs = quantiles[2])
    lower_bounds <- matrixStats::rowQuantiles(tpc_evals, probs = quantiles[1])
  } else {
    stop("Unsupported argument for 'summaryType'. Currently only 'quantile' and 'hdi' are supported.")
  }

  plot(Temp_interval, upper_bounds, type = "l", col = "blue", lty = 2, ylab = ylab, xlab = "Temperature (C)", ylim = c(0, max(upper_bounds, x$data$Trait)), ...)
  graphics::points(Temp_interval, lower_bounds, type = "l", col = "blue", lty = 2)
  graphics::points(Temp_interval, centers, type = "l", col = "red")
  graphics::points(x$data$Temp, x$data$Trait, pch = 16, cex = .75)
}

#' @export
predict.btpc_MCMC <- function(object, ...){

}
