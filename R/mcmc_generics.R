#' @export
print.btpc_MCMC <- function(x, digits = 3, ...){
  cat("bayesTPC MCMC of type:", x$model_type)
  cat("\n\nModel Formula:\n  ", as.character(get_formula(x$model_type)), "\n\n")
  s <- x$samples
  means <- round(matrixStats::colMeans2(s), digits)
  medians <- round(matrixStats::colMedians(s), digits)
  tbl <- cbind.data.frame(means,medians,x$priors)
  rownames(tbl) <- colnames(s)
  colnames(tbl) <- c("mean", "median", "priors")
  cat("Model Parameters:\n")
  print(tbl)
}

#' @export
summary.btpc_MCMC <- function(object, ...){

}

#' @export
plot.btpc_MCMC <- function(x,y, ...){

}

#' @export
predict.btpc_MCMC <- function(object, ...){

}

