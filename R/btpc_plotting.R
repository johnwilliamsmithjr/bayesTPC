
#' Wrapper for coda::traceplot()
#'
#' Wrapper for coda::traceplot() that can both directly accept samples of object type `mcmc` or `mcmc.list` as well as an object of class `list` with an element `samples` that is of class `mcmc` or `mcmc.list`
#'
#' @details This is a wrapper to create trace plots using coda's `traceplot` function.
#' @param object Either an object of class `mcmc` or `mcmc.list` OR an object of class `list` which contains an element called `samples` that is of class `mcmc` or `mcmc.list`
#' @param burn Integer, number of samples to discard as burn-in before creating traceplot (default = 0)
#' @param thin Integer, thinning interval used to generate traceplots (default = 1)
#' @param ... additional graphical parameters to be passed as arguments to coda::traceplot
#' @return Returns invisible(NULL) and creates trace plots for MCMC parameters
#' @examples
#' ## need data to set up example here. placeholder for now
#' ## set params and reference temperature set
#' myfun = str2tpc_fun(model = 'gaussian')
#' param_set = c(T.opt = 36, a = 6.5, rmax = 2.75)
#' Temp_ref = seq(from = 5, to = 50, length.out = 1000)
#' plot(Temp_ref, myfun(params = param_set, Temp = Temp_ref), type = 'l')


traceplot <- function(object, burn = 0, thin = 1, ...){
  if (is.list(object)) if (is.null(object$samples)) stop('Object of class list must have element "samples"')
  if (is.list(object)){
    if (is.mcmc(object$samples)){
      N = nrow(object$samples)
      thinned = seq(from = burn+1, to = N, by = thin)
      coda::traceplot(x = as.mcmc(object$samples[thinned,]), ...)
      return(invisible(NULL))
    }
  } else{
    N = nrow(object)
    thinned = seq(from = burn+1, to = N, by = thin)
    coda::traceplot(x = as.mcmc(object[thinned,]), ...)
    return(invisible(NULL))
  }
}

#' Wrapper for IDPmisc::ipairs()
#'
#' Wrapper for IDPmisc() that can both directly accept samples of object type `mcmc` or `matrix` as well as an object of class `list` with an element `samples` that is of class `mcmc` or `matrix`
#'
#' @details This is a wrapper to create trace plots using coda's `traceplot` function.
#' @param object Either an object of class `mcmc` or `matrix` OR an object of class `list` which contains an element called `samples` that is of class `mcmc` or `matrix`
#' @param burn Integer, number of samples to discard as burn-in before creating pairs plot (default = 0)
#' @param thin Integer, thinning interval used to generate pairs plots (default = 1)
#' @param ztransf Function, used to transform the counts per pixel in IDPmisc::ipairs.
#' @param ... additional graphical parameters to be passed as arguments to IDPmisc::ipairs(). For additional information, try ?IDPmisc::ipairs
#' @return Returns invisible(NULL) and creates pairs plots for MCMC parameters
#' @examples
#' ## need data to set up example here. placeholder for now
#' ## set params and reference temperature set
#' myfun = str2tpc_fun(model = 'gaussian')
#' param_set = c(T.opt = 36, a = 6.5, rmax = 2.75)
#' Temp_ref = seq(from = 5, to = 50, length.out = 1000)
#' plot(Temp_ref, myfun(params = param_set, Temp = Temp_ref), type = 'l')

bayesTPC_ipairs <- function(x, burn = 0, thin = 1,
                            ztransf = function(x){x[x<1] <- 1; log2(x)}, ...){
  if (is.list(x)) if(is.null(x$samples)) stop('Expected list "x" to have an element called samples')
  if (!(is.mcmc(x)) & !(is.matrix(x)) & !(is.list(x))) stop('Input "x" is expected as a list, matrix, or mcmc object. See ?bayesTPC.ipairs')
  if (is.list(x)){
    N = nrow(x$samples)
    thinned = seq(from = burn+1, to = N, by = thin)
    if (is.mcmc(x$samples)){
      samples = as.matrix(x$samples)
    } else{
      samples = x$samples
    }
    IDPmisc::ipairs(samples[thinned,], ztransf = ztransf, ...)
  } else{
    N = nrow(x)
    thinned = seq(from = burn+1, to = N, by = thin)
    if (is.mcmc(x)){
      samples = as.matrix(x)
    } else{
      samples = x
    }
    IDPmisc::ipairs(samples[thinned,], ztransf = ztransf, ...)
  }
  return(invisible(NULL))
}

#' Prior-Posterior Overlap Plot
#'
#' Create a prior-posterior overlap plot from output of the bTPC function
#'
#' @details This is a wrapper to create trace plots using coda's `traceplot` function.
#' @param bTPC.object List, usually output from the `bTPC` function. `ppo_plot` expects this list to have entries "samples", of class `mcmc` or `numeric` and "priors", with class `list` and entries that match the corresponding model parameters
#' @param burn Integer, number of samples to discard as burn-in before creating prior-posterior overlap plot (default = 0)
#' @return Returns invisible(NULL) and creates a prior posterior overlap plot
#' @examples
#' ## need data to set up example here. placeholder for now
#' ## set params and reference temperature set
#' myfun = str2tpc_fun(model = 'gaussian')
#' param_set = c(T.opt = 36, a = 6.5, rmax = 2.75)
#' Temp_ref = seq(from = 5, to = 50, length.out = 1000)
#' plot(Temp_ref, myfun(params = param_set, Temp = Temp_ref), type = 'l')

ppo_plot <- function(bTPC_object, burn = 0){
  if (!is.list(bTPC_object)) stop('Expected input bTPC_object to be a list')
  if (is.null(bTPC_object$samples)) stop('Expected input bTPC_object to have an entry called "samples"')
  if (is.null(bTPC_object$priors)) stop('Expected input bTPC_object to have an entry called "priors"')
  for (i in colnames(bTPC_object$samples)){
    par_samples <- as.numeric(bTPC_object$samples[(burn+1):(nrow(bTPC_object$samples)),i])
    #print(strsplit(bTPC.object$priors[i], '~')[[1]][2])
    prior_exp = strsplit(strsplit(as.character(bTPC_object$priors[i]), '~')[[1]][2], '\\(')
    prior_exp = paste0(prior_exp[[1]][1], '(', 'sort(par_samples),', prior_exp[[1]][2])
    plot(density(par_samples), type = 'l', col = 'red', lwd = 2, xlab = i,
         main = paste0('Prior-Posterior Overlap for ', i))
    points(sort(par_samples), eval(str2expression(prior_exp)), type = 'l', col = 'blue', lwd = 2, lty = 2)
  }
}
