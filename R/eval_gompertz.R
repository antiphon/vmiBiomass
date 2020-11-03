#' Gompertz Curve
#'
#' @param x argument
#' @param p parameters, as c(A,m,k) (see details).
#'
#' @details
#' The Gompertz parametric curve we used is \deqn{G(t) = A * exp(-m*k^t)}.
#'
#'@export
gompertz <- function(t, p) {
  p[1] * exp(- p[2] * p[3]^x)
}

#' Derivative of Gompertz Curve
#'
#' @param t argument
#' @param p parameters, as c(A,m,k) (see details).
#'
#' @details
#' The derivative in argument of the Gompertz parametric curve.
#'
#' @seealso \link{gompertz}
#'
#'@export
gompertzDeriv <- function(t, p) {
  -p[1] * p[2] * log(p[3]) *  p[3]^x * exp(-p[2]*p[3]^x)
}


#' Evaluate Estimated Gompertz
#'
#'
#' @param x argument (age in centuries) vector at which to evaluate the function
#' @param id the estimate identifier
#' @param l in log-scale?
#' @param deriv derivate instead?
#' @param mc If > 1, sample posterior (approximated by mv-normal) of the parameters, evaluate function, and compute pointwise envelopes.
#' @param level if mc > 1, the 100xlevel\% envelopes are returned alongside the mean. Recommend using > 1000.
#' @details
#' Note that the posterior envelope will be a bit off since the joined posterior is most likely not gaussian.
#' @import mvtnorm
#' @export
evalGompertz <- function(x, id, l = FALSE, deriv = FALSE, mc = 0, level = .975){
  data(results, package = "vmiBiomass")
  r <- results[[id]]
  fun0 <- if(l) log else identity
  fun <- if(deriv) function(...) fun0(gompertzDeriv(...)) else function(...) fun0(gompertz(...))
  i <- 1:4 # make sure works
  p <- r$est$stat[i,1]
  ev <- cbind(x = x, mean = fun(x, p[-1]) + p[1] )
  if(mc>1){
    S <- r$est$Cov
    pv <- rmvnorm(mc, p, S)
    sev <- apply(pv, 1, function(p) fun(x, p[-1]) + p[1] )
    qev <- apply(sev, 1, quantile, prob = c(1-level, level), na.rm = TRUE)
    ev <- cbind(x = x, mean = rowMeans(sev, na.rm=TRUE), q= t(qev) )
  }
  ev
}



