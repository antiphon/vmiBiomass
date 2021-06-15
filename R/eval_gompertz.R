#' Gompertz Curve
#'
#' @param x argument
#' @param p parameters, as c(A,m,k) (see details).
#'
#' @details
#' The Gompertz parametric curve we used is \deqn{G(t) = A * exp(-m*k^x)}.
#'
#'@export
gompertz <- function(x, p) {
  p[1] * exp(- p[2] * p[3]^x)
}

#' Derivative of Gompertz Curve
#'
#' @param x argument
#' @param p parameters, as c(A,m,k) (see details).
#'
#' @details
#' The derivative in argument of the Gompertz parametric curve.
#'
#' @seealso \link{gompertz}
#'
#'@export
gompertzDeriv <- function(x, p) {
  -p[1] * p[2] * log(p[3]) *  p[3]^x * exp(-p[2]*p[3]^x)
}


#' Evaluate Estimated Gompertz
#'
#' Evaluate the curve at posterior mean of the parameter.
#'
#' @param x argument (age in centuries) vector at which to evaluate the function
#' @param id the estimate identifier
#' @param log_scale in log-scale?
#' @param deriv derivate instead?
#' @param mc If > 1, sample posterior (approximated by mv-normal) of the parameters, evaluate function, and compute pointwise envelopes. Recommend using > 1000.
#' @param level if mc > 1, the 100xlevel\% envelopes are returned alongside the mean.
#' @details
#' Note that the posterior envelope will be a bit off since the joined posterior is most likely not gaussian.
#' @import mvtnorm dplyr
#' @export
evalGompertz <- function(x = seq(0, 2, l = 101),
                         id,
                         log_scale = TRUE,
                         deriv = FALSE,
                         mc = 0, level = .95){
  data(results, package = "vmiBiomass")
  #
  fun0 <- if(log_scale) log else identity
  fun <- if(deriv)
            function(x, p) fun0(gompertzDeriv(x, p[-1]))
         else
            function(x, p) fun0(gompertz(x, p[-1]) + p[1])
  #
  ix <- 1:4 # make sure correct @todo
  evs <- NULL
  for(id1 in id){
    r <- results[[id1]]
    p <- r$est$stat[ix, 1]
    ev <- cbind(x = x, mean = fun(x, p) )
    if(mc>1){
      S <- r$est$Cov
      pv <- rmvnorm(mc, p, S)
      sev <- apply(pv, 1, function(p) fun(x, p) )
      qev <- apply(sev, 1, quantile, prob = c((1-level)/2, 1-(1-level)/2), na.rm = TRUE)
      ev <- cbind(x = x, mean = rowMeans(sev, na.rm=TRUE),
                  sd = apply(sev, 1, sd, na.rm=TRUE),
                  t(qev) )
    }
    i <- lookup %>% dplyr::filter(id == id1)
    evs <- cbind(i, ev)
  }
  evs
}



