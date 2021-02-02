
#' Results Object
#'
#' Results for the biomass curve fitting. Gompertz curve.
#'
#' @details Note: the model was fitted with age in centuries.
#'
#' @format a list with sublists by a unique id, see \link[vmiBiomass]{lookup}. The structure for each element is
#' a list of posterior statistics:
#' \describe{
#' \item{ranef}{(optional) the group-wise estimated random effects}
#' \item{env}{95\% posterior envelope for the curve as a matrix, first column gives the argument}
#' \item{lenv}{envelope for the log-curve (base e)}
#' \item{denv}{envelope for the derivative}
#' \item{dlenv}{envelope for the derivative of the log-curve}
#' \item{est}{Posterior statistics for the parameters, a list(stat = table, Cov = matrix)}
#' \item{estnat}{ Posterior statistics for derived "interpretable parameters"}
#' }
#' @seealso \link[vmiBiomass]{getResults}, \link[vmiBiomass]{lookup}
"results"

#' Result Lookup
#'
#' @format a tibble with one info-line per fitted group:
#' \describe{
#'  \item{id}{unique identifier (key) for matching with results-object components}
#'  \item{with_ranefs}{does this result include random effects in its model?}
#'  \item{err_model}{"A"dditive or "M"ultiplicative.}
#'  \item{zone, fertility, soil}{the group information}
#' }
#' @seealso \link[vmiBiomass]{getResults}, \link[vmiBiomass]{results}
"lookup"



#' Get Estimates of Particular VMI Biomass Experiment Groups in Long Format
#'
#' Get specific results. The results will be returned as a tibble.
#'
#' @param zone which zone
#' @param fertility which f'ty
#' @param soil etc
#' @param with_re model with plot-wise random intercepts? (default: TRUE)
#' @param err_model default: M. Additive or Multiplicative error model
#' @param nat return the derived "interpretable" parameters? (default: FALSE)
#' @param long wide (FALSE) or long (TRUE) format tibble.
#' @param id this/these particular results by id
#'
#' @seealso \link[vmiBiomass]{results}, \link[vmiBiomass]{lookup}
#' @import dplyr tidyr
#' @export
getResults <- function(zone = "SOUTH",
                       fertility = "1-2",
                       soil = "mineral",
                       with_re = TRUE,
                       err_model = "M",
                       nat = FALSE,
                       long = FALSE,
                       id) {
  data(results, package = "vmiBiomass")
  if(!is.missing(sub))
    sub <- lookup %>% dplyr::filter( zone %in% !!zone &
                             fertility %in% !!fertility &
                             soil %in% !!soil &
                             with_ranefs %in% with_re &
                               err_model %in% !!err_model)
  else sub <- lookup %>% dplyr::filter(id %in% !!id)
  if(nrow(sub) == 0) return(NULL)
  tabu <- NULL
  for(id1 in sub$id) {
    i <- sub %>% dplyr::filter(id == id1)
#    browser()
    r <- results[[id1]]
    e <- if(nat) r$estnat else r$est
    st <- e$stat
    el <- st %>% as_tibble()
    if(long) el <- el %>% pivot_longer(where(is.numeric), names_to = "stat")
    d <- cbind(i, par = rownames(st), el )
    tabu <- tabu %>% rbind(d)
  }
  tabu
}




