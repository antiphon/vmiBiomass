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
#'  \item{region, fertility, soil}{the grouping variables}
#' }
#' @seealso \link[vmiBiomass]{getResults}, \link[vmiBiomass]{results}
"lookup"



#' Get Estimated Gompertz Parameters of Particular VMI Biomass Experiment Groups in Long Format
#'
#' Get specific results. The results will be returned as a table.
#'
#' @param region which region: SOUTH, MIDDLE, NORTH
#' @param fertility which fertility class: "1-2", "3", "4", "5-8"
#' @param soil which soil type: mineral, "drained peatl.", "undrained peatl."
#' @param with_re model with plot-wise random intercepts? (default: TRUE)
#' @param err_model default: M. Additive or Multiplicative error model?
#' @param nat return the derived "interpretable" parameters? (default: FALSE)
#' @param long Return type: wide (FALSE) or long (TRUE) format tibble.
#' @param id this/these particular results by internal identifier (see \link{lookup})
#'
#' @seealso \link[vmiBiomass]{results}, \link[vmiBiomass]{lookup}
#' @import dplyr tidyr
#' @export
getParameters <- function(region = "SOUTH",
                       fertility = "1-2",
                       soil = "mineral",
                       with_re = TRUE,
                       err_model = "M",
                       nat = FALSE,
                       long = FALSE,
                       id) {
  data(results, package = "vmiBiomass")
  if(missing(id))
    sub <- lookup %>%
      dplyr::filter( region %in% !!region &
                       fertility %in% !!fertility &
                       soil %in% !!soil &
                       with_ranefs %in% with_re &
                       err_model %in% !!err_model)
  else sub <- lookup %>% dplyr::filter(id %in% !!id)
  if(nrow(sub) == 0) return(NULL)
  tabu <- NULL
  for(id1 in sub$id) {
    i <- sub %>% dplyr::filter(id == id1)
    #browser()
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



#' Get Posterior Mean Curve of Particular VMI Biomass Experiment Groups in Long Format
#'
#' Get specific results. The results will be returned as a table
#'
#' @param region which region: SOUTH, MIDDLE, NORTH
#' @param fertility which fertility class: "1-2", "3", "4", "5-8"
#' @param soil which soil type: mineral, "drained peatl.", "undrained peatl."
#' @param with_re model with plot-wise random intercepts? (default: TRUE)
#' @param err_model default: M. Additive or Multiplicative error model?
#' @param log_scale return the curve in log-scale?
#' @param id this/these particular results by internal identifier (see \link{lookup})
#'
#' @seealso \link[vmiBiomass]{results}, \link[vmiBiomass]{lookup}
#' @import dplyr tidyr
#' @export
getCurves <- function(region = "SOUTH",
                          fertility = "1-2",
                          soil = "mineral",
                          with_re = TRUE,
                          err_model = "M",
                          log_scale = TRUE,
                          id) {
  data(results, package = "vmiBiomass")
  if(missing(id))
    sub <- lookup %>%
      dplyr::filter( region %in% !!region &
                       fertility %in% !!fertility &
                       soil %in% !!soil &
                       with_ranefs %in% with_re &
                       err_model %in% !!err_model)
  else sub <- lookup %>% dplyr::filter(id %in% !!id)
  if(nrow(sub) == 0) return(NULL)
  tabu <- NULL
  for(id1 in sub$id) {
    i <- sub %>% dplyr::filter(id == id1)
    #browser()
    r <- results[[id1]]
    e <- if(log_scale) r$lenv else r$env
    d <- cbind(i, e)
    tabu <- tabu %>% rbind(d)
  }
  tabu
}


