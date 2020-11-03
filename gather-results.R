# gather the results from the external results-folder and compile into a database-style
# list + lookup -format.

library(tibble)

infiles <- dir("../../results_rds/", full.names = TRUE)

# the format is
# ..._gombertz_A-M_[RE_]_NORTH.<fertility>.<soil>-sqrtw6k_summary.rds
#
# sqrtw means we use sqrt(area) weighting for the error sds, and 6k is Stan inerations (checked no difference to higher).
#

# Some factor related stuff, and output niceties
# for sorting factors
vlevs <- c("NORTH", "MIDDLE", "SOUTH")
# soil in english please
soil_en <- cbind(c("mineraalimaa", "ojitettu suo", "ojittamaton suo"), c("mineral", "drained peatl.", "natural peatl.") )
# fertility was in numbered classes
kptlevs <-c("Lehdot_lehKankaat", "Tuoreet kankaat", "Kuivahkot kankaat", "KuivatKank_KaruKank_Kallio_laki")
kptnums <- c("1-2", "3", "4", "5-8")



lu <- NULL
res <- list()

makeid <- function(i) sprintf("id%06i", i)

k <- -2
for(f in infiles){
  r <- readRDS(f)
  # parse model
  is_re <- grepl("_RE_", f)
  s     <- if(is_re) "RE_" else "A-M_"
  e     <- "-sqrtw6k"
  bits  <- strsplit(  substr(f, regexpr(s, f) + nchar(s), regexpr(e, f) - 1)  , "[.]")[[1]]
  id <- makeid( (k <- k + 2) + 1:2)
  info  <- tibble(
    id        = id,
    with_ranefs = is_re,
    err_model = c("A", "M"),
    zone      = bits[1],
    fertility = kptnums[match(bits[2], kptlevs)],
    soil      = soil_en[ match(bits[3], soil_en[,1]) ,2])
  res[id] <- r
  lu <- rbind(lu, info)
}

# nicer names
results <- res
lookup <- lu
# stoooore
save( list = c("results", "lookup"), file = "data/results.rda")
