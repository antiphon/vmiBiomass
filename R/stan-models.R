#' The additive error-model without RE's
#'
#' @examples
#' #not run
#' # dat1: data-frame for one group
#' modA <- stan_model( model_code = mA.stan , verbose=TRUE)
#' fitA <- sampling(modA,
#'                  data =
#'        list( n = nrow(dat1),
#'              p = length(unique(dat1$group)),
#'              group = dat1$group, # soiltype x zone x fertility class
#'              age = dat1$age,
#'              biomass = dat1$biomass,
#'              weight = dat1$weight/sum(dat1$weight) * nrow(dat1),
#'              nug_var = 10),
#' verbose = FALSE,
#' cores = 4,
#' iter=6000)
#'
#' @export
mA.stan <- "
data {
  int n;
  int p;
  vector[n] biomassa;
  vector[n] ika;
  vector[n] weight;
  real nug_var;
}
parameters{
  real nugget;
  real<lower=0> A;
  real m;
  real <lower=0, upper=1> k;
  real<lower=0> sigma_err;
}
model{
  A ~ normal(100, 50);
  k ~ beta(0.5, 1);
  sigma_err ~ exponential(.1);
  nugget ~ normal(0, nug_var);
  m ~normal(0, 1);
  for(i in 1:n) {
    target += normal_lpdf(biomassa[i] | nugget + A * exp( -m * k^ika[i] ), sigma_err) * weight[i];
  }
}
"

#' The additive error-model with RE's
#' @seealso \link{mA.stan}
#' @export
mAre.stan <- "
data {
  int n;
  int p;
  vector[n] biomassa;
  vector[n] ika;
  vector[n] weight;
  int group[n];
  real nug_var;
}
parameters{
  real nugget;
  real<lower=0> A;
  real m;
  real <lower=0, upper=1> k;
  real<lower=0> sigma_err;
  real <lower=0> sigma_ran;
  vector[p] ran;
}
model{
  A ~ normal(100, 50);
  k ~ beta(0.5, 1);
  sigma_err ~ exponential(.1);
  sigma_ran ~ exponential(.1);
  nugget ~ normal(0, nug_var);
  m ~normal(0, 1);
  ran ~ normal(0, sigma_ran);
  for(i in 1:n) {
    target += normal_lpdf(biomassa[i] | nugget + A * exp( -m * k^ika[i] ) + ran[ group[i] ], sigma_err) * weight[i];
  }
}
"




#' The multiplicative error-model without RE's
#'
#' @seealso \link{mA.stan}
#' @export
mM.stan <- "
data {
  int n;
  int p;
  vector[n] biomassa;
  vector[n] ika;
  vector[n] weight;
  real nug_var;
}
parameters{
  real nugget;
  real<lower=0> A;
  real m;
  real <lower=0, upper=1> k;
  real<lower=0> sigma_err;
}
model{
  A ~ normal(100, 50);
  k ~ beta(0.5, 1);
  sigma_err ~ exponential(1);
  nugget ~ normal(0, nug_var);
  m ~normal(0, 1);
  for(i in 1:n) {
    target += normal_lpdf(log(biomassa[i]) | log(nugget + A * exp( -m * k^ika[i] )), sigma_err) * weight[i];
  }
}
"

#' The multiplicative error-model with RE's
#'
#' @seealso \link{mM.stan}
#' @export
mMre.stan <- "
data {
  int n;
  int p;
  vector[n] biomassa;
  vector[n] ika;
  vector[n] weight;
  int group[n];
  real nug_var;
}
parameters{
  real nugget;
  real<lower=0> A;
  real m;
  real <lower=0, upper=1> k;
  real<lower=0> sigma_err;
  real <lower=0> sigma_ran;
  vector[p] ran;
}
model{
  A ~ normal(100, 50);
  k ~ beta(0.5, 1);
  sigma_err ~ exponential(1);
  sigma_ran ~ exponential(1);
  nugget ~ normal(0, nug_var);
  m ~normal(0, 1);
  ran ~ normal(0, sigma_ran);
  for(i in 1:n) {
    target += normal_lpdf(log(biomassa[i]) | log(nugget + A * exp( -m * k^ika[i] )) + ran[ group[i] ], sigma_err) * weight[i];
  }
}
"
