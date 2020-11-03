#' The additive error-model without RE's
#'
#' @examples
#' #not run
#' # dat1: data for one group
#' modA <- stan_model( model_code = m1.stan , verbose=TRUE)
#' fitA <- sampling(modA,
#'                  data =
#'        list( n = nrow(dat1),
#'              p = length(unique(dat1$group)),
#'              group = dat1$group, # soiltype x zone x fertility class
#'              ika = dat1$age,
#'              biomassa = dat1$biomass,
#'              weight = 1/sqrt(dat1$area),
#'              nug_var = 10),
#' verbose = FALSE,
#' cores = 4,
#' iter=6000)
#'
#' @export
m1.stan <- "
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
  sigma_err ~ inv_gamma(0.01, 0.01);
  nugget ~ normal(0, nug_var);
  m ~normal(0, 1);
  for(i in 1:n) {
    biomassa[i] ~ normal(nugget + A * exp( -m * k^ika[i] ), sigma_err * weight[i]);
  }
}
"

#' The additive error-model with RE's
#' @seealso \link{m1.stan}
#' @export
m1re.stan <- "
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
  sigma_err ~ inv_gamma(0.01, 0.01);
  sigma_ran ~ inv_gamma(0.01, 0.01);
  nugget ~ normal(0, nug_var);
  m ~normal(0, 1);
  ran ~ normal(0, sigma_ran);
  for(i in 1:n) {
    biomassa[i] ~ normal(nugget + A * exp( -m * k^ika[i] ) + ran[ group[i] ], sigma_err * weight[i]);
  }
}
"




#' The multiplicative error-model without RE's
#'
#' @seealso \link{m1.stan}
#' @export
m3.stan <- "
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
  sigma_err ~ inv_gamma(0.01, 0.01);
  nugget ~ normal(0, nug_var);
  m ~normal(0, 1);
  for(i in 1:n) {
    log(biomassa[i]) ~ normal( log( nugget + A * exp( -m * k^ika[i] )), sigma_err * weight[i]);
  }
}
"

#' The multiplicative error-model with RE's
#'
#' @seealso \link{m1.stan}
#' @export
m3re.stan <- "
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
  sigma_err ~ inv_gamma(0.01, 0.01);
  sigma_ran ~ inv_gamma(0.01, 0.01);
  nugget ~ normal(0, nug_var);
  m ~normal(0, 1);
  ran ~ normal(0, sigma_ran);
  for(i in 1:n) {
    log(biomassa[i]) ~ normal( log( nugget + A * exp( -m * k^ika[i] )) + ran[ group[i] ], sigma_err * weight[i]);
  }
}
"
