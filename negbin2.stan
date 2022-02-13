data {
  int N;  // sample size (number of rows)
  int J;  // sample size (number of countries)
  int<lower=0> depvar[N];  // dependent variable
  int<lower=0> country[N];  // random effect variable
  vector[N] age;  // predictor variable - age
  vector[N] AUDIT_SCORE;  // predictor variable - AUDIT score
}
parameters {
  real beta_int; // model intercept
  real beta_age;  // predictor coefficient - age
  real beta_AUDIT;  // predictor coefficient - AUDIT score
  real u[J];  // random effects parameter (country) 
  real<lower=0> sigma_u;  // random effect error
  real<lower=0> phi;  // neg. binomial dispersion parameter
}
transformed parameters {
  vector[N] mu; //the linear predictor
  real<lower=0> rr[J];  // declare rr variable
  for(i in 1:N) {
    mu[i] = exp(beta_int + beta_age*age[i] + beta_AUDIT*AUDIT_SCORE[i] + u[country[i]]); //using the log link 
  }
  for (i in 1:J) {
    rr[i] = exp(u[i]);  // calculate rr for random effect
  }
}
model {
  u ~ normal(0,sigma_u);  // prior for random effect
  beta_int ~ normal(0,5); //prior for the intercept following Gelman 2008
  beta_age ~ normal(0,5);  // prior for predictor - age
  beta_AUDIT ~ normal(0,5);  // prior for predictor - AUDIT
  phi ~ cauchy(0,3);  // prior for neg. binomial dispersion parameter
  depvar ~ neg_binomial_2(mu, phi);
}
// generated quantities {
//  real y_rep;
//  y_rep = neg_binomial_2_rng(mu,phi); 
//}




