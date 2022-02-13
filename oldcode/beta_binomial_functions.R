beta_binomial2 <- custom_family(
  "beta_binomial2", dpars = c("mu", "phi"),
  links = c("logit", "log"), lb = c(NA, 0),
  type = "int") #, vars = "trials[n]")

stan_funs <- "
  real beta_binomial2_lpmf(int y, real mu, real phi) {
return beta_binomial_lpmf(y, mu * phi, (1 - mu) * phi);
}
int beta_binomial2_rng(real mu, real phi) {
return beta_binomial_rng(mu * phi, (1 - mu) * phi);
}
"

stanvars <- stanvar(scode = stan_funs, block = "functions") # +
  # stanvar(as.integer(cbpp$size), name = "trials")


log_lik_beta_binomial2 <- function(i, draws) {
  mu <- draws$dpars$mu[, i]
  phi <- draws$dpars$phi
  N <- draws$data$trials[i]
  y <- draws$data$Y[i]
  beta_binomial2_lpmf(y, mu, phi, N)
}

predict_beta_binomial2 <- function(i, draws, ...) {
  mu <- draws$dpars$mu[, i]
  phi <- draws$dpars$phi
  N <- draws$data$trials[i]
  beta_binomial2_rng(mu, phi, N)
}

fitted_beta_binomial2 <- function(draws) {
  mu <- draws$dpars$mu
  trials <- draws$data$trials
  trials <- matrix(trials, nrow = nrow(mu), ncol = ncol(mu), byrow = TRUE)
  mu * trials
}
