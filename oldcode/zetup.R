remove.packages("rstan")
if (file.exists(".RData")) file.remove(".RData")

Sys.setenv(MAKEFLAGS = paste0("-j",parallel::detectCores()))
install.packages(c("StanHeaders","rstan"),type="source")
example(stan_model, package = "rstan", run.dontrun = TRUE)
