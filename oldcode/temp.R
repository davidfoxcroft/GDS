# This script combines cmdstan csv file subsets - useful if you do not have enough RAM for doing all in one go for all chains with read_cmdstan_csv

fit1 <- cmdstanr::read_cmdstan_csv(file = c("/tmp/RtmpYLMwhx/mv-202006111630-1-7e38ff.csv",
                                           "/tmp/RtmpYLMwhx/mv-202006111630-2-7e38ff.csv"))
saveRDS(fit1, "~/GDS_cmdstan_brms_multi_multi_data9_fit1.RDS")
str(fit1)
fit2 <- cmdstanr::read_cmdstan_csv(file = c("/tmp/RtmpYLMwhx/mv-202006111630-3-7e38ff.csv",
                                            "/tmp/RtmpYLMwhx/mv-202006111630-4-7e38ff.csv"))
saveRDS(fit2, "~/GDS_cmdstan_brms_multi_multi_data9_fit2.RDS")
str(fit2)

###########

fit1 <- readRDS("~/GDS_cmdstan_brms_multi_multi_data9_fit1.RDS")
fit <- readRDS("~/GDS_cmdstan_brms_multi_multi_data9_fit2.RDS")
str(fit1)
str(fit)

fit$metadata$id <- c(1:4)
fit$metadata$init <- c(2,2,2,2)
fit$metadata$seed <- c(44,45,46,47)
fit$metadata$step_size <- c(1,1,1,1)
fit$metadata$step_size_adaptation <- unlist(c(fit1$step_size[1],
                                              fit1$step_size[2],
                                              fit$step_size[1],
                                              fit$step_size[2]))
str(fit)
fit$inv_metric[1] <- fit1$inv_metric[1]
fit$inv_metric[2] <- fit1$inv_metric[2]
str(fit$inv_metric)

fit$step_size[1] <- fit1$step_size[1]
fit$step_size[2] <- fit1$step_size[2]
str(fit$step_size)


fit$post_warmup_draws <- abind::abind(fit1$post_warmup_draws,
          fit2$post_warmup_draws,
          along = 2)
fit$post_warmup_draws <- posterior::as_draws_array(fit$post_warmup_draws)
names(dimnames(fit$post_warmup_draws)) <- c("iteration", "chain", "variable")
str(fit$post_warmup_draws)
dimnames(fit$post_warmup_draws)[[2]] <- c("1", "2", "3", "4")
str(fit$post_warmup_draws)

fit$post_warmup_sampler_diagnostics <- abind::abind(fit1$post_warmup_sampler_diagnostics,
                                      fit$post_warmup_sampler_diagnostics,
                                      along = 2)
fit$post_warmup_sampler_diagnostics <- posterior::as_draws_array(fit$post_warmup_sampler_diagnostics)
names(dimnames(fit$post_warmup_sampler_diagnostics)) <- c("iteration", "chain", "variable")
str(fit$post_warmup_sampler_diagnostics)
dimnames(fit$post_warmup_sampler_diagnostics)[[2]] <- c("1", "2", "3", "4")
str(fit$post_warmup_sampler_diagnostics)
names(fit$post_warmup_sampler_diagnostics) 
str(fit$post_warmup_sampler_diagnostics)

str(fit)
saveRDS(fit, "~/GDS_cmdstan_brms_multi_multi_data9_fitmerge.RDS")
#fit <- readRDS("~/GDS_cmdstan_brms_multi_multi_data9_fitmerge.RDS")


# this is best run in base R
files <- c("/tmp/RtmpYLMwhx/mv-202006111630-1-7e38ff.csv", 
           "/tmp/RtmpYLMwhx/mv-202006111630-2-7e38ff.csv", 
           "/tmp/RtmpYLMwhx/mv-202006111630-3-7e38ff.csv", 
           "/tmp/RtmpYLMwhx/mv-202006111630-4-7e38ff.csv")
fit <- rstan::read_stan_csv(files)


