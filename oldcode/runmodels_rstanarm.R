# stan options -----------------
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
chains <- 4
cores <- chains
iter <- 2000
warmup <- 1000
seed <- sqrt(1963)

data <- data1 # data1 for full dataset, data2 for sampled dataset for test analyses

# GLMM models using lme4 --------------------
# alcheartnew (single variable for demo)
# out1 <- lme4::glmer(alcheartnew ~ 1 + scale(age) + sex + scale(AUDIT_SCORE) + (1 | ncountry), 
#                     data = data,
#                     family = poisson(link = "log"))
# awareness
out1a <- lme4::glmer(awareness ~ 1 + scale(age) + scale(AUDIT_SCORE) + (1 | ncountry), 
                     data = data,
                     family = poisson(link = "log"))
# believe
out1b <- lme4::glmer(invbelieve ~ 1 + scale(age) + scale(AUDIT_SCORE) + (1 | ncountry), 
                     data = data,
                     family = poisson(link = "log"))
# relevance
out1c <- lme4::glmer(relevance ~ 1 + scale(age) + scale(AUDIT_SCORE) + (1 | ncountry), 
                     data = data,
                     family = poisson(link = "log"))

# drink_less
out1d <- lme4::glmer(less ~ 1 + scale(age) + scale(AUDIT_SCORE) + (1 | ncountry), 
                     data = data,
                     family = poisson(link = "log"))



# bayesian GLMM using stan --------------------
# just for alcheartnew to demo
# out2 <- rstanarm::stan_glmer(alcheartnew ~ 1 + scale(age) + scale(AUDIT_SCORE^(1/3)) + (1 | ncountry), 
#                              data = data,
#                              family = poisson(link = log),
#                              prior = student_t(df = 7), 
#                              prior_intercept = student_t(df = 7),
#                              chains = chains, 
#                              cores = cores, 
#                              warmup = warmup,
#                              iter = iter,
#                              seed = seed)

# awareness
out3 <- rstanarm::stan_glmer(awareness ~ 1 + scale(age) + scale(AUDIT_SCORE^(1/3)) + (1 | ncountry), 
                             data = data,
                             family = poisson(link = log),
                             prior = student_t(df = 7), 
                             prior_intercept = student_t(df = 7),
                             chains = chains, 
                             cores = cores, 
                             warmup = warmup,
                             iter = iter,
                             seed = seed)
saveRDS(out3,"GDS_awareness_rstanarm_results.RDS")

# believe
out4 <- rstanarm::stan_glmer(invbelieve ~ 1 + scale(age) + scale(AUDIT_SCORE^(1/3)) + (1 | ncountry), 
                             data = data,
                             family = poisson(link = log),
                             prior = student_t(df = 7), 
                             prior_intercept = student_t(df = 7),
                             chains = chains, 
                             cores = cores, 
                             warmup = warmup,
                             iter = iter,
                             seed = seed)
saveRDS(out4,"GDS_believe_rstanarm_results.RDS")

# relevance
out5 <- rstanarm::stan_glmer(relevance ~ 1 + scale(age) + scale(AUDIT_SCORE^(1/3)) + (1 | ncountry), 
                             data = data,
                             family = poisson(link = log),
                             prior = student_t(df = 7), 
                             prior_intercept = student_t(df = 7),
                             chains = chains, 
                             cores = cores, 
                             warmup = warmup,
                             iter = iter,
                             seed = seed)
saveRDS(out5,"GDS_relevance_rstanarm_results.RDS")

# drink_less 
out6 <- rstanarm::stan_glmer(less ~ 1 + scale(age) + scale(AUDIT_SCORE^(1/3)) + (1 | ncountry), 
                             data = data,
                             family = poisson(link = log),
                             prior = student_t(df = 7), 
                             prior_intercept = student_t(df = 7),
                             chains = chains, 
                             cores = cores, 
                             warmup = warmup,
                             iter = iter,
                             seed = seed)
saveRDS(out6,"GDS_less_rstanarm_results.RDS")
