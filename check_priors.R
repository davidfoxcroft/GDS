library(here)

#### stan options -----------------
options(mc.cores = parallel::detectCores(logical = FALSE))
rstan_options(auto_write = TRUE)
chains <- 2
cores <- chains
iter <- 1000
warmup <- 500
seed <- as.integer(round(sqrt(1963),0)) # make integer for set.seed

### get and organise data -------------------

source(here("GDS", "dataprep.R"))

glimpse(data7)

data8 <- data7 %>%
  select(-awareness,-believe,-invbelieve,-relevance,-less) %>% 
  pivot_wider(names_from = impact, values_from = response) %>% 
  mutate(sex = as.factor(sex))

### set up and run model --------------------

bf.drinkless <- bf(drinkless ~ 
                     1 + 
                     age +  
                     AUDIT_SCORE + 
                     sex +
                     (1 | message ) +
                     (1 | Final_country ) + 
                     (1 | Final_country:id) 
)

get_prior(bf.drinkless, data = data8)

prior <- c(
  prior(student_t(3, 0, 1), class = "Intercept"),
  prior(student_t(3, 0, 1), class = "b")
)

t1 <- Sys.time()
mod_prior <- brm(bf.drinkless,
           data = data8, 
           family = bernoulli, 
           prior = prior, 
           iter = iter,
           control = list(adapt_delta = 0.99,
                          max_treedepth = 16),
           inits = "0",
           sample_prior = "yes",
           seed = seed,
           warmup = warmup,
           chains = chains, 
           cores = cores)
t2 <- Sys.time()
#saveRDS(mod, here("GDS", "GDS_brms_multi_multi_data8_priorcheck.RDS"))
tmod <- t2 - t1
tmod

summary(mod_prior1)
mod_prior1 <- mod_prior # brms default priors


plot(hypothesis(mod_prior, "Intercept = 0"))

plot(hypothesis(mod_prior, "sex1 = 0"))

str(mod_prior)





