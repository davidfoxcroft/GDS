
#### load libraries and get data ------------------
library(RColorBrewer)
library(forcats)
library(plotly)
library(htmlwidgets)
library(tidyverse)
#remotes::install_github("paul-buerkner/brms", force = TRUE, vignettes = TRUE)
#browseVignettes("brm")
library(brms)
library(here)
library(parallel)
library(lattice)
library(plotly)
library(bayesplot)
library(pushoverr)
library(officer)
#remotes::install_github("stan-dev/cmdstanr", ref = "v0.4.0")
library(cmdstanr)

# get data
here::i_am("GDS/alcohol paper/brms_stan_modelling_FIXED.R")
source(here("GDS", "dataprep.R"))



#### stan options -----------------
options(mc.cores = parallel::detectCores(logical = FALSE))
rstan_options(auto_write = FALSE) # TRUE to avoid auto recompile
seed <- as.integer(round(sqrt(1963),0)) # make integer for set.seed


#### set up and run model using brms -----------------

bf.new <- bf(new ~ 
               1 + 
               age +  
               AUDIT_SCORE + 
               sex +
               message +
               (1 | Final_country:id) 
)
bf.believe <- bf(believe ~ 
                   1 + 
                   age +  
                   AUDIT_SCORE + 
                   sex +
                   message +
                   (1 | Final_country:id) 
)
bf.relevant <- bf(relevant ~ 
                    1 + 
                    age +  
                    AUDIT_SCORE + 
                    sex +
                    message +
                    (1 | Final_country:id) 
)
bf.drinkless <- bf(drinkless ~ 
                     1 + 
                     age +  
                     AUDIT_SCORE + 
                     sex +
                     message +
                     (1 | Final_country:id) 
)

bf.mv <- mvbf(bf.new, bf.believe, bf.relevant, bf.drinkless)


# get_prior(bf.new, data = data9)
# see https://discourse.mc-stan.org/t/default-priors-for-logistic-regression-coefficients-in-brms/13742
prior <- c(
  prior(student_t(3, 0, 2.5), class = "Intercept", resp = "new"),
  prior(student_t(3, 0, 2.5), class = "b", resp = "new"),
  prior(student_t(3, 0, 2.5), class = "Intercept", resp = "believe"),
  prior(student_t(3, 0, 2.5), class = "b", resp = "believe"),
  prior(student_t(3, 0, 2.5), class = "Intercept", resp = "relevant"),
  prior(student_t(3, 0, 2.5), class = "b", resp = "relevant"),
  prior(student_t(3, 0, 2.5), class = "Intercept", resp = "drinkless"),
  prior(student_t(3, 0, 2.5), class = "b", resp = "drinkless")
)

# compile and run model using cmdstan via cmdstanr (backend command in brms)
set_cmdstan_path("~/cmdstan/cmdstan-2.24.1")
cmdstan_path() # cmdstan version
#stan_version() # rstan version
# calculate recommended default grainsize for interest
#(as.integer(round(nrow(data9) / (2 * (parallel::detectCores(logical = FALSE) - 4)), 0)))

t1  <- Sys.time()
mod <- brm(bf.mv,
           data = data9, 
           family = bernoulli(), 
           prior = prior, 
           inits = "0",
           control = list(adapt_delta = 0.99,
                          max_treedepth = 14),
           sample_prior = 'yes',
           seed = seed,
           iter = 2000,
           chains = 4, 
           cores = 4,
           threads = threading(4), 
           backend = 'cmdstanr',
           save_model = '/home/david/stanfiles/brms_mod_data_FIXED.stan',
           file = '/home/david/stanfiles/brms_mod_FIXED', # change filename to re-compile
           silent = FALSE)
t2 <- Sys.time()
tmod <- t2 - t1
tmod
#mod <- readRDS("/home/david/stanfiles/brms_mod_interaction_data.rds")
summary(mod)
#bayes_R2(mod)  



