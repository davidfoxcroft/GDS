
#### libraries and data ----------------
#renv::restore()

# get data
#source("dataprep.R")
#data8 <- readRDS("GDS_2018_data8.rds")
#data8a <- readRDS("GDS_2018_data8a.rds")
data9 <- readRDS("GDS_2018_data9.rds")

#remotes::install_github("paul-buerkner/brms", force = TRUE)
library(brms)
#remotes::install_github("stan-dev/cmdstanr", ref = "v0.4.0")
library(cmdstanr)


#### stan options -----------------
options(mc.cores = parallel::detectCores(logical = FALSE))
#rstan_options(auto_write = FALSE) # TRUE to avoid auto recompile
seed <- as.integer(round(sqrt(1963),0)) # make integer for set.seed


#### set up and run model using brms -----------------
## interaction term for Final_country * message added following feedback at SSA conference presentation 

bf.new <- brms::bf(new ~ 
               1 + 
               age +  
               AUDIT_SCORE + 
               sex +
               (1 | Final_country * message) +
               (1 | id) 
)
bf.believe <- brms::bf(believe ~ 
                   1 + 
                   age +  
                   AUDIT_SCORE + 
                   sex +
                   (1 | Final_country * message) +
                   (1 | id) 
)
bf.relevant <- brms::bf(relevant ~ 
                    1 + 
                    age +  
                    AUDIT_SCORE + 
                    sex +
                    (1 | Final_country * message) +
                    (1 | id) 
)
bf.drinkless <- brms::bf(drinkless ~ 
                     1 + 
                     age +  
                     AUDIT_SCORE + 
                     sex +
                     (1 | Final_country * message) +
                     (1 | id) 
)
bf.mv <- brms::mvbf(bf.new, bf.believe, bf.relevant, bf.drinkless)


# get_prior(bf.new, data = data9)
# see https://discourse.mc-stan.org/t/default-priors-for-logistic-regression-coefficients-in-brms/13742
prior <- c(
  brms::prior(student_t(3, 0, 2.5), class = "Intercept", resp = "new"),
  brms::prior(student_t(3, 0, 2.5), class = "b", resp = "new"),
  brms::prior(student_t(3, 0, 2.5), class = "Intercept", resp = "believe"),
  brms::prior(student_t(3, 0, 2.5), class = "b", resp = "believe"),
  brms::prior(student_t(3, 0, 2.5), class = "Intercept", resp = "relevant"),
  brms::prior(student_t(3, 0, 2.5), class = "b", resp = "relevant"),
  brms::prior(student_t(3, 0, 2.5), class = "Intercept", resp = "drinkless"),
  brms::prior(student_t(3, 0, 2.5), class = "b", resp = "drinkless")
)


# compile and run model using cmdstan via cmdstanr
cmdstanr::set_cmdstan_path("/home/david/.cmdstanr/cmdstan-2.29.0-rc2")
#cmdstanr::cmdstan_path() # cmdstan version

# calculate recommended default grainsize - for interest (brms allocates grainsize automatically)
#(as.integer(round(nrow(data9) / (2 * (parallel::detectCores(logical = FALSE) - 4)), 0)))

t1  <- Sys.time()
mod <- brm(bf.mv,
            data = data9, 
            family = bernoulli(), 
            prior = prior, 
            inits = "0",
            control = list(adapt_delta = 0.99,
                           max_treedepth = 14),
            #sample_prior = 'yes',
            seed = seed,
            iter = 2000,
            warmup = 1000,
            thin = 1,
            chains = 4, 
            cores = 4,
            threads = threading(4), 
            backend = 'cmdstanr',
            output_dir = '/home/david/stanfiles',
            save_model = '/home/david/stanfiles/brms_mod_data9_threading_Feb15.stan',
            file = '/home/david/stanfiles/brms_mod_data9_threading_Feb15', # change filename to re-compile
            silent = FALSE)
t2 <- Sys.time()
tmod <- t2 - t1
tmod

pushoverr::pushover(message = "And...the code is finished, phew!", user = Sys.getenv("PUSHOVER_USER"), app = Sys.getenv("PUSHOVER_APP"))  # use when keys set with .Renviron

# backup large & important model fit files to cloud (aws s3)
#aws.s3::s3saveRDS(mod, bucket = "rstudio-data", object = "brms_mod_data8_threading_Feb11.rds", multipart = TRUE)

# model summary
t1 <- Sys.time()
mod_sum <- summary(mod)
t2 <- Sys.time()
tmod <- t2 - t1
tmod
mod_sum
saveRDS(mod_sum,"mod_sum_data8.rds")
#aws.s3::s3saveRDS(mod_sum, bucket = "rstudio-data", object = "mod_sum_data8.rds")


# list session information
sessionInfo()
sink("session_Info.txt")
print(sessionInfo())
sink()
# save environment snapshot
renv::snapshot()


