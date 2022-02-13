csvfiles <- c('~/mv-202112221525-1-60539a.csv',
              '~/mv-202112221525-2-60539a.csv',
              '~/mv-202112221525-3-60539a.csv',
              '~/mv-202112221525-4-60539a.csv')

fit <- rstan::read_stan_csv(csvfiles)
str(fit)
mod$fit <- fit
mod <- rename_pars(mod)
summary(mod)

#mod <- readRDS("/home/david/stanfiles/brms_mod_data9a_2.rds")
mod <- readRDS("~/stanfiles/brms_mod_data9a_mv_nothreads_openblas_4thread_flags_clean.rds")
t1 <- Sys.time()
summary(mod)
t2 <- Sys.time()
tmod <- t2 - t1
tmod

# new data for predicted probabilities
newdat <- tidyr::expand_grid(Final_country = as.factor(levels(mod$data$Final_country)), message = as.factor(levels(mod$data$message)), sex = as.factor(levels(mod$data$sex)), age = c(seq(16,80,4)), AUDIT_SCORE = c(seq(1,40,2)))
dplyr::glimpse(newdat)

# use brms posterior predict to get predicted values from the posterior

t1  <- Sys.time()
preddat3 <-  brms::posterior_predict(mod, 
                                     newdata = newdat, 
                                     #allow_new_levels = TRUE, 
                                     #sample_new_levels = "gaussian",
                                     re_form = "~ (1 | Final_country * message)", 
                                     summary = TRUE,
                                     ndraws = 1000
                                     ) # use NULL or specify a random effect formula for re_form as this calculates predicted values for  group-level effects (i.e. take the actual estimated intercepts for each specific group and uses them to make new predictions for the same groups)
t2 <- Sys.time()
tmod <- t2 - t1
tmod


dplyr::glimpse(preddat3)

preddat1 <- preddat3

preddat1.new <- preddat1[,,1]
preddat1.believe <- preddat1[,,2]
preddat1.relevant <- preddat1[,,3]
preddat1.drinkless <- preddat1[,,4]

dplyr::glimpse(preddat1.new)


preddat2a <- apply(preddat1.new, 2, mean) # calculate p(Yes) 
preddat2b <- apply(preddat1.believe, 2, mean) # calculate p(Yes) 
preddat2c <- apply(preddat1.relevant, 2, mean) # calculate p(Yes) 
preddat2d <- apply(preddat1.drinkless, 2, mean) # calculate p(Yes) 







