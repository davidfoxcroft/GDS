# status: coded for running univariate model using cmnstanr and then transferring results back into brms format for post-processing. The advantage of cmdstan is that is uses latest stan version and math library with extra features for gpu processing and for threaded parallelization


library(RColorBrewer)
library(forcats)
library(plotly)
library(htmlwidgets)
library(tidyverse)
library(brms)
library(rstan)
library(here)
library(parallel)
library(lattice)
library(plotly)
library(bayesplot)
library(pushoverr)
library(officer)
library(cmdstanr)

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
               (1 | p | message ) +
               (1 | q | Final_country ) + 
               (1 | r | Final_country:id) 
)

bf.believe <- bf(believe ~ 
                   1 + 
                   age +  
                   AUDIT_SCORE + 
                   sex +
                   (1 | p | message ) +
                   (1 | q | Final_country ) + 
                   (1 | r | Final_country:id) 
)
bf.relevant <- bf(relevant ~ 
                    1 + 
                    age +  
                    AUDIT_SCORE + 
                    sex +
                    (1 | p | message ) +
                    (1 | q | Final_country ) + 
                    (1 | r | Final_country:id) 
)
bf.drinkless <- bf(drinkless ~ 
                     1 + 
                     age +  
                     AUDIT_SCORE + 
                     sex +
                     (1 | p | message ) +
                     (1 | q | Final_country ) + 
                     (1 | r | Final_country:id) 
)

bf.mv <- mvbf(bf.new, bf.believe, bf.relevant, bf.drinkless)


# get_prior(bf.new, data = data9)
# see https://discourse.mc-stan.org/t/default-priors-for-logistic-regression-coefficients-in-brms/13742
prior <- c(
  prior(student_t(7, 0, 1), class = "Intercept", resp = "new"),
  prior(student_t(3, 0, 2.5), class = "b", resp = "new"),
  prior(student_t(7, 0, 1), class = "Intercept", resp = "believe"),
  prior(student_t(3, 0, 2.5), class = "b", resp = "believe"),
  prior(student_t(7, 0, 1), class = "Intercept", resp = "relevant"),
  prior(student_t(3, 0, 2.5), class = "b", resp = "relevant"),
  prior(student_t(7, 0, 1), class = "Intercept", resp = "drinkless"),
  prior(student_t(3, 0, 2.5), class = "b", resp = "drinkless")
)

# compile and run model using cmdstan via cmdstanr
set_cmdstan_path("~/cmdstan/cmdstan-2.24.1")
cmdstan_path() # cmdstan version
#stan_version() # rstan version

# calculate recommended default grainsize
(as.integer(round(nrow(data8) / (2 * parallel::detectCores(logical = FALSE)),0)))

t1  <- Sys.time()
mod <- brm(bf.mv,
            data = data8, 
            family = bernoulli(), 
            prior = prior, 
            inits = '0' ,
            control = list(adapt_delta = 0.98,
                           max_treedepth = 12),
            sample_prior = 'yes',
            seed = seed,
            iter = 1000,
            chains = 4, threads = threading(5, grainsize = 635), 
            backend = 'cmdstanr',
            save_model = '/home/david/stanfiles/brms_mod_data8.stan',
            file = '/home/david/stanfiles/brms_mod_data8', # change filename to re-compile
            silent = FALSE)
t2 <- Sys.time()
tmod <- t2 - t1
tmod
summary(mod)
brms::bayes_R2(mod)  

t1  <- Sys.time()
mod2 <- update(mod, threads = threading(4, grainsize = 635))
t2 <- Sys.time()
tmod <- t2 - t1
tmod
summary(mod2)
brms::bayes_R2(mod2)

t1  <- Sys.time()
mod3 <- update(mod, threads = threading(4, grainsize = 1270))
t2 <- Sys.time()
tmod <- t2 - t1
tmod
summary(mod3)
brms::bayes_R2(mod3)

t1  <- Sys.time()
mod4 <- update(mod, threads = threading(4, grainsize = 1500))
t2 <- Sys.time()
tmod <- t2 - t1
tmod
summary(mod4)
brms::bayes_R2(mod4)
  



############ alternative direct to cmdstan----------------
# review check stan file - optional 
drinkless.code <-  make_stancode(bf.mv + set_rescor(FALSE), 
                                 data = data8a, 
                                family = bernoulli(),
                                prior = prior, 
                                sample_prior = "no")
cat(drinkless.code, file = "mv.stan")
drinkless.data <- unclass(make_standata(bf.mv + set_rescor(FALSE), data = data8a)) # change model and data as appropriate 
# compile and run model
mod <- cmdstan_model("~/Documents/coding/mv.stan", compile = TRUE)
t1 <- Sys.time()
fit <- mod$sample(
  data = drinkless.data, 
  seed = seed, 
  init = 0.5, # changes to (-0.5,0.5) from default (-2,2)
  chains = 4, 
  parallel_chains = 4,
  iter_sampling = 1000, # handles this differently to brm
  iter_warmup = 1000,
  max_treedepth = 14,
  adapt_delta = 0.98,
  output_dir = "/home/david/stanfiles/"
  )
fit$save_output_files(dir = "/home/david/stanfiles")
t2 <- Sys.time()
tmod <- t2 - t1
tmod # completion time
str(fit)

saveRDS(fit, "~/GDS_cmdstan_brms_multi_multi_data8a_fit.RDS")
#fit <- readRDS("~/GDS_cmdstan_brms_multi_multi_data8_fit.RDS")
# create brmsfit object without model fit and insert stanfit object into brmsfit object
brmmod <- brms::brm(bf.mv + set_rescor(TRUE),
                data = data8a, 
                empty = TRUE)

str(fit)

fit <- read_stan_csv(fit$output_files())
brmmod$fit <- fit 
brmmod <- brms::rename_pars(brmmod)
saveRDS(brmmod, "~/GDS_cmdstan_brms_multi_multi_data8a_modelbrm.RDS")
#mod <- readRDS("~/GDS_cmdstan_brms_multi_multi_data9_modelbrm.RDS")
#mod <- brmmod

#### model summary and fit -----------------

# model results and fit (bayes R-squared)
summary(mod)
bayesR2tab_mod <- brms::bayes_R2(mod)
bayesR2tab_mod
saveRDS(bayesR2tab_mod, "~/GDS_cmdstan_brms_multi_multi_data9_bayesR2tab_mod.RDS")




# new data for predicted probabilities
newdat <- tidyr::expand_grid(Final_country = as.factor(levels(mod$data$Final_country)), message = as.factor(levels(mod$data$message)), sex = as.factor(levels(mod$data$sex)), age = c(seq(16,80,4)), AUDIT_SCORE = c(seq(1,40,2)))
dplyr::glimpse(newdat)

# use brms posterior predict to get predicted values from the posterior
preddat1 <- brms::posterior_predict(mod, newdata = newdat, re_form = "~(1 | message) + (1 | Final_country)", summary = F) # use NULL or specify a random effect formula for re_form as this calculates predicted values for  group-level effects (i.e. take the actual estimated intercepts for each specific group and uses them to make new predictions for the same groups)



#### check model -----------------

mod <- readRDS("~/GDS_brms_multi_multi_data9.RDS")

prior_summary(mod)
summary(mod)
(ranef(mod))$message[,,1]
(ranef(mod))$Final_country[,,1]
pp_check(mod, resp = "new")
pp_check(mod, resp = "believe")
pp_check(mod, resp = "relevant")
pp_check(mod, resp = "drinkless")

### MCMC diagnostics



color_scheme_set("darkgray")
mcmc_pairs(posterior_cp, np = np_cp, pars = c("mu","tau","theta[1]"),
           off_diag_args = list(size = 0.75))

draws <- as.array(mod, pars = c("b_new_Intercept",
                                "b_believe_Intercept",
                                "b_relevant_Intercept",
                                "b_drinkless_Intercept",      
                                "b_new_age", 
                                "b_new_AUDIT_SCORE",
                                "b_new_sex1",
                                "b_believe_age",
                                "b_believe_AUDIT_SCORE", 
                                "b_believe_sex1", 
                                "b_relevant_age", 
                                "b_relevant_AUDIT_SCORE",
                                "b_relevant_sex1", 
                                "b_drinkless_age",
                                "b_drinkless_AUDIT_SCORE", 
                                "b_drinkless_sex1",
                                "lp__"))
lp <- bayesplot::log_posterior(mod)
np <- bayesplot::nuts_params(mod)
bayesplot::color_scheme_set("brightblue")
bayesplot::mcmc_parcoord(draws, alpha = 0.05, 
                         np = np, 
                         transform = function(x) {(x - mean(x)) / sd(x)},
                         pars = c("b_new_Intercept",
                                  "b_believe_Intercept",
                                  "b_relevant_Intercept",
                                  "b_drinkless_Intercept",
                                  "lp__"))
bayesplot::mcmc_parcoord(draws, alpha = 0.05, 
                         np = np,
                         transform = function(x) {(x - mean(x)) / sd(x)},
                         pars = c("b_new_age",
                                  "b_new_AUDIT_SCORE",
                                  "b_new_sex1",
                                  "b_believe_age",
                                  "b_believe_AUDIT_SCORE", 
                                  "b_believe_sex1", 
                                  "b_relevant_age", 
                                  "b_relevant_AUDIT_SCORE",
                                  "b_relevant_sex1", 
                                  "b_drinkless_age",
                                  "b_drinkless_AUDIT_SCORE", 
                                  "b_drinkless_sex1"))

bayesplot::mcmc_areas(draws, 
                      pars = c("b_new_age",
                               "b_new_AUDIT_SCORE",
                               "b_new_sex1",
                               "b_believe_age",
                               "b_believe_AUDIT_SCORE", 
                               "b_believe_sex1", 
                               "b_relevant_age", 
                               "b_relevant_AUDIT_SCORE",
                               "b_relevant_sex1", 
                               "b_drinkless_age",
                               "b_drinkless_AUDIT_SCORE", 
                               "b_drinkless_sex1"),
                      prob = 0.8, # 80% intervals
                      prob_outer = 0.99, # 99%
                      point_est = "mean"
)

bayesplot::mcmc_hist(draws, 
                     pars = c("b_new_age",
                              "b_new_AUDIT_SCORE",
                              "b_new_sex1",
                              "b_believe_age",
                              "b_believe_AUDIT_SCORE", 
                              "b_believe_sex1", 
                              "b_relevant_age", 
                              "b_relevant_AUDIT_SCORE",
                              "b_relevant_sex1", 
                              "b_drinkless_age",
                              "b_drinkless_AUDIT_SCORE", 
                              "b_drinkless_sex1")
)

bayesplot::color_scheme_set("viridis")
bayesplot::mcmc_trace(draws, pars = c("b_new_Intercept",
                                      "b_believe_Intercept",
                                      "b_relevant_Intercept",
                                      "b_drinkless_Intercept",
                                      "b_new_age",
                                      "b_new_AUDIT_SCORE",
                                      "b_new_sex1",
                                      "b_believe_age",
                                      "b_believe_AUDIT_SCORE", 
                                      "b_believe_sex1", 
                                      "b_relevant_age", 
                                      "b_relevant_AUDIT_SCORE",
                                      "b_relevant_sex1", 
                                      "b_drinkless_age",
                                      "b_drinkless_AUDIT_SCORE", 
                                      "b_drinkless_sex1",
                                      "lp__"))

rhats <- bayesplot::rhat(mod, pars = c("b_new_Intercept",
                                       "b_believe_Intercept",
                                       "b_relevant_Intercept",
                                       "b_drinkless_Intercept",
                                       "b_new_age",
                                       "b_new_AUDIT_SCORE",
                                       "b_new_sex1",
                                       "b_believe_age",
                                       "b_believe_AUDIT_SCORE", 
                                       "b_believe_sex1", 
                                       "b_relevant_age", 
                                       "b_relevant_AUDIT_SCORE",
                                       "b_relevant_sex1", 
                                       "b_drinkless_age",
                                       "b_drinkless_AUDIT_SCORE", 
                                       "b_drinkless_sex1",
                                       "lp__"))
print(rhats)
bayesplot::color_scheme_set("brightblue") # see help("color_scheme_set")
bayesplot::mcmc_rhat(rhats) + bayesplot::yaxis_text(hjust = 1)

ratios_mod <- bayesplot::neff_ratio(mod, pars = c("b_new_Intercept",
                                                  "b_believe_Intercept",
                                                  "b_relevant_Intercept",
                                                  "b_drinkless_Intercept",
                                                  "b_new_age",
                                                  "b_new_AUDIT_SCORE",
                                                  "b_new_sex1",
                                                  "b_believe_age",
                                                  "b_believe_AUDIT_SCORE", 
                                                  "b_believe_sex1", 
                                                  "b_relevant_age", 
                                                  "b_relevant_AUDIT_SCORE",
                                                  "b_relevant_sex1", 
                                                  "b_drinkless_age",
                                                  "b_drinkless_AUDIT_SCORE", 
                                                  "b_drinkless_sex1",
                                                  "lp__"))
print(ratios_mod)
bayesplot::mcmc_neff(ratios_mod, size = 2)

bayesplot::mcmc_acf(draws, pars = c("b_new_Intercept",
                                    "b_believe_Intercept",
                                    "b_relevant_Intercept",
                                    "b_drinkless_Intercept",
                                    "b_new_age",
                                    "b_new_AUDIT_SCORE",
                                    "b_new_sex1",
                                    "b_believe_age",
                                    "b_believe_AUDIT_SCORE", 
                                    "b_believe_sex1", 
                                    "b_relevant_age", 
                                    "b_relevant_AUDIT_SCORE",
                                    "b_relevant_sex1", 
                                    "b_drinkless_age",
                                    "b_drinkless_AUDIT_SCORE", 
                                    "b_drinkless_sex1",
                                    "lp__"),
                    lags = 100)

bayesplot::mcmc_nuts_divergence(np, bayesplot::log_posterior(mod))

bayesplot::mcmc_pairs(draws, np = np, pars = c("b_new_Intercept",
                                               "b_believe_Intercept",
                                               "b_relevant_Intercept",
                                               "b_drinkless_Intercept",
                                               "lp__"), 
                      off_diag_args = list(size = 0.75))

bayesplot::mcmc_pairs(draws, np = np, pars = c("b_new_age",
                                               "b_new_AUDIT_SCORE",
                                               "b_new_sex1",
                                               "b_believe_age",
                                               "b_believe_AUDIT_SCORE", 
                                               "b_believe_sex1", 
                                               "lp__"), 
                      off_diag_args = list(size = 0.75))

bayesplot::mcmc_pairs(draws, np = np, pars = c("b_relevant_age", 
                                               "b_relevant_AUDIT_SCORE",
                                               "b_relevant_sex1", 
                                               "b_drinkless_age",
                                               "b_drinkless_AUDIT_SCORE", 
                                               "b_drinkless_sex1",
                                               "lp__"), 
                      off_diag_args = list(size = 0.75))



#### model results wrangling for fitted estimates -----------------

conditions <- data.frame(Final_country = levels(data9$Final_country), cond__ = levels(data9$Final_country))
p1 <- plot(marginal_effects(mod, 
                      effects = "message", 
                      method = "fitted",
                      conditions = conditions,
                      re_form = "~(1 | message ) + (1 | Final_country)",
                      probs = c(.15,.85)
                      ),
           plot = F)


p1a <- p1$new.new_message$data %>% 
  mutate(impact = "new")
p1b <- p1$believe.believe_message$data %>% 
  mutate(impact = "believe")
p1c <- p1$relevant.relevant_message$data %>% 
  mutate(impact = "relevant")
p1d <- p1$drinkless.drinkless_message$data %>% 
  mutate(impact = "drinkless")

plotdat <- rbind(p1a,p1b,p1c,p1d)

plotdat <- as_tibble(plotdat) %>% 
  mutate(impact = as.factor(impact)) 
glimpse(plotdat)

levels(plotdat$impact) = c("new",
                           "believe",
                           "relevant",
                           "drinkless")

plotdat <- plotdat %>% 
  ungroup() %>% 
  group_by(message,impact) %>%
  mutate(lower = quantile(estimate__, probs = .15),
            upper = quantile(estimate__, probs = .85)) %>% 
  ungroup() %>% 
  mutate(filter__ = ifelse(estimate__ < lower | estimate__ > upper, TRUE, FALSE))

plotdat_s <- plotdat %>% 
  filter(filter__ == "TRUE")


#### plot results fitted estimates -----------------

plot1 <- ggplot(data = plotdat, aes(x = Final_country,
                                  y = estimate__)) +
  facet_grid(vars(impact), vars(message), scales = "free") +
  geom_hline(aes(yintercept = upper), 
             colour = "red", 
             alpha = 0.2) +
  geom_hline(aes(yintercept = lower), 
             colour = "green", 
             alpha = 0.2) +
  geom_point(data = plotdat, 
             aes(x = Final_country,
                 y = estimate__),
             colour = alpha("grey", 0.5)) + 
  geom_point(data = plotdat_s, 
             aes(x = Final_country,
                 y = estimate__,
                 colour = Final_country),
             size = 2) +
  theme_light() +
  xlab("") +
  ylab("") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey90"),
        strip.background = element_rect(colour = "grey50", fill = "grey50"),
        strip.text = element_text(colour = "white", size = 11, face = "bold"),
        legend.title = element_blank(),
        legend.position = "bottom") 



plot1


#### model results wrangling for posterior predictions -----------------

# use brms posterior predict to get predicted values from the posterior
preddat1 <- brms::posterior_predict(mod, newdata = NULL, re_form = "~(1 | message) + (1 | Final_country)", summary = F) # use NULL or specify a random effect formula for re_form as this calculates predicted values for  group-level effects (i.e. take the actual estimated intercepts for each specific group and uses them to make new predictions for the same groups)

saveRDS(preddat1, "~/GDS_brms_multi_multi_data9_posteriorpredictions.RDS")
preddat1 <- readRDS("~/GDS_brms_multi_multi_data9_posteriorpredictions.RDS")




preddat1.new <- preddat1[,,1]
preddat1.believe <- preddat1[,,2]
preddat1.relevant <- preddat1[,,3]
preddat1.drinkless <- preddat1[,,4]

preddat2a <- apply(preddat1.new, 2, mean) # calculate p(Yes) 
preddat2b <- apply(preddat1.believe, 2, mean) # calculate p(Yes) 
preddat2c <- apply(preddat1.relevant, 2, mean) # calculate p(Yes) 
preddat2d <- apply(preddat1.drinkless, 2, mean) # calculate p(Yes) 

datarun2 <- cbind(data9, pYes.new = preddat2a,
                  pYes.believe = preddat2b,
                  pYes.relevant = preddat2c,
                  pYes.drinkless = preddat2d)
dplyr::glimpse(datarun2)


plotdat2 <- datarun2 %>% 
  ungroup() %>% 
  pivot_longer(cols = c(pYes.new:pYes.drinkless),
               names_to = "impact",
               values_to = "pYes") %>% 
  mutate(impact = as.factor(impact)) %>% 
  mutate(impact = fct_recode(impact, "new" = "pYes.new",
                             "believe" = "pYes.believe",
                             "relevant" = "pYes.relevant",
                             "drinkless" = "pYes.drinkless"))

glimpse(plotdat2)
levels(plotdat2$impact) = c("new",
                           "believe",
                           "relevant",
                           "drinkless")

plotdat2 <- plotdat2 %>% 
  ungroup() %>% 
  group_by(message,impact) %>%
  mutate(lower = quantile(pYes, probs = .15),
         upper = quantile(pYes, probs = .85)) %>% 
  ungroup() %>% 
  mutate(filter__ = ifelse(pYes < lower | pYes > upper, TRUE, FALSE)) %>% 
  group_by(message,impact,Final_country) %>% 
  summarise(ll = quantile(pYes, 0.15),
            l = quantile(pYes, 0.30),
            med = quantile(pYes, 0.5),
            h = quantile(pYes, 0.70),
            hh = quantile(pYes, 0.85),
            pYes = mean(pYes), 
            lower = mean(lower), 
            upper = mean(upper), 
            filter__ = median(filter__)
)
  

plotdat_s <- plotdat2 %>% 
  filter(filter__ == "1")

glimpse(plotdat_s)
glimpse(plotdat2)

plotdat3 <- plotdat2 %>%
  ungroup() %>%
  select(Final_country,message,impact,pYes,ll,hh)

glimpse(plotdat3)

# results tables for posterior predicted values - needs polish
myft <- flextable(
  subset(plotdat3,impact == "new"), 
  col_keys = c("message", "Final_country", "pYes", "ll", "hh" ))
myft <- set_header_labels( myft, Final_country = "Country", ll = "15%ile", hh = "85%ile" )
myft <- autofit(myft)
myft

myft <- flextable(
  subset(plotdat3,impact == "believe"), 
  col_keys = c("message", "Final_country", "pYes", "ll", "hh" ))
myft <- set_header_labels( myft, Final_country = "Country", ll = "15%ile", hh = "85%ile" )
myft <- autofit(myft)
myft

myft <- flextable(
  subset(plotdat3,impact == "relevant"), 
  col_keys = c("message", "Final_country", "pYes", "ll", "hh" ))
myft <- set_header_labels( myft, Final_country = "Country", ll = "15%ile", hh = "85%ile" )
myft <- autofit(myft)
myft

myft <- flextable(
  subset(plotdat3,impact == "drinkless"), 
  col_keys = c("message", "Final_country", "pYes", "ll", "hh" ))
myft <- set_header_labels( myft, Final_country = "Country", ll = "15%ile", hh = "85%ile" )
myft <- autofit(myft)
myft


#### plot results posterior predictions -----------------

plot2 <- ggplot(data = plotdat2, 
                aes(x = Final_country,
                    y = pYes,
                    text = paste(Final_country,
                                     ":",
                                     "<br>Pr(Yes) = ",
                                     round(med,2)))) +
  facet_grid(vars(impact), vars(message), scales = "free") +
  geom_hline(aes(yintercept = upper),
             colour = "red",
             alpha = 0.2) +
  geom_hline(aes(yintercept = lower),
             colour = "green",
             alpha = 0.2) +
  geom_point(data = plotdat2,
             aes(x = Final_country,
                 y = med),
             colour = alpha("grey", 0.5)) +
  geom_pointrange(data = plotdat_s, 
             aes(x = Final_country,
                 y = med,
                 ymin = ll,
                 ymax = hh,
                 colour = Final_country)) +
  theme_light() +
  xlab("") +
  ylab("") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey90"),
        strip.background = element_rect(colour = "grey50", fill = "grey50"),
        strip.text = element_text(colour = "white", size = 11, face = "bold"),
        legend.title = element_blank(),
        legend.position = "bottom") 

dev.off()
plot2


# plots to plotly
subp <- plotly::ggplotly(plot2, tooltip = c("text")) %>% 
  plotly::layout(legend = list(orientation = "h", y = -0.02))
subp

# send to plotly web - needs keys
chart_link <- plotly::api_create(subp, filename = "GDS-alcinfo-brms_plot9_message")
chart_link # opens up browser and displays plot


