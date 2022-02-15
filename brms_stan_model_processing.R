#renv::restore()


library(RColorBrewer)
library(forcats)
library(plotly)
library(htmlwidgets)
library(tidyr)
library(tibble)
library(brms)
library(rstan)
library(lattice)
library(plotly)
library(bayesplot)
library(pushoverr)
library(officer)
library(flextable)
library(tidybayes)
library(gt)

data8 <- readRDS("GDS_2018_data8.rds")
#data8a <- readRDS("GDS_2018_data8a.rds")
#data9 <- readRDS("GDS_2018_data9.rds")

restab1 <- data8 %>% 
  group_by(Final_country,message) %>% 
  select(Final_country,message,new,believe,relevant,drinkless) %>% 
  summarise(mean_new = mean(new),
            mean_believe = mean(believe),
            mean_relevant = mean(relevant),
            mean_drinkless = mean(drinkless))
restab1




#### model summary and fit -----------------

# get data
##### retrieve large rds files from cloud (aws s3) - do once and save locally ----------
# mod <- aws.s3::s3readRDS("brms_mod_data8_threading_Feb11.rds", bucket = "rstudio-data")
# mod_sum <- aws.s3::s3load("mod_sum_data8.rds", bucket = "rstudio-data")
# bayesR2tab <- aws.s3::s3load("brms_mod_data8_threading_Feb11_bayesR2tab.rds", bucket = "rstudio-data")
# saveRDS(mod,"~/stanfiles/brms_mod_data8_threading_Feb11.rds")
# saveRDS(mod_sum,"mod_sum_data8.rds")
# saveRDS(bayesR2tab, "brms_mod_data8_threading_Feb11.rds")
####-----------

# model results and fit (bayes R-squared)
mod <- readRDS("~/stanfiles/brms_mod_data9_threading_Feb2022.rds")
mod_sum <- readRDS("mod_sum_data9.rds")
mod_sum
bayesR2tab <- brms::bayes_R2(mod)
bayesR2tab
saveRDS(bayesR2tab, "brms_mod_data9_threading_Feb2022_bayesR2tab.rds")
#aws.s3::s3save(bayesR2tab, bucket = "rstudio-data", object = "brms_mod_data8_threading_Feb11_bayesR2tab.rds")



mod$version
mod$formula


#### Summary fit table -------------------

mod <- readRDS("~/stanfiles/brms_mod_data8_threading_Feb11.rds")
mod_sum <- readRDS("mod_sum_data8.rds")
bayesR2tab <- readRDS("brms_mod_data8_threading_Feb11_bayesR2tab.rds")


# table of fit, fixed and random effects
fe1 <- mod_sum$fixed[c(1,5:7,2,8:10,3,11:13,4,14:16),] %>%
  as_tibble(rownames = "Parameter") %>% 
  select(-Est.Error,
         -Bulk_ESS) %>% 
  separate(Parameter, c("outcome","parameter") , "_")  %>% 
  pivot_wider(names_from = outcome,
              values_from = c(3:7)) %>% 
  select(c(1,2,6,10,14,18,
           3,7,11,15,19,
           4,8,12,16,20,
           5,9,13,17,21)) 

re1 <- as_tibble(mod_sum$random$`Final_country:message`[1:4,], rownames = "Parameter") %>% 
  mutate(random_effect = "message:country") %>% 
  select(-Est.Error,
         -Bulk_ESS) %>% 
  separate(Parameter, c("outcome","sd") , "_") %>% 
  pivot_wider(names_from = outcome,
              values_from = c(3:7)) %>% 
  select(-sd) %>% 
  select(c(1,2,6,10,14,18,
           3,7,11,15,19,
           4,8,12,16,20,
           5,9,13,17,21)) 

re1a <- as_tibble(mod_sum$random$message[1:4,], rownames = "Parameter") %>% 
  mutate(random_effect = "message") %>% 
  select(-Est.Error,
         -Bulk_ESS) %>% 
  separate(Parameter, c("outcome","sd") , "_") %>% 
  pivot_wider(names_from = outcome,
              values_from = c(3:7)) %>% 
  select(-sd) %>% 
  select(c(1,2,6,10,14,18,
           3,7,11,15,19,
           4,8,12,16,20,
           5,9,13,17,21)) 

re1b <- as_tibble(mod_sum$random$Final_country[1:4,], rownames = "Parameter") %>% 
  mutate(random_effect = "country") %>% 
  select(-Est.Error,
         -Bulk_ESS) %>% 
  separate(Parameter, c("outcome","sd") , "_") %>% 
  pivot_wider(names_from = outcome,
              values_from = c(3:7)) %>% 
  select(-sd) %>% 
  select(c(1,2,6,10,14,18,
           3,7,11,15,19,
           4,8,12,16,20,
           5,9,13,17,21)) 

re2 <- as_tibble(mod_sum$random$`id`[1:4,], rownames = "Parameter") %>% 
  mutate(random_effect = "id") %>% 
  select(-Est.Error,
         -Bulk_ESS) %>% 
  separate(Parameter, c("outcome","sd") , "_") %>% 
  pivot_wider(names_from = outcome,
              values_from = c(3:7)) %>% 
  select(-sd) %>% 
  select(c(1,2,6,10,14,18,
           3,7,11,15,19,
           4,8,12,16,20,
           5,9,13,17,21)) 

colnames(fe1) <- c("parameter",
                   "estimate",
                   "LCI",
                   "UCI",
                   "Rhat",
                   "ESS",
                   "estimate",
                   "LCI",
                   "UCI",
                   "Rhat",
                   "ESS",
                   "estimate",
                   "LCI",
                   "UCI",
                   "Rhat",
                   "ESS",
                   "estimate",
                   "LCI",
                   "UCI",
                   "Rhat",
                   "ESS")
retab <- rbind(re1,re1a, re1b, re2) 
colnames(retab) <- c("parameter",
                     "estimate",
                     "LCI",
                     "UCI",
                     "Rhat",
                     "ESS",
                     "estimate",
                     "LCI",
                     "UCI",
                     "Rhat",
                     "ESS",
                     "estimate",
                     "LCI",
                     "UCI",
                     "Rhat",
                     "ESS",
                     "estimate",
                     "LCI",
                     "UCI",
                     "Rhat",
                     "ESS")


allres <- rbind(fe1, retab) 


restab <- as_tibble(allres, 
                    .name_repair = ~ c(
                      "parameter",
                      "n1","n2","n3","n4","n5",
                      "b1","b2","b3","b4","b5",
                      "r1","r2","r3","r4","r5",
                      "d1","d2","d3","d4","d5")) %>% 
  add_row(parameter = "bayes_R2",
          n1 = bayesR2tab[1,1],
          n2 = bayesR2tab[1,3],
          n3 = bayesR2tab[1,4],
          b1 = bayesR2tab[1,1],
          b2 = bayesR2tab[2,3],
          b3 = bayesR2tab[2,4],
          r1 = bayesR2tab[3,1],
          r2 = bayesR2tab[3,3],
          r3 = bayesR2tab[3,4],
          d1 = bayesR2tab[4,1],
          d2 = bayesR2tab[4,3],
          d3 = bayesR2tab[4,4]) %>% 
  gt(rowname_col = "parameter") %>% 
  fmt_missing(
    columns = 1:21,
    missing_text = "") %>% 
  tab_stubhead(label = "") %>% 
  tab_spanner(
    label = "new",
    columns = c(2:6)
  ) %>% 
  tab_spanner(
    label = "believe",
    columns = c(7:11)
  ) %>% 
  tab_spanner(
    label = "relevant",
    columns = c(12:16)
  ) %>% 
  tab_spanner(
    label = "drinkless",
    columns = c(17:21)
  ) %>% 
  fmt_number(columns = c(2:5,7:10,12:15,17:20), 
             decimals = 2) %>% 
  fmt_number(columns = c(6,11,16,21), 
             decimals = 0) %>% 
  cols_align(align = c("center"), columns = c(2:21)) %>%
  cols_merge_range(col_begin = vars(n2),
                   col_end = vars(n3),
                   sep = html(",")) %>% 
  cols_label(n1 = md("*Est*"),
             n2 = md("*95% CI*"),
             n4 = md("*Rhat*"),
             n5 = md("*ESS*")) %>% 
  cols_merge_range(col_begin = vars(b2),
                   col_end = vars(b3),
                   sep = html(",")) %>% 
  cols_label(b1 = md("*Est*"),
             b2 = md("*95% CI*"),
             b4 = md("*Rhat*"),
             b5 = md("*ESS*")) %>% 
  cols_merge_range(col_begin = vars(r2),
                   col_end = vars(r3),
                   sep = html(",")) %>% 
  cols_label(r1 = md("*Est*"),
             r2 = md("*95% CI*"),
             r4 = md("*Rhat*"),
             r5 = md("*ESS*")) %>% 
  cols_merge_range(col_begin = vars(d2),
                   col_end = vars(d3),
                   sep = html(",")) %>% 
  cols_label(d1 = md("*Est*"),
             d2 = md("*95% CI*"),
             d4 = md("*Rhat*"),
             d5 = md("*ESS*")) %>% 
  tab_row_group(group = "Random Effects (Est = sd)",
                rows = parameter == "message" | 
                  parameter == "country" |
                  parameter == "message:country" |
                  parameter == "id") %>% 
  tab_row_group(group = "Population Effects",
                rows = parameter == "Intercept" | 
                  parameter == "age" |
                  parameter == "AUDIT" |
                  parameter == "sex1") %>% 
  tab_row_group(group = "Model Fit",
                rows = parameter == "bayes_R2") 
restab



#### check model -----------------

prior_summary(mod)
summary(mod)
(ranef(mod))$message[,,1]
(ranef(mod))$Final_country[,,1]
pp_check(mod, resp = "new")
pp_check(mod, resp = "believe")
pp_check(mod, resp = "relevant")
pp_check(mod, resp = "drinkless")

### MCMC diagnostics
draws <- as.array(mod, variable = c("b_new_Intercept",
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

conditions <- data.frame(Final_country = levels(data8$Final_country), cond__ = levels(data8$Final_country))
p1 <- plot(conditional_effects(mod, 
                      effects = "message", 
                      method = "fitted",
                      conditions = conditions,
                      re_form = "~(1 | message * Final_country)",
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

# new data for predicted probabilities
newdat <- tidyr::expand_grid(Final_country = as.factor(levels(mod$data$Final_country)), message = as.factor(levels(mod$data$message)), sex = as.factor(levels(mod$data$sex)), age = c(seq(16,80,4)), AUDIT_SCORE = c(seq(1,40,2)))
dplyr::glimpse(newdat)

# use brms posterior predict to get predicted values from the posterior

t1  <- Sys.time()
preddat1 <-  brms::posterior_predict(mod, newdata = newdat, allow_new_levels = T, re_form = "~(1 | Final_country) + (1 | message) + (1 | Final_country:message)", summary = F, cores = 20) # use NULL or specify a random effect formula for re_form as this calculates predicted values for  group-level effects (i.e. take the actual estimated intercepts for each specific group and uses them to make new predictions for the same groups)
t2 <- Sys.time()
tmod <- t2 - t1
tmod

pushoverr::pushover(message = "And...the code is finished, phew!", user = Sys.getenv("PUSHOVER_USER"), app = Sys.getenv("PUSHOVER_APP"))  # use when keys set with .Renvir

saveRDS(preddat1, "brms_mod_data8_preddat1.rds")
#preddat1 <- readRDS("brms_mod_data8_preddat1.rds")

preddat1.new <- preddat1[,,1]
preddat1.believe <- preddat1[,,2]
preddat1.relevant <- preddat1[,,3]
preddat1.drinkless <- preddat1[,,4]
glimpse(preddat1.new)

preddat2a <- apply(preddat1.new, 2, mean) # calculate p(Yes) 
preddat2b <- apply(preddat1.believe, 2, mean) # calculate p(Yes) 
preddat2c <- apply(preddat1.relevant, 2, mean) # calculate p(Yes) 
preddat2d <- apply(preddat1.drinkless, 2, mean) # calculate p(Yes) 

datarun2 <- cbind(newdat, pYes.new = preddat2a,
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

#################### below is code for final paper plots & tables ########################


# data munging for plotting and tables
newdat.merged <- cbind(newdat,preddat2a,preddat2b,preddat2c,preddat2d)
newdat.merged
dplyr::glimpse(newdat.merged)


colnames(newdat.merged) <- c("country", "message", 
                             "sex", "age",
                             "AUDIT",
                             "new", "believe",
                             "relevant", "drinkless")

dplyr::glimpse(newdat.merged)

restab2 <- newdat.merged %>% 
  group_by(country,message) %>% 
  select(country,message,new,believe,relevant,drinkless) %>% 
  summarise(mean_new = mean(new),
            mean_believe = mean(believe),
            mean_relevant = mean(relevant),
            mean_drinkless = mean(drinkless))
restab2
glimpse(cbind(restab1,restab2))

restab <- cbind(restab1,restab2) %>% 
  select("Country" = "Final_country",
         "message" = "message...2",
         "new, observed data" = "mean_new...3",
         "new, predicted probability" = "mean_new...9",
         "believe, observed data" = "mean_believe...4",
         "believe, predicted probability" = "mean_believe...10",
         "relevant, observed data" = "mean_relevant...5",
         "relevant, predicted probability" = "mean_relevant...11",
         "drinkless, observed data" = "mean_drinkless...6",
         "drinkless, predicted probability" = "mean_drinkless...12") %>% 
  pivot_wider(names_from = message,
              values_from = c("new, observed data",
                              "new, predicted probability",
                              "believe, observed data",
                              "believe, predicted probability",
                              "relevant, observed data",
                              "relevant, predicted probability",
                              "drinkless, observed data",
                              "drinkless, predicted probability")) %>% 
  ungroup()

# appx table for 'new'
restab %>% 
  select(Country,contains("new")) %>% 
  mutate(Country = as.character(Country)) %>% 
  select(1,2,9,3,10,4,11,5,12,6,13,7,14,8,15) %>% 
  gt(rowname_col = "Country") %>% 
  fmt_percent(columns = c(2:15), decimals = 1) %>% 
  cols_align(align = "center", columns = c(2:15)) %>% 
  cols_label("new, observed data_calories" = "observed data",
             "new, observed data_cancer" = "observed data",
             "new, observed data_freedays" = "observed data",
             "new, observed data_heart" = "observed data",
             "new, observed data_liver" = "observed data",
             "new, observed data_myth" = "observed data",
             "new, observed data_violence" = "observed data",
             "new, predicted probability_calories" = "predicted probability",
             "new, predicted probability_cancer" = "predicted probability",
             "new, predicted probability_freedays" = "predicted probability",
             "new, predicted probability_heart" = "predicted probability",
             "new, predicted probability_liver" = "predicted probability",
             "new, predicted probability_myth" = "predicted probability",
             "new, predicted probability_violence" = "predicted probability") %>% 
  tab_spanner(label = "calories", columns = c(2,3)) %>% 
  tab_spanner(label = "cancer", columns = c(4,5)) %>% 
  tab_spanner(label = "freedays", columns = c(6,7)) %>% 
  tab_spanner(label = "heart", columns = c(8,9)) %>% 
  tab_spanner(label = "liver", columns = c(10,11)) %>% 
  tab_spanner(label = "myth", columns = c(12,13)) %>% 
  tab_spanner(label = "violence", columns = c(14,15)) %>% 
  tab_header(title = "Observed data and predicted probability by country for \'new\' measure") %>% 
  tab_footnote(footnote = "predicted probability estimates are aggregates across equally weighted age, sex and AUDIT score categories, whereas observed data may be skewed to particular demographic groups ",
               locations = cells_title(groups = c("title"))) %>% 
  gtsave("appxtab_new.pdf")

# appx table for 'believe'
restab %>% 
  select(Country,contains("believe")) %>% 
  mutate(Country = as.character(Country)) %>% 
  select(1,2,9,3,10,4,11,5,12,6,13,7,14,8,15) %>% 
  gt(rowname_col = "Country") %>% 
  fmt_percent(columns = c(2:15), decimals = 1) %>% 
  cols_align(align = "center", columns = c(2:15)) %>% 
  cols_label("believe, observed data_calories" = "observed data",
             "believe, observed data_cancer" = "observed data",
             "believe, observed data_freedays" = "observed data",
             "believe, observed data_heart" = "observed data",
             "believe, observed data_liver" = "observed data",
             "believe, observed data_myth" = "observed data",
             "believe, observed data_violence" = "observed data",
             "believe, predicted probability_calories" = "predicted probability",
             "believe, predicted probability_cancer" = "predicted probability",
             "believe, predicted probability_freedays" = "predicted probability",
             "believe, predicted probability_heart" = "predicted probability",
             "believe, predicted probability_liver" = "predicted probability",
             "believe, predicted probability_myth" = "predicted probability",
             "believe, predicted probability_violence" = "predicted probability") %>% 
  tab_spanner(label = "calories", columns = c(2,3)) %>% 
  tab_spanner(label = "cancer", columns = c(4,5)) %>% 
  tab_spanner(label = "freedays", columns = c(6,7)) %>% 
  tab_spanner(label = "heart", columns = c(8,9)) %>% 
  tab_spanner(label = "liver", columns = c(10,11)) %>% 
  tab_spanner(label = "myth", columns = c(12,13)) %>% 
  tab_spanner(label = "violence", columns = c(14,15)) %>% 
  tab_header(title = "Observed data and predicted probability by country for \'believe\' measure") %>% 
  tab_footnote(footnote = "predicted probability estimates are aggregates across equally weighted age, sex and AUDIT score categories, whereas observed data may be skewed to particular demographic groups ",
               locations = cells_title(groups = c("title"))) %>% 
  gtsave("appxtab_believe.pdf")

restab %>% 
  select(Country,contains("relevant")) %>% 
  mutate(Country = as.character(Country)) %>% 
  select(1,2,9,3,10,4,11,5,12,6,13,7,14,8,15) %>% 
  gt(rowname_col = "Country") %>% 
  fmt_percent(columns = c(2:15), decimals = 1) %>% 
  cols_align(align = "center", columns = c(2:15)) %>% 
  cols_label("relevant, observed data_calories" = "observed data",
             "relevant, observed data_cancer" = "observed data",
             "relevant, observed data_freedays" = "observed data",
             "relevant, observed data_heart" = "observed data",
             "relevant, observed data_liver" = "observed data",
             "relevant, observed data_myth" = "observed data",
             "relevant, observed data_violence" = "observed data",
             "relevant, predicted probability_calories" = "predicted probability",
             "relevant, predicted probability_cancer" = "predicted probability",
             "relevant, predicted probability_freedays" = "predicted probability",
             "relevant, predicted probability_heart" = "predicted probability",
             "relevant, predicted probability_liver" = "predicted probability",
             "relevant, predicted probability_myth" = "predicted probability",
             "relevant, predicted probability_violence" = "predicted probability") %>% 
  tab_spanner(label = "calories", columns = c(2,3)) %>% 
  tab_spanner(label = "cancer", columns = c(4,5)) %>% 
  tab_spanner(label = "freedays", columns = c(6,7)) %>% 
  tab_spanner(label = "heart", columns = c(8,9)) %>% 
  tab_spanner(label = "liver", columns = c(10,11)) %>% 
  tab_spanner(label = "myth", columns = c(12,13)) %>% 
  tab_spanner(label = "violence", columns = c(14,15)) %>% 
  tab_header(title = "Observed data and predicted probability by country for \'relevant\' measure") %>% 
  tab_footnote(footnote = "predicted probability estimates are aggregates across equally weighted age, sex and AUDIT score categories, whereas observed data may be skewed to particular demographic groups ",
               locations = cells_title(groups = c("title"))) %>% 
  gtsave("appxtab_relevant.pdf")

# appx table for 'drinkless'
restab %>% 
  select(Country,contains("drinkless")) %>% 
  mutate(Country = as.character(Country)) %>% 
  select(1,2,9,3,10,4,11,5,12,6,13,7,14,8,15) %>% 
  gt(rowname_col = "Country") %>% 
  fmt_percent(columns = c(2:15), decimals = 1) %>% 
  cols_align(align = "center", columns = c(2:15)) %>% 
  cols_label("drinkless, observed data_calories" = "observed data",
             "drinkless, observed data_cancer" = "observed data",
             "drinkless, observed data_freedays" = "observed data",
             "drinkless, observed data_heart" = "observed data",
             "drinkless, observed data_liver" = "observed data",
             "drinkless, observed data_myth" = "observed data",
             "drinkless, observed data_violence" = "observed data",
             "drinkless, predicted probability_calories" = "predicted probability",
             "drinkless, predicted probability_cancer" = "predicted probability",
             "drinkless, predicted probability_freedays" = "predicted probability",
             "drinkless, predicted probability_heart" = "predicted probability",
             "drinkless, predicted probability_liver" = "predicted probability",
             "drinkless, predicted probability_myth" = "predicted probability",
             "drinkless, predicted probability_violence" = "predicted probability") %>% 
  tab_spanner(label = "calories", columns = c(2,3)) %>% 
  tab_spanner(label = "cancer", columns = c(4,5)) %>% 
  tab_spanner(label = "freedays", columns = c(6,7)) %>% 
  tab_spanner(label = "heart", columns = c(8,9)) %>% 
  tab_spanner(label = "liver", columns = c(10,11)) %>% 
  tab_spanner(label = "myth", columns = c(12,13)) %>% 
  tab_spanner(label = "violence", columns = c(14,15)) %>% 
  tab_header(title = "Observed data and predicted probability by country for \'drinkless\' measure") %>% 
  tab_footnote(footnote = "predicted probability estimates are aggregates across equally weighted age, sex and AUDIT score categories, whereas observed data may be skewed to particular demographic groups ",
               locations = cells_title(groups = c("title"))) %>% 
  gtsave("appxtab_drinkless.pdf")


# aggregate by sex, age (16-24, 25+) and AUDIT (0-15, 16+) 

nd1 <- newdat.merged %>% 
  ungroup() %>% 
  filter(age < 25,
         AUDIT < 16) %>% 
  group_by(sex,message,country) %>% 
  summarize(Mean_new = mean(new),
            ll_new = quantile(new,.15), 
            hh_new = quantile(new,.85),
            Mean_believe = mean(believe),
            ll_believe = quantile(believe,.15), 
            hh_believe = quantile(believe,.85),
            Mean_relevant = mean(relevant),
            ll_relevant = quantile(relevant,.15), 
            hh_relevant = quantile(relevant,.85),
            Mean_drinkless = mean(drinkless),
            ll_drinkless = quantile(drinkless,.15), 
            hh_drinkless = quantile(drinkless,.85)) %>% 
  mutate(agegrp = c("16-24"),
         AUDITgrp = c("AUDIT 1-15"))

nd2 <- newdat.merged %>% 
  ungroup() %>% 
  filter(age < 25,
         AUDIT > 15) %>% 
  group_by(sex,message,country) %>% 
  summarize(Mean_new = mean(new),
            ll_new = quantile(new,.15), 
            hh_new = quantile(new,.85),
            Mean_believe = mean(believe),
            ll_believe = quantile(believe,.15), 
            hh_believe = quantile(believe,.85),
            Mean_relevant = mean(relevant),
            ll_relevant = quantile(relevant,.15), 
            hh_relevant = quantile(relevant,.85),
            Mean_drinkless = mean(drinkless),
            ll_drinkless = quantile(drinkless,.15), 
            hh_drinkless = quantile(drinkless,.85)) %>% 
  mutate(agegrp = c("16-24"),
         AUDITgrp = c("AUDIT 16+"))

nd3 <- newdat.merged %>% 
  ungroup() %>% 
  filter(age > 24,
         AUDIT < 16) %>% 
  group_by(sex,message,country) %>% 
  summarize(Mean_new = mean(new),
            ll_new = quantile(new,.15), 
            hh_new = quantile(new,.85),
            Mean_believe = mean(believe),
            ll_believe = quantile(believe,.15), 
            hh_believe = quantile(believe,.85),
            Mean_relevant = mean(relevant),
            ll_relevant = quantile(relevant,.15), 
            hh_relevant = quantile(relevant,.85),
            Mean_drinkless = mean(drinkless),
            ll_drinkless = quantile(drinkless,.15), 
            hh_drinkless = quantile(drinkless,.85)) %>% 
  mutate(agegrp = c("25+"),
         AUDITgrp = c("AUDIT 1-15"))

nd4 <- newdat.merged %>% 
  ungroup() %>% 
  filter(age > 24,
         AUDIT > 15) %>% 
  group_by(sex,message,country) %>% 
  summarize(Mean_new = mean(new),
            ll_new = quantile(new,.15), 
            hh_new = quantile(new,.85),
            Mean_believe = mean(believe),
            ll_believe = quantile(believe,.15), 
            hh_believe = quantile(believe,.85),
            Mean_relevant = mean(relevant),
            ll_relevant = quantile(relevant,.15), 
            hh_relevant = quantile(relevant,.85),
            Mean_drinkless = mean(drinkless),
            ll_drinkless = quantile(drinkless,.15), 
            hh_drinkless = quantile(drinkless,.85)) %>% 
  mutate(agegrp = c("25+"),
         AUDITgrp = c("AUDIT 16+"))


nd.merged <- rbind(nd1,nd2,nd3,nd4)

glimpse(nd.merged)

ndm <- nd.merged %>%
  ungroup() %>% 
  mutate(agegrp = as.factor(agegrp),
         AUDITgrp = as.factor(AUDITgrp),
         sex = fct_recode(sex, "male" = "1", "female" = "0")) %>% 
  unite("sexage", sex, agegrp, sep = "_") %>% 
  mutate(sexage = as.factor(sexage),
         sexage = fct_recode(sexage, "Females aged 16-24" = "female_16-24",
                             "Females aged 25+" = "female_25+",
                             "Males aged 16-24" = "male_16-24",
                             "Males aged 25+" = "male_25+"),
         sexage = fct_relevel(sexage, 
                              "Females aged 16-24",
                              "Males aged 16-24",
                              "Females aged 25+",
                              "Males aged 25+"),
         message = fct_relevel(message,
                               "cancer",
                               "freedays",
                               "calories",
                               "liver",
                               "violence",
                               "heart",
                               "myth")) #this releveling order is not consisent across dep vars...hmmm....

ndm$sexage
ndm

colourCount = length(unique(ndm$country))
getPalette = colorRampPalette(brewer.pal(29, "Dark2"))

plotndm1 <- ggplot(data = ndm, 
                   aes(x = country,
                       y = Mean_new,
                       text = paste(country,
                                    ",",
                                    AUDITgrp,
                                    ":",
                                    "<br>Pr(Yes) = ",
                                    round(Mean_new,2)))) +
  facet_grid(message ~ sexage) +
  geom_point(data = ndm, 
             aes(x = fct_reorder(country, Mean_new),
                 y = Mean_new,
                 colour = fct_reorder(country, Mean_new),
                 shape = AUDITgrp),
             size = 1.5) +
  scale_colour_manual(values = getPalette(colourCount)) +
  scale_shape_manual(values = c(3, 17)) +
  scale_y_continuous(breaks = c(.2, .4, .6, .8, 1), limits = c(0,1)) +
  theme_light() +
  xlab("") +
  ylab("Predicted Probability\n") +
  theme(plot.margin = unit(c(2, 1, 1, 1), "cm"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey70"),
        strip.background = element_rect(colour = "grey50", 
                                        fill = "grey50"),
        strip.text = element_text(colour = "white", size = 10, 
                                  face = "bold"),
        legend.title = element_text(color = "black",
                                    face = "bold",
                                    size = 11),
        legend.spacing = unit(1, "cm"),
        legend.position = "right") +
  guides(colour = guide_legend(override.aes = list(size = 3),
                               title = "Country (low to high\n predicted probability)", 
                               keyheight = 0.9,
                               ncol = 1,
                               order = 2),
         shape = guide_legend(override.aes = list(size = 3),
                              title = "AUDIT score",
                              keyheight = 0.9,
                              reverse = TRUE,
                              order = 1))

plotndm2 <- ggplot(data = ndm, 
                   aes(x = country,
                       y = Mean_believe,
                       text = paste(country,
                                    ",",
                                    AUDITgrp,
                                    ":",
                                    "<br>Pr(Yes) = ",
                                    round(Mean_believe,2)))) +
  facet_grid(message ~ sexage) +
  geom_point(data = ndm, 
             aes(x = fct_reorder(country, Mean_believe),
                 y = Mean_believe,
                 colour = fct_reorder(country, Mean_believe),
                 shape = AUDITgrp),
             size = 1.5) +
  scale_colour_manual(values = getPalette(colourCount)) +
  scale_shape_manual(values = c(3, 17)) +
  scale_y_continuous(breaks = c(.2, .4, .6, .8, 1), limits = c(0,1)) +
  theme_light() +
  xlab("") +
  ylab("Predicted Probability\n") +
  theme(plot.margin = unit(c(2, 1, 1, 1), "cm"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey70"),
        strip.background = element_rect(colour = "grey50", 
                                        fill = "grey50"),
        strip.text = element_text(colour = "white", size = 10, 
                                  face = "bold"),
        legend.title = element_text(color = "black",
                                    face = "bold",
                                    size = 11),
        legend.spacing = unit(1, "cm"),
        legend.position = "right") +
  guides(colour = guide_legend(override.aes = list(size = 3),
                               title = "Country (low to high\n predicted probability)", 
                               keyheight = 0.9,
                               ncol = 1,
                               order = 2),
         shape = guide_legend(override.aes = list(size = 3),
                              title = "AUDIT score",
                              keyheight = 0.9,
                              reverse = TRUE,
                              order = 1))


plotndm3 <- ggplot(data = ndm, 
                   aes(x = country,
                       y = Mean_relevant,
                       text = paste(country,
                                    ",",
                                    AUDITgrp,
                                    ":",
                                    "<br>Pr(Yes) = ",
                                    round(Mean_relevant,2)))) +
  facet_grid(message ~ sexage) +
  geom_point(data = ndm, 
             aes(x = fct_reorder(country, Mean_relevant),
                 y = Mean_relevant,
                 colour = fct_reorder(country, Mean_relevant),
                 shape = AUDITgrp),
             size = 1.5) +
  scale_colour_manual(values = getPalette(colourCount)) +
  scale_shape_manual(values = c(3, 17)) +
  scale_y_continuous(breaks = c(.2, .4, .6, .8, 1), limits = c(0,1)) +
  theme_light() +
  xlab("") +
  ylab("Predicted Probability\n") +
  theme(plot.margin = unit(c(2, 1, 1, 1), "cm"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey70"),
        strip.background = element_rect(colour = "grey50", 
                                        fill = "grey50"),
        strip.text = element_text(colour = "white", size = 10, 
                                  face = "bold"),
        legend.title = element_text(color = "black",
                                    face = "bold",
                                    size = 11),
        legend.spacing = unit(1, "cm"),
        legend.position = "right") +
  guides(colour = guide_legend(override.aes = list(size = 3),
                               title = "Country (low to high\n predicted probability)", 
                               keyheight = 0.9,
                               ncol = 1,
                               order = 2),
         shape = guide_legend(override.aes = list(size = 3),
                              title = "AUDIT score",
                              keyheight = 0.9,
                              reverse = TRUE,
                              order = 1))


plotndm4 <- ggplot(data = ndm, 
                   aes(x = country,
                       y = Mean_drinkless,
                       text = paste(country,
                                    ",",
                                    AUDITgrp,
                                    ":",
                                    "<br>Pr(Yes) = ",
                                    round(Mean_drinkless,2)))) +
  facet_grid(message ~ sexage) +
  geom_point(data = ndm, 
             aes(x = fct_reorder(country, Mean_drinkless),
                 y = Mean_drinkless,
                 colour = fct_reorder(country, Mean_drinkless),
                 shape = AUDITgrp),
             size = 1.5) +
  scale_colour_manual(values = getPalette(colourCount)) +
  scale_shape_manual(values = c(3, 17)) +
  scale_y_continuous(breaks = c(.2, .4, .6, .8, 1), limits = c(0,1)) +
  theme_light() +
  xlab("") +
  ylab("Predicted Probability\n") +
  theme(plot.margin = unit(c(2, 1, 1, 1), "cm"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey70"),
        strip.background = element_rect(colour = "grey50", 
                                        fill = "grey50"),
        strip.text = element_text(colour = "white", size = 10, 
                                  face = "bold"),
        legend.title = element_text(color = "black",
                                    face = "bold",
                                    size = 11),
        legend.spacing = unit(1, "cm"),
        legend.position = "right") +
  guides(colour = guide_legend(override.aes = list(size = 3),
                               title = "Country (low to high\n predicted probability)", 
                               keyheight = 0.9,
                               ncol = 1,
                               order = 2),
         shape = guide_legend(override.aes = list(size = 3),
                              title = "AUDIT score",
                              keyheight = 0.9,
                              reverse = TRUE,
                              order = 1))




dev.off()
plotndm1
plotndm2
plotndm3
plotndm4


m <- list(
  l = 90,
  r = 250, # 250 to add png legend (if can figure out how to do this)
  b = 50,
  t = 50,
  pad = 4
)
y <- list(
  title = "Predicted Probability"
)


# plots to plotly - new
subp <- plotly::ggplotly(plotndm1, tooltip = c("text")) %>%
  plotly::hide_legend()  %>% 
  plotly::layout(margin = m)

str(subp[['x']][['layout']][['annotations']][[1]][['x']])  # check position of y-axis label and then change it:
subp[['x']][['layout']][['annotations']][[1]][['x']] <- -0.07

subp <- subp %>% 
  layout(
    images = list(
      list(
        source =  "https://raw.githubusercontent.com/davidfoxcroft/publicimages/master/plotly_legend_new.png",
        xref = "paper",
        yref = "paper",
        x = 1.03,
        y = 1.02,
        sizex = 1,
        sizey = 1
      )
    )
  )
#subp
htmlwidgets::saveWidget(subp, "plot_new.html")

# plots to plotly - believe
subp <- plotly::ggplotly(plotndm2, tooltip = c("text")) %>%
  plotly::hide_legend()  %>% 
  plotly::layout(margin = m)

str(subp[['x']][['layout']][['annotations']][[1]][['x']])  # check position of y-axis label and then change it:
subp[['x']][['layout']][['annotations']][[1]][['x']] <- -0.07

subp <- subp %>% 
  layout(
    images = list(
      list(
        source =  "https://raw.githubusercontent.com/davidfoxcroft/publicimages/master/plotly_legend_believe.png",
        xref = "paper",
        yref = "paper",
        x = 1.03,
        y = 1.02,
        sizex = 1,
        sizey = 1
      )
    )
  )
#subp
htmlwidgets::saveWidget(subp, "plot_believe.html")

# plots to plotly - relevant
subp <- plotly::ggplotly(plotndm3, tooltip = c("text")) %>%
  plotly::hide_legend()  %>% 
  plotly::layout(margin = m)

str(subp[['x']][['layout']][['annotations']][[1]][['x']])  # check position of y-axis label and then change it:
subp[['x']][['layout']][['annotations']][[1]][['x']] <- -0.07

subp <- subp %>% 
  layout(
    images = list(
      list(
        source =  "https://raw.githubusercontent.com/davidfoxcroft/publicimages/master/plotly_legend_relevant.png",
        xref = "paper",
        yref = "paper",
        x = 1.03,
        y = 1.02,
        sizex = 1,
        sizey = 1
      )
    )
  )
#subp
htmlwidgets::saveWidget(subp, "plot_relevant.html")

# plots to plotly - drinkless
subp <- plotly::ggplotly(plotndm4, tooltip = c("text")) %>%
  plotly::hide_legend()  %>% 
  plotly::layout(margin = m)

str(subp[['x']][['layout']][['annotations']][[1]][['x']])  # check position of y-axis label and then change it:
subp[['x']][['layout']][['annotations']][[1]][['x']] <- -0.07

subp <- subp %>% 
  layout(
    images = list(
      list(
        source =  "https://raw.githubusercontent.com/davidfoxcroft/publicimages/master/plotly_legend_drinkless.png",
        xref = "paper",
        yref = "paper",
        x = 1.03,
        y = 1.02,
        sizex = 1,
        sizey = 1
      )
    )
  )
#subp
htmlwidgets::saveWidget(subp, "plot_drinkless.html")



# list session information
sessionInfo()

# save environment snapshot
renv::snapshot()







######################### demo plots for revision ###################

plotndm8 <- ggplot(data = ndm, 
                   aes(x = country,
                       y = Mean_drinkless,
                       text = paste(country,
                                    ",",
                                    AUDITgrp,
                                    ":",
                                    "<br>Pr(Yes) = ",
                                    round(Mean_drinkless,2)))) +
  facet_grid(message ~ sexage) +
  geom_point(data = ndm, 
             aes(x = fct_reorder(country, Mean_drinkless),
                 y = Mean_drinkless,
                 colour = fct_reorder(country, Mean_drinkless),
                 shape = AUDITgrp),
             size = 1.5) +
  geom_linerange(aes(x = fct_reorder(country, Mean_drinkless),
                     colour = fct_reorder(country, Mean_drinkless),
                     ymin = ll_drinkless,
                     ymax = hh_drinkless)) +
  scale_colour_manual(values = getPalette(colourCount)) +
  scale_shape_manual(values = c(16, 17)) +
  scale_y_continuous(breaks = c(.2, .4, .6, .8, 1), limits = c(0,1)) +
  theme_light() +
  xlab("") +
  ylab("Predicted Probability\n") +
  theme(plot.margin = unit(c(2, 1, 1, 1), "cm"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey70"),
        strip.background = element_rect(colour = "grey50", 
                                        fill = "grey50"),
        strip.text = element_text(colour = "white", size = 10, 
                                  face = "bold"),
        legend.title = element_text(color = "black",
                                    face = "bold",
                                    size = 11),
        legend.spacing = unit(1, "cm"),
        legend.position = "right") +
  guides(colour = guide_legend(override.aes = list(size = 4),
                               title = "Country (low to high\n predicted probability)", 
                               keyheight = 0.9,
                               ncol = 1,
                               order = 2),
         shape = guide_legend(override.aes = list(size = 3),
                              title = "AUDIT score",
                              keyheight = 0.9,
                              reverse = TRUE,
                              order = 1))

plotndm8
















