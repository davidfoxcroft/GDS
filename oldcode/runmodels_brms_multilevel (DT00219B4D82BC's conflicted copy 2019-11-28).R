########################################
# stan options -----------------
options(mc.cores = parallel::detectCores(logical = FALSE))
rstan_options(auto_write = TRUE)
chains <- 3 
cores <- chains
iter <- 2000
warmup <- 1000
seed <- as.integer(round(sqrt(1963),0)) # make integer for set.seed

# load data and scale relevant vars
datarun <- data7 %>% # data6 for full data set, data7 for test dataset with n=100 per country 
  mutate(., age, age.s = scale(age)[1,])  %>%
  mutate(., AUDIT_SCORE, as.s = scale(AUDIT_SCORE)[1,]) %>%
  filter(., as.integer(ncountry) == sample(1:length(unique(data$ncountry)), 10, replace = F)) # randomly select subset of 10 countries to speed up runs during testing

glimpse(datarun)

# initial look at data using base logistic regression - turn off CIs for clarity
binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}

p1 <- ggplot(datarun, aes(x = AUDIT_SCORE, 
                       y = response, 
                       color = impact)) +
  binomial_smooth(se = FALSE) +
  xlab("") + ylab("Pr (Yes)")
p1 + facet_wrap(.~message)

p2 <- ggplot(datarun, aes(x = AUDIT_SCORE, 
                       y = response, 
                       color = ncountry)) +
  binomial_smooth(se = FALSE) +
  xlab("AUDIT score") + ylab("Pr (Yes)")
p2 + facet_grid(rows = vars(impact), cols = vars(message))



# specify model formula
form <- bf(response ~ 
             1 + 
             age.s +  
             as.s + 
             (1 | id) +
             (1 + as.s | message + impact + ncountry))
prior <- c(prior_string("normal(0,3)", 
                        class = "b"))

# run model through brms
t1 <- Sys.time()
mod <- brms::brm(formula = form,
                 data = datarun,
                 family = bernoulli,
                 prior = prior,
                 iter = iter,
                 chains = chains,
                 warmup = warmup,
                 cores = cores,
                 seed = seed,
                 init = 0,
                 control = list(adapt_delta = 0.99,
                                max_treedepth = 11)
                 )
t2 <- Sys.time()
runtime <- t2 - t1
runtime
saveRDS(mod,"GDS_brms_model.RDS")
pushover(message = "And...the code is finished, phew!", user = PUSHOVER_USER, app = PUSHOVER_APP)


# look over model results
summary(mod)
ranef(mod)

# model diagnostics - use shinystan
my_sso <- launch_shinystan(mod)
my_sso

mod <- readRDS("GDS_brms_model.RDS")

# posterior predictive checks - can also be done in shinystan
mod.ppc <- pp_check(mod) 
mod.ppc + xlim(0, 1)


# data wrangling model
vars <- enframe(get_variables(mod)) %>% # peruse mod output parameters
  filter(!grepl("r_id",value)) # exclude id random effect vars 
vars

# use brms posterior predict to get predicted values from the posterior (different calcs below are checking how re are handled in the calculation)
preddat1 <- brms::posterior_predict(mod, newdata = NULL, re_formula = ~(1 + as.s | message + impact + ncountry), set.seed = 123)  
preddat2 <- brms::posterior_predict(mod, newdata = NULL, re_formula = NULL, set.seed = 123)  
preddat3 <- brms::posterior_predict(mod, newdata = NULL, re_formula = NA, set.seed = 123)  

cor(colMeans(preddat1),colMeans(preddat2))
cor(colMeans(preddat1),colMeans(preddat3))
plot(colMeans(preddat1),colMeans(preddat2))
plot(colMeans(preddat1),colMeans(preddat3))

# use NULL or formula for re_formula as this calculates predicted values for all group-level effects (i.e. take the actual estimated intercepts for each specific group and uses them to make new predictions for the same groups)




glimpse(preddat)
glimpse(datarun)
head(preddat)














plot(marginal_effects(mod), ask = FALSE)
me <- marginal_effects(mod, "impact:message")
plot(me, plot = FALSE)[[1]] + 
  scale_color_grey() +
  scale_fill_grey()
## only plot the marginal interaction effect of 'impact:message'
## for different values for 'age.s'
conditions <- data.frame(as.s = c(-1, 0, 1))

plot(marginal_effects(mod, effects = "impact:message", 
                      conditions = "ncountry"))

me1 <- marginal_effects(mod)
glimpse(me1)

post1 <- data %>%
  # modelr::data_grid(ncountry) %>%
  add_fitted_draws(mod, dpar = TRUE) %>%
  ggplot(aes(x = .value, y = ncountry, color = impact)) +
  stat_pointinterval(position = position_dodge(width = .6)) +
  scale_size_continuous(guide = FALSE) +
  scale_color_manual(values = RColorBrewer::brewer.pal(6, "Blues")[-c(1,2)])

post1

post2 <- data %>%
  # data_grid(agegp) %>%
  add_fitted_draws(mod) %>%
  ggplot(aes(x = ncountry, y = .value)) +
  stat_summaryh(fun.x = median, geom = "barh", fill = "gray75", width = 1, color = "white") +
  stat_pointintervalh() +
  coord_cartesian(expand = FALSE) +
  facet_grid(. ~ impact, switch = "x") +
  theme_classic() +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  ggtitle("P(response category | impact)") +
  ylab("impact")












    
grid = data %>%
  modelr::data_grid(impact)

fits = grid %>%
  add_fitted_draws(mod)

preds = grid %>%
  add_predicted_draws(mod)

data %>%
  ggplot(aes(x = response, y = impact)) +
  geom_halfeyeh(aes(x = .value), relative_scale = 0.7, position = position_nudge(y = 0.1), data = fits) +
  stat_intervalh(aes(x = .prediction), data = preds) +
  geom_point(data = data) +
  scale_color_brewer()

















mod %>%
  spread_draws(r_impact[impact,]) %>%
  compare_levels(r_impact, by = impact) %>%
  ungroup() %>%
  mutate(impact = reorder(impact, r_impact)) %>%
  ggplot(aes(y = impact, x = r_impact)) +
  geom_pointintervalh() + 
  geom_vline(xintercept = 0, linetype = "dashed") 
  
  
  
  mod %>%
    spread_draws(b_Intercept,r_ncountry[ncountry,Intercept]) %>%
    median_qi(ncountry_mean = b_Intercept + r_ncountry,
              .width = c(.90, .5)) %>%
    ggplot(aes(y = ncountry, x = ncountry_mean)) +
    geom_pointintervalh() 
  head(10)
  
  


  compare_levels(r_ncountry, by = ncountry) %>%
  ggplot(aes(y = ncountry, x = r_ncountry)) +
  geom_halfeyeh() + 
  xlim(-1,1)

mod %>%
  spread_draws(r_message[message,]) %>%
  compare_levels(r_message, by = message) %>%
  ggplot(aes(y = message, x = r_message)) +
  geom_halfeyeh()




# loo for model checking and comparison
loo.aware.mod.new <- loo(mod.new, reloo = TRUE, save_psis = TRUE, cores = 4)
options(future.globals.maxSize = 891289600)
loo.aware.mod.new3 <- loo(mod, reloo = TRUE, cores = 8) # NB reloo = TRUE takes a loooong time so run overnight
loo.aware.mod.new3a <- loo(mod.new3, save_psis = TRUE, cores = 8)
print(loo.aware.mod.new)
print(loo.aware.mod.new3)
plot(loo.aware.mod.new, label_points = TRUE)
plot(loo.aware.mod.new3, label_points = TRUE)
yrep.aware.mod.new <- posterior_predict(mod.new)
yrep.aware.mod.new3a <- posterior_predict(mod.new3a)
plot_overlay.aware.mod.new <- ppc_loo_pit_overlay(data$response, yrep.aware.mod.new, lw = weights(loo.aware.mod.new$psis_object))
plot_overlay.aware.mod.new3a <- ppc_loo_pit_overlay(data$response, yrep.aware.mod.new3a, lw = weights(loo.aware.mod.new3a$psis_object))
plot_overlay.aware.mod.new
plot_overlay.aware.mod.new3a
print(loo_compare(loo.aware.mod.new, loo.aware.mod.new3), digits = 3)

# model plots







####################### code development !! work in progress --------------------------



