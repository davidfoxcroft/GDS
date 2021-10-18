#### stan options -----------------
options(mc.cores = parallel::detectCores(logical = FALSE))
rstan_options(auto_write = TRUE)
chains <- 4 
cores <- chains
iter <- 2000
warmup <- 1000
seed <- as.integer(round(sqrt(1963),0)) # make integer for set.seed

# A. load data and scale relevant vars ---------------
datarun <- data7 %>% 
  mutate(age, age.m = scale(age, scale = FALSE)[,1])  %>%
  mutate(AUDIT_SCORE, as.m = scale(AUDIT_SCORE, scale = FALSE)[,1])

# %>%
  # filter(Final_country == c("Brazil") |  # select subset of countries to speed up ini tial analyses
  #          Final_country == c("Colombia") |
  #          Final_country == c("Denmark") |
  #          Final_country == c("Finland") |
  #          Final_country == c("Israel") |
  #          Final_country == c("Poland") |
  #          Final_country == c("United Kingdom"))

datarun$Final_country <- droplevels(datarun$Final_country)

table(datarun$Final_country)

glimpse(datarun)
which(!complete.cases(datarun))


# B. initial look at data ------------------
binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}

p01 <- ggplot(datarun, aes(x = AUDIT_SCORE, 
                       y = response, 
                       color = Final_country)) +
    # geom_point() +
    binomial_smooth(formula = y ~ splines::ns(x,2), se = F) + # turn off CIs for clarity 
  xlab("AUDIT Score") + ylab("Pr (Yes)") +
  theme_light()
p01
p01 + facet_wrap(.~impact, nrow = 2)

p02 <- ggplot(datarun, aes(x = message, 
                       y = response, 
                       color = Final_country)) +
  geom_tile(aes(fill = message, color = NULL, alpha = 0.2), show.legend = F) + 
  scale_fill_manual(values = c("white", "grey95", "white", "grey95", "white", "grey95", "white")) +
  geom_point(data = datarun, aes(x = message, 
                                   y = response, 
                                   color = Final_country), stat = 'summary', fun.y = mean, size = 2, alpha = .8, position = position_jitter(width = .3, height = 0)) +
  coord_cartesian(xlim = NULL, ylim = c(0,1), expand = TRUE,
                  default = FALSE, clip = "on") +
   theme_light() +
  theme(legend.title = element_blank()) +
   theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1)) +
  xlab("Message") + 
  ylab("Pr (Yes)") 
 p02 <- p02 + facet_wrap(.~impact) +
   theme(strip.text.x = element_text(size = 12, face = "bold", colour = "white"))
p02
#ggp02 <- ggplotly(p02)
#ggp02

# C. fit brms models for y = binary Y/N response  ----------------
form1 <- bf(response ~ 
              1 + 
              (1 | Final_country / id)) # intercept varies by individuals nested within countries
form2 <- bf(response ~ 
              1 + 
              (1 | id) +
              (1 | Final_country)) # intercept varies by individuals and separately by country 
form3 <- bf(response ~ 
              1 + 
              age.m +  
              as.m + 
              sex + 
              (1 | Final_country / id)) # intercept varies by individuals nested within country; with mean centred age and audit score (mean = 0) and sex (female = 0) as predictors of response
form4 <- bf(response ~ 
              1 + 
              age.m +  
              as.m + 
              sex +
              message +
              impact + 
              (1 | Final_country )) # intercept varies by countries. Fixed / population effects are mean centered age and audit score (mean = 0), sex (female = 0), message type (reference = calories), and impact type (reference = new)
form5 <- bf(response ~ 
               1 + 
               age.m +  
               as.m + 
               sex +
               message:impact + 
               (1 | Final_country / id )) # intercept varies by individuals nested within countries. Fixed / population effects are mean centred age and audit score (mean = 0), sex (female = 0), message type (reference = calories), and impact type (reference = new), and message x impact interaction
form6 <- bf(response ~ 
              1 + 
              age.m +  
              as.m + 
              sex +
              message:impact + 
              (1 + as.m | Final_country ) +
              (1 | Final_country:id)) # intercept varies by individuals nested within countries. Fixed / population effects are mean centred age and audit score (mean = 0), sex (female = 0), message type (reference = calories), and impact type (reference = new), and message x impact interaction (reference = calories_new). Slope for audit score varies by country.
form7 <- bf(response ~ 
              1 + 
              age.m +  
              as.m + 
              sex +
              (1 | message) + 
              (1 | impact) + 
              (1 | Final_country/id)) # intercept varies by individuals nested within countries. Fixed / population effects are mean centred age and audit score (mean = 0), sex (female = 0), message type (reference = calories), and impact type (reference = new), and message x impact interaction. Slope for message and impact varies by country.
form8 <- bf(response ~ 
              1 + 
              age.m +  
              as.m + 
              sex +
              message*impact + 
              (1 | id) +
              (1 | Final_country))

prior <- c(
  prior(student_t(3, 0, 2.5), class = "Intercept"),
  prior(student_t(3, 0, 2.5), class = "b"),
  prior(student_t(3, 0, 1), class = "sd"),
  prior(student_t(3, 0, 1), class = "sd", group = "Final_country"), 
  prior(student_t(3, 0, 1), class = "sd", group = "id")
)
# get_prior(form7, data = datarun)


brms::make_stancode(formula = form7,
                    data = datarun,
                    family = bernoulli,
                    prior = prior,
                    inits = 0)
  



print(mod)
ranef(mod)




t1 <- Sys.time()
update(mod, form2)
saveRDS(mod,"GDS_brms_data7_form7.RDS")
t2 <- Sys.time()
tmod2 <- t2 - t1

t1 <- Sys.time()
update(mod, form3)
saveRDS(mod,"GDS_brms_data7_form3.RDS")
t2 <- Sys.time()
tmod3 <- t2 - t1

t1 <- Sys.time()
update(mod, form4)
saveRDS(mod,"GDS_brms_data7_form4.RDS")
t2 <- Sys.time()
tmod4 <- t2 - t1

t1 <- Sys.time()
update(mod, form5)
saveRDS(mod,"GDS_brms_data7_form5.RDS")
t2 <- Sys.time()
tmod5 <- t2 - t1

t1 <- Sys.time()
update(mod, form6)
saveRDS(mod,"GDS_brms_data7_form6.RDS")
t2 <- Sys.time()
tmod6 <- t2 - t1

t1 <- Sys.time()
update(mod, form7)
saveRDS(mod,"GDS_brms_data7_form7.RDS")
t2 <- Sys.time()
tmod7 <- t2 - t1

tmod1
tmod2
tmod3
tmod4
tmod5
tmod6
tmod7

# timings (data7):
#
#   form1: 7.8 mins
#   form2: 7.8 mins
#   form3: 10.4 mins
#   form4: 41.3 mins
#   form5: 1.1 hours
#   form6: 1.67 hours (increased to 2500 iters and 4 chains)
#   form7: 18.55 hours
#   form8: 6.03 hours
# data7 has c.20,000 rows but full dataset has c.82 million rows!!

pushover(message = "And...the code is finished, phew!", user = PUSHOVER_USER, app = PUSHOVER_APP)

#----#----#----#----
#### NOTE 1. update form5 with no interaction (check about interaction post-processing with brms marginal_effects); 2. compare models form5, form6 and form7 using loo #######
#----#----#----#----


# D. look over model results (for C) ------------------

print(mod)
ranef(mod)
prior_summary(mod)

# model diagnostics - use shinystan
my_sso <- launch_shinystan(mod)
my_sso

mod <- readRDS("GDS_brms_data7_full_form7.RDS")

print(mod)


# posterior predictive checks - can also be done in shinystan
mod.ppc <- pp_check(mod) 
mod.ppc + xlim(0, 1)


# E. model data wrangling (for C) -----------------
vars <- enframe(get_variables(mod)) %>% # peruse mod output parameters
  filter(!grepl("r_id",value)) # exclude id random effect vars 
vars

# use brms posterior predict to get predicted values from the posterior 
preddat1 <- brms::posterior_predict(mod, newdata = NULL, re_formula = NULL) # use NULL or model formula for re_formula as this calculates predicted values for all group-level effects (i.e. take the actual estimated intercepts for each specific group and uses them to make new predictions for the same groups)
glimpse(preddat1)

# see this for explanation of difference between fitted and predicted functions: https://discourse.mc-stan.org/t/difference-between-method-fitted-vs-predict-in-marginal-effects-function/6901/3

preddat2 <- apply(preddat1, 2, mean) # calculate p(Yes) 
glimpse(preddat2)

datarun2 <- cbind(datarun, pYes = preddat2)
glimpse(datarun2)

#######################
#      trying marginal_effects
#######################

conds <- make_conditions(datarun, vars = c("message", "impact", "Final_country"))

preddatm <- marginal_effects(mod, 
                             # effects = message:impact,
                             conditions = conds,
                             re_formula = NULL,
                             method = "predict",
                             plot = FALSE
                             )

plot(preddatm)


# F. model plots (for C) --------------

p1 <- ggplot(datarun2, aes(x = AUDIT_SCORE, 
                          y = pYes, 
                          color = Final_country)) +
  geom_smooth(method = lm, formula = y ~ splines::ns(x,2), se = F) +
  theme_light() +
  xlab("AUDIT score") + ylab("Pr (Yes)")
p1 + facet_grid(rows = vars(impact), cols = vars(message))


datarun3 <- datarun2 %>% 
  group_by(message,impact,Final_country) %>% 
  summarise(ll = quantile(pYes, 0.16),
            l = quantile(pYes, 0.33),
            med = quantile(pYes, 0.5),
            h = quantile(pYes, 0.66),
            hh = quantile(pYes, 0.84))
datarun3


p2 <- ggplot(data = datarun3, 
             aes(text = paste(Final_country,
                              ":",
                              "<br>Pr(Yes) = ",
                              round(med,2),
                              "<br>(84% CrI=",
                              round(ll,2),
                              "-",
                              round(hh,2),
                              ")"))) +
   geom_pointrange(aes(x = impact, 
               y = med, 
               color = message,
               ymin = ll,
               ymax = hh),
               size = 0.3, 
               alpha = .8, 
               position = position_jitter(width = .4,
                                          height = 0)) +
  theme_light() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(NA)) +
  ylab("") 
#p2


p3 <- p2 + facet_wrap(.~Final_country, ncol = 5)

+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(colour = "grey50", fill = "grey50"),
        strip.text = element_text(colour = "white", size = 11, face = "bold"))
p3

# plots to plotly
# ggplotly(p.less)
subp <- plotly::ggplotly(p3, tooltip = c("text")) %>% 
  layout(legend = list(
    orientation = "h",
    y = -0.02))
subp

chart_link <- plotly::api_create(subp, filename = "GDS-alcinfo-brms_plot3")
chart_link # opens up browser and displays plot

# G. fit separate brms models when y = impact variables-----------------
# (impact variables left on original scales, so use negbin)

datarun2 <- data1 %>% 
  mutate(age, age.m = scale(age, scale = FALSE)[,1],
         AUDIT_SCORE, as.m = scale(AUDIT_SCORE, scale = FALSE)[,1],
         awareness = as.integer(awareness),
         invbelieve = as.integer(invbelieve),
         relevance = as.integer(relevance),
         less = as.integer(less),
         id = as.factor(id),
         ncountry = as.factor(ncountry)) %>% 
  select(age.m, as.m, awareness, invbelieve, relevance, less, id, ncountry, sex)

glimpse(datarun2)


datarun2$ncountry <- droplevels(datarun2$ncountry)
glimpse(datarun2)

table(datarun2$ncountry)

# set partially informative priors
priors <- c(prior(normal(0,10), class = b),
            prior(normal(0,3), class = Intercept),
            prior(student_t(3,1), class = sd),
            prior(student_t(3,1), class = sd, coef = Intercept, group = ncountry))
# get_prior(form1, data = datarun2)

# run models through brms
t1 <- Sys.time()
form1 <- bf(awareness ~ 1 + age.m + as.m + sex +
              (1 | ncountry ))
fit1 <- brms::brm(formula = form1,
            data = datarun2, # data in wide format
            family = negbinomial,
            prior = priors,
            sample_prior = TRUE,
            inits = 0,
            control = list(adapt_delta = 0.995,
                           max_treedepth = 12),
            iter = iter,
            chains = chains,
            warmup = warmup,
            cores = cores,
            seed = seed )
saveRDS(fit1c,"GDS_brms_results_data2_negbin_awareness.RDS")
t2 <- Sys.time()
tmod1 <- t2 - t1

# compare random effects for full then with sex as predictor (b)

prior_summary(fit1)
plot(hypothesis(fit1, "age.m > 0"))
plot(hypothesis(fit1, "as.m > 0"))
plot(hypothesis(fit1, "Intercept = 0"))
plot(hypothesis(fit1, "Intercept > 0", class = "sd", group  = "ncountry"))

summary(fit1)
ranef(fit1)

pushover(message = "And...the code is finished, phew!", user = keyring::key_list("pushover")[1,2], app = keyring::key_get("pushover"))


depvars <- c("invbelieve","relevance","less")
# x <- 1
lapply(seq_along(depvars), function(x) {
  formx <- as.formula(paste0(depvars[x],
                            " ~ 1 +
                            age.m +
                            as.m +
                            sex + 
                            (1 | ncountry )"))
  fitx <- update(fit1, formx, newdata = datarun2)
  filename <- paste0("GDS_brms_results_data2_negbin_", 
                     depvars[x],
                     ".RDS")
  saveRDS(fitx, filename)
}) # end lapply

pushover(message = "And...the code is finished, phew!", user = keyring::key_list("pushover")[1,2], app = keyring::key_get("pushover"))



# H. look over model results (for G) --------------------

m.aware.negbin <- readRDS("GDS_brms_results_data2_negbin_awareness.RDS")
m.invbelieve.negbin <- readRDS("GDS_brms_results_data2_negbin_invbelieve.RDS")
m.relevance.negbin <- readRDS("GDS_brms_results_data2_negbin_relevance.RDS")
m.less.negbin <- readRDS("GDS_brms_results_data2_negbin_less.RDS")

brmsout <- list(m.aware.negbin, m.invbelieve.negbin, m.relevance.negbin, m.less.negbin)

brmsout[[1]]$fit

summary(m.aware.negbin)
ranef(m.aware.negbin)
summary(m.invbelieve.negbin)
ranef(m.invbelieve.negbin)
summary(m.relevance.negbin)
ranef(m.relevance.negbin)
summary(m.less.negbin)
ranef(m.less.negbin)

# model diagnostics - use shinystan
my_sso <- launch_shinystan(m.aware.negbin)
my_sso
my_sso <- launch_shinystan(m.invbelieve.negbin)
my_sso
my_sso <- launch_shinystan(m.relevance.negbin)
my_sso
my_sso <- launch_shinystan(m.less.negbin)
my_sso


# posterior predictive checks - can also be done in shinystan
pp.aware.negbin <- pp_check(m.aware.negbin) 
pp.aware.negbin + xlim(0, 20)
pp.invbelieve.negbin <- pp_check(m.invbelieve.negbin) 
pp.invbelieve.negbin + xlim(0, 20)
pp.relevance.negbin <- pp_check(m.relevance.negbin) 
pp.relevance.negbin + xlim(0, 20)
pp.less.negbin <- pp_check(m.less.negbin) 
pp.less.negbin + xlim(0, 20)

# I. model data wrangling (for G) ------------------



# J. model plots (for G) --------------------

#----#----#----#----
#### NOTE 2. need to update model parameter names as code below uses stan code and now switched to brms #######
#----#----#----#----

countries <- levels(datarun2$ncountry)

#glimpse(m.aware.negbin$fit@par_dims)
whichplot <- c("awareness","don\'t believe","relevance","drink less")
# x <- 1
plotout <- lapply(seq_along(whichplot), function(x) {
  brms_sims <- extract(brmsout[[x]]$fit, permuted = TRUE)
  overall_intercept <- exp(median(brms_sims[["b_Intercept"]])) # overall intercept on original scale
  country_median <- apply(brms_sims[["r_1_1"]],2,median)
  ci84 <- posterior_interval(brms_sims$r_1_1, prob = 0.84, 
                             regex_pars = "^[r_1_1]")
  ci66 <- posterior_interval(brms_sims$r_1_1, prob = 0.66, 
                             regex_pars = "^[r_1_1]")
  res <- as.data.frame(cbind(countries,round(country_median,3),round(ci84, 3),round(ci66, 3)))
  res <- res[,c(1,2,3,5,6,4)]
  res[,c(2:6)] <- apply(res[,c(2:6)], 2, as.numeric) 
  res[,c(2:6)] <- exp(res[,c(2:6)])
  rownames(res) <- NULL
  colnames(res) <- c("country","median","ll","l","h","hh")
  titlelab <- paste0("Ratio of individual country intercept \nto overall intercept for \"",whichplot[x],"\"") 
  myannotation <- paste0("(overall intercept on the original \nvariable scale = ", round(overall_intercept,2),")")
  
  p <- ggplot(res, aes(
    x = median,
    y = reorder(country, median),
    text = paste(
      country,
      ": IRR = ",
      round(median, 2),
      "<br>(84% CrI=",
      round(ll, 2),
      "-",
      round(hh, 2),
      ")"
    )
  ))  +
    geom_errorbarh(aes(
      xmax = h,
      xmin = l,
      size = 2,
      colour = 5,
      alpha = .8,
      height = .1
    )) +
    geom_errorbarh(aes(
      xmax = hh,
      xmin = ll,
      size = 0.5,
      colour = 1,
      alpha = .8,
      height = .05
    )) +
    geom_point(colour = "red",
               size = 2,
               shape = 21) +
    geom_vline(xintercept = 1,
               size = .5,
               alpha = .2) +
    scale_x_continuous(limits = c(0.5, 1.85)) +
    theme_light() +
    theme(legend.position = "none") +
    theme(text = element_text(family = "Helvetica")) +
    labs(x = NULL, y = NULL) +
    annotate(
      "text",
      x = 1.6,
      y = 8,
      size = 2,
      label = paste0(titlelab, "\n", myannotation)
    ) +
    theme(
      axis.title = element_text(size = 10),
      axis.text.y = element_text(size = 6),
      panel.grid.major.y = element_blank()
    )
  p
})

py.awareness <- ggplotly(plotout[[1]], tooltip = c("text"))
py.believe <- ggplotly(plotout[[2]], tooltip = c("text"))
py.relevance <- ggplotly(plotout[[3]], tooltip = c("text"))
py.less <- ggplotly(plotout[[4]], tooltip = c("text"))

# plots to plotly
# ggplotly(p.awareness, tooltip = c("text"))


subp <- plotly::subplot(py.awareness, py.believe, py.relevance, py.less, nrows = 2, margin = 0.045, heights = c(0.5, 0.5))
subp

chart_link <- api_create(subp, filename = "GDS-alcinfo2-rstan")
chart_link # opens up browser and displays plot



#### ideas: -------------
# 1. Perhaps use gghighlight to show up certain points: https://www.littlemissdata.com/blog/highlight  or also this: https://stackoverflow.com/questions/32423167/ggplot-xy-scatter-how-to-change-alpha-transparency-for-select-points
#
# 2. work through model results by hand to check understanding - use this as a guide: https://www.bristol.ac.uk/media-library/sites/cmm/migrated/documents/multi-br.pdf
#
# 3. add gender to model (recode as 0,1 first) and specify newdata in brms::predict for predictions by gender (both), age (average, i.e. 0 for age.s), and message, impact and country combinations
# 
# 4. check and use brms::marginal_effects function for conditioned predictions and plots
#
# 5. consider whether to use loo for model checking and comparison
#
# 6. in binomial_smooth try bs spline method and compare
#
# 7. revise dataprep to include new message and impact score variables in data sets for modelling and then incorporate model code from runmodels_brms.R into this script file
#
# 8. instead of using scale for age and audit use mean centering instead - will make interpretation a bit easier (ie still use original units rather than sd / z scores)
#
# 9. use brms::update rather than rerunning full model - does it make any difference?
#
# 10. check and tweak priors


# DONE idea 3. --------------------
table(data$sex)
ifelse(data$sex == 1, 0, 1) # recode to 0 and 1, 0 == female
form6 <- bf(response ~ 
              1 + 
              age.s +
              sex + 
              as.s + 
              (1 | id) +
              (as.s | message) + 
              (as.s | impact) + 
              (as.s | ncountry)) # intercept varies by individuals, by message type, by impact type, and by country with standardised age and audit score (mean = 0, s.d. = 1) and sex (female == 0, male == 1) predictors of response. Slope for audit score varies by message, by impact, and by country

# idea 4. -----------
conds <- data7 %>% 
  modelr::data_grid(message,impact,ncountry)
ht(conds)

me1 <- marginal_effects(mod,
                        effects = c("as.m"),
                        conditions = conds,
                        re_formula = NULL,
                        robust = T, # uses median; F = mean
                        probs = c(0.16,0.84),  # 84% ( 2 x 42 (the answer to everything) 
                        methods = "predict",
                        spaghetti = T, # spaghetti plot
                        nsamples = 100, # for spaghetti plot to be manageable
                        points = T # include jittered points
)

glimpse(me1) 
plot(me1, effects = "as.m")























# idea 5. ----------

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


# DONE idea 6. ----------------
y ~ splines::bs(x)

# DONE idea 8. --------------
age.m <- scale(age, scale = FALSE)
as.m <- scale(AUDIT_SCORE, scale = FALSE)

# DONE idea 9. -----------------
update(mod, formula)
# idea 10. ------------------------
# see https://www.rensvandeschoot.com/tutorials/brms-priors/

