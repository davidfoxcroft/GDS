# log book ----------------
# 1. believe is left skewed so inverted for analysis
# 2. awareness and invbelieve have similar means and variances so poisson and negbin models compared: no diff so use negbin
# 3. relevance and less have higher variance than means so negbin models likely more appropriate
# 4. for relevance negbin and zero inflated negbin tested and zero inflated comes out better in loo comparison. Question is how interpretable is this (negbin also a reasonable model)
# 5. for less negbin and zero inflated negbin compared and zero inflated comes out better in loo comparison. Question is how interpretable is this (negbin also a reasonable model)
# 

########################################


# stan options -----------------
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
chains <- 3
cores <- chains
iter <- 2000
warmup <- 1000
seed <- sqrt(1963)

data <- data3 # data1 for full dataset, data2 for sampled dataset for test analyses, data3 for full with min size per country >= 100

# change Macedonia label for plotting
# countries <- as.factor(data$ncountry)
# countries <- as.character(levels(countries))
# countries[countries == "Macedonia, The Former Yugoslav Republic of"] <- "FYR Macedonia"

# run brms models
depvars <- c("awareness","invbelieve","relevance","less")
family <- c("negbinomial", "negbinomial", "negbinomial", "negbinomial")
# x <- 1

brmsout <- lapply(seq_along(depvars), function(x) {
  fitform <- as.formula(paste0(depvars[x],
                               " ~ 1 + age + 
                               AUDIT_SCORE + 
                              (1 | ncountry)"))  
  brms::brm(formula = fitform,
              data = data,
              family = family[x],
              iter = iter,
              chains = chains,
              warmup = warmup,
              cores = cores)
}) # end brmsout loop
saveRDS(brmsout,"GDS_brmsout_results_data3_nnnn.RDS")

pushover(message = "And...the code is finished, phew!", user = PUSHOVER_USER, app = PUSHOVER_APP)

brmsout <- readRDS("GDS_brmsout_results_data3_nnnn.RDS")
m.aware.negbinl <- brmsout[[1]]
m.invbelieve.negbinl <- brmsout[[2]]
m.relevance.negbinl <- brmsout[[3]]
m.less.negbinl <- brmsout[[4]]

brmsout <- readRDS("GDS_brmsout_results_ppnn.RDS")
m.aware.poisson <- brmsout[[1]]
m.invbelieve.poisson <- brmsout[[2]]
m.relevance.negbin <- brmsout[[3]]
m.less.negbin <- brmsout[[4]]

brmsout <- readRDS("GDS_brmsout_results_cccc.RDS")
m.aware.cumulative <- brmsout[[1]]
m.invbelieve.cumulative <- brmsout[[2]]
m.relevance.cumulative <- brmsout[[3]]
m.less.cumulative <- brmsout[[4]]


# how long did it take!
tm1 <- print(get_elapsed_time(brmsout[[1]]$fit))
tm2 <- print(get_elapsed_time(brmsout[[2]]$fit))
tm3 <- print(get_elapsed_time(brmsout[[3]]$fit))
tm4 <- print(get_elapsed_time(brmsout[[4]]$fit))
tm <- tm1 + tm2 + tm3 + tm4
tm


# first compare (a) m.relevance.negbinl vs m.relevance.negbin and (b) m.less.negbinl vs m.less.negbin to check that log link is correctly specified 

# (a) 
print(m.relevance.negbinl)
print(m.relevance.negbin)
diff <- exp(as.data.frame(ranef(m.relevance.negbinl))[,1]) - 
exp(as.data.frame(ranef(m.relevance.negbin))[,1])
diff[diff > .01]


# (b)
print(m.less.negbinl)
print(m.less.negbin)
diff <- exp(as.data.frame(ranef(m.less.negbinl))[,1]) - 
exp(as.data.frame(ranef(m.less.negbin))[,1])
diff[diff > .01]

# then compare (a) m.aware.negbinl vs m.aware.poisson and (b) m.invbelieve.negbinl vs m.less.negbinl to check if poisson or negbin is better

# model summary and comparison

pp.aware.negbinl <- pp_check(m.aware.negbinl) 
pp.aware.negbinl + xlim(0, 20)
pp.aware.poisson <- pp_check(m.aware.poisson) 
pp.aware.poisson + xlim(0, 20)

loo.aware.negbinl <- loo(m.aware.negbinl, save_psis = TRUE, cores = 4)
loo.aware.poisson <- loo(m.aware.poisson, save_psis = TRUE, cores = 4)
print(loo.aware.negbinl)
print(loo.aware.poisson)
plot(loo.aware.negbinl, label_points = TRUE)
plot(loo.aware.poisson, label_points = TRUE)
yrep.aware.negbinl <- posterior_predict(m.aware.negbinl)
yrep.aware.poisson <- posterior_predict(m.aware.poisson)
plot_overlay.aware.negbinl <- ppc_loo_pit_overlay(data$awareness, yrep.aware.negbinl, lw = weights(loo.aware.negbinl$psis_object))
plot_overlay.aware.poisson <- ppc_loo_pit_overlay(data$awareness, yrep.aware.poisson, lw = weights(loo.aware.poisson$psis_object))
plot_overlay.aware.negbinl
plot_overlay.aware.poisson
print(loo_compare(loo.aware.negbinl, loo.aware.poisson), digits = 3)


# then compare (a) m.aware.negbinl vs m.aware.cumulative...etc 

# model summary and comparison

pp.aware.negbinl <- pp_check(m.aware.negbinl) 
pp.aware.negbinl + xlim(0, 20)
pp.aware.cumulative <- pp_check(m.aware.cumulative) 
pp.aware.cumulative + xlim(0, 20)

loo.aware.negbinl <- loo(m.aware.negbinl, save_psis = TRUE, cores = 4)
loo.aware.cumulative <- loo(m.aware.cumulative, save_psis = TRUE, cores = 4)
print(loo.aware.negbinl)
print(loo.aware.cumulative)
plot(loo.aware.negbinl, label_points = TRUE)
plot(loo.aware.cumulative, label_points = TRUE)
yrep.aware.negbinl <- posterior_predict(m.aware.negbinl)
yrep.aware.cumulative <- posterior_predict(m.aware.cumulative)
plot_overlay.aware.negbinl <- ppc_loo_pit_overlay(data$awareness, yrep.aware.negbinl, lw = weights(loo.aware.negbinl$psis_object))
plot_overlay.aware.cumulative <- ppc_loo_pit_overlay(data$awareness, yrep.aware.cumulative, lw = weights(loo.aware.cumulative$psis_object))
plot_overlay.aware.negbinl
plot_overlay.aware.cumulative
print(loo_compare(loo.aware.negbinl, loo.aware.cumulative), digits = 3)










# diagnostics 
# traceplots
traceout1 <- lapply(stanout, function(x) {
  rstan::traceplot(x, pars = c("beta_int","beta_age", "beta_AUDIT", "sigma_u", "phi"))
})
traceout2 <- lapply(stanout, function(x) {
  rstan::traceplot(x, pars = c("u"))
})
traceout1
traceout2

# ggplot results plots
whichplot <- c("awareness","don\'t believe","relevance","drink less")
# x <- 4
plotout <- lapply(seq_along(whichplot), function(x) {
  stanout_sims <- extract(stanout[[x]], permuted = TRUE)
  overall_intercept <- exp(median(stanout_sims[["beta_int"]])) # overall intercept on original scale
  country_median <- apply(stanout_sims[["u"]],2,median)
  ci90 <- posterior_interval(stanout_sims$u, prob = 0.90, 
                           regex_pars = "^[u]")
  ci50 <- posterior_interval(stanout_sims$u, prob = 0.50, 
                           regex_pars = "^[u]")
  res <- as.data.frame(cbind(countries,round(country_median,3),round(ci90, 3),round(ci50, 3)))
  res <- res[,c(1,2,3,5,6,4)]
  res[,c(2:6)] <- apply(res[,c(2:6)], 2, as.numeric) 
  res[,c(2:6)] <- exp(res[,c(2:6)])
  rownames(res) <- NULL
  colnames(res) <- c("country","median","ll","l","h","hh")
  titlelab <- paste0("Ratio of individual country intercept \nto overall intercept for \"",whichplot[x],"\"") 
  myannotation <- paste0("(overall intercept on the original \nvariable scale = ", round(overall_intercept,2),")")

p <- ggplot(res, aes(x = median, 
                       y = reorder(country,median))) + 
        geom_errorbarh(aes(xmax = h, 
                           xmin = l, 
                           size = 2, 
                           colour = 5, 
                           alpha = .8, 
                           height = .00)) +
        geom_errorbarh(aes(xmax = hh, 
                           xmin = ll, 
                           colour = 1, 
                           alpha = .5, 
                           height = .00)) + 
        geom_point(colour = "red",
                   size = 2,
                   shape = 21) +
        geom_vline(xintercept = 1, 
                   size = .5, 
                   alpha = .2) +
        scale_x_continuous(limits = c(0.5, 1.8)) + 
        bayesplot::bayesplot_theme_get() +
        theme(legend.position = "none") +
        theme(text = element_text(family = "Helvetica")) +
        labs(x = NULL, y = NULL) +
        annotate("text", 
                 x = 1.3, 
                 y = 8, 
                 size = 2.5, 
                 label = paste0(titlelab,"\n",myannotation)) +  
        theme(axis.title = element_text(size = 10)) +
        theme(axis.text.y = element_text(size = 6)) 
p
})

p.awareness <- plotout[[1]]
p.believe <- plotout[[2]]
p.relevance <- plotout[[3]]
p.less <- plotout[[4]]

# plots to plotly
# ggplotly(p.less)
subp <- plotly::subplot(p.awareness, p.believe, p.relevance, p.less, nrows = 2, margin = 0.03, heights = c(0.5, 0.5))

chart_link <- api_create(subp, filename = "GDS-alcinfo-rstan")
chart_link # opens up browser and displays plot



list_of_draws <- extract(stanout[[4]])
print(names(list_of_draws))

summary(stanout[[4]])
short_summary <- summary(stanout[[4]], pars = c("rr"), probs = c(0.1, 0.9))$summary
print(short_summary)





# trying bayesplot, but can't reorder ... added github issue request 26/7/19

# posterior <- as.array(stanout[[4]])
# sogg1 <- bayesplot::mcmc_intervals(posterior, pars = u_levels, transformations = "exp") + 
#   scale_y_discrete(labels = c(countries))
# ggplotly(sogg1)

