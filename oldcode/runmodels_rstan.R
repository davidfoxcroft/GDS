# stan options -----------------
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
chains <- 3
cores <- chains
iter <- 2000
warmup <- 1000
seed <- sqrt(1963)

data <- data2 # data1 for full dataset, data2 for sampled dataset for test analyses

# convert country to numeric vector for stan
country <- as.factor(data$ncountry)
levels(country) <- 1:length(levels(country))
country <- as.numeric(country)
table(country)
# change Macedonia label for plotting
# countries <- as.factor(data$ncountry)
# countries <- as.character(levels(countries))
# countries[countries == "Macedonia, The Former Yugoslav Republic of"] <- "FYR Macedonia"


# starting values - if used
# initf <- function() { list(beta_age = rnorm(1,0,1),
#                            beta_AUDIT = rnorm(1,0,1),
#                            sigma_u = rnorm(1,0.2,.05)
#                            )}

# run stan models
depvars <- c("awareness","invbelieve","relevance","less")
#x <- c("less")


stanout <- lapply(depvars, function(x) {
     mod <- stan(file = "negbin2.stan",
          data = list(N = dim(data)[1],
                      J = length(unique(country)),
                      depvar = as.numeric(unlist(data[c(x)])),
                      age = scale(as.numeric(data$age))[,1],
                      AUDIT_SCORE = scale(data$AUDIT_SCORE)[,1],
                      country = country),
              # init = initf,
              warmup = warmup,
              iter = iter,
              chains = chains,
              cores = chains,
              seed = seed) 
}) # end stanout loop

summary(mod)
print(mod)

post <- as.matrix(mod) #the sampled model values
str(post)

post <- as.matrix(mod) %>% 
  select(contains("y_rep")) 






postcheck <- pp_check(post) + xlim(0, 40)
loo3a <- loo(post, save_psis = TRUE, cores = 4)
plot(loo3a, label_points = TRUE)

saveRDS(mod,"GDS_stanout_results_less_brms.RDS")
print(mod)
modcheck <- pp_check(mod) + xlim(0, 40)
str(mod@sim)
launch_shinystan(mod)


pushover(message = "And...the code is finished, phew!", user = PUSHOVER_USER, app = PUSHOVER_APP)

tm1 <- print(get_elapsed_time(stanout[[1]]))
tm2 <- print(get_elapsed_time(stanout[[2]]))
tm3 <- print(get_elapsed_time(stanout[[3]]))
tm4 <- print(get_elapsed_time(stanout[[4]]))
tm <- tm1 + tm2 + tm3 + tm4
tm
mbm


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

