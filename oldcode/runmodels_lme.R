modlme <- lme4::glmer(response ~ 
              1 + 
              age.m +  
              as.m + 
              sex +
              (1 | message) + 
              (1 | impact) + 
              (1 | Final_country/id),
              data = datarun,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa")) #


summary(modlme)
ranef(modlme)

nsims <- 10000


PI.time <- system.time(
  PI <- merTools::predictInterval(merMod = modlme, newdata = as.data.frame(datarun),
                        level = 0.95, 
                        n.sims = nsims,
                        stat = "median", 
                        type = "probability",
                        include.resid.var = TRUE,
                        returnSims = TRUE)
)

str(PI)
PIx <- attr(PI, "sim.results")
datarun2 <- cbind(datarun,as_tibble(PIx))
str(datarun2)
# function to convert predicted value to probability 
pv_to_p <- function(x) {exp(x) / (1 + exp(x))}

# Calculate the number of cores
no_cores <- detectCores() - 1
# Initiate cluster
cl <- makeCluster(no_cores, type = "FORK")
PI_out <- parLapply(cl, 1:nrow(PIx), function(i) {
  ll <- quantile(as.numeric(as.vector(PIx[i,])), 0.16)
  l <- quantile(as.numeric(as.vector(PIx[i,])), 0.333)
  med <- quantile(as.numeric(as.vector(PIx[i,])), 0.50)
  h <- quantile(as.numeric(as.vector(PIx[i,])), 0.666)
  hh <- quantile(as.numeric(as.vector(PIx[i,])), 0.84)
  c(ll,l,med,h,hh)
} )
stopCluster(cl)
PI_out1 <- do.call(rbind.data.frame, PI_out)
colnames(PI_out1) <- c("ll","l","med","h","hh")
PI <- cbind(datarun,PI_out1)

newPI <- PI %>%  
  group_by(message,impact,Final_country) %>% 
  summarise(med = pv_to_p(mean(med)), 
            ll = pv_to_p(mean(ll)), 
            l = pv_to_p(mean(l)),
            h = pv_to_p(mean(h)),
            hh = pv_to_p(mean(hh)))
newPI




str(modlme)


PI2 <- PI %>% 
  group_by(message,impact,Final_country) %>% 
  summarise(ll = quantile(fit, 0.16),
            l = quantile(fit, 0.33),
            med = quantile(fit, 0.5),
            h = quantile(fit, 0.66),
            hh = quantile(fit, 0.84))
PI2



p2 <- ggplot(data = newPI, 
             aes(text = paste(Final_country,
                              ":",
                              "<br>Pr(Yes) = ",
                              round(med,2),
                              "<br>(84% CI=",
                              round(ll,2),
                              "-",
                              round(hh,2),
                              ")"))) +
  geom_pointrange(aes(x = Final_country, 
                      y = med, 
                      color = Final_country,
                      ymin = ll,
                      ymax = hh),
                  size = 0.5, 
                  alpha = .8, 
                  position = position_jitter(width = .2,
                                             height = 0)) +
  theme_light() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(NA)) +
  ylab("") 
#p2


p3 <- p2 + facet_grid(impact ~ message) +
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

