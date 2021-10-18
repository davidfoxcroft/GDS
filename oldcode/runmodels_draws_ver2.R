library(RColorBrewer)
library(forcats)

setwd("~/Dropbox/Coding/GDS") #use if not in Rstudio project
source("dataprep.R")

### get and organise data -------------------
data8 <- data7 %>%
  select(-awareness,-believe,-invbelieve,-relevance,-less) %>% 
  pivot_wider(names_from = impact, values_from = response) %>% 
  mutate(sex = as.factor(sex))

data9 <- data6 %>%
  select(-awareness,-believe,-invbelieve,-relevance,-less) %>% 
  pivot_wider(names_from = impact, values_from = response) %>% 
  mutate(sex = as.factor(sex))


### model ------------------

#mod <- readRDS("GDS_brms_multi_multi_data8.RDS")
mod <- readRDS("~/GDS_brms_multi_multi_data9.RDS")

newdat <- tidyr::expand_grid(Final_country = as.factor(levels(mod$data$Final_country)), message = as.factor(levels(mod$data$message)), sex = as.factor(levels(mod$data$sex)), age = c(seq(16,80,4)), AUDIT_SCORE = c(seq(1,40,2)))
dplyr::glimpse(newdat)

# use brms posterior predict to get predicted values from the posterior
preddat1 <- brms::posterior_predict(mod, newdata = newdat, re_form = "~(1 | message) + (1 | Final_country)", summary = F) # use NULL or specify a random effect formula for re_form as this calculates predicted values for  group-level effects (i.e. take the actual estimated intercepts for each specific group and uses them to make new predictions for the same groups)

preddat1.new <- preddat1[,,1]
preddat1.believe <- preddat1[,,2]
preddat1.relevant <- preddat1[,,3]
preddat1.drinkless <- preddat1[,,4]

preddat2a <- apply(preddat1.new, 2, mean) # calculate p(Yes) 
preddat2b <- apply(preddat1.believe, 2, mean) # calculate p(Yes) 
preddat2c <- apply(preddat1.relevant, 2, mean) # calculate p(Yes) 
preddat2d <- apply(preddat1.drinkless, 2, mean) # calculate p(Yes) 

glimpse(preddat1.new)
glimpse(preddat2a)


newdat.merged <- cbind(newdat,preddat2a,preddat2b,preddat2c,preddat2d)
newdat.merged

newdat.merged <- readRDS("newdat_merged.RDS") # saved from AWS as won't run on my mac with full data model (data9)
dplyr::glimpse(newdat.merged)

colnames(newdat.merged) <- c("country", "message", 
                             "sex", "age",
                             "AUDIT",
                             "new", "believe",
                             "relevant", "drinkless")

dplyr::glimpse(newdat.merged)


# aggregate by sex, age (16-24, 25+) and AUDIT (0-15, 16+) 

nd1 <- newdat.merged %>% 
  ungroup() %>% 
  filter(AUDIT < 16) %>% 
  group_by(sex,message,country) %>% 
  summarize(Mean_new = mean(new),
            Mean_believe = mean(believe),
            Mean_relevant = mean(relevant),
            Mean_drinkless = mean(drinkless)) %>% 
  mutate(AUDITgrp = c("AUDIT 1-15"))
  
nd2 <- newdat.merged %>% 
  ungroup() %>% 
  filter(AUDIT > 15) %>% 
  group_by(sex,message,country) %>% 
  summarize(Mean_new = mean(new),
            Mean_believe = mean(believe),
            Mean_relevant = mean(relevant),
            Mean_drinkless = mean(drinkless)) %>% 
  mutate(AUDITgrp = c("AUDIT 16+"))


nd.merged <- rbind(nd1,nd2)

glimpse(nd.merged)

ndm <- nd.merged %>%
  ungroup() %>% 
  mutate(AUDITgrp = as.factor(AUDITgrp),
         sex = fct_recode(sex, "Males" = "1", "Females" = "0")) %>%
  mutate(country = fct_reorder(country, Mean_drinkless)) %>% 
  pivot_longer(cols = 4:7, names_to = "impact", values_to = "PredProb") %>% 
  mutate(impact = as.factor(impact),
         impact = fct_recode(impact, "New" = "Mean_new",
                          "Believe" = "Mean_believe",
                          "Relevant" = "Mean_relevant",
                          "Drink less" = "Mean_drinkless")) %>% 
  filter(impact == "New" |
           impact == "Drink less") %>% 
  unite("seximpact", sex, impact, sep = "_") %>% 
  mutate(seximpact = as.factor(seximpact),
         seximpact = fct_recode(seximpact, "Females: New" = "Females_New",
                             "Females: Drink less" = "Females_Drink less",
                             "Males: New" = "Males_New",
                             "Males: Drink less" = "Males_Drink less"),
         seximpact = fct_relevel(seximpact, 
                              "Females: New",
                              "Males: New",
                              "Females: Drink less",
                              "Males: Drink less"))


colourCount = length(unique(ndm$country))
getPalette = colorRampPalette(brewer.pal(29, "Dark2"))

plotndm <- ggplot(data = ndm, 
                aes(x = country,
                    y = PredProb,
                    text = paste(country,
                                 ",",
                                 AUDITgrp,
                                 ":",
                                 "<br>Pr(Yes) = ",
                                  round(PredProb,2)))) +
  facet_grid(message ~ seximpact) +
  geom_point(data = ndm, 
                aes(x = country,
                    y = PredProb,
                    colour = country,
                    shape = AUDITgrp),
             size = 1.5) +
  scale_colour_manual(values = getPalette(colourCount)) +
  scale_shape_manual(values = c(3, 17)) +
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
        panel.grid.major.y = element_line(colour = "grey90"),
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
                               title = 'Country (ordered low to high\n predicted probability for\n"Drink less" outcome)', 
                               keyheight = 0.9,
                               ncol = 1,
                               order = 2),
         shape = guide_legend(override.aes = list(size = 3),
                              title = "AUDIT score",
                              keyheight = 0.9,
                              reverse = TRUE,
                              order = 1))

dev.off()
plotndm  


library('plotly')
library('htmlwidgets')

m <- list(
  l = 80,
  r = 250, # 250 to add png legend (if can figure out how to do this)
  b = 50,
  t = 50,
  pad = 4
)
y <- list(
  title = "Predicted Probability"
)


# plots to plotly
subp <- plotly::ggplotly(plotndm, tooltip = c("text")) %>%
  plotly::hide_legend()  %>% 
  plotly::layout(margin = m)

str(subp[['x']][['layout']][['annotations']][[1]][['x']])  # check position of y-axis label and then change it:
subp[['x']][['layout']][['annotations']][[1]][['x']] <- -0.07

subp <- subp %>% 
  layout(
    images = list(
      list(
        source =  "https://raw.githubusercontent.com/davidfoxcroft/publicimages/master/plotly_legend.png",
        xref = "paper",
        yref = "paper",
        x = 1.05,
        y = 1.01,
        sizex = 1,
        sizey = 1
      )
    )
  )

htmlwidgets::saveWidget(subp, "plot.html")

# send to plotly web - needs keys (and paid account if file size > 500kb)
chart_link <- plotly::api_create(subp, filename = "GDS-alcinfo-brms_plotnewdat_drinkless")
chart_link # opens up browser and displays plot




