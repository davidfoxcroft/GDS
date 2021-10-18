#### load required packages ------------

library(RColorBrewer)
library(forcats)
library(plotly)
library(htmlwidgets)
library(tidyverse)
library(here)
library(gt)
library(webshot)
library(rstan)


#### get data -------------
source(here("GDS", "dataprep.R"))

restab1 <- data9a %>% 
  group_by(Final_country,message) %>% 
  select(Final_country,message,new,drinkless) %>% 
  summarise(mean_new = mean(new),
            mean_drinkless = mean(drinkless))
restab1

#### read model ------------------

#mod <- readRDS(here("GDS", "GDS_brms_multi_multi_data8.RDS"))
mod <- readRDS(here("GDS", "brms_mod_interaction2_data9a.rds"))

#### model fit (bayes R-squared) -------------
bayesR2tab <- brms::bayes_R2(mod) # crashes R session - ? more memory needed. (update - works but slow if set up 256GB swap file on psyworkstation)
#bayesR2tab <- readRDS(here("~/GDS_cmdstan_brms_data9_bayesR2tab_interact_mod.RDS"))

#### table of fit, fixed and random effects ---------
sum_mod <- summary(mod)

fe1 <- sum_mod$fixed[c(1,5:7,2,36:38,3,67:69,4,98:100),] %>%
  as_tibble(rownames = "Parameter") %>% 
  select(-Est.Error,
         -Bulk_ESS) %>% 
  separate(Parameter, c("outcome","parameter") , "_") %>% 
  pivot_wider(names_from = outcome,
              values_from = c(3:7)) %>% 
  select(c(1,2,6,10,14,18,
           3,7,11,15,19,
           4,8,12,16,20,
           5,9,13,17,21)) 

re1a <- as_tibble(sum_mod$random$message[c(1,30,59,88),], rownames = "Parameter") %>% 
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

re1b <- as_tibble(sum_mod$random$Final_country[1:4,], rownames = "Parameter") %>% 
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

re1c <- as_tibble(sum_mod$random$`Final_country:id`[1:4,], rownames = "Parameter") %>% 
  mutate(random_effect = "country/id") %>% 
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
retab <- rbind(re1a,re1b,re1c) 
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

restab <- rbind(fe1, retab) 
as_tibble(restab, 
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
                  parameter == "country/id")  %>% 
  tab_row_group(group = "Population Effects",
                rows = parameter == "Intercept" | 
                  parameter == "age" |
                  parameter == "AUDIT" |
                  parameter == "sex1") %>% 
  tab_row_group(group = "Model Fit",
                rows = parameter == "bayes_R2") 


#### new data for predicted probabilities -------------
newdat <- tidyr::expand_grid(Final_country = as.factor(levels(mod$data$Final_country)), message = as.factor(levels(mod$data$message)), sex = as.factor(levels(mod$data$sex)), age = c(seq(16,80,4)), AUDIT_SCORE = c(seq(1,40,2)))
dplyr::glimpse(newdat)

#### use brms posterior predict to get predicted values from the posterior --------------
preddat1 <- brms::posterior_predict(mod, newdata = newdat, re_form = "~(1 + Final_country | message)", summary = F) # use NULL or specify a random effect formula for re_form as this calculates predicted values for  group-level effects (i.e. take the actual estimated intercepts for each specific group and uses them to make new predictions for the same groups)

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


#### data munging for plotting and tables --------------
newdat.merged <- cbind(newdat,preddat2a,preddat2b,preddat2c,preddat2d)
dplyr::glimpse(newdat.merged)

colnames(newdat.merged) <- c("country", "message", 
                             "sex", "age",
                             "AUDIT",
                             "new", "believe",
                             "relevant", "drinkless")
dplyr::glimpse(newdat.merged)

restab2 <- newdat.merged %>% 
  group_by(country,message) %>% 
  select(country,message,new,drinkless) %>% 
  summarise(mean_new = mean(new),
            mean_drinkless = mean(drinkless))
restab2
glimpse(cbind(restab1,restab2))

restab <- cbind(restab1,restab2) %>% 
  select("Country" = "Final_country",
         "message" = "message...2",
         "new, observed data" = "mean_new...3",
         "new, predicted probability" = "mean_new...7",
         "drinkless, observed data" = "mean_drinkless...4",
         "drinkless, predicted probability" = "mean_drinkless...8") %>% 
  pivot_wider(names_from = message,
              values_from = c("new, observed data",
                              "new, predicted probability",
                              "drinkless, observed data",
                              "drinkless, predicted probability")) %>% 
  ungroup()

#### appx table for 'new' --------------
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


#### appx table for 'drinkless' ---------------
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


#### aggregate by sex, age (16-24, 25+) and AUDIT (0-15, 16+) --------------- 

nd1 <- newdat.merged %>% 
  ungroup() %>% 
  filter(age < 25,
         AUDIT < 16) %>% 
  group_by(sex,message,country) %>% 
  summarize(Mean_new = mean(new),
            Mean_believe = mean(believe),
            Mean_relevant = mean(relevant),
            Mean_drinkless = mean(drinkless)) %>% 
  mutate(agegrp = c("16-24"),
         AUDITgrp = c("AUDIT 1-15"))

nd2 <- newdat.merged %>% 
  ungroup() %>% 
  filter(age < 25,
         AUDIT > 15) %>% 
  group_by(sex,message,country) %>% 
  summarize(Mean_new = mean(new),
            Mean_believe = mean(believe),
            Mean_relevant = mean(relevant),
            Mean_drinkless = mean(drinkless)) %>% 
  mutate(agegrp = c("16-24"),
         AUDITgrp = c("AUDIT 16+"))

nd3 <- newdat.merged %>% 
  ungroup() %>% 
  filter(age > 24,
         AUDIT < 16) %>% 
  group_by(sex,message,country) %>% 
  summarize(Mean_new = mean(new),
            Mean_believe = mean(believe),
            Mean_relevant = mean(relevant),
            Mean_drinkless = mean(drinkless)) %>% 
  mutate(agegrp = c("25+"),
         AUDITgrp = c("AUDIT 1-15"))

nd4 <- newdat.merged %>% 
  ungroup() %>% 
  filter(age > 24,
         AUDIT > 15) %>% 
  group_by(sex,message,country) %>% 
  summarize(Mean_new = mean(new),
            Mean_believe = mean(believe),
            Mean_relevant = mean(relevant),
            Mean_drinkless = mean(drinkless)) %>% 
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

#### do some fancy pants plotting ----------
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
plotndm1

plotndm2 <- ggplot(data = ndm, 
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

#### plots to plotly - change plotndm1 or plotndm2 as required ---------------

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



subp <- plotly::ggplotly(plotndm2, tooltip = c("text")) %>%
  plotly::hide_legend()  %>% 
  plotly::layout(margin = m)

str(subp[['x']][['layout']][['annotations']][[1]][['x']])  # check position of y-axis label and then change it:
subp[['x']][['layout']][['annotations']][[1]][['x']] <- -0.07

# change source to .../plotly_legend_new.png or .../plotly_legend_drinkless.png as required
subp <- subp %>% 
  layout(
    images = list(
      list(
        source =  "https://raw.githubusercontent.com/davidfoxcroft/publicimages/master/plotly_legend_drinkless.png",
        xref = "paper",
        yref = "paper",
        x = 1.05,
        y = 1.01,
        sizex = 1,
        sizey = 1
      )
    )
  )
#subp
htmlwidgets::saveWidget(subp, here("plot_drinkless.html"))

# send to plotly web - needs keys (and paid account if file size > 500kb)
chart_link <- plotly::api_create(subp, filename = "GDS-alcinfo-brms_plotnewdat_drinkless")
chart_link # opens up browser and displays plot




