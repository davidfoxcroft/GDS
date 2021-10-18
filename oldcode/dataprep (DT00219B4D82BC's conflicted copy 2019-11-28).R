# setwd("~/Dropbox/Coding/GDS") use if not in Rstudio
library(tidyverse)
library(lme4)
library(rstan)
library(brms)
library(parallel)
library(lattice)
library(emmeans)
library(tidybayes)
library(plotly)source("~/plotlydetails.R") # load credentials for plotly login
library(bayesplot)
library(patchwork)
library(pushoverr)
source("~/pushoverdetails.R")

# data load and checking ------------------
data <- read_csv("GDS 2018 Alcohol labels for David.csv")
glimpse(data)

which(!complete.cases(data))
which(is.na(data$id))

# replace missing ids - 
data$id_orig <- data$id
data$id <- c(1:dim(data[,1])[1])
head(data)
tail(data)
which(is.na(data$id))


table(data$ncountry)
table(data$alcheartnew)
table(data$alclivernew)
table(data$alccancernew)
table(data$alcfatnew)
table(data$alcfreedaysnew)
table(data$alcmythnew)
table(data$alcviolencenew)
table(data$age, exclude = NULL)
hist(scale(log(data$age)))
table(data$sex, exclude = NULL)
table(data$AUDIT_SCORE, exclude = NULL)
hist(scale(data$AUDIT_SCORE^(1/3)))
table(data$pdthighqual, exclude = NULL)
table(data$pdtincome, exclude = NULL)
table(data$year_illegal, exclude = NULL)
table(data$freq_tobacco, exclude = NULL)

# compute new variables ----------------------
data$awareness <- data$alcheartnew + 
  data$alclivernew + 
  data$alccancernew +
  data$alcfatnew +
  data$alcfreedaysnew + 
  data$alcmythnew + 
  data$alcviolencenew
table(data$awareness)
data %>% summarize(mean = mean(awareness), var = var(awareness)) # check for equal mean and variance to see if poisson is appropriate, otherwise neagtive binomial


table(data$alcheartbelieve)
table(data$alcliverbelieve)
table(data$alccancerbelieve)
table(data$alcfatbelieve)
table(data$alcfreedaysbelieve)
table(data$alcmythbelieve)
table(data$alcviolencebelieve)

data$alcheartbelieve <- car::recode(data$alcheartbelieve, "0=0; 1=2; 2=1")
data$alcliverbelieve <- car::recode(data$alcliverbelieve, "0=0; 1=2; 2=1")
data$alccancerbelieve <- car::recode(data$alccancerbelieve, "0=0; 1=2; 2=1")
data$alcfatbelieve <- car::recode(data$alcfatbelieve, "0=0; 1=2; 2=1")
data$alcfreedaysbelieve <- car::recode(data$alcfreedaysbelieve, "0=0; 1=2; 2=1")
data$alcmythbelieve <- car::recode(data$alcmythbelieve, "0=0; 1=2; 2=1")
data$alcviolencebelieve <- car::recode(data$alcviolencebelieve, "0=0; 1=2; 2=1")

table(data$alcheartbelieve)
table(data$alcliverbelieve)
table(data$alccancerbelieve)
table(data$alcfatbelieve)
table(data$alcfreedaysbelieve)
table(data$alcmythbelieve)
table(data$alcviolencebelieve)

data$believe <- data$alcheartbelieve +
  data$alcliverbelieve + 
  data$alccancerbelieve +
  data$alcfatbelieve +
  data$alcfreedaysbelieve +
  data$alcmythbelieve +
  data$alcviolencebelieve
table(data$believe)
hist(data$believe)
data$invbelieve <- 14 - data$believe # invert var for analysis
table(data$invbelieve)
hist(data$invbelieve)
data %>% summarize(mean = mean(believe), var = var(believe))
data %>% summarize(mean = mean(invbelieve), var = var(invbelieve))


table(data$alcheartrelevance)
table(data$alcliverrelevance)
table(data$alccancerrelevance)
table(data$alcfatrelevance)
table(data$alcfreedaysrelevance)
table(data$alcmythrelevance)
table(data$alcviolencerelevance)

data$relevance <- data$alcheartrelevance +
  data$alcliverrelevance + 
  data$alccancerrelevance +
  data$alcfatrelevance +
  data$alcfreedaysrelevance +
  data$alcmythrelevance +
  data$alcviolencerelevance - 7
table(data$relevance)
hist(data$relevance)
data %>% summarize(mean = mean(relevance), var = var(relevance))


table(data$alcheartless) # check why none coded as '3'
table(data$alcliverless)
table(data$alccancerless)
table(data$alcfatless)
table(data$alcfreedaysless)
table(data$alcmythless)
table(data$alcviolenceless)

data$alcheartless <- car::recode(data$alcheartless, "0=0; 3=1; 2=2; 1=3")
data$alcliverless <- car::recode(data$alcliverless, "0=0; 3=1; 2=2; 1=3")
data$alccancerless <- car::recode(data$alccancerless, "0=0; 3=1; 2=2; 1=3")
data$alcfatless <- car::recode(data$alcfatless, "0=0; 3=1; 2=2; 1=3")
data$alcfreedaysless <- car::recode(data$alcfreedaysless, "0=0; 3=1; 2=2; 1=3")
data$alcmythless <- car::recode(data$alcmythless, "0=0; 3=1; 2=2; 1=3")
data$alcviolenceless <- car::recode(data$alcviolenceless, "0=0; 3=1; 2=2; 1=3")

table(data$alcheartless)
table(data$alcliverless)
table(data$alccancerless)
table(data$alcfatless)
table(data$alcfreedaysless)
table(data$alcmythless)
table(data$alcviolenceless)

data$less <- data$alcheartless +
  data$alcliverless + 
  data$alccancerless +
  data$alcfatless +
  data$alcfreedaysless +
  data$alcmythless +
  data$alcviolenceless
table(data$less)
hist(data$less)
data %>% summarize(mean = mean(less), var = var(less))




# sample data by country for initial analyses -------------
data1 <- data # full data set
data2 <- data %>%  # n=300 for each country with N >= 300, for model testing
  semi_join(count(., ncountry) %>% 
  filter(n >= 100), by = "ncountry") %>% 
  group_by(ncountry) %>% 
  sample_n(100, replace = FALSE) %>%
  ungroup()
data3 <- data %>% # full data for countries with n >= 100 (some countries have very small n, e.g. Albania with n = 14...)
  semi_join(count(., ncountry) %>% 
  filter(n >= 100), by = "ncountry") %>%
  ungroup()
glimpse(data3)



# reshape data wide to long for three level (message or impact; id; country) crossed / nested mixed model: message and impact are crossed with id; id is nested in country.

data4 <- data %>%  # data for new only
  select(c("id","age","ncountry","alcheartnew",
           "alclivernew","alccancernew","alcfatnew",
           "alcfreedaysnew","alcmythnew","alcviolencenew",
           "AUDIT_SCORE")) %>%
  mutate(., id, id = factor(id)) %>%
  mutate(., ncountry, ncountry = factor(ncountry)) %>%
  gather(., message, response, alcheartnew:alcviolencenew, factor_key = TRUE) %>%
  arrange(desc(id))
glimpse(data4)

data5 <- data2 %>% # data for "new" only, based on model testing data set
  select(c("id","age","ncountry","alcheartnew",
           "alclivernew","alccancernew","alcfatnew",
           "alcfreedaysnew","alcmythnew","alcviolencenew",
           "AUDIT_SCORE")) %>%
  mutate(., id, id = factor(id)) %>%
  mutate(., ncountry, ncountry = factor(ncountry)) %>%
  gather(., message, response, alcheartnew:alcviolencenew, factor_key = TRUE) %>%
  arrange(desc(id))
glimpse(data5)

# could repeat above for other impact vars, but nb response is on different scale for each one so separate (ordinal) logistic regressions would be appropriate. Alternatively, use the dummy recoding of impact vars to create one response var across all impact vars:

glimpse(data)

data6 <- data %>% # data for all impact variables (new, believe, relevance, less) but binary (0,1) dummy re-coded versions
  select(c("id","age","AUDIT_SCORE", "ncountry","alcheartnew",
           "alclivernew","alccancernew","alcfatnew",
           "alcfreedaysnew","alcmythnew","alcviolencenew",
           "heartbelieve_yes", "liverbelieve_yes",
           "cancerbelieve_yes", "caloriesbelieve_yes", 
           "alcfreedaysbelieve_yes","mythbelieve_yes",
           "violencebelieve_yes","heartrevelance_new", 
           "liverrelevance_new","cancerrelevance_new",
           "calorierelevance_new", "daysoffrelevance_new",
           "mythrelevance_new","violence_relevance_new",
           "heartless_yesmaybe", "liverless_yesmaybe",
           "cancerless_yesmaybe","caloriesless_yesmaybe",
           "Twodaysoffless_yesmaybe","Mythless_yes_maybe",
           "violenceless_yesmaybe")) %>%
  mutate(., id, id = factor(id)) %>% # ? still need this with pivot_longer
  pivot_longer(.,
               cols = c(alcheartnew:violenceless_yesmaybe),
               names_to = "message_impact",
               values_to = "response"
               ) %>%
  mutate(., impact = ifelse(grepl("_new", message_impact), 
                            "relevant",
                            ifelse(grepl("believe", message_impact), 
                                   "believe", 
                                   ifelse(grepl("new", message_impact), 
                                          "new",
                                          "drinkless")))) %>%
  mutate(., message = ifelse(grepl("heart", message_impact), 
                            "heart",
                            ifelse(grepl("liver", message_impact), 
                                   "liver", 
                                   ifelse(grepl("cancer", message_impact), 
                                          "cancer",
                                          ifelse(grepl("fat", message_impact), 
                                                 "calories",
                                                 ifelse(grepl("calorie", message_impact), 
                                                        "calories",
                                                        ifelse(grepl("days", message_impact), 
                                                               "freedays",
                                                               ifelse(grepl("myth", message_impact), 
                                                                      "myth",
                                                                      ifelse(grepl("Myth", message_impact), 
                                                                             "myth",
                                                                             "violence"))))))))) %>%
  select(.,-c(message_impact)) %>%
  mutate(., ncountry, ncountry = factor(ncountry)) %>%
  mutate(., message, message = factor(message)) %>%
  mutate(., impact, impact = factor(impact, levels = c("new", "believe", "relevant", "drinkless")))


data7 <- data2 %>% # data for all impact variables (new, believe, relevance, less) but binary (0,1) dummy re-coded versions
  select(c("id","age","AUDIT_SCORE", "ncountry","alcheartnew",
           "alclivernew","alccancernew","alcfatnew",
           "alcfreedaysnew","alcmythnew","alcviolencenew",
           "heartbelieve_yes", "liverbelieve_yes",
           "cancerbelieve_yes", "caloriesbelieve_yes", 
           "alcfreedaysbelieve_yes","mythbelieve_yes",
           "violencebelieve_yes","heartrevelance_new", 
           "liverrelevance_new","cancerrelevance_new",
           "calorierelevance_new", "daysoffrelevance_new",
           "mythrelevance_new","violence_relevance_new",
           "heartless_yesmaybe", "liverless_yesmaybe",
           "cancerless_yesmaybe","caloriesless_yesmaybe",
           "Twodaysoffless_yesmaybe","Mythless_yes_maybe",
           "violenceless_yesmaybe")) %>%
  mutate(., id, id = factor(id)) %>% # ? still need this with pivot_longer
  pivot_longer(.,
               cols = c(alcheartnew:violenceless_yesmaybe),
               names_to = "message_impact",
               values_to = "response"
  ) %>%
  mutate(., impact = ifelse(grepl("_new", message_impact), 
                            "relevant",
                            ifelse(grepl("believe", message_impact), 
                                   "believe", 
                                   ifelse(grepl("new", message_impact), 
                                          "new",
                                          "drinkless")))) %>%
  mutate(., message = ifelse(grepl("heart", message_impact), 
                             "heart",
                             ifelse(grepl("liver", message_impact), 
                                    "liver", 
                                    ifelse(grepl("cancer", message_impact), 
                                           "cancer",
                                           ifelse(grepl("fat", message_impact), 
                                                  "calories",
                                                  ifelse(grepl("calorie", message_impact), 
                                                         "calories",
                                                         ifelse(grepl("days", message_impact), 
                                                                "freedays",
                                                                ifelse(grepl("myth", message_impact), 
                                                                       "myth",
                                                                       ifelse(grepl("Myth", message_impact), 
                                                                              "myth",
                                                                              "violence"))))))))) %>%
  select(.,-c(message_impact)) %>%
  mutate(., ncountry, ncountry = factor(ncountry)) %>%
  mutate(., message, message = factor(message)) %>%
  mutate(., impact, impact = factor(impact, levels = c("new", "believe", "relevant", "drinkless")))

glimpse(data7)
which(!complete.cases(data7))





