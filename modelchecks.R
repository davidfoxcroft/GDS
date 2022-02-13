# nebin models ------------------
datarun3 <- data3 %>% 
  add_count(ncountry) %>% 
  filter(n >= 300) %>% 
  group_by(ncountry) %>% 
  sample_n(100, replace = FALSE) %>%
  ungroup() %>%
  filter(ncountry == c("Brazil") |  
         ncountry == c("Colombia") |
         ncountry == c("Finland") |
         ncountry == c("United Kingdom")) %>%
  mutate(age, age.m = scale(age, scale = FALSE)[,1],
         AUDIT_SCORE, as.m = scale(AUDIT_SCORE, scale = FALSE)[,1],
         awareness = as.integer(awareness),
         invbelieve = as.integer(invbelieve),
         relevance = as.integer(relevance),
         less = as.integer(less),
         id = as.factor(id),
         sex = as.factor(sex),
         ncountry = as.factor(ncountry)) %>% 
  select(age.m, as.m, awareness, invbelieve, relevance, less, id, ncountry, sex)

datarun$ncountry <- droplevels(datarun$ncountry)
glimpse(datarun3)

describe(datarun3$age.m)
describe(datarun3$as.m)




# run models through brms

form1 <- bf(awareness ~ 1 + sex)

fit1 <- brms::brm(formula = form1,
                  data = datarun3, # data in wide format
                  family = negbinomial,
                  sample_prior = TRUE,
                  inits = 0,
                  control = list(adapt_delta = 0.995,
                                 max_treedepth = 12),
                  iter = iter,
                  chains = chains,
                  warmup = warmup,
                  cores = cores,
                  seed = seed )


form2 <- bf(awareness ~ 1 + sex + (1 | ncountry))
fit2 <- update(fit1, form2, newdata = datarun3)

form3 <- bf(awareness ~ 1 + as.m + sex + (1 | ncountry))
fit3 <- update(fit1, form3, newdata = datarun3)

form4 <- bf(awareness ~ 1 + as.m + sex + (as.m | ncountry))
fit4 <- update(fit1, form4, newdata = datarun3)




prior_summary(fit1)

summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
ranef(fit3)
ranef(fit4)

marginal_effects(fit1)
marginal_effects(fit2)
marginal_effects(fit3)
marginal_effects(fit4)


glimpse(me1)


# logreg models ------------------------- 

datarun4 <- data %>% # all data - data for all impact variables (new, believe, relevance, less) but binary (0,1) dummy re-coded versions
  add_count(ncountry) %>% 
  filter(n >= 300) %>% 
  group_by(ncountry) %>% 
  sample_n(100, replace = FALSE) %>%
  ungroup() %>%
  filter(ncountry == c("Brazil") |  
           ncountry == c("Colombia") |
           ncountry == c("Finland") |
           ncountry == c("United Kingdom")) %>%
  mutate(age, age.m = scale(age, scale = FALSE)[,1],
         AUDIT_SCORE, as.m = scale(AUDIT_SCORE, scale = FALSE)[,1],
         awareness = as.integer(awareness),
         invbelieve = as.integer(invbelieve),
         relevance = as.integer(relevance),
         less = as.integer(less),
         id = as.factor(id),
         sex = as.factor(sex),
         ncountry = as.factor(ncountry))  %>%
  select(c("id","sex","age.m","as.m", "ncountry",
           "awareness", "believe", "invbelieve",
           "relevance", "less",
           "alcheartnew","alclivernew","alccancernew",
           "alcfatnew", "alcfreedaysnew","alcmythnew",
           "alcviolencenew", "heartbelieve_yes", 
           "liverbelieve_yes", "cancerbelieve_yes", 
           "caloriesbelieve_yes", "alcfreedaysbelieve_yes",
           "mythbelieve_yes", "violencebelieve_yes",
           "heartrevelance_new", # spot the typo!
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
  mutate(., message = ifelse(grepl("heart", message_impact), # messy naming in original dataset so tidied up using alternative ifelse statements
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



glimpse(datarun4)


# run models through brms

form1 <- bf(response ~ 1 + sex)

fit1 <- brms::brm(formula = form1,
                  data = datarun4, # data in wide format
                  family = bernoulli,
                  sample_prior = TRUE,
                  inits = 0,
                  control = list(adapt_delta = 0.995,
                                 max_treedepth = 12),
                  iter = iter,
                  chains = chains,
                  warmup = warmup,
                  cores = cores,
                  seed = seed )


form2 <- bf(response ~ 1 + sex + (1 | ncountry))
fit2 <- update(fit1, form2, newdata = datarun4)

form3 <- bf(response ~ 1 + AUDIT_SCORE + sex + (1 | ncountry))
fit3 <- update(fit1, form3, newdata = datarun4)

form4 <- bf(response ~ 1 + AUDIT_SCORE + sex + message + (1 | ncountry))
fit4 <- update(fit1, form4, newdata = datarun4)

form5 <- bf(response ~ 1 + AUDIT_SCORE + sex + message*impact + (1 | ncountry))
fit5 <- update(fit1, form5, newdata = datarun4)

form6 <- bf(response ~ 1 + age.m + as.m + sex  + (1 | ncountry / id) + (1 | message) + (1 | impact))
fit6 <- update(fit6, form6, newdata = datarun4)


prior_summary(fit6)

summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)
summary(fit6)
ranef(fit3)
ranef(fit4)
ranef(fit5)
plot(ranef(fit6)$ncountry)

saveRDS(fit6,"fit6.RDS")
fit6 <- readRDS("fit6.RDS")


marginal_effects(fit1)
marginal_effects(fit2)
marginal_effects(fit3)
marginal_effects(fit4)
marginal_effects(fit5)
conds <- data.frame(impact = c("new", "believe", "relevance", "less"))
p2 <- plot(marginal_effects(fit6, re_formula = NULL, method = "predict",   points = T, rug = T), plot = F)

p1
p2



glimpse(me1)





