

    # Chatfield Farms Models

    # ER 390 University of Victoria Continuing Studies

    # Molly Munro

setwd("~/Downloads/R")

library(magrittr)
library(dplyr)
library(tidyverse)
library(mgcv)
library(nlme)
library(DHARMa)

install.packages("mgcViz")
install.packages("ggplot2")

# read in data --------------------------------------------------------------------

r_prob_seeded <- read.csv('data/Chatfield/r_prob_seeded.csv')

response_prob <- read.csv('data/Chatfield/response_prob.csv')

survey_clean <- read.csv('data/Chatfield/survey_clean.csv')


### joining data

# distinguish probability column for seeded_prob vs overall prob
r_prob_seeded <- r_prob_seeded %>% 
  
  rename(seeded_prob = probability)

# adding column 
response_prob$seeded_prob <- r_prob_seeded$seeded_prob

## compiling final dataframes for models

complete <- survey_clean  %>% 
  
  left_join(response_prob,
            by = c("row",
                   "point", 
                   "seeded", 
                   "presence", 
                   "spp", 
                   "niche_breadth",
                   "class"))  %>% 
  
  select(-year.x,
         -year.y) %>% 

  mutate(spp = as.character(spp)) %>% 
  
## CONDENSE TO ROW LEVEL OBSERVATIONS
  # had to reduce the number of zeroes to be able to run the models
  
  mutate(presence = coalesce(presence, 0L)) %>%
  
  group_by(row, spp) %>%
  # if any point = 1, row presence = 1
  mutate(presence = max(presence)) %>%  
  
  distinct(row, spp, .keep_all = TRUE) %>%
  
  ungroup() %>% 
  
  select(-point) %>% 
  
  mutate(
         row = as.numeric(row),
         spp = factor(spp),
         presence = as.integer(presence))
  

  
# these spp are too powerful, disrupt the trend
  
noNAVI <- complete %>% 
  
  filter(spp != "NAVI4", spp != "PASM")


## seeded only dataframes

complete_seeded <- complete %>% 
  
  filter(seeded == "yes")



noNAVI_seeded <- noNAVI %>% 
  
  filter(seeded == "yes")


### for continuous variable models, had to remove the spp SYLA3 as there was no niche_breadth record included in the field data

c1 <- complete %>% 
  
  filter(spp != "SYLA3")


c2 <- complete_seeded %>% 
  
  filter(spp != "SYLA3")

n1 <- noNAVI %>% 
  
  filter(spp != "SYLA3")

n2 <- noNAVI_seeded %>% 
  
  filter(spp != "SYLA3")

# Models --------------------------------------------------------------
### MODELS 1-4 USE CATEGORICAL VARIABLES (class)
### MODELS 5-8 USE CONTINUOUS VARIABLES (niche_breadth)


## categorical variable
# all observations
m1 <- gam(
  
  presence ~ class +
             s(row, k = 3) +
             s(spp, bs = "re"),
  family = binomial,
  data = complete,
  method = "REML"
)

summary(m1)



# categorical variable
# only rows where seeded
m2 <- gam(
  
  presence ~ class +
    s(row, k = 3) +
    s(spp, bs = "re"),
  
  family = binomial,
  data = complete_seeded,
  method = "REML"
)

summary (m2)



# categorical variable
# no NAVI or PASM
m3 <- gam(
  
  presence ~ class +
    s(row, k = 3) +
    s(spp, bs = "re"),
  
  family = binomial,
  data = noNAVI,
  method = "REML"
)


summary(m3)



# categorical variable
# no NAVI or PASM
# only rows where seeded
m4 <- gam(
  
  presence ~ class +
    s(row, k = 3) +
    s(spp, bs = "re"),
  
  family = binomial,
  data = noNAVI_seeded,
  method = "REML"
)

summary (m4)



# continuous variable
# all observations
m5 <- gam(
  presence ~ niche_breadth +
  s(row, k = 3) +
  s(spp, bs = "re"),

family = binomial,
data = c1,
method = "REML"
)
  

summary(m5)
  



# continuous variable
# only rows where seeded
m6 <- gam(
  presence ~ niche_breadth +
    s(row, k = 3) +
    s(spp, bs = "re"),
  
  family = binomial,
  data = c2,
  method = "REML"
)

summary (m6)



# continuous variable
# no NAVI or PASM
m7 <- gam(presence ~ niche_breadth +
  s(row, k = 3) +
  s(spp, bs = "re"),

family = binomial,
data = n1,
method = "REML"
)

summary(m7)


# continuous varaible
# no NAVI or PASM
# only rows where seeded
m8 <- gam(
  presence ~ niche_breadth +
    s(row, k = 3) +
    s(spp, bs = "re"),
  
  family = binomial,
  data = n2,
  method = "REML"
)


summary (m8)



# simulate residuals ------------------------------------------------------

## MODEL 1
## includes NAVI and PASM, all observations of the 10 selected spp
## categorical variable

sim_res1 <- simulateResiduals(m1)

plot(sim_res1)

testDispersion(sim_res1)

testZeroInflation(sim_res1)


## MODEL 2
## includes NAVI and PASM, only observations where seeded == yes
## categorical variable

sim_res2 <- simulateResiduals(m2)

plot(sim_res2)

testDispersion(sim_res2)

testZeroInflation(sim_res2)


## MODEL 3
## does not include NAVI and PASM, all observations of the 10 selected spp
## categorical variable

sim_res3 <- simulateResiduals(m3)

plot(sim_res3)

testDispersion(sim_res3)

testZeroInflation(sim_res3)


## MODEL 4
## does not include NAVI and PASM, only observations where seeded == yes
## categorical variable

sim_res4 <- simulateResiduals(m4)

plot(sim_res4)

testDispersion(sim_res4)

testZeroInflation(sim_res4)


## MODEL 5
## includes NAVI and PASM, all observations of the 10 selected spp
## continuous variable

sim_res5 <- simulateResiduals(m5)

plot(sim_res5)

testDispersion(sim_res5)

testZeroInflation(sim_res5)


## MODEL 6
## includes NAVI and PASM, only observations where seeded == yes
## continuous variable

simres6 <- simulateResiduals(m6)

plot(simres6)

testDispersion(simres6)

testZeroInflation(simres6)


## MODEL 7
## does not include NAVI and PASM, all observations of the 10 selected spp
## continuous variable

simres7 <- simulateResiduals(m7)

plot(simres7)

testDispersion(simres7)

testZeroInflation(simres7)


## MODEL 8
## does not include NAVI and PASM, only observations where seeded == yes
## continuous variable

simres8 <- simulateResiduals(m8)

plot(simres8)

testDispersion(simres8)

testZeroInflation(simres8)


# final figures -----------------------------------------------------------
install.packages("gratia")

library(gratia)

# predicted probability plots
s1 <- smooth_estimates(m6) %>% mutate(model = "With dominant spp")
s2 <- smooth_estimates(m8) %>% mutate(model = "Without dominant spp")

all_smooths <- bind_rows(s1, s2)

row_smooths <- all_smooths %>%
  filter(.smooth == "s(row)") %>% 
  mutate(
    .lower = .estimate - 2 * .se,
    .upper = .estimate + 2 * .se
  )


library(ggplot2)

ggplot(row_smooths, aes(x = row, y = .estimate)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.15) +
  geom_line(linewidth = 1) +
  facet_wrap(~ model) +
  theme_classic() +
  labs(
    x = "Row position",
    y = "Predicted Probability of Occurence"
  )

### species random effects

re_with <- smooth_estimates(m6, select = "s(spp)") %>%
  mutate(group = "With dominant spp")

re_without <- smooth_estimates(m8, select = "s(spp)") %>%
  mutate(group = "Without dominant spp")

re_all <- bind_rows(re_with, re_without) %>% 
mutate(
  .lower = .estimate - 2 * .se,
  .upper = .estimate + 2 * .se
)

re_all <- re_all %>%
  mutate(spp = reorder(spp, .estimate))


# species random effects plot
ggplot(re_all, aes(x = spp, y = .estimate)) +
  
  geom_point() +
  
  geom_errorbar(aes(ymin = .lower, ymax = .upper),
                width = 0.2) +
  
  facet_wrap(~ group) +
  
  coord_flip() +
  
  theme_classic() +
  
  labs(
    x = "Species",
    y = "Random effect (log-odds)",
  ) +
  theme(text = element_text(size = 14))



## niche breadth effect plot
library (ggeffects)

pred_m2 <- ggpredict(m2, terms = "class")
pred_m4 <- ggpredict(m4, terms = "class")

pred_m2$model <- "(a) With dominant spp"
pred_m4$model <- "(b) Without dominant spp"

pred_all <- bind_rows(pred_m2, pred_m4)

ggplot(pred_all, aes(x = x, y = predicted)) +
  
  geom_point(size = 3) +
  
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.1) +
  
  facet_wrap(~ model) +
  
  theme_classic() +
  
  labs(
    x = "Niche breadth",
    y = "Predicted probability of occurrence"
  ) +
  
  theme(text = element_text(size = 14))
