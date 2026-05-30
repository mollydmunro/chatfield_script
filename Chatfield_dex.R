
    ## Chatfield Farms Data Exploration

    ## ER 390 University of Victoria Continuing Studies

    ## Molly Munro

setwd("~/Downloads/R")

library(magrittr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(PerformanceAnalytics)

# read in data ------------------------------------------------------------

# response variable = presence/absence = 'presence column'


survey_clean <- read.csv('data/Chatfield/seeded_survey_full.csv')


  # abiotic soil data

abiotic <- read.csv('data/Chatfield/upper_slope/data/2021_upper_slope_abiotic.csv') %>% 
  
  # column names to lower
  
  rename_with(tolower)


species_codes <- read.csv('data/Chatfield/species_codes.csv') %>% 
  
  # column names to lower
  
  rename_with(tolower)


treatment_spp <- read.csv('data/Chatfield/treatments_spp.csv')



# join data for explanatory variables -------------------------------------

full_survey_clean <- full_join (survey_clean, abiotic, 
                                by = c("row","point")) 

rm(abiotic)


# add generalist/specialist to all obs

full_survey_clean <- full_survey_clean %>% 
  
  group_by(spp) %>% 
  
  # for species labelled as gen/spec, add the label to all obs
  
  mutate(
    class = case_when(
      "generalist" %in% class ~ "generalist",
      "specialist" %in% class ~ "specialist",
      TRUE ~ NA_character_)) %>% 
  
  # copy the niche breadth values for each species to each entry within the column
  
  fill(niche_breadth, .direction = "downup") %>% 
  ungroup() 
  

## add seeded column yes/no

# add SPP codes
treatment_spp <- treatment_spp %>% 
  
  left_join(species_codes %>% 
              
              select(spp, 
                     species),
            
            by = 'species')

# add seeded column

full_survey_clean <- full_survey_clean %>% 
  
  mutate(seeded = ifelse(
    
    # if in treatment for the row, print yes, otherwise, print no
    paste(row, spp) %in% paste(treatment_spp$row, treatment_spp$spp),
    "yes",
    "no"
  ))


# WRITE CSV

write_csv(full_survey_clean, 'survey_clean.csv')



## only potential explanatory variable for a correlation check

ex.var2024 <-full_survey_clean %>% 
  
  select(-class,
         -spp,
         -row,
         -point,
         -presence)
  
  

# probability of spp occurence by row -------------------------------------------

## all observations, not including seeded vs nonseeded data

response_prob <- full_survey_clean %>% 
  
  select(-elevation,
         -slope,
         -aspect,
         -clay,
         -silt,
         -sand) %>% 
  
  group_by(spp) %>% 
  
  # probability of a spp showing up = average presence independent of other variables
  mutate(probability = mean(presence, 
                            na.rm = TRUE),
         )

# write csv
write_csv(response_prob, "response_prob.csv")



## creating a df ONLY IF SEEDED IN THAT ROW

r_prob_seeded <- full_survey_clean %>% 
  
  select(-elevation,
         -slope,
         -aspect,
         -clay,
         -silt,
         -sand) %>%
  
  group_by(spp, seeded) %>%
  
  mutate(probability = mean(presence[
                                    seeded == "yes"], 
                            
                            na.rm = TRUE))

write_csv(r_prob_seeded, "r_prob_seeded.csv")



## REMOVE NAVI and PASM - too powerful

r_noNAVI_noPASM <- full_survey_clean %>% 
  
  # remove environmental variables
  select(-elevation,
         -slope,
         -aspect,
         -clay,
         -silt,
         -sand) %>% 
  
  # remove NAVI and PASM
  filter(spp != "NAVI4", spp != "PASM") %>% 
  
  #calculate probability for each spp for both y/n seeding
  group_by(spp, seeded) %>% 
  
  mutate(probability = mean(presence[
                                    seeded == "yes"], 
                            
                            na.rm = TRUE))

write_csv(r_noNAVI_noPASM, "no_NAVI4_PASM.csv")



# boxplots and correlation checks --------------------------------------------------

## pivot longer for analysis
# only using 2024 data doesn't matter because those physical characteristics would not be different over four years

c2024_long <- full_survey_clean %>% 
  
  pivot_longer(
              cols = c(clay, silt, sand),
              names_to = "grain_size",
              values_to = "grain_percent"
              )

  
    ## then plot

    boxplot(
           grain_percent ~ grain_size,
           data = c2024_long,
           ylab = "Percent",
           main = "Soil Texture Components"
           )
  

## generalist/specialist boxplot (seeded not accounted)
    
  ggplot(response_prob, aes(x = class, y = probability)) +
    geom_boxplot() +
    labs(title = "Generalist vs Specialist accross all rows",
         x = "Species Classification",
         y = "Probability of Occurrence")
    
# overlay spp
  
  ggplot(response_prob, aes(x = class, y = probability)) +
    geom_boxplot() +
    geom_jitter(aes(color = spp))+
    labs(title = "Generalist vs Specialist accross all rows",
         x = "Species Classification",
         y = "Probability of Occurrence")
  
## generalist/specialist boxplot (only seeded spp)
  
  ggplot(r_prob_seeded, aes(x = class, y = probability)) +
    geom_boxplot() +
    labs(title = "Generalist vs Specialist where seeded",
         x = "Species Classification",
         y = "Probability of Occurrence")

# overlay spp

  ggplot(r_prob_seeded, aes(x = class, y = probability)) +
    geom_boxplot() +
    geom_jitter(aes(color = spp))+
    labs(title = "Generalist vs Specialist where seeded",
         x = "Species Classification",
         y = "Probability of Occurrence")


## only seeded gen/spec NO NAVI4 NO PASM
  
  ggplot(r_noNAVI_noPASM, aes(x = class, y = probability)) +
    geom_boxplot() +
    labs(title = "NO NAVI/PASM Generalist vs Specialist where seeded",
         x = "Species Classification",
         y = "Probability of Occurrence")
  
  # overlay spp
  
  ggplot(r_noNAVI_noPASM, aes(x = class, y = probability)) +
    geom_boxplot() +
    geom_jitter(aes(color = spp))+
    labs(title = "NO NAVI/PASM Generalist vs Specialist where seeded",
         x = "Species Classification",
         y = "Probability of Occurrence")
  
## potential correlation between explanatory variables
  
chart.Correlation(ex.var2024[,1:6], 
                  histogram = TRUE, 
                  method = "pearson")

