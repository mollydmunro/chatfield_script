
## Chatfield Farms Data 

## ER 390 University of Victoria Continuing Studies

## Molly Munro


setwd("~/Downloads/R")

# library packages ----------------------------------------------------------------

library(magrittr)
library(tidyverse)

# data files --------------------------------------------------------------

## read in species codes data

species_codes <- read.csv('data/Chatfield/species_codes.csv') %>% 
  
  # column names to lower
  
  rename_with(tolower)




## read in upper slope abiotic 

abiotic <- read.csv('data/Chatfield/upper_slope/data/2021_upper_slope_abiotic.csv') %>% 
  
  # column names to lower
  
  rename_with(tolower)




## read in seeded species traits

seeded_species_traits <- read.csv('data/Chatfield/upper_slope/data/seeded_species_traits.csv') %>% 
  
  # column names to lower
  
  rename_with(tolower)



## read in treatment data

treatment <- read_csv('data/Chatfield/upper_slope/data/treatments.csv') %>% 
  
  rename_with(tolower)



## read in survey data 2021

surv21 <- read_csv('data/Chatfield/upper_slope/data/LPI_2021.csv') %>% 
  
  # column names to lower
  
  rename_with(tolower)




## read in survey data 2022

surv22 <- read_csv('data/Chatfield/upper_slope/data/LPI_2022.csv') %>% 
  
  # column names to lower
  
  rename_with(tolower)




## read in survey data 2023

surv23 <- read_csv('data/Chatfield/upper_slope/data/LPI_2023.csv') %>% 
  
  # column names to lower
  
  rename_with(tolower)




## read in survey data 2024

surv24 <- read_csv('data/Chatfield/upper_slope/data/LPI_2024.csv') %>% 
  
  # column names to lower
  
  rename_with(tolower)


# data cleaning -----------------------------------------------------------

### adding year columns to the survey data 

surv21 <- surv21 %>% 
  
  mutate(year = 2021) %>% 
  
  # drop n/a row

  drop_na(row)


surv22 <- surv22 %>% 
  
  mutate(year = 2022) %>% 
  
  # drop n/a row
  
  drop_na (row)


surv23 <- surv23 %>% 
  
  mutate(year = 2023) %>% 
  
  drop_na (row)


surv24 <- surv24 %>% 
  
  mutate(year = 2024) %>% 
  
  drop_na(row)


### add in species codes to seeded species dataset

seeded_species_traits <- seeded_species_traits %>% 
  
  left_join(species_codes %>% select(spp, species), by = 'species')



### join survey data together

survey_full <- bind_rows (surv21, surv22, surv23, surv24) %>% 
  
  ## remove NA column
  
  select(where(~ !all(is.na(.))))


rm(surv21)
rm(surv22)
rm(surv23)
rm(surv24)

#### join complete survey data to seeded species


seeded_survey <- survey_full %>% 
  
  left_join(seeded_species_traits, by = "spp") %>% 
  
  # only spp that were seeded
  # ! indicates remove
  filter(!is.na(seed_mass)) %>% 
  
  # select(- ) to remove columns
  select(-order,
         -family,
         -reference,
         -lifeform,
         -species,
         -cycle,
         -seed_mass,
         -n_occ) %>% 
  
  #create a presence column for seeded species
  mutate(presence = 1)


#### classify species based on S/G row treatment

# join treatment data and survey data

spp_class <- seeded_survey %>% 
  
  left_join(treatment, by = c("row")) %>% 
  
  # removing unnecessary rows
  select(-niche_mean,
         -niche_weight,
         -seed_wt,
         -seed_dens)
  
# determine how often each species occurs under treatments

spp_class <- spp_class %>%
  filter(presence == 1) %>%
  
  ## 'occurences' help to determine where the species was actually seeded (as a generalist or specialist) vs where it may have appeared
  count(spp, niche_trt, name = "occurences")

# classify each species explicitly based on the treatment in which it was applied

# since some spp are showing up in rows where they werent necesarily seeded, using the number of times it shows up in 'narrow' or 'broad' columns to determine which treatment it was actually a part of

spp_class <- spp_class %>%
  group_by(spp) %>%
  summarize(
    
    narrow_n = sum(
                  if_else(
                  niche_trt == "Narrow", 
                  occurences,
                  0L),
                  na.rm = TRUE),
    
    broad_n  = sum(
                  if_else(
                  niche_trt == "Broad", 
                  occurences,
                  0L),
                  na.rm = TRUE),
    
    total_occurrences = narrow_n + broad_n,
    
    # using the number of occurrences to categorize whether it is a generalist or specialist species
    
    class = case_when(
      total_occurrences < 5      ~ "ambiguous",
      narrow_n > broad_n         ~ "specialist",
      broad_n  > narrow_n        ~ "generalist",
      TRUE                       ~ "ambiguous"
    ),
    .groups = "drop"
  )


# join classification into full survey data

seeded_survey <- seeded_survey %>% 
  
  left_join(
            spp_class %>% 
              
              select(spp, class),
              by = "spp"
              ) %>% 
  
  filter(year == 2024)

#### Create explicit zeros in the data

seeded_survey_full <- seeded_survey %>% 
  
  # for every combination of row, point, and spp, add a zero where there is not currently a value assigned
  complete(
          row,
          point,
          spp,
          fill = list(presence = 0)
           ) %>% 
  
  # filter to select the top five species in the generalist and specialist categories as determined above
  
filter(spp %in% c('NAVI4', 'PASM', 'ELTR7', 'LILE3', 'RACO3', 'BOGR', 'LIPU', 'SYLA3', 'SPCO', 'HECO26')) 

## save as csv for data exploration

write_csv(seeded_survey_full, "seeded_survey_full.csv")
