# The project determines the abundance of mycorrhizal morphotype across forest fragments in Kenya
# Author: Mercy Korir
# Last edited : 06.04.2024

# Libraries
library(tidyverse)    # version 2.0.0
library(vegan)        # version 2.6.4
library(agricolae)    # version 1.3.7
library(circlize)     # version 0.4.16
library(ethnobotanyR) # version 0.1.9

# Read data ---------------------------------------------------------------

morphotypes <- read_csv('data/morphotypes.csv')
head(morphotypes) # checking first six rows of the data
chordDiagram(morphotypes)

# Checking for normality --------------------------------------------------

shapiro.test(morphotypes$abundance)
shapiro.test(log1p(morphotypes$abundance))

# ANOVA Summary -----------------------------------------------------------

summary(aov(abundance ~ fragments * amf_species, morphotypes))

# The abundance of morphotypes per fragment -------------------------------

counts <- morphotypes %>% 
  group_by(fragments, amf_species) %>% 
  summarise(n = mean(abundance)) %>% 
  arrange(-n) 
counts

# Log transformed ANOVA Summary -------------------------------------------

summary(aov(log1p( morphotypes$abundance) ~ morphotypes$fragments))

# Building final ANOVA model ----------------------------------------------

aov_model <- aov(log1p(abundance) ~ fragments, data = morphotypes)  

TukeyHSD(aov_model)
morphotypes_lsd <-  LSD.test(aov_model, trt = 'fragments')
morphotypes_lsd$groups
morphotypes_lsd$statistics

aov_model <- aov(log1p(abundance) ~ amf_species, data = morphotypes)  
morphotypes_lsd <-  LSD.test(aov_model, trt = 'amf_species')
morphotypes_lsd$groups
morphotypes_lsd$statistics

aov_model <- aov(log1p(abundance) ~ amf_species, data = morphotypes)

TukeyHSD(aov_model)
morphotypes_lsd <-  LSD.test(aov_model, trt = 'amf_species')
morphotypes_lsd$groups
morphotypes_lsd$statistics

aov_model <- aov(log1p(abundance) ~ fragments * amf_species, data = morphotypes)
morphotypes_lsd <-  LSD.test(aov_model, trt = c('fragments', 'amf_species'))
morphotypes_lsd$groups
morphotypes_lsd$statistics

morphotypes %>% 
  mutate(log_abundance = log1p(abundance)) %>% 
  group_by(fragments) %>% 
  summarise(sd = sd(log_abundance))

morphotypes %>% 
  mutate(log_abundance = log1p(abundance)) %>% 
  group_by(amf_species) %>% 
  summarise(sd = sd(log_abuwndance)) 
 
morphotypes %>% 
  mutate(log_abundance = log1p(abundance)) %>% 
  #group_by(amf_species * fragments) %>% 
  summarise(sd = sd(log_abundance)) 

#####Chawia########

morphoch <- read_csv('data/morphoch.csv')
shapiro.test(morphoch$abundance)
shapiro.test(log1p(morphoch$abundance))

morphoch<- read_csv('data/morphoch.csv') %>% 
  select(fragments, amf_species, abundance)
summary(aov(abundance ~ fragments * amf_species, morphotypes))
counts <- morphoch %>% 
  group_by(fragments, amf_species) %>% 
  summarise(n = mean(abundance)) %>% 
  arrange(-n) 

aov_model <- aov(log1p(abundance) ~ amf_species, data = morphoch)
summary(aov_model <- aov(log1p(abundance) ~ amf_species, data = morphoch))
TukeyHSD(aov_model)
morphoch_lsd <-  LSD.test(aov_model, trt = 'amf_species')
morphoch_lsd$groups
morphoch_lsd$statistics

morphong <- read_csv('data/morphong.csv')
shapiro.test(morphong$abundance)
shapiro.test(log1p(morphong$abundance))

morphong<- read_csv('data/morphong.csv') %>% 
  select(fragments, amf_species, abundance)
summary(aov(abundance ~ fragments, morphong))
counts <- morphong %>% 
  group_by(fragments, amf_species) %>% 
  summarise(n = mean(abundance)) %>% 
  arrange(-n) 

aov_model <- aov(log1p(abundance) ~ amf_species, data = morphong)
summary(aov_model <- aov(log1p(abundance) ~ amf_species, data = morphong))

TukeyHSD(aov_model)
morphong_lsd <-  LSD.test(aov_model, trt = 'amf_species')
morphong_lsd$groups
morphong_lsd$statistics


morphofur <- read_csv('data/morphofur.csv')
shapiro.test(morphofur$abundance)
shapiro.test(log1p(morphofur$abundance))

morphofur<- read_csv('data/morphofur.csv') %>% 
  select(fragments, amf_species, abundance)
summary(aov(abundance ~ fragments, morphong))
counts <- morphofur %>% 
  group_by(fragments, amf_species) %>% 
  summarise(n = mean(abundance)) %>% 
  arrange(-n) 

aov_model <- aov(log1p(abundance) ~ amf_species, data = morphofur)
summary(aov_model <- aov(log1p(abundance) ~ amf_species, data = morphofur))

TukeyHSD(aov_model)
morphofur_lsd <-  LSD.test(aov_model, trt = 'amf_species')
morphofur_lsd$groups
morphofur_lsd$statistics
