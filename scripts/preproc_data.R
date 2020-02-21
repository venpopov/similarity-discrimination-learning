#############################################################################
# GOAL: Analyze character similarity data from Reder et al (2016), PB&R
# AUTHOR: Ven Popov
# DATE: Sep 21, 2018
#############################################################################

rm(list=ls())
library(tidyverse)
library(cowplot)
library(lme4)
# library(colorRamps)
theme_set(theme_classic())
setwd(here::here())


#############################################################################
# DATA
#############################################################################

# load and clean up the search data
search <- read.csv("data/search_raw.csv", na.strings="")
names(search) <- tolower(names(search))

# remove subject 19 who completed only 1 sesion
# remove subject 1 whose subject number was miscoded and is a mixture of two 
# people's partial data
search <- filter(search, subject != 19, subject != 1)

# calculate the repetition number of each target within each session
search <- search %>% 
  group_by(subject, session, target) %>% 
  mutate(rep = 1:length(target))


# code whether the preceding occurence of a target lead to an error response
search <- search %>% 
  group_by(subject, session, target) %>% 
  mutate(prev_acc = factor(c(NA, acc[-length(acc)])),
         prev_cresp = factor(c(NA, cresp[-length(cresp)])))

# code how many of the last 3 repetitions of a specific character were accurate responses
search <- search %>% 
  group_by(subject, session, target) %>% 
  mutate(maxrep=length(target))

searchtmp <- filter(search, maxrep < 4)

search <- search %>% 
  filter(maxrep >= 4) %>%
  mutate(prev_target2_acc = c(NA, acc[-length(acc)]) + c(NA,NA, acc[-c((length(acc)-1):length(acc))]) + c(NA,NA,NA, acc[-c((length(acc)-2):length(acc))])) %>% 
  bind_rows(searchtmp)

# code number of trials between character repetitions
search <- search %>% 
  group_by(subject, session, target) %>% 
  mutate(trial_distance = block - c(NA, block[-length(block)])) 


# load character distance info
char_dist <- read.csv("expfiles/char_dist.csv")[,-1] %>% select(id, tddist_f, dddist_f)
search <- left_join(search, char_dist, by=c('target'='id'))



# create low and high similarity categories
search <- search %>% 
  ungroup() %>% 
  mutate(
    td_group = case_when(
      tddist_f <= mean(char_dist$tddist_f, na.rm=T)-sd(char_dist$tddist_f, na.rm=T) ~ 'High TD similarity',
      tddist_f >= mean(char_dist$tddist_f, na.rm=T)+sd(char_dist$tddist_f, na.rm=T) ~ 'Low TD similarity',
      TRUE ~ 'Med TD similarity'),
    dd_group = case_when(
      dddist_f <= mean(char_dist$dddist_f, na.rm=T)-sd(char_dist$dddist_f, na.rm=T) ~ 'High DD similarity',
      dddist_f >= mean(char_dist$dddist_f, na.rm=T)+sd(char_dist$dddist_f, na.rm=T) ~ 'Low DD similarity',
      TRUE ~ 'Med DD similarity'))

# center and scale measures of distance and transform them to similarity
search <- search %>% 
  ungroup() %>% 
  mutate(tds = -scale(tddist_f),
         dds = -scale(dddist_f))

# trim RT's that are more than 3 MAD above or below the median and RTs below 200ms.
search <- search %>%  
  filter(rt >= 200) %>% 
  group_by(subject, session, freq) %>% 
  mutate(mad=mad(rt), reltomad=(rt-median(rt))/mad(rt)) %>% 
  filter(!is.na(reltomad)) %>% 
  filter(abs(reltomad) <= 3)  

search$invblock <- search$block/679

write.csv(search,'data/search_preproc.csv', row.names=F)
