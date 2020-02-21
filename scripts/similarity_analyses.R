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
search <- read.csv("data/search_preproc.csv", na.strings="")

# calculate average TD distance during search for each subject & character
subj_char_dists <- search %>% 
  group_by(subject, target) %>% 
  summarise(tds = mean(tds, na.rm=T),
            dds = mean(dds, na.rm=T),
            td_group = td_group[1],
            dd_group = dd_group[1])

# load nback performance and merge with similarity
nback <- read.csv("data/nback.csv")
names(nback) <- tolower(names(nback))
nback <- left_join(nback, subj_char_dists, by=c('subject'='subject','id'='target')) %>% 
  filter(subject != 1)


#############################################################################
# MAIN PLOTS OF SIMILARITY BY SESSION
#############################################################################

# RT ~ similarity by session
(f1 <- search %>% 
   filter(freq=='HF',acc == 1, td_group != 'Med TD similarity') %>% 
   ggplot(aes(session,rt, group=td_group, fill=td_group, shape=td_group)) +
   stat_summary(fun.y = mean, geom="line") +
   stat_summary(fun.data = mean_se, geom="errorbar", width=0.15) +
   stat_summary(fun.y = mean, geom="point", size=2) +
   scale_x_continuous(breaks=seq(1,12,1)) +
   scale_y_continuous(name="Reaction times (in ms.)") +
   scale_shape_manual(name='TD similarity', values=c(21,24), labels=c("High", "Low")) +
   scale_fill_manual(name='TD similarity', values=c("black","white"), labels=c("High", "Low")) +
   theme_classic() +
   theme(legend.position=c(1,1),
         legend.justification=c(1,1),
         panel.grid.major = element_blank(),
         axis.title.x = element_text(vjust = -0.2),
         # text = element_text(size=16),
         panel.margin = unit(2, 'lines'),
         axis.line.x = element_line(color="black"),
         axis.line.y = element_line(color="black")))

# ACC ~ similarity by session
(f2 <- search %>% 
    filter(freq=='HF',td_group != 'Med TD similarity') %>% 
    ggplot(aes(session,acc, group=td_group, fill=td_group, shape=td_group)) +
    stat_summary(fun.y = mean, geom="line") +
    stat_summary(fun.data = mean_se, geom="errorbar", width=0.15) +
    stat_summary(fun.y = mean, geom="point", size=2) +
    scale_x_continuous(breaks=seq(1,12,1)) +
    scale_y_continuous(name="Accuracy") +
    scale_shape_manual(name='TD similarity',values=c(21,24), labels=c("High", "Low")) +
    scale_fill_manual(name='TD similarity',values=c("black","white"), labels=c("High", "Low")) +
    theme_classic() +
    theme(legend.position=c(1,1),
          legend.justification=c(1,1),
          panel.grid.major = element_blank(),
          axis.title.x = element_text(vjust = -0.2),
          # text = element_text(size=16),
          panel.margin = unit(2, 'lines'),
          axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black")))

# RTs for session 1 by Char. repeition and similarity
(f3 <- search %>% 
    filter(freq=='HF', acc == 1, td_group != 'Med TD similarity', session == 1) %>% 
    mutate(rep = ceiling(rep/5)) %>% 
    ggplot(aes(rep, rt, group=td_group, fill=td_group, shape=td_group)) +
    stat_summary(fun.y = mean, geom="line") +
    stat_summary(fun.data = mean_se, geom="errorbar", width=0.15) +
    stat_summary(fun.y = mean, geom="point", size=2) +
    scale_x_continuous(name="Char. repetition in session 1", breaks=seq(1,4,1), labels=c('1-5','6-10','11-15','16-20')) +
    scale_y_continuous(name="Reaction times (in ms.)") +
    scale_shape_manual(name='TD similarity',values=c(21,24), labels=c("High", "Low")) +
    scale_fill_manual(name='TD similarity',values=c("black","white"), labels=c("High", "Low")) + 
    theme_classic() +
    theme(legend.position=c(1,1),
          legend.justification=c(1,1),
          panel.grid.major = element_blank(),
          axis.title.x = element_text(vjust = -0.2),
          # text = element_text(size=16),
          panel.margin = unit(2, 'lines'),
          axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black")))

# Accuracy for session 1 by Char. repeition and similarity
(f4 <- search %>% 
    filter(freq=='HF', td_group != 'Med TD similarity', session == 1) %>% 
    mutate(rep = ceiling(rep/5)) %>% 
    ggplot(aes(rep, acc, group=td_group, fill=td_group, shape=td_group)) +
    stat_summary(fun.y = mean, geom="line") +
    stat_summary(fun.data = mean_se, geom="errorbar", width=0.15) +
    stat_summary(fun.y = mean, geom="point", size=2) +
    scale_x_continuous(name="Char. repetition in session 1", breaks=seq(1,4,1), labels=c('1-5','6-10','11-15','16-20')) +
    scale_y_continuous(name="Accuracy") +
    scale_shape_manual(name='TD similarity',values=c(21,24), labels=c("High", "Low")) +
    scale_fill_manual(name='TD similarity',values=c("black","white"), labels=c("High", "Low")) + 
    theme_classic() +
    theme(legend.position=c(1,1),
          legend.justification=c(1,1),
          panel.grid.major = element_blank(),
          axis.title.x = element_text(vjust = -0.2),
          # text = element_text(size=16),
          panel.margin = unit(2, 'lines'),
          axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black")))

plot_grid(f3 + theme(legend.position = 'none'), 
          f4 + theme(legend.position = 'none'),
          f1 + theme(legend.position = 'none'), 
          f2 + theme(legend.position = c(1,0.1), legend.justification = c(1,0.1)))

ggsave('figures/similarity_final_main.tiff', width=6, height=5.5, units='in', compression='lzw')



#############################################################################
# PLOTS OF SIMILARITY BY SESSION AND PRESENT VS ABSET
#############################################################################

# ACC ~ similarity by session by cresp
search %>% 
  mutate(cresp = ifelse(cresp == 'a', 'Target absent', 'Target present')) %>% 
  filter(freq=='HF',td_group != 'Med TD similarity') %>% 
  ggplot(aes(session,acc, group=td_group, fill=td_group, shape=td_group)) +
  stat_summary(fun.y = mean, geom="line") +
  stat_summary(fun.data = mean_se, geom="errorbar", width=0.15) +
  stat_summary(fun.y = mean, geom="point", size=2) +
  scale_x_continuous(breaks=seq(1,12,1)) +
  scale_y_continuous(name="Accuracy") +
  scale_shape_manual(name='TD similarity',values=c(21,24), labels=c("High", "Low")) +
  scale_fill_manual(name='TD similarity',values=c("black","white"), labels=c("High", "Low")) +
  theme_classic() +
  facet_wrap(~cresp) +
  theme(legend.position=c(1,0.05),
        legend.justification=c(1,0.05),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(vjust = -0.2),
        # text = element_text(size=16),
        panel.margin = unit(2, 'lines'),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black")) 

ggsave('figures/similarity_final_by_cresp.tiff', width=5, height=2.25, units='in', compression='lzw')

# ACC ~ similarity by session by cresp
search %>% 
  mutate(cresp = ifelse(cresp == 'a', 'Target absent', 'Target present')) %>% 
  filter(freq=='HF',td_group != 'Med TD similarity') %>% 
  ggplot(aes(session,rt, group=td_group, fill=td_group, shape=td_group)) +
  stat_summary(fun.y = mean, geom="line") +
  stat_summary(fun.data = mean_se, geom="errorbar", width=0.15) +
  stat_summary(fun.y = mean, geom="point", size=2) +
  scale_x_continuous(breaks=seq(1,12,1)) +
  scale_y_continuous(name="Accuracy") +
  scale_shape_manual(name='TD similarity',values=c(21,24), labels=c("High", "Low")) +
  scale_fill_manual(name='TD similarity',values=c("black","white"), labels=c("High", "Low")) +
  theme_classic() +
  facet_wrap(~cresp) +
  theme(legend.position=c(1,1),
        legend.justification=c(1,1),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(vjust = -0.2),
        # text = element_text(size=16),
        panel.margin = unit(2, 'lines'),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black")) 



#############################################################################
# FIRST SESSION REGRESSIONS
#############################################################################
# RTS
s1_dat <- filter(search, acc==1, freq=='HF', session==1)
s1_ml1 <- lmer(rt ~ setsize*cresp + rep + (1|subject), data=s1_dat, REML=F)
s1_ml2 <- lmer(rt ~ setsize*cresp + rep + tds + (1|subject), data=s1_dat, REML=F)
s1_ml3 <- lmer(rt ~ setsize*cresp + rep*tds + (1|subject), data=s1_dat, REML=F)
anova(s1_ml1, s1_ml2, s1_ml3)
summary(s1_ml3)

# accuracy
s1_dat1 <- filter(search, freq=='HF', session==1)
s1_ml1_a <- glmer(acc ~ setsize+cresp + rep + (1|subject), data=s1_dat1, family='binomial')
s1_ml2_a <- glmer(acc ~ setsize+cresp + rep + tds + (1|subject), data=s1_dat1, family='binomial')
s1_ml3_a <- glmer(acc ~ setsize+cresp + rep*tds + (1|subject), data=s1_dat1, family='binomial')
anova(s1_ml1_a, s1_ml2_a, s1_ml3_a)
summary(s1_ml3_a)


#############################################################################
# RTS MIXED_EFFECTS modelling
#############################################################################

fit1 <- lmer(rt ~ invblock + session:invblock + cresp:invblock +setsize:invblock+ 
               setsize*cresp + session*setsize + (1|subject), filter(search, acc==1, freq=='HF'), REML=FALSE)
fit2.1 <- update(fit1, .~.+dds)
fit2.2 <- update(fit1, .~.+tds)
fit3 <- update(fit1, .~.+tds+dds)
fit4 <- update(fit1, .~.+tds+dds+tds:session)
fit5 <- update(fit1, .~.+tds+dds+tds:dds +tds:session + dds:session)

anova(fit1, fit2.2)
anova(fit3, fit4)
anova(fit1, fit2.1)

summary(fit1)
summary(fit4)
summary(fit5)
anova(fit2.1, fit3)
anova(fit2.2, fit3)
anova(fit3, fit4, fit5)


#############################################################################
# ACC MIXED_EFFECTS modelling
#############################################################################

afit1 <- glmer(acc ~ invblock + session:invblock + cresp:invblock + 
                 cresp + setsize + session + (1|subject), filter(search, freq=='HF'), family='binomial', nAGQ=0, control=glmerControl(calc.derivs = F))
afit2.1 <- update(afit1, .~.+dds)
afit2.2 <- update(afit1, .~.+tds)
afit3 <- update(afit1, .~.+tds+dds)
afit4 <- update(afit1, .~.+tds+dds+tds:session)  
afit5 <- update(afit1, .~.+tds+dds+tds:session + tds:cresp)

anova(afit1, afit2.2)
anova(afit1, afit2.1)
anova(afit2.1, afit3, afit4)
anova(afit4, afit5)

#############################################################################
# PLOTS OF BOTH DD AND TD
#############################################################################




#############################################################################
# RT slow down figures post error
#############################################################################

search %>% 
  filter(acc==1, !is.na(prev_target2_acc)) %>% 
  group_by(subject, session, prev_target2_acc) %>% 
  summarise(rt = mean(rt, na.rm=T)) %>% 
  ggplot(aes(prev_target2_acc, rt)) +
  stat_summary(geom='pointrange') +
  stat_summary(geom='line') +
  stat_summary(geom='errorbar', width=0.15) +
  scale_x_continuous('Average accuracy of the last\n 3 occurences of the target', labels=c(0,0.33,0.67,1)) +
  scale_y_continuous('Response times (ms.)')

ggsave('figures/similarity_final_rtslowdown.tiff', width=3, height=2.8, units='in', compression='lzw')


search %>% 
  filter(acc==1, !is.na(prev_acc)) %>% 
  mutate(prev_acc = ifelse(prev_acc == 1, 'correct','incorrect')) %>% 
  group_by(subject, session, prev_acc, prev_cresp, cresp) %>% 
  summarise(rt = mean(rt, na.rm=T)) %>% 
  spread(prev_acc, rt) %>% 
  mutate(slowdown = incorrect-correct) %>% 
  ggplot(aes(cresp, slowdown, fill=prev_cresp, shape=prev_cresp)) +
  stat_summary(geom='line') +
  stat_summary(geom='errorbar', width=0.15) +
  stat_summary(geom='pointrange') +
  scale_x_discrete('Current trial type', labels=c('Absent','Present')) +
  scale_y_continuous('Slow-down in RTs (ms.)\n') +
  scale_fill_manual(name="Type of trial during\n previous occurence", values=c('black','white'), labels=c('Absent','Present')) +
  scale_shape_manual(name="Type of trial during\n previous occurence", values=c(21,24), labels=c('Absent','Present')) +
  coord_cartesian(ylim=c(0,140))

ggsave('figures/similarity_final_rtslowdown_cresp.tiff', width=4.5, height=2.8, units='in', compression='lzw')

slow_ml0 <- update(fit5, data=filter(search, acc==1, freq=='HF', !is.na(prev_target2_acc)))
slow_ml1 <- update(slow_ml0, .~.+prev_target2_acc)
slow_ml2 <- update(slow_ml0, .~.+prev_target2_acc*cresp)
slow_ml3 <- update(slow_ml0, .~.+prev_target2_acc*cresp + prev_cresp)
slow_ml4 <- update(slow_ml0, .~.+prev_target2_acc*cresp + prev_cresp*cresp)
slow_ml5 <- update(slow_ml0, .~.+prev_target2_acc*cresp*prev_cresp)
anova(slow_ml0, slow_ml1, slow_ml2, slow_ml3, slow_ml4, slow_ml5)
summary(slow_ml1)



#############################################################################
# Performance on N-back as a function of TD similarity during 
# visual search
#############################################################################
pd = position_dodge(-0.05)
nback %>% 
  filter(!is.na(td_group), td_group != 'Med TD similarity') %>% 
  ggplot(aes(level, acc, fill=td_group, shape=td_group)) +
  stat_summary(geom='line') +
  stat_summary(geom='errorbar', width=0.05) +
  stat_summary(geom='pointrange') +
  scale_shape_manual(name='TD similarity',values=c(21,24), labels=c("High", "Low")) +
  scale_fill_manual(name='TD similarity',values=c("black","white"), labels=c("High", "Low")) +
  scale_x_continuous(labels=c(1,2,3), breaks=c(1,2,3)) +
  xlab('N-back load') +
  ylab("Accuracy") +
  # coord_cartesian(ylim=c(0.7, 1)) +
  theme(legend.position=c(1,1),
        legend.justification = c(1,1))

ggsave('figures/similarity_final_nback.tiff', width=3, height=2.8, units='in', compression='lzw')

nback_fit1 <- lme4::glmer(acc ~ level + wordtype + frequency +  (1|subject), data=filter(nback, !is.na(td_group)), family="binomial")
nback_fit2 <- lme4::glmer(acc ~ level + wordtype + frequency + td_group + (1|subject), data=filter(nback, !is.na(td_group)), family="binomial")
anova(nback_fit1, nback_fit2)
summary(nback_fit2)


#############################################################################
# Performance on cued-recall as a function of TD similarity during 
# visual search
#############################################################################

# load cued data and merge with similarity
cued <- read.csv("data/open_data/02-weekly-cued-recall.csv")
names(cued) <- tolower(names(cued))

cued <- cued %>% 
  filter(subject != 1) %>% 
  left_join(subj_char_dists, by=c('subject'='subject','c1'='target')) %>% 
  left_join(subj_char_dists, by=c('subject'='subject','c2'='target'))

# create low and high similarity categories
cued <- cued %>% 
  mutate(td = (td.x+td.y)) %>% 
  mutate(td_group = case_when(
    td <= mean(td, na.rm=T)-sd(td, na.rm=T) ~ 'High TD similarity',
    td >= mean(td, na.rm=T)+sd(td, na.rm=T) ~ 'Low TD similarity'
  ))
# mutate(
#   td_group1 = case_when(
#     td.x <= mean(td.x, na.rm=T)-sd(td.x, na.rm=T) ~ 'High TD similarity',
#     td.x >= mean(td.x, na.rm=T)+sd(td.x, na.rm=T) ~ 'Low TD similarity'
#   ),
#   td_group2 = case_when(
#     td.y <= mean(td.y, na.rm=T)-sd(td.y, na.rm=T) ~ 'High TD similarity',
#     td.y >= mean(td.y, na.rm=T)+sd(td.y, na.rm=T) ~ 'Low TD similarity'
#   ))

cued <- filter(cued, !is.na(td_group1), !is.na(td_group2))

cued %>% 
  filter(!is.na(td_group)) %>% 
  ggplot(aes(session, acc, color=td_group)) +
  stat_summary(geom='pointrange')

a <- lme4::glmer(acc~td*session + (td_group*session|subject), data=cued, family='binomial')
b <- lme4::glmer(acc~td.x*session + (1|subject), data=cued, family='binomial')

