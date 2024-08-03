rm(list=ls())
library(wql)
library(ggplot2)
library(readxl)
library(tibble)
library(respR)
library(lubridate)
library(tidyverse)
library(dplyr)
library(skimr)
library(car)
library(rstatix)
library(lme4)
library(emmeans)
######### Analyze mass specific rate
dt <-read_excel("./Data/Rimouski_Iqaluit_convertedrate_raw_newBG.xlsx")
dt <- dt %>% mutate( Treatment=Trt)
dt$Treatment<-as.character(dt$Treatment)
#dt <- dt %>% mutate(date_character = paste0(Site, "_", Date_order))
dt %>% filter(!is.na(Delta)) %>% group_by(Trt, Site) %>% tally()
#m <- lmer(Delta ~ (mass_whole_g|Site/date_character)+ factor(Trt), data=dt)
#summary(m)
#m <- lmer(Delta ~ (1|Site)+ mass_whole_g + factor(Trt), data=dt)
m <- lm(Delta ~ (Date_order*Site)+ mass_whole_g  + factor(Treatment), data=dt)
#dt %>% group_by(Site,Date_order) %>% tally()
summary(m) ## make graph with overal model intercept and mass_whole_body as slope
cov2cor(vcov(m)) # to get the correlation coefficient value

dt <- mutate(dt, Treatment = factor(Treatment, levels = c("7", "10", "13", "16", "19", "22")))
p<- ggplot(data = dt, aes(x = mass_whole_g, y = Delta, alpha= Site)) +
  geom_point() +
  geom_abline(intercept = (-1.968282  - (7.575477*1.96)), slope = ( -0.002338 - (0.117393 * 1.96)), color = "black", linetype = 2) +
  geom_abline(intercept = (-1.968282  + (7.575477*1.96)), slope = ( -0.002338 + (0.117393 * 1.96)), color = "black", linetype = 2) +
  geom_abline(intercept = -1.968282 , slope = -0.002338) +
  theme_classic() +
  labs(x= "Whole-body mass (g)")+
  theme(text = element_text(size=14))
p
ggsave(plot=p, file="./output/Result_graph/Random_effect_newBG.jpeg")

