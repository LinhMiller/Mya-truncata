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



#str(rand_site <- coef(m)$Site)
#lattice::qqmath(ranef(m, condVar = TRUE)) ##slope is mass_whole_g
## try different packages tocompare values
#randomSims <- merTools::REsim(m, n.sims = 5000)
#merTools::plotREsim(merTools::REsim(m, n.sims = 500))
###
#remotes::install_github('m-clark/mixedup')
#mix <-mixedup::extract_random_effects(m, digits=30) ###
  
#mix <- as.data.frame(mix)
#write.csv(mix, file = "./output/slope_intercept_2sites.csv")
#p<- ggplot(data = dt, aes(x = mass_whole_g, y = Delta, color = Site)) +
  geom_point() +
  scale_color_manual(values = c("North" = "black", "South" = "grey"))+
  geom_abline(intercept = (-1.46338111944585 - (0.999474801832449
*1.96)), slope = (-0.0103424066930278 - ( 0.00706376127354908  * 1.96)), color = "black", linetype = 2) +
  geom_abline(intercept = (-1.46338111944585 + (0.9994748018324490*1.96)), slope = (-0.0103424066930278 + (0.00706376127354908 * 1.96)), color = "black", linetype = 2) + 
  geom_abline(intercept = -1.463381119, slope = -0.010342407, color = "black") + 
  
  geom_abline(intercept = 1.290429816, slope = 0.009120078 , color = "grey") +
  geom_abline(intercept = (1.290429816- (1.218034268*1.96)), slope = ( 0.009120078- (0.008608424 * 1.96)), color = "grey", linetype = 2) + 
  geom_abline(intercept = (1.290429816 + (1.218034268*1.96)), slope = ( 0.009120078 + (0.008608424* 1.96)), color = "grey", linetype = 2) +
  theme_classic() +
  labs(x= "Whole-body mass")+
  theme(text = element_text(size=14))
p
ggsave(plot=p, file="./output/Result_graph/Random_effect_newBG.jpeg")

##########
summary(m)
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

###need to finish this with the SOUTH")
##RESULTS: NO EFFECT OF MASS NOR SITE NOR DATES ON MO2

##### I label the day of the trial as day 1 to day n (when the ended), then saperature each site
dt 
N <- dt[c(1:48),]
S <- dt[c(49:95),]

N <- N %>% mutate( Treatment=Trt)
mN <- lmer(Delta ~ (1|Date_order)+ factor(Trt), data=N)
summary(mN)

performance::check_model(mN) #check model fit #looked OK

pairs(emmeans(mN, ~ Trt, adjust = "Tukey"))
df_partial_pooling <- coef(mN)[["Date_order"]] %>% 
  rownames_to_column("Date_order")

str(rand_site <- coef(mN)$Date_order)
lattice::qqmath(ranef(mN, condVar = TRUE)) ###there was not an effect of DATE_ORDER on MO2 between treatments

S <- S %>% mutate( Treatment=Trt)
mS<- lmer(Delta ~ (1|Date_order)+ factor(Trt), data=S)
summary(mS)

performance::check_model(mS) #check model fit #looked OK

pairs(emmeans(mS, ~ Trt, adjust = "Tukey"))
df_partial_pooling <- coef(mS)[["Date_order"]] %>% 
  rownames_to_column("Date_order")

str(rand_site <- coef(mS)$Date_order)
lattice::qqmath(ranef(mS, condVar = TRUE)) ###there was not an effect of DATE_ORDER on MO2 between treatments

###
dt <- dt %>% group_by(Site)
summarize(dt,
          mean_mass = mean(mass_whole_g, na.rm = TRUE),
          sd_mass = sd(mass_whole_g, na.rm = TRUE),
          n = n(),
          se_mass = sd_mass / sqrt(n), # Calculate Standard Error
          ci_mass = 1.96 * se_mass, # CI margin of error
          ci_low_mass = mean_mass - ci_mass, # The lower range
          ci_high_mass = mean_mass + ci_mass)

###correlation test of Delta oxygen consumption and temperature treatment
res <- cor.test(N$Delta, N$Trt, 
                method = "pearson")
res

res <- cor.test(S$Delta, S$Trt, 
                method = "pearson")
res
