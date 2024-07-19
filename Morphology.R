library(readr)
library(tidyverse)
library(readxl)
library(car)
library(stats)
library(multcompView)
library(multcomp)
library(emmeans)
Dt <-readxl::read_excel("./Data/Clam_measurement_no18.xlsx")
Dt
Dt$Trt <- as.character(Dt$Trt)
Dt$ID<- as.character(Dt$ID)


Dt <- Dt %>% group_by(Trt)

Dt_sum<-summarize(Dt, 
                  n = n(),
                      Mean_Length = mean(Length_mm),
                      sd_Length= sd(Length_mm),
                      Mean_Width = mean(Width_mm),
                  sd_Width= sd(Width_mm),
                  Mean_Flesh_weight = mean(Weight_flesh_g),
                  sd_Flesh_weight= sd(Weight_flesh_g),
                  Mean_Whole_weight = mean(Weight_whole_g),
                  sd_Whole_weight= sd(Weight_whole_g))
                 
Dt_sum
write_csv(Dt_sum, file="./output/Tundra_respirometry_morphology_sum.csv")

tt <-readxl::read_excel("./Data/Clam_measurement_no18.xlsx")
tt
tt$Trt <- as.character(Dt$Trt)
tt$ID<- as.character(Dt$ID)


tt_sum<-summarize(tt, 
                  n = n(),
                  Mean_Length = mean(Length_mm),
                  sd_Length= sd(Length_mm),
                  Mean_Width = mean(Width_mm),
                  sd_Width= sd(Width_mm),
                  Mean_Flesh_weight = mean(Weight_flesh_g),
                  sd_Flesh_weight= sd(Weight_flesh_g),
                  Mean_Whole_weight = mean(Weight_whole_g),
                  sd_Whole_weight= sd(Weight_whole_g))

tt_sum <- as.data.frame(tt_sum)
tt_sum
write_csv(tt_sum, file="./output/Tundra_respirometry_total_morphology_sum.csv")

### compare between treatment
LANOVA<- aov(Length_mm ~ Trt, data = Dt)
LANOVA
anova(LANOVA)
TukeyHSD(LANOVA)

WANOVA<- aov(Width_mm ~ Trt, data = Dt)
WANOVA
summary(WANOVA)
anova(WANOVA)
TukeyHSD(WANOVA)

FlANOVA<- aov(Weight_flesh_g ~ Trt, data = Dt)
FlANOVA
anova(FlANOVA)
TukeyHSD(FlANOVA)

WhANOVA<- aov(Weight_whole_g ~ Trt, data = Dt)
WhANOVA
anova(WhANOVA)
TukeyHSD(WhANOVA)
#### compare 2 locations: Rimouski vs Iqaluit
Data <-readxl::read_excel("./Data/Clam_measurement_Iqaluit_Rimouski.xlsx")
Data

Data <- Data %>% group_by(Site)

Data_sum<-summarize(Data, 
                  n = n(),
                  Mean_Length = mean(Length_mm),
                  sd_Length= sd(Length_mm),
                  Mean_Width = mean(Width_mm),
                  sd_Width= sd(Width_mm),
                  Mean_Flesh_weight = mean(Weight_flesh_g),
                  sd_Flesh_weight= sd(Weight_flesh_g),
                  Mean_Whole_weight = mean(Weight_whole_g),
                  sd_Whole_weight= sd(Weight_whole_g))

Data_sum
write_csv(Data_sum, file="./output/Tundra_Rimouski_respirometry_morphology_sum.csv")

Data$Site <- as.factor(Data$Site)

str(Data)
#assess homegenity of variance
leveneTest(Length_mm~Site, data=Data)
#normality
shapiro.test(Data$Length_mm)

LengthANOVA <- aov(Length_mm~ Site, data = Data)
LengthANOVA
summary(LengthANOVA)
TukeyHSD(LengthANOVA)


leveneTest(Width_mm~Site, data=Data)
#normality
shapiro.test(Data$Width_mm)

WidthANOVA <- aov(Width_mm~ Site, data = Data)
WidthANOVA
summary(WidthANOVA)
TukeyHSD(WidthANOVA)


leveneTest(Weight_flesh_g~Site, data=Data)
#normality
shapiro.test(Data$Weight_flesh_g)

WefleshANOVA <- aov(Weight_flesh_g ~ Site, data = Data)
WefleshANOVA
summary(WefleshANOVA)
TukeyHSD(WefleshANOVA)

#####
leveneTest(Weight_whole_g~Site, data=Data)
#normality
shapiro.test(Data$Weight_whole_g)

WeWholehANOVA <- aov(Weight_whole_g~ Site, data = Data)
WeWholehANOVA
summary(WeWholehANOVA)
TukeyHSD(WeWholehANOVA)



