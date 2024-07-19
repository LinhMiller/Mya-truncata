library(tidyverse)
library(readxl)
library(car)
library(stats)
library(multcompView)
library(multcomp)
library(emmeans)
data<- read_xlsx("Delta_Ct.xlsx", sheet= "2^dCT")
data <- data %>% group_by(Treatment)
data$Treatment <-as.character(data$Treatment)
data <- mutate(data, Treatment = factor(Treatment, levels = c("7", "10", "13", "16", "19", "22"))) 
sum<- summarize(data,
                mean_Cq_RPS4 = mean(RPS4, na.rm = TRUE),
                quant_RPS4 = quantile (RPS4,probs = 0.75,  na.rm = TRUE),
                
                mean_Cq_RPL18A = mean(RPL18A, na.rm = TRUE),
                quant_RPL18A = quantile(RPL18A,probs = 0.75, na.rm = TRUE),
                
                mean_Cq_HSP60 = mean(HSP60, na.rm = TRUE),
                quant_HSP60 = quantile(HSP60, probs = 0.75,na.rm = TRUE),
                
                mean_Cq_HSP90 = mean(HSP90, na.rm = TRUE),
                quant_HSP90 = quantile(HSP90,probs = 0.75, na.rm = TRUE),
                
                mean_Cq_MnSOD = mean(MnSOD, na.rm = TRUE),
                quant_MnSOD = quantile(MnSOD,probs = 0.75, na.rm = TRUE),
                
                mean_Cq_CS = mean(CS, na.rm = TRUE),
                quant_CS = quantile(CS,probs = 0.75, na.rm = TRUE),
                n=n(),
                mean_Cq_CAT = mean(CAT, na.rm = TRUE),
                quant_CAT = quantile(CAT,probs = 0.75, na.rm = TRUE),
                n=n())
sum
sum<- as.data.frame(sum)
write.csv(sum, file= "SUM_TABLE.CSV")
###########HSP60
#### use qq plot to assess normality
mHSP60<-lm(HSP60~Treatment, data=data)
dHSP60 <- data.frame(residuals = residuals(mHSP60),
                     std_residuals = rstudent(mHSP60),
                     fitted = fitted(mHSP60),
                     cooks = cooks.distance(mHSP60))
dHSP60 <- mutate(dHSP60, obs = 1:n())

ggplot(data = dHSP60, aes(sample = std_residuals)) +
  stat_qq() +
  stat_qq_line()

HSP60 <- ggplot(data, aes(x= Treatment, y = HSP60))+
  geom_boxplot()+
  geom_point()
HSP60
#changing the data from a charater/numeric to a factor
data$Treatment <- as.factor(data$Treatment)

str(data)

#assess homegenity of variance
leveneTest(HSP60~Treatment, data=data)

#normality
shapiro.test(data$HSP60)

#transformation
data$sqrtHSP60 <- sqrt(data$HSP60)

# transformed data
#assess homegenity of variance
leveneTest(sqrtHSP60~Treatment, data=data)


#normality
shapiro.test(data$sqrtHSP60)

#anova
HSP60ANOVA <- aov(sqrtHSP60 ~ Treatment, data = data)
HSP60ANOVA
summary(HSP60ANOVA)
###comparision between treatments
TukeyHSD(HSP60ANOVA)
letters_HSP60<- emmeans(HSP60ANOVA, list(pairwise~Treatment), adjust="Tukey")
letters_HSP60

cldHSP60<- cld(letters_HSP60, details= FALSE, source= FALSE, alpha= 0.05, Letters= LETTERS,reversed = TRUE, adjust= "Tukey")

cldHSP60
 ############ HSP90

mHSP90<-lm(HSP90~Treatment, data=data)
dHSP90 <- data.frame(residuals = residuals(mHSP90),
                     std_residuals = rstudent(mHSP90),
                     fitted = fitted(mHSP90),
                     cooks = cooks.distance(mHSP90))
dHSP90 <- mutate(dHSP90, obs = 1:n())

ggplot(data = dHSP90, aes(sample = std_residuals)) +
  stat_qq() +
  stat_qq_line()

#assess homegenity of variance
leveneTest(HSP90~Treatment, data=data)

#normality
shapiro.test(data$HSP90)

#transformation
data$logHSP90 <- log(data$HSP90)

# transformed data
#assess homegenity of variance
leveneTest(logHSP90~Treatment, data=data)

#normality
shapiro.test(data$logHSP90)
#anova
HSP90ANOVA <- aov(logHSP90 ~ Treatment, data = data)
HSP90ANOVA
summary(HSP90ANOVA)
###comparision between treatments
TukeyHSD(HSP90ANOVA)

letters_HSP90<- emmeans(HSP90ANOVA, list(pairwise~Treatment), adjust="Tukey")
letters_HSP90
cld(letters_HSP90, details= FALSE, source= FALSE, alpha= 0.05, Letters= LETTERS, adjust= "Tukey")

#########MnSOD
mMnSOD<-lm(MnSOD~Treatment, data=data)
dMnSOD <- data.frame(residuals = residuals(mMnSOD),
                     std_residuals = rstudent(mMnSOD),
                     fitted = fitted(mMnSOD),
                     cooks = cooks.distance(mMnSOD))
dMnSOD <- mutate(dMnSOD, obs = 1:n())

ggplot(data = dMnSOD, aes(sample = std_residuals)) +
  stat_qq() +
  stat_qq_line()

#assess homegenity of variance
leveneTest(MnSOD~Treatment, data=data)

#normality
shapiro.test(data$MnSOD)

#transformation
data$sqrtMnSOD <- sqrt(data$MnSOD)

# transformed data
#assess homegenity of variance
leveneTest(sqrtMnSOD ~Treatment, data=data)
#normality
shapiro.test(data$sqrtMnSOD )

#anova
MnSODANOVA <- aov(sqrtMnSOD ~ Treatment, data = data)
MnSODANOVA
summary(MnSODANOVA)
###comparision between treatments
TukeyHSD(MnSODANOVA)

letters_MnSOD<- emmeans(MnSODANOVA, list(pairwise~Treatment), adjust="Tukey")
cld(letters_MnSOD, details= FALSE, source= FALSE, alpha= 0.05, Letters= LETTERS, adjust= "Tukey")
############### CS

mCS<-lm(CS~Treatment, data=data)
dCS <- data.frame(residuals = residuals(mCS),
                     std_residuals = rstudent(mCS),
                     fitted = fitted(mCS),
                     cooks = cooks.distance(mCS))
dCS <- mutate(dCS, obs = 1:n())

ggplot(data = dCS, aes(sample = std_residuals)) +
  stat_qq() +
  stat_qq_line()

#assess homegenity of variance
leveneTest(CS~Treatment, data=data)

#normality
shapiro.test(data$CS)

#transformation
data$logCS <- log(data$CS)

# transformed data
#assess homegenity of variance
leveneTest(logCS ~Treatment, data=data)
#normality
shapiro.test(data$logCS )

#anova
CSANOVA <- aov(logCS ~ Treatment, data = data)
CSANOVA
summary(CSANOVA)
###comparision between treatments
TukeyHSD(CSANOVA)

letters_CS<- emmeans(CSANOVA, list(pairwise~Treatment), adjust="Tukey")
cld(letters_CS, details= FALSE, source= FALSE, alpha= 0.05, Letters= LETTERS, adjust= "Tukey")

###########RPS4
mRPS4<-lm(RPS4~Treatment, data=data)
dRPS4 <- data.frame(residuals = residuals(mRPS4),
                  std_residuals = rstudent(mRPS4),
                  fitted = fitted(mRPS4),
                  cooks = cooks.distance(mRPS4))
dRPS4<- mutate(dRPS4, obs = 1:n())

ggplot(data = dRPS4, aes(sample = std_residuals)) +
  stat_qq() +
  stat_qq_line()

#assess homegenity of variance
leveneTest(RPS4~Treatment, data=data)

#normality
shapiro.test(data$RPS4)

#anova
RPS4ANOVA <- aov(RPS4 ~ Treatment, data = data)
RPS4ANOVA
summary(RPS4ANOVA)
###comparision between treatments
TukeyHSD(RPS4ANOVA)

#letters_RPS4<- emmeans(RPS4ANOVA, list(pairwise~Treatment), adjust="fdr")
#cld(letters_RPS4, details= FALSE, source= FALSE, alpha= 0.05, Letters= LETTERS, adjust= "fdr")

##########RPL18A
mRPL18A<-lm(RPL18A~Treatment, data=data)
dRPL18A <- data.frame(residuals = residuals(mRPL18A),
                    std_residuals = rstudent(mRPL18A),
                    fitted = fitted(mRPL18A),
                    cooks = cooks.distance(mRPL18A))
dRPL18A<- mutate(dRPL18A, obs = 1:n())

ggplot(data = dRPL18A, aes(sample = std_residuals)) +
  stat_qq() +
  stat_qq_line()

#assess homegenity of variance
leveneTest(RPL18A~Treatment, data=data)

#normality
shapiro.test(data$RPL18A)

#transformation
data$logRPL18A <- log(data$RPL18A)

#assess homegenity of variance
leveneTest(logRPL18A~Treatment, data=data)

#normality
shapiro.test(data$logRPL18A)
#anova
RPL18AANOVA <- aov(logRPL18A ~ Treatment, data = data)
RPL18AANOVA
summary(RPL18AANOVA)
###comparision between treatments
TukeyHSD(RPL18AANOVA)

#letters_RPL18A<- emmeans(RPL18AANOVA, list(pairwise~Treatment), adjust="fdr")
#cld(letters_RPL18A, details= FALSE, source= FALSE, alpha= 0.05, Letters= LETTERS, adjust= "fdr")

###### CAT
mCAT<-lm(CAT~Treatment, data=data)
dCAT <- data.frame(residuals = residuals(mCAT),
                      std_residuals = rstudent(mCAT),
                      fitted = fitted(mCAT),
                      cooks = cooks.distance(mCAT))
dCAT<- mutate(dCAT, obs = 1:n())

ggplot(data = dCAT, aes(sample = std_residuals)) +
  stat_qq() +
  stat_qq_line()

#assess homegenity of variance
leveneTest(CAT~Treatment, data=data)

#normality
shapiro.test(data$CAT)

#transformation
data$logCAT<- log(data$CAT)

#assess homegenity of variance
leveneTest(logCAT~Treatment, data=data)

#normality
shapiro.test(data$logCAT)
#anova
CATANOVA <- aov(logCAT ~ Treatment, data = data)
CATANOVA
summary(CATANOVA)
###comparision between treatments
TukeyHSD(CATANOVA)

letters_CAT<- emmeans(CATANOVA, list(pairwise~Treatment), adjust="Tukey")

cld(letters_CAT, details= FALSE, source= FALSE, alpha= 0.05, Letters= LETTERS, adjust= "Tukey")

##########make graph
HSP60 <- ggplot(data, aes(x= Treatment, y = HSP60))+
  geom_boxplot()+
  labs(x="Temperature Treatment (C)", y="HSP60 Expression")+
  geom_point()
HSP60
ggsave(plot=HSP60, file= "./Graphs/HSP60.jpeg")
###
HSP90 <- ggplot(data, aes(x= Treatment, y = HSP90))+
  geom_boxplot()+
  labs(x="Temperature Treatment (C)", y="HSP90 Expression")+
  geom_point()
HSP90
ggsave(plot=HSP90, file= "./Graphs/HSP90.jpeg")
###
MnSOD <- ggplot(data, aes(x= Treatment, y = MnSOD))+
  geom_boxplot()+
  labs(x="Temperature Treatment (C)", y="MnSOD Expression")+
  geom_point()
MnSOD
ggsave(plot=MnSOD, file= "./Graphs/MnSOD.jpeg")
### CS
CS <- ggplot(data, aes(x= Treatment, y = CS))+
  geom_boxplot()+
  labs(x="Temperature Treatment (C)", y="CS Expression")+
  geom_point()
CS
ggsave(plot=CS, file= "./Graphs/CS.jpeg")
#### RPS4
RPS4 <- ggplot(data, aes(x= Treatment, y = RPS4))+
  geom_boxplot()+
  labs(x="Temperature Treatment (C)", y="RPS4 Expression")+
  geom_point()
RPS4
ggsave(plot=RPS4, file= "./Graphs/RPS4.jpeg")
####RPL18A
RPL18A <- ggplot(data, aes(x= Treatment, y = RPL18A))+
  geom_boxplot()+
  labs(x="Temperature Treatment (C)", y="RPL18A Expression")+
  geom_point()


RPL18A

ggsave(plot=RPL18A, file= "./Graphs/RPL18A.jpeg")

###CAT
CAT <- ggplot(data, aes(x= Treatment, y = CAT))+
  geom_boxplot()+
  labs(x="Temperature Treatment (C)", y="CAT Expression")+
  geom_point()
CAT
ggsave(plot=CAT, file= "./Graphs/CAT.jpeg")
###identify outlier in data
outHSP60 <- boxplot.stats(data$HSP60)$out
out_ind_HSP60 <- which(data$HSP60 %in% c(outHSP60))
out_ind_HSP60

outHSP90 <- boxplot.stats(data$HSP90)$out
out_ind_HSP90 <- which(data$HSP90 %in% c(outHSP90))
out_ind_HSP90

outMnSOD <- boxplot.stats(data$MnSOD)$out
out_ind_MnSOD <- which(data$MnSOD %in% c(outMnSOD))
out_ind_MnSOD

outCS <- boxplot.stats(data$CS)$out
out_ind_CS <- which(data$CS %in% c(outCS))
out_ind_CS


outCAT <- boxplot.stats(data$CAT)$out
out_ind_CAT <- which(data$CAT %in% c(outCAT))
out_ind_CAT

outRPL18A <- boxplot.stats(data$RPL18A)$out
out_ind_RPL18A <- which(data$RPL18A %in% c(outRPL18A))
out_ind_RPL18A
######graphs with letters
library(datasets)
library(ggplot2)
library(multcompView)
library(dplyr)
dt1 <- read_xlsx("Delta_Ct.xlsx", sheet= "2^dCT")
dt1$Treatment<-as.character(dt1$Treatment)
DT<- read_csv ("SUM_TABLE_LT.csv") ###USE TABLE THAT ADDED LETTERS
DT$Treatment<-as.character(DT$Treatment)
DT<- mutate(DT, Treatment = factor(Treatment, levels = c("7", "10", "13", "16", "19", "22"))) 

# extracting the compact letter display and adding to the Tk table



HSP60_LT <- ggplot(dt1, aes(Treatment, HSP60)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_point(alpha=0.5)+
  labs(x="Temperature (°C) ", y="HSP60 mRNA Expression") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank())  +
  theme(axis.line = element_line(color = "black"))+
  geom_text(data = DT,aes(x = Treatment, y = quant_HSP90, label = LT_HSP60, vjust=-1, hjust =-1))
HSP60_LT
ggsave(plot=HSP60_LT, file= "./Graphs/HSP60LT.jpeg")


HSP90_LT <- ggplot(dt1, aes(Treatment, HSP90)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_point(alpha=0.5)+
  labs(x="Temperature (°C)", y="HSP90 mRNA Expression") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank())  +
  theme(axis.line = element_line(color = "black"))+
  geom_text(data = DT, aes(x = Treatment, y = quant_HSP90, label = LT_HSP90, vjust=-1, hjust =-1))
HSP90_LT
ggsave(plot=HSP90_LT, file= "./Graphs/HSP90LT.jpeg")

MnSOD_LT <- ggplot(dt1, aes(Treatment, MnSOD)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_point(alpha=0.5)+
  labs(x="Temperature (°C)", y="MnSOD mRNA Expression") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank())  +
  theme(axis.line = element_line(color = "black")) +
  geom_text(data = DT, aes(x = Treatment, y = quant_MnSOD, label = LT_MnSOD, vjust=-1, hjust =-1))
MnSOD_LT
ggsave(plot=MnSOD_LT, file= "./Graphs/MnSODLT.jpeg")


CS_LT <- ggplot(dt1, aes(Treatment, CS)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_point(alpha=0.5)+
  labs(x="Temperature (°C)", y="CS mRNA Expression") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank())  +
  theme(axis.line = element_line(color = "black"))+
  geom_text(data = DT, aes(x = Treatment, y = quant_CS, label = LT_CS, vjust=-1, hjust =-1))

ggsave(plot=CS_LT, file= "./Graphs/CSLT.jpeg")

CAT_LT <- ggplot(dt1, aes(Treatment, CAT)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_point(alpha=0.5)+
  labs(x="Temperature (°C)", y="CAT mRNA Expression") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank())  +
  theme(axis.line = element_line(color = "black"))+
  geom_text(data = DT, aes(x = Treatment, y = quant_CAT, label = LT_CAT, vjust=-1, hjust =-1))

ggsave(plot=CAT_LT, file= "./Graphs/CATLT.jpeg")





