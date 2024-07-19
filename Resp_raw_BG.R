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
###load and separate data

Cl1_4 <- read_excel("./Mydata/0804_edited.xlsx") ##row with invalid measurements were removed
Cl5_8 <- read_excel("./Mydata/0805.xlsx")

Cl9_12 <- read_excel("./Mydata/0806.xlsx")
Cl13_16 <- read_excel("./Mydata/0810.xlsx")

Cl17_20 <- read_excel("./Mydata/0811.xlsx")
Cl21_24 <- read_excel("./Mydata/0812.xlsx")

Cl25_28 <- read_excel("./Mydata/0813.xlsx")
Cl29_32 <- read_excel("./Mydata/0816.xlsx")

Cl33_36 <- read_excel("./Mydata/0817.xlsx")
Cl37_40 <- read_excel("./Mydata/0818.xlsx")

Cl41_44 <- read_excel("./Mydata/0819.xlsx")
Cl45_48 <- read_excel("./Mydata/0820.xlsx")

###format time
Cl1_4	<-	format_time (	Cl1_4, time = 4, format = "ymdHMS", start = 1) 
Cl5_8	<-	format_time (	Cl5_8, time = 4, format = "ymdHMS", start = 1) 
Cl9_12	<-	format_time (	Cl9_12, time = 4, format = "ymdHMS", start = 1) 
Cl13_16	<-	format_time (	Cl13_16, time = 4, format = "ymdHMS", start = 1) 
Cl17_20	<-	format_time (	Cl17_20, time = 4, format = "ymdHMS", start = 1)                                       
Cl21_24	<-	format_time (	Cl21_24, time = 4, format = "ymdHMS", start = 1) 
Cl25_28	<-	format_time (	Cl25_28, time = 4, format = "ymdHMS", start = 1) 
Cl29_32	<-	format_time (	Cl29_32, time = 4, format = "ymdHMS", start = 1) 

Cl33_36	<-	format_time (	Cl33_36, time = 4, format = "ymdHMS", start = 1) 
Cl37_40	<-	format_time (	Cl37_40, time = 4, format = "ymdHMS", start = 1) 
Cl41_44	<-	format_time (	Cl41_44, time = 4, format = "ymdHMS", start = 1) 
Cl45_48	<-	format_time (	Cl45_48, time = 4, format = "ymdHMS", start = 1) 

###subset for background 

Cl1_4BG <-	subset (Cl1_4, Cl1_4$`Delta T [min]` > 350) 

Cl5_8BG <-	subset (Cl5_8, Cl5_8$`Delta T [min]` > 340)

Cl9_12BG <-	subset (Cl9_12, Cl9_12$`Delta T [min]` > 212 )

Cl13_16BG <-	subset (Cl13_16, Cl13_16$`Delta T [min]` > 250)


Cl17_20BG <-	subset (Cl17_20, Cl17_20$`Delta T [min]` > 200)

Cl21_24BG <-	subset (Cl21_24, Cl21_24$`Delta T [min]` > 260)

Cl25_28BG <-	subset (Cl25_28, Cl25_28$`Delta T [min]` > 300)

Cl29_32BG <-	subset (Cl29_32, Cl29_32$`Delta T [min]` > 220)

Cl33_36BG <-	subset (Cl33_36, Cl33_36$`Delta T [min]` > 260)

Cl37_40BG <-	subset (Cl37_40, Cl37_40$`Delta T [min]` > 230)

Cl41_44BG <-	subset (Cl41_44, Cl41_44$`Delta T [min]` > 270)

Cl45_48BG <-	subset (Cl45_48, Cl45_48$`Delta T [min]` > 320)

#####bg
C1bg<- subset(Cl1_4BG,ChamberID=="1")

C2bg<- subset(Cl1_4BG,ChamberID=="2")
C3bg<- subset(Cl1_4BG,ChamberID=="3")
C4bg<- subset(Cl1_4BG,ChamberID=="4")

C5bg<- subset(Cl5_8BG,ChamberID=="1")
C6bg<- subset(Cl5_8BG,ChamberID=="2")
C7bg<- subset(Cl5_8BG,ChamberID=="3")
C8bg<- subset(Cl5_8BG,ChamberID=="4")

C9bg<- subset(Cl9_12BG,ChamberID=="1")
C10bg<- subset(Cl9_12BG,ChamberID=="2")
C11bg<- subset(Cl9_12BG,ChamberID=="3")
C12bg<- subset(Cl9_12BG,ChamberID=="4")

C13bg<- subset(Cl13_16BG,ChamberID=="1")
C14bg<- subset(Cl13_16BG,ChamberID=="2")

C15bg<- subset(Cl13_16BG,ChamberID=="3")
C16bg<- subset(Cl13_16BG,ChamberID=="4")
C17bg<- subset(Cl17_20BG,ChamberID=="1")
C18bg<- subset(Cl17_20BG,ChamberID=="2")
C19bg<- subset(Cl17_20BG,ChamberID=="3")
C20bg<- subset(Cl17_20BG,ChamberID=="4")

C21bg<- subset(Cl21_24BG,ChamberID=="1")
C22bg<- subset(Cl21_24BG,ChamberID=="2")
C23bg<- subset(Cl21_24BG,ChamberID=="3")
C24bg<- subset(Cl21_24BG,ChamberID=="4")

C25bg<- subset(Cl25_28BG,ChamberID=="1")
C26bg<- subset(Cl25_28BG,ChamberID=="2")
C27bg<- subset(Cl25_28BG,ChamberID=="3")
C28bg<- subset(Cl25_28BG,ChamberID=="4")


C29bg<- subset(Cl29_32BG,ChamberID=="1")
C30bg<- subset(Cl29_32BG,ChamberID=="2")
C31bg<- subset(Cl29_32BG,ChamberID=="3")

C33bg<- subset(Cl33_36BG,ChamberID=="1")
C34bg<- subset(Cl33_36BG,ChamberID=="2")
C35bg<- subset(Cl33_36BG,ChamberID=="3")
C36bg<- subset(Cl33_36BG,ChamberID=="4")

C37bg<- subset(Cl37_40BG,ChamberID=="1")
C38bg<- subset(Cl37_40BG,ChamberID=="2")
C39bg<- subset(Cl37_40BG,ChamberID=="3")
C40bg<- subset(Cl37_40BG,ChamberID=="4")

C41bg<- subset(Cl41_44BG,ChamberID=="1")
C42bg<- subset(Cl41_44BG,ChamberID=="2")
C43bg<- subset(Cl41_44BG,ChamberID=="3")
C44bg<- subset(Cl41_44BG,ChamberID=="4")
C45bg<- subset(Cl45_48BG,ChamberID=="1")
C46bg<- subset(Cl45_48BG,ChamberID=="2")
C47bg<- subset(Cl45_48BG,ChamberID=="3")
C48bg<- subset(Cl45_48BG,ChamberID=="4")

######BG
Cl1bg<-inspect(C1bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()


Cl2bg<-inspect(C2bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
plot(Cl2bg, pos=2)


Cl3bg<-inspect(C3bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl4bg<-inspect(C4bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl5bg<-inspect(C5bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()


Cl6bg<-inspect(C6bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl7bg<-inspect(C7bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl8bg<-inspect(C8bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl9bg<-inspect(C9bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

C9bg

Cl10bg<-inspect(C10bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl11bg<-inspect(C11bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl12bg<-inspect(C12bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

C13bg <- C13bg %>%drop_na(Oxygen)
Cl13bg<-inspect(C13bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl14bg<-inspect(C14bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

C15bg <- C15bg %>% drop_na(Oxygen)
Cl15bg<-inspect(C15bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

C16bg <- C16bg %>% drop_na(Oxygen)
Cl16bg<-inspect(C16bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl17bg<-inspect(C17bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

C18bg <- C18bg %>% drop_na(Oxygen)
Cl18bg<-inspect(C18bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
plot(Cl18bg, pos=2)
Cl19bg<-inspect(C19bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()


Cl20bg<-inspect(C20bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl21bg<-inspect(C21bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

C22bg <- C22bg %>% drop_na(Oxygen)
Cl22bg<-inspect(C22bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl23bg<-inspect(C23bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl24bg<-inspect(C24bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()


Cl25bg<-inspect(C25bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

C26bg <-C26bg%>%drop_na(Oxygen)
Cl26bg<-inspect(C26bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
C27bg <-C27bg%>%drop_na(Oxygen)
Cl27bg<-inspect(C27bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl28bg<-inspect(C28bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl29bg<-inspect(C29bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl30bg<-inspect(C30bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl31bg<-inspect(C31bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl33bg<-inspect(C33bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()


Cl34bg<-inspect(C34bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()


Cl35bg<-inspect(C35bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()


Cl36bg<-inspect(C36bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl37bg<-inspect(C37bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl38bg<-inspect(C38bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl39bg<-inspect(C39bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl40bg<-inspect(C40bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()


Cl41bg<-inspect(C41bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl42bg<-inspect(C42bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()


Cl43bg<-inspect(C43bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()


Cl44bg<-inspect(C44bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl45bg<-inspect(C45bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()


Cl46bg<-inspect(C46bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl47bg<-inspect(C47bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl48bg<-inspect(C48bg, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

### calculate effective volume
install.packages("devtools")
devtools::install_github("nicholascarey/respfun")
library(respfun)

###convert rate to mass specific at INT
DT_VOL <- read_excel("./Mydata/Rimouski_Sum_all_raw_newBG.xlsx")

EFF_VOL<- eff_vol(resp_vol = DT_VOL$Chamber_volume_l, # total respirometer vol in L
                  spec_vol = NULL, 
                  spec_mass = DT_VOL$W_whole_kg, # specimen mass in kg
                  spec_density = NULL,
                  t = DT_VOL$Temp, # C
                  S = DT_VOL$Sal_ppt, # ppt
                  P = 1.013253) # default
EFF_VOL
####

###convert to mass specific_INT
Trt7_INT <- read_excel("./Mydata/Trt7_newBG.xlsx")
Trt7INT<-convert_rate(
  Trt7_INT $INT_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt7_INT$eff_vol_L,
  mass = Trt7_INT$W_whole_kg,
  S = Trt7_INT$Sal_ppt,
  t = Trt7_INT$Temp_int,
  P = 1.013253
) %>%
  summary()

Trt7INT<-convert_rate(
  Trt7_INT $INT_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt7_INT$eff_vol_L,
  mass = Trt7_INT$W_whole_kg,
  S = 26.8,
  t = 7.5,
  P = 1.013253
) %>%
  summary()
###Trt 10
Trt10_INT <- read_excel("./Mydata/Trt10_newBG.xlsx")
Trt10INT<-convert_rate(
  Trt10_INT $INT_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt10_INT$eff_vol_L,
  mass = Trt10_INT$W_whole_kg,
  S = Trt10_INT$Sal_ppt,
  t = Trt10_INT$Temp_int,
  P = 1.013253
) %>%
  summary()

Trt10INT<-convert_rate(
  Trt10_INT$INT_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt10_INT$eff_vol_L,
  mass = Trt10_INT$W_whole_kg,
  S = 27.3,
  t = 7.6,
  P = 1.013253
) %>%
  summary()
###TRT13
Trt13_INT <- read_excel("./Mydata/Trt13_newBG.xlsx")
Trt13INT<-convert_rate(
  Trt13_INT $INT_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt13_INT$eff_vol_L,
  mass = Trt13_INT$W_whole_kg,
  S = Trt13_INT$Sal_ppt,
  t = Trt13_INT$Temp_int,
  P = 1.013253
) %>%
  summary()
Trt13INT[[7]]

Trt13INT<-convert_rate(
  Trt13_INT $INT_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt13_INT$eff_vol_L,
  mass = Trt13_INT$W_whole_kg,
  S = 27.1,
  t = 7.6,
  P = 1.013253
) %>%
  summary()
Trt13INT[[7]]
##Trt16
Trt16_INT <- read_excel("./Mydata/Trt16_newBG.xlsx")
Trt16INT<-convert_rate(
  Trt16_INT $INT_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt16_INT$eff_vol_L,
  mass = Trt16_INT$W_whole_kg,
  S = Trt16_INT$Sal_ppt,
  t = Trt16_INT$Temp_int,
  P = 1.013253
) %>%
  summary()

Trt16INT<-convert_rate(
  Trt16_INT $INT_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt16_INT$eff_vol_L,
  mass = Trt16_INT$W_whole_kg,
  S = 27.1,
  t = 7.6,
  P = 1.013253
) %>%
  summary()
##Trt19
Trt19_INT <- read_excel("./Mydata/Trt19_newBG.xlsx")
Trt19INT<-convert_rate(
  Trt19_INT $INT_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt19_INT$eff_vol_L,
  mass = Trt19_INT$W_whole_kg,
  S = Trt19_INT$Sal_ppt,
  t = Trt19_INT$Temp_int,
  P = 1.013253
) %>%
  summary()
Trt19INT[[7]]

Trt19INT<-convert_rate(
  Trt19_INT $INT_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt19_INT$eff_vol_L,
  mass = Trt19_INT$W_whole_kg,
  S = 27.1,
  t = 8.4,
  P = 1.013253
) %>%
  summary()

##Trt22
Trt22_INT <- read_excel("./Mydata/Trt22_newBG.xlsx")
Trt22INT<-convert_rate(
  Trt22_INT $INT_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt22_INT$eff_vol_L,
  mass = Trt22_INT$W_whole_kg,
  S = Trt22_INT$Sal_ppt,
  t = Trt22_INT$Temp_int,
  P = 1.013253
) %>%
  summary()

Trt22INT<-convert_rate(
  Trt22_INT $INT_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt22_INT$eff_vol_L,
  mass = Trt22_INT$W_whole_kg,
  S = 27,
  t = 7.6,
  P = 1.013253
) %>%
  summary()
###convert to mass specific_MD
Trt7_MD <- read_excel("./Mydata/Trt7_newBG.xlsx")
Trt7MD<-convert_rate(
  Trt7_MD $MD_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt7_MD$eff_vol_L,
  mass = Trt7_MD$W_whole_kg,
  S = Trt7_MD$Sal_ppt,
  t = Trt7_MD$Temp,
  P = 1.013253
) %>%
  summary()

Trt7_MD <- read_excel("./Mydata/Trt7_newBG.xlsx")
Trt7MD<-convert_rate(
  Trt7_MD $MD_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt7_MD$eff_vol_L,
  mass = Trt7_MD$W_whole_kg,
  S = 26.8,
  t = 7.5,
  P = 1.013253
) %>%
  summary()



###Trt 10
Trt10_MD <- read_excel("./Mydata/Trt10_newBG.xlsx")
Trt10MD<-convert_rate(
  Trt10_MD $MD_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt10_MD$eff_vol_L,
  mass = Trt10_MD$W_whole_kg,
  S = Trt10_MD$Sal_ppt,
  t = Trt10_MD$Temp,
  P = 1.013253
) %>%
  summary()

Trt10MD<-convert_rate(
  Trt10_MD $MD_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt10_MD$eff_vol_L,
  mass = Trt10_MD$W_whole_kg,
  S = 27.3,
  t = 10.8,
  P = 1.013253
) %>%
  summary()

###TRT13
Trt13_MD <- read_excel("./Mydata/Trt13_newBG.xlsx")
Trt13MD<-convert_rate(
  Trt13_MD $MD_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt13_MD$eff_vol_L,
  mass = Trt13_MD$W_whole_kg,
  S = Trt13_MD$Sal_ppt,
  t = Trt13_MD$Temp,
  P = 1.013253
) %>%
  summary()

Trt13_MD <- read_excel("./Mydata/Trt13_newBG.xlsx")
Trt13MD<-convert_rate(
  Trt13_MD $MD_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt13_MD$eff_vol_L,
  mass = Trt13_MD$W_whole_kg,
  S = 27.1,
  t = 13.8,
  P = 1.013253
) %>%
  summary()
Trt13MD[[7]]

##Trt16
Trt16_MD <- read_excel("./Mydata/Trt16_newBG.xlsx")
Trt16MD<-convert_rate(
  Trt16_MD $MD_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt16_MD$eff_vol_L,
  mass = Trt16_MD$W_whole_kg,
  S = Trt16_MD$Sal_ppt,
  t = Trt16_MD$Temp,
  P = 1.013253
) %>%
  summary()


Trt16_MD <- read_excel("./Mydata/Trt16_newBG.xlsx")
Trt16MD<-convert_rate(
  Trt16_MD $MD_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt16_MD$eff_vol_L,
  mass = Trt16_MD$W_whole_kg,
  S = 27.1,
  t = 16.9,
  P = 1.013253
) %>%
  summary()
##Trt19
Trt19_MD <- read_excel("./Mydata/Trt19_newBG.xlsx")
Trt19MD<-convert_rate(
  Trt19_MD $MD_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt19_MD$eff_vol_L,
  mass = Trt19_MD$W_whole_kg,
  S = Trt19_MD$Sal_ppt,
  t = Trt19_MD$Temp,
  P = 1.013253
) %>%
  summary()

Trt19MD<-convert_rate(
  Trt19_MD $MD_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt19_MD$eff_vol_L,
  mass = Trt19_MD$W_whole_kg,
  S = 27.1,
  t = 19.6,
  P = 1.013253
) %>%
  summary()


##Trt22
Trt22_MD <- read_excel("./Mydata/Trt22_newBG.xlsx")
Trt22MD<-convert_rate(
  Trt22_MD $MD_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt22_MD$eff_vol_L,
  mass = Trt22_MD$W_whole_kg,
  S = Trt22_MD$Sal_ppt,
  t = Trt22_MD$Temp,
  P = 1.013253
) %>%
  summary()

Trt22MD<-convert_rate(
  Trt22_MD $MD_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt22_MD$eff_vol_L,
  mass = Trt22_MD$W_whole_kg,
  S = 27.0,
  t = 23.2,
  P = 1.013253
) %>%
  summary()
###data analysis

dt <-read_excel("./Mydata/Rimouski_Sum_all_raw_newBG.xlsx")
dt <- dt %>% group_by (Treatment)


data_delta <- dt


data_delta <- mutate(data_delta, Treatment = factor(Treatment, levels = c("7", "10", "13", "16", "19", "22")))
mo1 <- lm( Delta ~ Treatment , data =data_delta)
f <- data.frame(residuals = residuals(mo1),
                std_residuals = rstudent(mo1),
                fitted = fitted(mo1),
                cooks = cooks.distance(mo1))
f <- mutate(f, obs = 1:n())

#assess homegenity of variance
leveneTest(Delta~Treatment, data=data_delta)

#normality
shapiro.test(data_delta$Delta)
##not a normal distribution

library(emmeans)
anova(mo1)
summary(mo1)
emm_sp <- emmeans(mo1, specs = "Treatment")
emm_sp
c<-pairs(emm_sp)
c<-as.data.frame(c)
c ## no significance was found
g <-data_delta %>%
  group_by(Treatment) %>%
  get_summary_stats (Delta, type = "mean_sd")%>%
  as.data.frame()
g

g1 <-ggplot(data=data_delta, aes(x= Treatment, y=Delta))+
  geom_boxplot()+
  geom_point(alpha=0.5)+
  theme_classic()+
  labs(
       x = "Treatments")+
  theme(text = element_text(size=14))+
  theme(axis.text=element_text(size=14))
g1
ggsave(plot= g1, filename = "output/Result_graph/Deltaplot_raw_all_values_newBG.png")

#### remove outlier in plot not in the analysis
g_no <-ggplot(data=data_delta, aes(x= Treatment, y=Delta, fill= Treatment))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(alpha=0.5)+
  labs(title = "Oxygen Consumption at Elevating Temperatures - Southern population",
       x = "Treatments",
       y = "Delta_Oxygen_Consumption_mgO2/hour/kg")
g_no
g_no2 <- g_no + scale_colour_manual(values=c("lightblue","pink", "green", 'orange', "red", "brown"))

g_no2
ggsave(plot= g_no2, filename = "output/Result_graph/Deltaplot_raw_all_values_no_outlier_shape_newBG.png")

###no geom_point
gbox <-data_delta %>%
  group_by(Treatment) %>%
  get_summary_stats (Delta, type = "mean_sd")%>%
  as.data.frame()
gbox

gbox <-ggplot(data=data_delta, aes(x= Treatment, y=Delta, fill= Treatment))+
  geom_boxplot()+
  labs(title = "Oxygen Consumption at Elevating Temperatures - Southern population",
       x = "Treatments",
       y = "Delta_Oxygen_Consumption_mgO2/hour/kg")
gbox
gbox2 <- gbox + scale_colour_manual(values=c("lightblue","pink", "green", 'orange', "red", "brown"))
ggsave(plot= gbox2, filename = "output/Result_graph/Deltaplot_raw_all_values_no_geom_point_newBG.png")
gbox2
#####no geompoint, no_outlier shape
gno_no <-data_delta %>%
  group_by(Treatment) %>%
  get_summary_stats (Delta, type = "mean_sd")%>%
  as.data.frame()
gno_no

gno_no1 <-ggplot(data=data_delta, aes(x= Treatment, y=Delta, fill= Treatment))+
  geom_boxplot(outlier.shape = NA)+
  labs(title = "Oxygen Consumption at Elevating Temperatures - Southern population",
       x = "Treatments",
       y = "Delta_Oxygen_Consumption_mgO2/hour/kg")
gno_no1
gno_no2 <- gno_no1 + scale_colour_manual(values=c("lightblue","pink", "green", 'orange', "red", "brown"))
ggsave(plot= gno_no2, filename = "output/Result_graph/Deltaplot_raw_all_values_no_geom_no_outliershape_newBG.png")

######
DT <-read_excel("./Mydata/Rimouski_Sum_all_raw_newBG.xlsx")

DT$Treatment <-as.factor(DT$Treatment)
DT <- DT %>% 
  rename(
    "7°C" = INT_converted_possitive,
    Target = MD_converted_posstive
  )
DT <- DT %>%
  gather(key = "time", value = "Oxygen",  "7°C", Target) %>%
  convert_as_factor(Clam_ID, time)
tail(DT)
DT %>%
  group_by(Treatment, time) %>%
  get_summary_stats(Oxygen, type = "mean_sd")

##USE LINEAR MODEL
model <-lm(Oxygen ~ Treatment*time, data=DT)
summary(model)
anova(model)


emm_sp <- emmeans(model, specs = c("Treatment"))
emm_sp
cp1<-pairs(emm_sp)
cp1 ###no significance was found
# pairwise comparisons
library(xlsx)
write.xlsx(file="output/Result_tables/pairwise_newBG.xlsx",cp1)

#stat <-dt_trt %>%
#group_by(Trt,time) %>%
#get_summary_stats (Oxygen, type = "mean_sd")
#stat
library(ggpubr)
DT <- mutate(DT, Treatment = factor(Treatment, levels = c("7", "10", "13", "16", "19", "22")))
box <- ggplot(data = DT, aes(x = time, y = Oxygen, colour= Treatment)) +
  geom_boxplot(outlier.shape = NA) +
  scale_colour_manual(values=c("grey60","grey50", "grey40", 'grey30', "grey20", "grey1"))+
  geom_point(position=position_dodge(width=0.75),aes(group=Treatment),alpha = 0.6) +
  labs(
    x = "Time") +
  theme_classic() +
  theme(text = element_text(size=14))+
  theme(axis.text=element_text(size=14))

box
ggsave(plot=box, filename= "output/Result_graph/before_after_plot_newBG.png")

bxp1 <- ggboxplot (DT, x= "time", y = "Oxygen", color= "Treatment", palette= "jco"
) +
  theme(text = element_text(size=14))

bxp1
bxp1<-bxp1 + labs(title = "Oxygen Consumption Before and After Ramping",
                x = "Time",
                y = "mgO2/hour/kg",
                colour = "Treatment")
bxp1
ggsave(plot=bxp1, filename= "output/Result_graph/before_after_plot_withoutliershape_newBG.png")

#### create subset of each treatment
DT <- group_by(DT, Treatment, time)
DT$time <- as.character(DT$time)
trt7 <-DT[which(DT$Treatment=='7'),]
trt7$time[trt7$time == "Target"] <- "7°C(target)"
trt7 <- mutate(trt7, time = factor(time, levels = c ("7°C", "7°C(target)")))

trt10 <-DT[which(DT$Treatment=='10'),]
trt10$time[trt10$time == "Target"] <- "10°C"
trt10 <- mutate(trt10, time = factor(time, levels = c ("7°C", "10°C")))

trt13 <-DT[which(DT$Treatment=='13'),]
trt13$time[trt13$time == "Target"] <- "13°C"
trt13 <-mutate(trt13, time = factor(time, levels = c ("7°C", "13°C")))

trt16 <-DT[which(DT$Treatment=='16'),]
trt16$time[trt16$time == "Target"] <- "16°C"
trt16 <-mutate(trt16, time = factor(time, levels = c ("7°C", "16°C")))

trt19 <-DT[which(DT$Treatment=='19'),]
trt19$time[trt19$time == "Target"] <- "19°C"
trt19 <- mutate(trt19, time = factor(time, levels = c ("7°C", "19°C")))

trt22 <-DT[which(DT$Treatment=='22'),]
trt22$time[trt22$time == "Target"] <- "22°C"
trt22 <- mutate(trt22, time = factor(time, levels = c ("7°C", "22°C")))

g7<-ggplot(data=trt7,aes(x = time, y = Oxygen))  +
  geom_point(aes(group= Clam_ID), shape =21) +
  geom_line(aes(group=Clam_ID))+
  theme_classic()
g7<-g7+
  scale_y_continuous(limits = c(0, 40))+
  theme(axis.text=element_text(size=12),                                            axis.title=element_text(size=12),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.tag.position = "top")
g7


g10<-ggplot(data=trt10,aes(x = time, y = Oxygen))  +
  geom_point(aes(group= Clam_ID), shape =21) +
  geom_line(aes(group=Clam_ID))+
  theme_classic()
g10<-g10 +
  scale_y_continuous(limits = c(0, 40))+
  theme(axis.text=element_text(size=12),                                            axis.title=element_text(size=12),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.tag.position = "top")
g10


g13<-ggplot(data=trt13,aes(x = time, y = Oxygen))  +
  geom_point(aes(group= Clam_ID), shape =21) +
  geom_line(aes(group=Clam_ID))+
  theme_classic()
g13<-g13 +
  scale_y_continuous(limits = c(0, 40))+
  theme(axis.text=element_text(size=12),                                            axis.title=element_text(size=12),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.tag.position = "top")
g13


g16<-ggplot(data=trt16,aes(x = time, y = Oxygen))  +
  geom_point(aes(group= Clam_ID), shape =21) +
  geom_line(aes(group=Clam_ID))+
  theme_classic()
g16<-g16 +
  scale_y_continuous(limits = c(0, 40))+
  theme(axis.text=element_text(size=12),                                            axis.title=element_text(size=12),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.tag.position = "top")
g16

g19<-ggplot(data=trt19,aes(x = time, y = Oxygen))  +
  geom_point(aes(group= Clam_ID), shape =21) +
  geom_line(aes(group=Clam_ID))+
  theme_classic()
g19<-g19 +
  theme(axis.text=element_text(size=12),                                            axis.title=element_text(size=12),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.tag.position = "top")
g19

g22<-ggplot(data=trt22,aes(x = time, y = Oxygen))  +
  geom_point(aes(group= Clam_ID), shape =21) +
  geom_line(aes(group=Clam_ID))+
  theme_classic()
g22<-g22 +
  scale_y_continuous(limits = c(0, 40))+
  theme(axis.text=element_text(size=12),                                            axis.title=element_text(size=12),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.tag.position = "top")
g22

library(ggpubr)
ggarrange(g7, g10, g13, g16, g19, g22, 
          ncol = 2, nrow = 3)

fig<-ggarrange(g7 + rremove("ylab") + rremove("xlab"), g10 + rremove("ylab") + rremove("xlab"), g13 + rremove("ylab") + rremove("xlab"), g16+ rremove("ylab") + rremove("xlab"), g19+ rremove("ylab") + rremove("xlab"), g22+ rremove("ylab") + rremove("xlab"),# remove axis labels from plots
               labels = NULL,
               ncol = 2, nrow = 3,
               common.legend = TRUE, legend = "bottom",
               #align = "hv", 
               font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top")) +
  theme(axis.text=element_text(size=14))
fig

all<-annotate_figure(fig,
                     bottom = text_grob("Measuring Period", color = "black", size = 14))
 ggsave(plot=fig, file="./output/Result_graph/trt_pannels_newBG.jpeg", height= 10, width = 6.8, units="in")
all
#### BG plot
dt <-read_excel("./Mydata/Rimouski_Sum_all_raw_newBG.xlsx")
dt$Treatment <- as.character(dt$Treatment)
dt <- dt %>% group_by (Treatment)
dt<- mutate(dt, Treatment = factor(Treatment, levels = c("7", "10", "13", "16", "19", "22")))

box_bg <- ggplot(data = dt, aes(x = Treatment, y = (BG_at_target*(-1)))) +
  geom_boxplot() +
  theme(text = element_text(size=14))+
  geom_point(alpha = 0.6) +
  labs(
    y = "Rate (unitless)",
    colour = "Treatment") +
  theme_classic()+
  theme(text=element_text(size=14))+
  theme(axis.text=element_text(size=14))
box_bg
ggsave(plot=box_bg, file="./output/Result_graph/Rimouski_BG_rate_newBG.jpeg")

