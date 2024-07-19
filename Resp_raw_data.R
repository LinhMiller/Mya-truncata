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


#Cl17_20$Trt	<-c("Control")## add treatment info
#Cl25_28$Trt	<-c("22")## add treatment info

###subset data for INT measurement
Cl1_4INT <-	subset (Cl1_4, Cl1_4$`Delta T [min]` <100 ) 
Cl5_8INT <-	subset (Cl5_8, Cl5_8$`Delta T [min]` <100 )
Cl9_12INT <-	subset (Cl9_12, Cl9_12$`Delta T [min]` < 62 )
Cl13_16INT <-	subset (Cl13_16, Cl13_16$`Delta T [min]` < 90 )
Cl17_20INT <-	subset (Cl17_20, Cl17_20$`Delta T [min]` <70 )
Cl21_24INT <-	subset (Cl21_24, Cl21_24$`Delta T [min]` <70 )
Cl25_28INT <-	subset (Cl25_28, Cl25_28$`Delta T [min]` <100 )
Cl29_32INT <-	subset (Cl29_32, Cl29_32$`Delta T [min]` <70 )
Cl33_36INT <-	subset (Cl33_36, Cl33_36$`Delta T [min]` <70 )
Cl37_40INT <-	subset (Cl37_40, Cl37_40$`Delta T [min]` <70 )
Cl41_44INT <-	subset (Cl41_44, Cl41_44$`Delta T [min]` <100 )
Cl45_48INT <-	subset (Cl45_48, Cl45_48$`Delta T [min]` <70 )
####

####drop the first 15 mins

Cl1_4INT <-	Cl1_4INT [(	Cl1_4INT	$time_num	>	800),]
Cl5_8INT <-	Cl5_8INT [(	Cl5_8INT	$time_num	>	900),]
Cl9_12INT <-	Cl9_12INT [(Cl9_12INT	$time_num	>	900),]
Cl13_16INT <-	Cl13_16INT [(Cl13_16INT	$time_num	>	800),]
Cl17_20INT <-	Cl17_20INT [(Cl17_20INT	$time_num	>	900),]
Cl21_24INT <-	Cl21_24INT [(Cl21_24INT	$time_num	>	800),]
Cl25_28INT <-	Cl25_28INT [(Cl25_28INT	$time_num	>	900),]
Cl29_32INT <-	Cl29_32INT [(Cl29_32INT	$time_num	>	800),]
Cl33_36INT <-	Cl33_36INT [(Cl33_36INT	$time_num	>	900),]
Cl37_40INT <-	Cl37_40INT [(Cl37_40INT	$time_num	>	800),]
Cl41_44INT <-	Cl41_44INT [(Cl41_44INT	$time_num	>	900),]
Cl45_48INT <-	Cl45_48INT [(Cl45_48INT	$time_num	>	900),]

###subset 2hr and  eliminate the first measurement
Cl1_4MD <-	subset (Cl1_4, Cl1_4$`Delta T [min]` <350 &  Cl1_4$`Delta T [min]`> 200)
Cl1_4MD <- Cl1_4MD %>% filter(!is.na(Oxygen))
Cl1_4MD <- Cl1_4MD[(Cl1_4MD$time_num>11000),] 

Cl5_8MD <-	subset (Cl5_8, Cl5_8$`Delta T [min]` <335  & Cl5_8$`Delta T [min]`> 200)
Cl5_8MD <- Cl5_8MD[(Cl5_8MD$time_num>13000),] 

Cl9_12MD <-	subset (Cl9_12, Cl9_12$`Delta T [min]` < 212 & Cl9_12$`Delta T [min]`>62)
Cl9_12MD <- Cl9_12MD[(Cl9_12MD$time_num>10000),] 

Cl13_16MD <-	subset (Cl13_16, Cl13_16$`Delta T [min]` < 250 & Cl13_16$`Delta T [min]`>100)
Cl13_16MD <- Cl13_16MD[(Cl13_16MD$time_num>8000),] 

Cl17_20MD <-	subset (Cl17_20, Cl17_20$`Delta T [min]` < 200& Cl17_20$`Delta T [min]`>70)
Cl17_20MD <- Cl17_20MD[(Cl17_20MD$time_num>5000),] 

Cl21_24MD <-	subset (Cl21_24, Cl21_24$`Delta T [min]` < 260 & Cl21_24$`Delta T [min]` >70 )
Cl21_24MD <- Cl21_24MD[(Cl21_24MD$time_num>9000),] 

Cl25_28MD <-	subset (Cl25_28, Cl25_28$`Delta T [min]` < 300 & Cl25_28$`Delta T [min]`>100)
Cl25_28MD <- Cl25_28MD[(Cl25_28MD$time_num>11000),] 

Cl29_32MD <-	subset (Cl29_32, Cl29_32$`Delta T [min]` < 220 & Cl29_32$`Delta T [min]`>90)
Cl29_32MD <- Cl29_32MD[(Cl29_32MD$time_num>6000),] 

Cl33_36MD <-	subset (Cl33_36, Cl33_36$`Delta T [min]` < 260 & Cl33_36$`Delta T [min]`>100)
Cl33_36MD <- Cl33_36MD[(Cl33_36MD$time_num>10000),] 

Cl37_40MD <-	subset (Cl37_40, Cl37_40$`Delta T [min]` < 230 & Cl37_40$`Delta T [min]`>100)
Cl37_40MD <- Cl37_40MD[(Cl37_40MD$time_num>7000),] 

Cl41_44MD <-	subset (Cl41_44, Cl41_44$`Delta T [min]` < 260 & Cl41_44$`Delta T [min]`>100)
Cl41_44MD <- Cl41_44MD[(Cl41_44MD$time_num>5000),] 

Cl45_48MD <-	subset (Cl45_48, Cl45_48$`Delta T [min]` < 320 & Cl45_48$`Delta T [min]`>180)
Cl45_48MD <- Cl45_48MD[(Cl45_48MD$time_num>12000),] 
####
###subset for background and eliminate the first measurement

Cl1_4BG <-	subset (Cl1_4, Cl1_4$`Delta T [min]` > 350) 
Cl1_4BG <- Cl1_4BG[(Cl1_4BG$time_num>20000),] 


Cl5_8BG <-	subset (Cl5_8, Cl5_8$`Delta T [min]` > 340)
Cl5_8BG <- Cl5_8BG[(Cl5_8BG$time_num>20000),] 

Cl9_12BG <-	subset (Cl9_12, Cl9_12$`Delta T [min]` > 212 )
Cl9_12BG <- Cl9_12BG[(Cl9_12BG$time_num>18000),] 

Cl13_16BG <-	subset (Cl13_16, Cl13_16$`Delta T [min]` > 250)
Cl13_16BG <- Cl13_16BG[(Cl13_16BG$time_num>16000),] 

Cl17_20BG <-	subset (Cl17_20, Cl17_20$`Delta T [min]` > 200)
Cl17_20BG <- Cl17_20BG[(Cl17_20BG$time_num>12550),] 

Cl21_24BG <-	subset (Cl21_24, Cl21_24$`Delta T [min]` > 260)
Cl21_24BG <- Cl21_24BG[(Cl21_24BG$time_num>16500),] 

Cl25_28BG <-	subset (Cl25_28, Cl25_28$`Delta T [min]` > 300)
Cl25_28BG <- Cl25_28BG[(Cl25_28BG$time_num>19000),] 

Cl29_32BG <-	subset (Cl29_32, Cl29_32$`Delta T [min]` > 220)
Cl29_32BG <- Cl29_32BG[(Cl29_32BG$time_num>14000),] 

Cl33_36BG <-	subset (Cl33_36, Cl33_36$`Delta T [min]` > 260)
Cl33_36BG <- Cl33_36BG[(Cl33_36BG$time_num>17000),] 

Cl37_40BG <-	subset (Cl37_40, Cl37_40$`Delta T [min]` > 230)
Cl37_40BG <- Cl37_40BG[(Cl37_40BG$time_num>15000),] 

Cl41_44BG <-	subset (Cl41_44, Cl41_44$`Delta T [min]` > 270)
Cl41_44BG <- Cl41_44BG[(Cl41_44BG$time_num>16000),] 

Cl45_48BG <-	subset (Cl45_48, Cl45_48$`Delta T [min]` > 320)
Cl45_48BG <- Cl45_48BG[(Cl45_48BG$time_num>20000),] 


#dt<- Cl17_20#upload dataframe of the control treatment
#dt <- dt %>% drop_na(Oxygen) ## drop NA values 
#dt$Clam<-as.character(dt$Clam)
#dt <- dt %>%group_by(Clam)

###each clam each period
C1int<- subset(Cl1_4INT,ChamberID=="1")
C2int<- subset(Cl1_4INT,ChamberID=="2")
C3int<- subset(Cl1_4INT,ChamberID=="3")
C4int<- subset(Cl1_4INT,ChamberID=="4")

C5int<- subset(Cl5_8INT,ChamberID=="1")
C6int<- subset(Cl5_8INT,ChamberID=="2")
C7int<- subset(Cl5_8INT,ChamberID=="3")
C8int<- subset(Cl5_8INT,ChamberID=="4")

C9int<- subset(Cl9_12INT,ChamberID=="1")
C10int<- subset(Cl9_12INT,ChamberID=="2")
C11int<- subset(Cl9_12INT,ChamberID=="3")
C12int<- subset(Cl9_12INT,ChamberID=="4")

C13int<- subset(Cl13_16INT,ChamberID=="1")
C14int<- subset(Cl13_16INT,ChamberID=="2")
C15int<- subset(Cl13_16INT,ChamberID=="3")
C16int<- subset(Cl13_16INT,ChamberID=="4")

C17int<- subset(Cl17_20INT,ChamberID=="1")
C18int<- subset(Cl17_20INT,ChamberID=="2")
C19int<- subset(Cl17_20INT,ChamberID=="3")
C20int<- subset(Cl17_20INT,ChamberID=="4")

C21int<- subset(Cl21_24INT,ChamberID=="1")
C22int<- subset(Cl21_24INT,ChamberID=="2")
C23int<- subset(Cl21_24INT,ChamberID=="3")
C24int<- subset(Cl21_24INT,ChamberID=="4")

C25int<- subset(Cl25_28INT,ChamberID=="1")
C26int<- subset(Cl25_28INT,ChamberID=="2")
C27int<- subset(Cl25_28INT,ChamberID=="3")
C28int<- subset(Cl25_28INT,ChamberID=="4")

C29int<- subset(Cl29_32INT,ChamberID=="1")
C30int<- subset(Cl29_32INT,ChamberID=="2")
C31int<- subset(Cl29_32INT,ChamberID=="3")

C33int<- subset(Cl33_36INT,ChamberID=="1")
C34int<- subset(Cl33_36INT,ChamberID=="2")
C35int<- subset(Cl33_36INT,ChamberID=="3")
C36int<- subset(Cl33_36INT,ChamberID=="4")

C37int<- subset(Cl37_40INT,ChamberID=="1")
C38int<- subset(Cl37_40INT,ChamberID=="2")
C39int<- subset(Cl37_40INT,ChamberID=="3")
C40int<- subset(Cl37_40INT,ChamberID=="4")

C41int<- subset(Cl41_44INT,ChamberID=="1")
C42int<- subset(Cl41_44INT,ChamberID=="2")
C43int<- subset(Cl41_44INT,ChamberID=="3")
C44int<- subset(Cl41_44INT,ChamberID=="4")

C45int<- subset(Cl45_48INT,ChamberID=="1")
C46int<- subset(Cl45_48INT,ChamberID=="2")
C47int<- subset(Cl45_48INT,ChamberID=="3")
C48int<- subset(Cl45_48INT,ChamberID=="4")
### temp INT
tempbf1 <-mean(C1int$Temperature, na.rm = TRUE)
tempbf2 <-mean(C2int$Temperature, na.rm = TRUE)
tempbf3 <-mean(C3int$Temperature, na.rm = TRUE)
tempbf4 <-mean(C4int$Temperature, na.rm = TRUE)
tempbf5 <-mean(C5int$Temperature, na.rm = TRUE)
tempbf6 <-mean(C6int$Temperature, na.rm = TRUE)
tempbf7 <-mean(C7int$Temperature, na.rm = TRUE)
tempbf8 <-mean(C8int$Temperature, na.rm = TRUE)
tempbf9 <-mean(C9int$Temperature, na.rm = TRUE)
tempbf10 <-mean(C10int$Temperature, na.rm = TRUE)
tempbf11 <-mean(C11int$Temperature, na.rm = TRUE)
tempbf12 <-mean(C12int$Temperature, na.rm = TRUE)
tempbf13 <-mean(C13int$Temperature, na.rm = TRUE)
tempbf14 <-mean(C14int$Temperature, na.rm = TRUE)
tempbf15 <-mean(C15int$Temperature, na.rm = TRUE)
tempbf16 <-mean(C16int$Temperature, na.rm = TRUE)
tempbf17 <-mean(C17int$Temperature, na.rm = TRUE)
tempbf18 <-mean(C18int$Temperature, na.rm = TRUE)
tempbf19 <-mean(C19int$Temperature, na.rm = TRUE)
tempbf20 <-mean(C20int$Temperature, na.rm = TRUE)
tempbf21 <-mean(C21int$Temperature, na.rm = TRUE)
tempbf22 <-mean(C22int$Temperature, na.rm = TRUE)
tempbf23 <-mean(C23int$Temperature, na.rm = TRUE)
tempbf24 <-mean(C24int$Temperature, na.rm = TRUE)
tempbf25 <-mean(C25int$Temperature, na.rm = TRUE)
tempbf26 <-mean(C26int$Temperature, na.rm = TRUE)
tempbf27 <-mean(C27int$Temperature, na.rm = TRUE)
tempbf28 <-mean(C28int$Temperature, na.rm = TRUE)
tempbf29 <-mean(C29int$Temperature, na.rm = TRUE)
tempbf30 <-mean(C30int$Temperature, na.rm = TRUE)
tempbf31 <-mean(C31int$Temperature, na.rm = TRUE)
tempbf33 <-mean(C33int$Temperature, na.rm = TRUE)
tempbf34 <-mean(C34int$Temperature, na.rm = TRUE)
tempbf35 <-mean(C35int$Temperature, na.rm = TRUE)
tempbf36 <-mean(C36int$Temperature, na.rm = TRUE)
tempbf37 <-mean(C37int$Temperature, na.rm = TRUE)
tempbf38 <-mean(C38int$Temperature, na.rm = TRUE)
tempbf39 <-mean(C39int$Temperature, na.rm = TRUE)
tempbf40 <-mean(C40int$Temperature, na.rm = TRUE)
tempbf41 <-mean(C41int$Temperature, na.rm = TRUE)
tempbf42 <-mean(C42int$Temperature, na.rm = TRUE)
tempbf43 <-mean(C43int$Temperature, na.rm = TRUE)
tempbf44 <-mean(C44int$Temperature, na.rm = TRUE)
tempbf45 <-mean(C45int$Temperature, na.rm = TRUE)
tempbf46 <-mean(C46int$Temperature, na.rm = TRUE)
tempbf47 <-mean(C47int$Temperature, na.rm = TRUE)
tempbf48 <-mean(C48int$Temperature, na.rm = TRUE)


write.csv(tempbf1, file="output/tempbf1.csv")
write.csv(tempbf2, file="output/tempbf2.csv")
write.csv(tempbf3, file="output/tempbf3.csv")
write.csv(tempbf4, file="output/tempbf4.csv")
write.csv(tempbf5, file="output/tempbf5.csv")
write.csv(tempbf6, file="output/tempbf6.csv")
write.csv(tempbf7, file="output/tempbf7.csv")
write.csv(tempbf8, file="output/tempbf8.csv")
write.csv(tempbf9, file="output/tempbf9.csv")
write.csv(tempbf10, file="output/tempbf10.csv")
write.csv(tempbf11, file="output/tempbf11.csv")
write.csv(tempbf12, file="output/tempbf12.csv")
write.csv(tempbf13, file="output/tempbf13.csv")
write.csv(tempbf14, file="output/tempbf14.csv")
write.csv(tempbf15, file="output/tempbf15.csv")
write.csv(tempbf16, file="output/tempbf16.csv")
write.csv(tempbf17, file="output/tempbf17.csv")
write.csv(tempbf18, file="output/tempbf18.csv")
write.csv(tempbf19, file="output/tempbf19.csv")
write.csv(tempbf20, file="output/tempbf20.csv")
write.csv(tempbf21, file="output/tempbf21.csv")
write.csv(tempbf22, file="output/tempbf22.csv")
write.csv(tempbf23, file="output/tempbf23.csv")
write.csv(tempbf24, file="output/tempbf24.csv")
write.csv(tempbf25, file="output/tempbf25.csv")
write.csv(tempbf26, file="output/tempbf26.csv")
write.csv(tempbf27, file="output/tempbf27.csv")
write.csv(tempbf28, file="output/tempbf28.csv")
write.csv(tempbf29, file="output/tempbf29.csv")
write.csv(tempbf30, file="output/tempbf30.csv")
write.csv(tempbf31, file="output/tempbf31.csv")
write.csv(tempbf33, file="output/tempbf33.csv")
write.csv(tempbf34, file="output/tempbf34.csv")
write.csv(tempbf35, file="output/tempbf35.csv")
write.csv(tempbf36, file="output/tempbf36.csv")
write.csv(tempbf37, file="output/tempbf37.csv")
write.csv(tempbf38, file="output/tempbf38.csv")
write.csv(tempbf39, file="output/tempbf39.csv")
write.csv(tempbf40, file="output/tempbf40.csv")
write.csv(tempbf41, file="output/tempbf41.csv")
write.csv(tempbf42, file="output/tempbf42.csv")
write.csv(tempbf43, file="output/tempbf43.csv")
write.csv(tempbf44, file="output/tempbf44.csv")
write.csv(tempbf45, file="output/tempbf45.csv")
write.csv(tempbf46, file="output/tempbf46.csv")
write.csv(tempbf47, file="output/tempbf47.csv")
write.csv(tempbf48, file="output/tempbf48.csv")

temp_before <- list.files(path ="./output/TEMP-BF",  # Identify all CSV files
                          pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
temp_before 
write.csv(temp_before , file="output/temp_before.csv")
####2hr
C1md<- subset(Cl1_4MD,ChamberID=="1")
C2md<- subset(Cl1_4MD,ChamberID=="2")
C3md<- subset(Cl1_4MD,ChamberID=="3")
C4md<- subset(Cl1_4MD,ChamberID=="4")

C5md<- subset(Cl5_8MD,ChamberID=="1")
C6md<- subset(Cl5_8MD,ChamberID=="2")
C7md<- subset(Cl5_8MD,ChamberID=="3")
C8md<- subset(Cl5_8MD,ChamberID=="4")

C9md<- subset(Cl9_12MD,ChamberID=="1")
C10md<- subset(Cl9_12MD,ChamberID=="2")
C11md<- subset(Cl9_12MD,ChamberID=="3")
C12md<- subset(Cl9_12MD,ChamberID=="4")

C13md<- subset(Cl13_16MD,ChamberID=="1")
C14md<- subset(Cl13_16MD,ChamberID=="2")
C15md<- subset(Cl13_16MD,ChamberID=="3")
C16md<- subset(Cl13_16MD,ChamberID=="4")

C17md<- subset(Cl17_20MD,ChamberID=="1")
C18md<- subset(Cl17_20MD,ChamberID=="2")
C19md<- subset(Cl17_20MD,ChamberID=="3")
C20md<- subset(Cl17_20MD,ChamberID=="4")

C21md<- subset(Cl21_24MD,ChamberID=="1")
C22md<- subset(Cl21_24MD,ChamberID=="2")
C23md<- subset(Cl21_24MD,ChamberID=="3")
C24md<- subset(Cl21_24MD,ChamberID=="4")

C25md<- subset(Cl25_28MD,ChamberID=="1")
C26md<- subset(Cl25_28MD,ChamberID=="2")
C27md<- subset(Cl25_28MD,ChamberID=="3")
C28md<- subset(Cl25_28MD,ChamberID=="4")

C29md<- subset(Cl29_32MD,ChamberID=="1")
C30md<- subset(Cl29_32MD,ChamberID=="2")
C31md<- subset(Cl29_32MD,ChamberID=="3")


C33md<- subset(Cl33_36MD,ChamberID=="1")
C34md<- subset(Cl33_36MD,ChamberID=="2")
C35md<- subset(Cl33_36MD,ChamberID=="3")
C36md<- subset(Cl33_36MD,ChamberID=="4")

C37md<- subset(Cl37_40MD,ChamberID=="1")
C38md<- subset(Cl37_40MD,ChamberID=="2")
C39md<- subset(Cl37_40MD,ChamberID=="3")
C40md<- subset(Cl37_40MD,ChamberID=="4")

C41md<- subset(Cl41_44MD,ChamberID=="1")
C42md<- subset(Cl41_44MD,ChamberID=="2")
C43md<- subset(Cl41_44MD,ChamberID=="3")
C44md<- subset(Cl41_44MD,ChamberID=="4")

C45md<- subset(Cl45_48MD,ChamberID=="1")
C46md<- subset(Cl45_48MD,ChamberID=="2")
C47md<- subset(Cl45_48MD,ChamberID=="3")
C48md<- subset(Cl45_48MD,ChamberID=="4")
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

#### rate at INT 
Cl1int<-inspect(C1int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

C2int <-C2int%>% drop_na(Oxygen)
Cl2int<-inspect(C2int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()


Cl3int<-inspect(C3int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl4int<-inspect(C4int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl5int<-inspect(C5int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
plot(Cl5int, pos=1)

Cl6int<-inspect(C6int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
#C7int <-C7int [-c(1:4),], didnt give a better R2 either
Cl7int<-inspect(C7int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl8int<-inspect(C8int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl9int<-inspect(C9int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl10int<-inspect(C10int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl11int<-inspect(C11int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl12int<-inspect(C12int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl13int<-inspect(C13int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
plot(Cl13int, pos=2)

Cl14int<-inspect(C14int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()


Cl15int<-inspect(C15int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
C16int <- C16int %>% drop_na(Oxygen)
Cl16int<-inspect(C16int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl17int<-inspect(C17int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
C17int

Cl18int<-inspect(C18int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl19int<-inspect(C19int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
C20int<- C20int %>% drop_na(Oxygen)
Cl20int<-inspect(C20int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl21int<-inspect(C21int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl22int<-inspect(C22int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl23int<-inspect(C23int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl24int<-inspect(C24int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl25int<-inspect(C25int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl26int<-inspect(C26int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl27int<-inspect(C27int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl28int<-inspect(C28int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
plot(Cl28int, pos=2)
Cl29int<-inspect(C29int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl30int<-inspect(C30int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
plot(Cl30int, pos=2)
Cl31int<-inspect(C31int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl33int<-inspect(C33int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl34int<-inspect(C34int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl35int<-inspect(C35int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
C36int <- C36int[-8,] ### removed because the measurement was not correct, supported by "low amplitude"
Cl36int<-inspect(C36int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl37int<-inspect(C37int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl38int<-inspect(C38int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl39int<-inspect(C39int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl40int<-inspect(C40int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl41int<-inspect(C41int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
C42int <- C42int[-6,]
Cl42int<-inspect(C42int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl43int<-inspect(C43int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl44int<-inspect(C44int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl45int<-inspect(C45int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

C46int <- C46int[-7,]
Cl46int<-inspect(C46int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl47int<-inspect(C47int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
C48int <- C48int %>% drop_na(Oxygen)
Cl48int<-inspect(C48int, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()


###MD

Cl1md<-inspect(C1md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()


Cl2md<-inspect(C2md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

C3md <- C3md[-c(6:14),]
Cl3md<-inspect(C3md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl4md<-inspect(C4md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl5md<-inspect(C5md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
plot(Cl5md, pos=2
     )

Cl6md<-inspect(C6md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl7md<-inspect(C7md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl8md<-inspect(C8md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl9md<-inspect(C9md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
plot(Cl9md, pos=2) 
C10md <- C10md %>% drop_na(Oxygen)
Cl10md<-inspect(C10md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl11md<-inspect(C11md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
C12md<-C12md %>%drop_na(Oxygen)
Cl12md<-inspect(C12md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl13md<-inspect(C13md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl14md<-inspect(C14md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()


Cl15md<-inspect(C15md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl16md<-inspect(C16md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl17md<-inspect(C17md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

C18md <- C18md %>% drop_na(Oxygen)
Cl18md<-inspect(C18md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl19md<-inspect(C19md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl20md<-inspect(C20md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl21md<-inspect(C21md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl22md<-inspect(C22md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl23md<-inspect(C23md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl24md<-inspect(C24md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

C25md <-C25md%>%drop_na(Oxygen)
Cl25md<-inspect(C25md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

C26md <-C26md%>%drop_na(Oxygen)
Cl26md<-inspect(C26md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
C27md <-C27md%>%drop_na(Oxygen)
Cl27md<-inspect(C27md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl28md<-inspect(C28md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl29md<-inspect(C29md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
C30md <-C30md%>% drop_na(Oxygen)
Cl30md<-inspect(C30md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
plot(Cl30md, pos=2)
Cl31md<-inspect(C31md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl33md<-inspect(C33md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

C34md <-C34md%>% drop_na(Oxygen)
Cl34md<-inspect(C34md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
C35md <-C35md%>% drop_na(Oxygen)
Cl35md<-inspect(C35md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

C36md <-C36md%>% drop_na(Oxygen) 
Cl36md<-inspect(C36md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl37md<-inspect(C37md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl38md<-inspect(C38md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl39md<-inspect(C39md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl40md<-inspect(C40md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

C41md <- C41md %>%drop_na(Oxygen)
Cl41md<-inspect(C41md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl42md<-inspect(C42md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

C43md <- C43md %>%drop_na(Oxygen)
Cl43md<-inspect(C43md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
plot(Cl43md, pos=2)

Cl44md<-inspect(C44md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl45md<-inspect(C45md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()


Cl46md<-inspect(C46md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl47md<-inspect(C47md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl48md<-inspect(C48md, time = 26, oxygen = 9, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

### temp
###convert to mass specific_INT
Trt7_INT <- read_excel("./Mydata/Trt7.xlsx")
Trt7INT<-convert_rate(
  Trt7_INT $INT_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt7_INT$Chamber_volume_l,
  mass = Trt7_INT$W_whole_kg,
  S = Trt7_INT$Sal_ppt,
  t = Trt7_INT$Temp,
  P = 1.013253
) %>%
  summary()


###Trt 10
Trt10_INT <- read_excel("./Mydata/Trt10.xlsx")
Trt10INT<-convert_rate(
  Trt10_INT $INT_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt10_INT$Chamber_volume_l,
  mass = Trt10_INT$W_whole_kg,
  S = Trt10_INT$Sal_ppt,
  t = Trt10_INT$Temp,
  P = 1.013253
) %>%
  summary()

###TRT13
Trt13_INT <- read_excel("./Mydata/Trt13.xlsx")
Trt13INT<-convert_rate(
  Trt13_INT $INT_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt13_INT$Chamber_volume_l,
  mass = Trt13_INT$W_whole_kg,
  S = Trt13_INT$Sal_ppt,
  t = Trt13_INT$Temp,
  P = 1.013253
) %>%
  summary()
Trt13INT[[7]]

##Trt16
Trt16_INT <- read_excel("./Mydata/Trt16.xlsx")
Trt16INT<-convert_rate(
  Trt16_INT $INT_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt16_INT$Chamber_volume_l,
  mass = Trt16_INT$W_whole_kg,
  S = Trt16_INT$Sal_ppt,
  t = Trt16_INT$Temp,
  P = 1.013253
) %>%
  summary()

##Trt19
Trt19_INT <- read_excel("./Mydata/Trt19.xlsx")
Trt19INT<-convert_rate(
  Trt19_INT $INT_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt19_INT$Chamber_volume_l,
  mass = Trt19_INT$W_whole_kg,
  S = Trt19_INT$Sal_ppt,
  t = Trt19_INT$Temp,
  P = 1.013253
) %>%
  summary()
Trt19INT[[7]]


##Trt22
Trt22_INT <- read_excel("./Mydata/Trt22.xlsx")
Trt22INT<-convert_rate(
  Trt22_INT $INT_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt22_INT$Chamber_volume_l,
  mass = Trt22_INT$W_whole_kg,
  S = Trt22_INT$Sal_ppt,
  t = Trt22_INT$Temp,
  P = 1.013253
) %>%
  summary()

###convert to mass specific_MD
Trt7_MD <- read_excel("./Mydata/Trt7.xlsx")
Trt7MD<-convert_rate(
  Trt7_MD $MD_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt7_MD$Chamber_volume_l,
  mass = Trt7_MD$W_whole_kg,
  S = Trt7_MD$Sal_ppt,
  t = Trt7_MD$Temp,
  P = 1.013253
) %>%
  summary()


###Trt 10
Trt10_MD <- read_excel("./Mydata/Trt10.xlsx")
Trt10MD<-convert_rate(
  Trt10_MD $MD_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt10_MD$Chamber_volume_l,
  mass = Trt10_MD$W_whole_kg,
  S = Trt10_MD$Sal_ppt,
  t = Trt10_MD$Temp,
  P = 1.013253
) %>%
  summary()

###TRT13
Trt13_MD <- read_excel("./Mydata/Trt13.xlsx")
Trt13MD<-convert_rate(
  Trt13_MD $MD_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt13_MD$Chamber_volume_l,
  mass = Trt13_MD$W_whole_kg,
  S = Trt13_MD$Sal_ppt,
  t = Trt13_MD$Temp,
  P = 1.013253
) %>%
  summary()
Trt13MD[[7]]

##Trt16
Trt16_MD <- read_excel("./Mydata/Trt16.xlsx")
Trt16MD<-convert_rate(
  Trt16_MD $MD_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt16_MD$Chamber_volume_l,
  mass = Trt16_MD$W_whole_kg,
  S = Trt16_MD$Sal_ppt,
  t = Trt16_MD$Temp,
  P = 1.013253
) %>%
  summary()

##Trt19
Trt19_MD <- read_excel("./Mydata/Trt19.xlsx")
Trt19MD<-convert_rate(
  Trt19_MD $MD_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt19_MD$Chamber_volume_l,
  mass = Trt19_MD$W_whole_kg,
  S = Trt19_MD$Sal_ppt,
  t = Trt19_MD$Temp,
  P = 1.013253
) %>%
  summary()
Trt19MD[[7]]


##Trt22
Trt22_MD <- read_excel("./Mydata/Trt22.xlsx")
Trt22MD<-convert_rate(
  Trt22_MD $MD_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt22_MD$Chamber_volume_l,
  mass = Trt22_MD$W_whole_kg,
  S = Trt22_MD$Sal_ppt,
  t = Trt22_MD$Temp,
  P = 1.013253
) %>%
  summary()
###data analysis

dt <-read_excel("./Mydata/Rimouski_Sum_all_raw.xlsx")
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
#Anova(mo1)
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

g1 <-ggplot(data=data_delta, aes(x= Treatment, y=Delta, fill= Treatment))+
  geom_boxplot()+
  geom_point(alpha=0.5)+
  labs(title = "Oxygen Consumption at Elevating Temperatures - Southern population",
       x = "Treatments",
       y = "Delta_Oxygen_Consumption_mgO2/hour/kg")
g1
g2 <- g1 + scale_colour_manual(values=c("lightblue","pink", "green", 'orange', "red", "brown"))
ggsave(plot= g2, filename = "output/Result_graph/Deltaplot_raw_all_values.png")
g2

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
ggsave(plot= g_no2, filename = "output/Result_graph/Deltaplot_raw_all_values_no_outlier_shape.png")

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
ggsave(plot= gbox2, filename = "output/Result_graph/Deltaplot_raw_all_values_no_geom_point.png")
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
ggsave(plot= gno_no2, filename = "output/Result_graph/Deltaplot_raw_all_values_no_geom_no_outliershape.png")

######
DT <-read_excel("./Mydata/Rimouski_Sum_all_raw.xlsx")

DT <- DT %>% 
  rename(
    "7°C" = INT_converted_possitive,
    Target = MD_converted_posstive
  )
DT <- DT %>%
  gather(key = "time", value = "Oxygen",  "7°C", Target) %>%
  convert_as_factor(Clam_ID, time)
tail(DT)


###


#### check normality: VERY good if (p>0.05)

DT %>%
  group_by(time, Treatment) %>%
  shapiro_test(Oxygen)


str(DT)
DT$Treatment <- as.factor(DT$Treatment)
#assess homegenity of variance
leveneTest(Oxygen~Treatment, data= DT)


##USE LINEAR MODEL
DT$Treatment<-as.character(DT$Treatment)

model <-lm(Oxygen ~ Treatment, data=DT)
summary(model)
Anova(model)


emm_sp <- emmeans(model, specs = c("Treatment"))
emm_sp
cp1<-pairs(emm_sp)
cp1 ###no significance was found
# pairwise comparisons
library(xlsx)
write.xlsx(file="output/Result_tables/pairwise.xlsx",cp1)

#stat <-dt_trt %>%
#group_by(Trt,time) %>%
#get_summary_stats (Oxygen, type = "mean_sd")
#stat
library(ggpubr)
DT <- mutate(DT, Treatment = factor(Treatment, levels = c("7", "10", "13", "16", "19", "22")))
bxp <- ggboxplot (DT, x= "time", y = "Oxygen", color= "Treatment", palette= "jco",
                  outlier.shape = NA
)

bxp

box <- ggplot(data = DT, aes(x = time, y = Oxygen, colour = Treatment)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_dodge(width=0.75),aes(group=Treatment),alpha = 0.6) +
  labs(
    x = "Time",
    y = "mgO2/hour/kg",
    colour = "Treatment") +
  theme_classic()
box
ggsave(plot=box, filename= "output/Result_graph/before_after_plot.png")


bxp<-bxp + labs(title = "Oxygen Consumption Before and After Ramping",
                x = "Time",
                y = "mgO2/hour/kg",
                colour = "Treatment")
bxp
ggsave(plot=bxp, filename= "output/Result_graph/before_after_plot.png")
#####with outlier shape
bxp1 <- ggboxplot (DT, x= "time", y = "Oxygen", color= "Treatment", palette= "jco"
)

bxp1
bxp1<-bxp1 + labs(title = "Oxygen Consumption Before and After Ramping",
                x = "Time",
                y = "mgO2/hour/kg",
                colour = "Treatment")
bxp1
ggsave(plot=bxp1, filename= "output/Result_graph/before_after_plot_withoutliershape.png")
####linear mized model with a random effect
t <- read_excel("./Mydata/Sum_all_raw.xlsx")
t$Treatment <-as.factor(t$Treatment)
t <- t %>% 
  rename(
    "7°C" = INT_converted_possitive,
    Target = MD_converted_posstive
  )
t <- t %>%
  gather(key = "time", value = "Oxygen", "7°C", Target) %>%
  convert_as_factor(Clam_ID, time)
mt <-lmer( Oxygen ~ Treatment + time +(1|Clam_ID
), data=t)
summary(mt)
mm<-emmeans(mt, specs=c("Treatment","time"))
pairs(mm) ##no significance was found

#### create subset of each treatment
DT <- group_by(DT, Treatment, time)
trt7 <-DT[which(DT$Treatment=='7'),]
trt10 <-DT[which(DT$Treatment=='10'),]
trt13 <-DT[which(DT$Treatment=='13'),]
trt16 <-DT[which(DT$Treatment=='16'),]
trt19 <-DT[which(DT$Treatment=='19'),]
trt22 <-DT[which(DT$Treatment=='22'),]

g7<-ggplot(data=trt7,aes(x = time, y = Oxygen))  +
  geom_point(aes(group= Clam_ID), shape =21) +
  geom_line(aes(group=Clam_ID))+
  theme_classic()
g7<-g7 + labs(tag = "7°C",
              x = "Time",
              y = "mgO2/hour/kg") +
  theme(axis.text=element_text(size=12),                                            axis.title=element_text(size=12),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.tag.position = "top")
g7


g10<-ggplot(data=trt10,aes(x = time, y = Oxygen))  +
  geom_point(aes(group= Clam_ID), shape =21) +
  geom_line(aes(group=Clam_ID))+
  theme_classic()
g10<-g10 + labs(tag = "10°C",
                x = "Time",
                y = "mgO2/hour/kg") +
  theme(axis.text=element_text(size=12),                                            axis.title=element_text(size=12),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.tag.position = "top")
g10


g13<-ggplot(data=trt13,aes(x = time, y = Oxygen))  +
  geom_point(aes(group= Clam_ID), shape =21) +
  geom_line(aes(group=Clam_ID))+
  theme_classic()
g13<-g13 + labs(tag = "13°C",
                x = "Time",
                y = "mgO2/hour/kg") +
  theme(axis.text=element_text(size=12),                                            axis.title=element_text(size=12),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.tag.position = "top")
g13


g16<-ggplot(data=trt16,aes(x = time, y = Oxygen))  +
  geom_point(aes(group= Clam_ID), shape =21) +
  geom_line(aes(group=Clam_ID))+
  theme_classic()
g16<-g16 + labs(tag = "16°C",
                x = "Time",
                y = "mgO2/hour/kg") +
  theme(axis.text=element_text(size=12),                                            axis.title=element_text(size=12),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.tag.position = "top")
g16

g19<-ggplot(data=trt19,aes(x = time, y = Oxygen))  +
  geom_point(aes(group= Clam_ID), shape =21) +
  geom_line(aes(group=Clam_ID))+
  theme_classic()
g19<-g19 + labs(tag = "19°C",
                x = "Time",
                y = "mgO2/hour/kg") +
  theme(axis.text=element_text(size=12),                                            axis.title=element_text(size=12),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.tag.position = "top")
g19

g22<-ggplot(data=trt22,aes(x = time, y = Oxygen))  +
  geom_point(aes(group= Clam_ID), shape =21) +
  geom_line(aes(group=Clam_ID))+
  theme_classic()
g22<-g22 + labs(tag = "22°C",
                x = "Time",
                y = "mgO2/hour/kg") +
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
               font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))
fig

all<-annotate_figure(fig,
                     bottom = text_grob("Measuring Period", color = "black", size = 14),
                     left = text_grob("mgO2/hour/kg", color = "black", rot = 90))
all
ggsave(plot=all, file="./output/Result_graph/trt_pannels.jpeg", height= 10, width = 6.8, units="in")
#### BG plot
dt <-read_excel("./Mydata/Rimouski_Sum_all_raw.xlsx")
dt$Treatment <- as.character(dt$Treatment)
dt <- dt %>% group_by (Treatment)
dt<- mutate(dt, Treatment = factor(Treatment, levels = c("7", "10", "13", "16", "19", "22")))

box_bg <- ggplot(data = dt, aes(x = Treatment, y = BG_at_target)) +
  geom_boxplot() +
  geom_point(alpha = 0.6) +
  labs(
    x = "Treatment",
    y = "Rate",
    colour = "Treatment") +
  theme_classic()
box_bg
ggsave(plot=box_bg, file="./output/Result_graph/Rimouski_BG_rate.jpeg")

