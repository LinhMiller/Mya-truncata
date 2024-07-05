# RESPIROMETRY
#### Northern population experiment
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

###Initial measurement
Cl1_4INT<-read_excel ("./Data/Mydata_raw/2022-08-19-Trt10-19-C14INT_raw.xlsx") 
Cl5_8INT<-read_excel ("./Data/Mydata_raw/2022-08-19-Trt10-19-C58INT_raw.xlsx")

Cl9_12INT<-read_excel("./Data/Mydata_raw/2022-08-20-Trt13-16-C14INT_raw.xlsx")
Cl13_16INT<-read_excel("./Data/Mydata_raw/2022-08-20-Trt13-16-C58INT_raw.xlsx")

Cl17_20INT<-read_excel("./Data/Mydata_raw/2022-08-21-Trt22-7-C14INT_raw.xlsx")
Cl21_24INT<-read_excel("./Data/Mydata_raw/2022-08-21-Trt22-7-C58INT_raw.xlsx")

Cl25_28INT<-read_excel("./Data/Mydata_raw/2022-08-22-Trt19-13-C14INT_raw.xlsx")
Cl29_32INT<-read_excel("./Data/Mydata_raw/2022-08-22-Trt19-13-C58INT_raw.xlsx")

Cl33_36INT<-read_excel("./Data/Mydata_raw/2022-08-23-Trt16-10-C14INT_raw.xlsx")
Cl37_40INT<-read_excel("./Data/Mydata_raw/2022-08-23-Trt16-10-C58INT_raw.xlsx")

Cl41_44INT<-read_excel("./Data/Mydata_raw/2022-08-24-Trt22-7-C14INT_raw.xlsx")
Cl45_48INT<-read_excel("./Data/Mydata_raw/2022-08-24-Trt22-7-C58INT_raw.xlsx")

### subset data "at target temperature measurement"
Cl1_4MD<-read_excel ("./Data/Mydata_raw/2022-08-19-Trt10-19-C14MD_raw.xlsx")
Cl5_8MD<-read_excel ("./Data/Mydata_raw/2022-08-19-Trt10-19-C58MD_raw.xlsx")

Cl9_12MD<-read_excel("./Data/Mydata_raw/2022-08-20-Trt13-16-C14MD_raw.xlsx")
Cl13_16MD<-read_excel("./Data/Mydata_raw/2022-08-20-Trt13-16-C58MD_raw.xlsx")

Cl17_20MD<-read_excel("./Data/Mydata_raw/2022-08-21-Trt22-7-C14MD_raw.xlsx")
Cl21_24MD<-read_excel("./Data/Mydata_raw/2022-08-21-Trt22-7-C58MD_raw.xlsx")

Cl25_28MD<-read_excel("./Data/Mydata_raw/2022-08-22-Trt19-13-C14MD_raw.xlsx")
Cl29_32MD<-read_excel("./Data/Mydata_raw/2022-08-22-Trt19-13-C58MD_raw.xlsx")

Cl33_36MD<-read_excel("./Data/Mydata_raw/2022-08-23-Trt16-10-C14MD_raw.xlsx")
Cl37_40MD<-read_excel("./Data/Mydata_raw/2022-08-23-Trt16-10-C58MD_raw.xlsx")

Cl41_44MD<-read_excel("./Data/Mydata_raw/2022-08-24-Trt22-7-C14MD_raw.xlsx")
Cl45_48MD<-read_excel("./Data/Mydata_raw/2022-08-24-Trt22-7-C58MD_raw.xlsx")
### subset data "background measurement"
Cl1_4BG<-read_excel ("./Data/Mydata_raw/2022-08-19-Trt10-19-C14BG_raw.xlsx")
Cl5_8BG<-read_excel ("./Data/Mydata_raw/2022-08-19-Trt10-19-C58BG_raw.xlsx")

Cl9_12BG<-read_excel("./Data/Mydata_raw/2022-08-20-Trt13-16-C14BG_raw.xlsx")
Cl13_16BG<-read_excel("./Data/Mydata_raw/2022-08-20-Trt13-16-C58BG_raw.xlsx")

Cl17_20BG<-read_excel("./Data/Mydata_raw/2022-08-21-Trt22-7-C14BG_raw.xlsx")
Cl21_24BG<-read_excel("./Data/Mydata_raw/2022-08-21-Trt22-7-C58BG_raw.xlsx")

Cl25_28BG<-read_excel("./Data/Mydata_raw/2022-08-22-Trt19-13-C14BG_raw.xlsx")
Cl29_32BG<-read_excel("./Data/Mydata_raw/2022-08-22-Trt19-13-C58BG_raw.xlsx")

Cl33_36BG<-read_excel("./Data/Mydata_raw/2022-08-23-Trt16-10-C14BG_raw.xlsx")
Cl37_40BG<-read_excel("./Data/Mydata_raw/2022-08-23-Trt16-10-C58BG_raw.xlsx")

Cl41_44BG<-read_excel("./Data/Mydata_raw/2022-08-24-Trt22-7-C14BG_raw.xlsx")
Cl45_48BG<-read_excel("./Data/Mydata_raw/2022-08-24-Trt22-7-C58BG_raw.xlsx")

####format time INT
Cl1_4INT	<-format_time(Cl1_4INT, time = 1, format = "ymdHMS", start = 1)
Cl5_8INT	<-format_time(Cl5_8INT, time = 1, format = "ymdHMS", start = 1)

Cl9_12INT <-format_time(Cl9_12INT, time = 1, format = "ymdHMS", start = 1)
Cl13_16INT <-format_time(Cl13_16INT, time = 1, format = "ymdHMS", start = 1)

Cl17_20INT <-format_time(Cl17_20INT, time = 1, format = "ymdHMS", start = 1)
Cl21_24INT <-format_time(Cl21_24INT, time = 1, format = "ymdHMS", start = 1)

Cl25_28INT <-format_time(Cl25_28INT, time = 1, format = "ymdHMS", start = 1)
Cl29_32INT <-format_time(Cl29_32INT, time = 1, format = "ymdHMS", start = 1)

Cl33_36INT <-format_time(Cl33_36INT, time = 1, format = "ymdHMS", start = 1)
Cl37_40INT <-format_time(Cl37_40INT, time = 1, format = "ymdHMS", start = 1)

Cl41_44INT <-format_time(Cl41_44INT, time = 1, format = "ymdHMS", start = 1)
Cl45_48INT <-format_time(Cl45_48INT, time = 1, format = "ymdHMS", start = 1)

### format time MD
Cl1_4MD	<-format_time(Cl1_4MD, time = 1, format = "ymdHMS", start = 1)
#Cl5_8MD <-format_time(Cl5_8MD, time = 1, format = "ymdHMS", start = 1)

#Cl9_12MD <-format_time(Cl9_12MD, time = 1, format = "ymdHMS", start = 1)
Cl13_16MD <-format_time(Cl13_16MD, time = 1, format = "ymdHMS", start = 1)

Cl17_20MD <-format_time(Cl17_20MD, time = 1, format = "ymdHMS", start = 1)
#Cl21_24MD <-format_time(Cl21_24MD, time = 1, format = "ymdHMS", start = 1)

#Cl25_28MD <-format_time(Cl25_28MD, time = 1, format = "ymdHMS", start = 1)
#Cl29_32MD <-format_time(Cl29_32MD, time = 1, format = "ymdHMS", start = 1)

#Cl33_36MD <-format_time(Cl33_36MD, time = 1, format = "ymdHMS", start = 1)
Cl37_40MD <-format_time(Cl37_40MD, time = 1, format = "ymdHMS", start = 1)

Cl41_44MD <-format_time(Cl41_44MD, time = 1, format = "ymdHMS", start = 1)
Cl45_48MD <-format_time(Cl45_48MD, time = 1, format = "ymdHMS", start = 1)

###format time BG
#Cl1_4BG	<-format_time(Cl1_4BG, time = 1, format = "ymdHMS", start = 1)
Cl5_8BG <-format_time(Cl5_8BG, time = 1, format = "ymdHMS", start = 1)

Cl9_12BG <-format_time(Cl9_12BG, time = 1, format = "ymdHMS", start = 1)
Cl13_16BG <-format_time(Cl13_16BG, time = 1, format = "ymdHMS", start = 1)

Cl17_20BG <-format_time(Cl17_20BG, time = 1, format = "ymdHMS", start = 1)
Cl21_24BG <-format_time(Cl21_24BG, time = 1, format = "ymdHMS", start = 1)

Cl25_28BG <-format_time(Cl25_28BG, time = 1, format = "ymdHMS", start = 1)
Cl29_32BG <-format_time(Cl29_32BG, time = 1, format = "ymdHMS", start = 1)

Cl33_36BG <-format_time(Cl33_36BG, time = 1, format = "ymdHMS", start = 1)
Cl37_40BG <-format_time(Cl37_40BG, time = 1, format = "ymdHMS", start = 1)

#Cl41_44BG <-format_time(Cl41_44BG, time = 1, format = "ymdHMS", start = 1)
Cl45_48BG <-format_time(Cl45_48BG, time = 1, format = "ymdHMS", start = 1)
###eliminate the first 15 minuates at INT
Cl1_4INT	<-Cl1_4INT[-c(1:900),]
Cl5_8INT	<-Cl5_8INT[-c(1:900),]

Cl9_12INT	 <-Cl9_12INT[-c(1:900),]
Cl13_16INT <-Cl13_16INT[-c(1:900),]

Cl17_20INT	<-Cl17_20INT[-c(1:900),]
Cl21_24INT	<-Cl21_24INT[-c(1:900),]

Cl25_28INT	<-Cl25_28INT[-c(1:900),]
Cl29_32INT	<-Cl29_32INT[-c(1:900),]

Cl33_36INT	<-Cl33_36INT[-c(1:900),]
Cl37_40INT	<-Cl37_40INT[-c(1:900),]

Cl41_44INT	<-Cl41_44INT[-c(1:900),]
Cl45_48INT	<-Cl45_48INT[-c(1:900),]
#### eliminate the first 15 at MD
Cl1_4MD	<-Cl1_4MD[-c(1:900),]
Cl5_8MD	<-Cl5_8MD[-c(1:900),]

Cl9_12MD	 <-Cl9_12MD[-c(1:900),]
Cl13_16MD <-Cl13_16MD[-c(1:900),]

Cl17_20MD	<-Cl17_20MD[-c(1:900),]
Cl21_24MD	<-Cl21_24MD[-c(1:900),]

Cl25_28MD	<-Cl25_28MD[-c(1:900),]
Cl29_32MD	<-Cl29_32MD[-c(1:900),]

Cl33_36MD	<-Cl33_36MD[-c(1:900),]
Cl37_40MD	<-Cl37_40MD[-c(1:900),]

Cl41_44MD	<-Cl41_44MD[-c(1:900),]
Cl45_48MD	<-Cl45_48MD[-c(1:900),]
####eliminate the first 15min at BG
Cl1_4BG	<-Cl1_4BG[-c(1:900),]
Cl5_8BG	<-Cl5_8BG[-c(1:900),]

Cl9_12BG	 <-Cl9_12BG[-c(1:900),]
Cl13_16BG <-Cl13_16BG[-c(1:900),]

Cl17_20BG	<-Cl17_20BG[-c(1:900),]
Cl21_24BG	<-Cl21_24BG[-c(1:900),]

Cl25_28BG	<-Cl25_28BG[-c(1:900),]
Cl29_32BG	<-Cl29_32BG[-c(1:900),]

Cl33_36BG	<-Cl33_36BG[-c(1:900),]
Cl37_40BG	<-Cl37_40BG[-c(1:900),]

Cl41_44BG	<-Cl41_44BG[-c(1:900),]
Cl45_48BG	<-Cl45_48BG[-c(1:900),]
###inspect INT
Cl1_int<-inspect(Cl1_4INT, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time") %>%
  summary()
Cl2_int<-inspect(Cl1_4INT, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
plot(Cl2_int, pos=6)

Cl3_int<-inspect(Cl1_4INT, time = 35, oxygen = 13 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
plot(Cl3_int, pos=4)

Cl4_int<-inspect(Cl1_4INT, time = 35, oxygen = 16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
plot(Cl1_4INT$`CH2 O2 [%air sat.]`)
plot(Cl2_int, pos=4)
Cl5_int<-inspect(Cl5_8INT, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl6_int<-inspect(Cl5_8INT, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl7_int<-inspect(Cl5_8INT, time = 35, oxygen = 13 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl8_int<-inspect(Cl5_8INT, time = 35, oxygen = 16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl9_int<-inspect(Cl9_12INT, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl10_int<-inspect(Cl9_12INT, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl11_int<-inspect(Cl9_12INT, time = 35, oxygen = 13 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl12_int<-inspect(Cl9_12INT, time = 35, oxygen = 16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl13_int<-inspect(Cl13_16INT, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl14_int<-inspect(Cl13_16INT, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl15_int<-inspect(Cl13_16INT, time = 35, oxygen = 13 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl16_int<-inspect(Cl13_16INT, time = 35, oxygen =  16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl17_int<-inspect(Cl17_20INT, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl18_int<-inspect(Cl17_20INT, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl19_int<-inspect(Cl17_20INT, time = 35, oxygen = 13 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl20_int<-inspect(Cl17_20INT, time = 35, oxygen = 16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl21_int<-inspect(Cl21_24INT, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl22_int<-inspect(Cl21_24INT, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl23_int<-inspect(Cl21_24INT, time = 35, oxygen = 13, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl24_int<-inspect(Cl21_24INT, time = 35, oxygen = 16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl25_int<-inspect(Cl25_28INT, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl26_int<-inspect(Cl25_28INT, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl27_int<-inspect(Cl25_28INT, time = 35, oxygen = 13, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
plot (Cl27_int, pos=2)

Cl28_int<-inspect(Cl25_28INT, time = 35, oxygen = 16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
plot(Cl28_int, pos=2)

Cl29_int<-inspect(Cl29_32INT, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl30_int<-inspect(Cl29_32INT, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl31_int<-inspect(Cl29_32INT, time = 35, oxygen = 13  , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl32_int<-inspect(Cl29_32INT, time = 35, oxygen = 16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl33_int<-inspect(Cl33_36INT, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl34_int<-inspect(Cl33_36INT, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl35_int<-inspect(Cl33_36INT, time = 35, oxygen = 13 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
plot(Cl35_int, pos=2)
Cl36_int<-inspect(Cl33_36INT, time = 35, oxygen = 16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl37_int<-inspect(Cl37_40INT, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl38_int<-inspect(Cl37_40INT, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl39_int<-inspect(Cl37_40INT, time = 35, oxygen = 13 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl40_int<-inspect(Cl37_40INT, time = 35, oxygen = 16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl41_int<-inspect(Cl41_44INT, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl42_int<-inspect(Cl41_44INT, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl43_int<-inspect(Cl41_44INT, time = 35, oxygen = 13 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl44_int<-inspect(Cl41_44INT, time = 35, oxygen = 16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl45_int<-inspect(Cl45_48INT, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl46_int<-inspect(Cl45_48INT, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl47_int<-inspect(Cl45_48INT, time = 35, oxygen = 13 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl48_int<-inspect(Cl45_48INT, time = 35, oxygen = 16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

####inspect MD
Cl1_md<-inspect(Cl1_4MD, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time") %>%
  summary()
Cl2_md<-inspect(Cl1_4MD, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl3_md<-inspect(Cl1_4MD, time = 35, oxygen = 13 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl4_md<-inspect(Cl1_4MD, time = 35, oxygen = 16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl5_md<-inspect(Cl5_8MD, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl6_md<-inspect(Cl5_8MD, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl7_md<-inspect(Cl5_8MD, time = 35, oxygen = 13 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl8_md<-inspect(Cl5_8MD, time = 35, oxygen = 16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl9_md<-inspect(Cl9_12MD, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl10_md<-inspect(Cl9_12MD, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl11_md<-inspect(Cl9_12MD, time = 35, oxygen = 13 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl12_md<-inspect(Cl9_12MD, time = 35, oxygen = 16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl13_md<-inspect(Cl13_16MD, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl14_md<-inspect(Cl13_16MD, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl15_md<-inspect(Cl13_16MD, time = 35, oxygen = 13 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl16_md<-inspect(Cl13_16MD, time = 35, oxygen =  16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl17_md<-inspect(Cl17_20MD, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl18_md<-inspect(Cl17_20MD, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl19_md<-inspect(Cl17_20MD, time = 35, oxygen = 13 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl20_md<-inspect(Cl17_20MD, time = 35, oxygen = 16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl21_md<-inspect(Cl21_24MD, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl22_md<-inspect(Cl21_24MD, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl23_md<-inspect(Cl21_24MD, time = 35, oxygen = 13, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl24_md<-inspect(Cl21_24MD, time = 35, oxygen = 16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl25_md<-inspect(Cl25_28MD, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl26_md<-inspect(Cl25_28MD, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl27_md<-inspect(Cl25_28MD, time = 35, oxygen = 13, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl28_md<-inspect(Cl25_28MD, time = 35, oxygen = 16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl29_md<-inspect(Cl29_32MD, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
plot(Cl29_md, pos=2)

Cl30_md<-inspect(Cl29_32MD, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
plot(Cl30_md, pos=4)

Cl31_md<-inspect(Cl29_32MD, time = 35, oxygen = 13  , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl32_md<-inspect(Cl29_32MD, time = 35, oxygen = 16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl33_md<-inspect(Cl33_36MD, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl34_md<-inspect(Cl33_36MD, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl35_md<-inspect(Cl33_36MD, time = 35, oxygen = 13 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl36_md<-inspect(Cl33_36MD, time = 35, oxygen = 16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl37_md<-inspect(Cl37_40MD, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl38_md<-inspect(Cl37_40MD, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl39_md<-inspect(Cl37_40MD, time = 35, oxygen = 13 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl40_md<-inspect(Cl37_40MD, time = 35, oxygen = 16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl41_md<-inspect(Cl41_44MD, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl42_md<-inspect(Cl41_44MD, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl43_md<-inspect(Cl41_44MD, time = 35, oxygen = 13 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl44_md<-inspect(Cl41_44MD, time = 35, oxygen = 16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl45_md<-inspect(Cl45_48MD, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl46_md<-inspect(Cl45_48MD, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl47_md<-inspect(Cl45_48MD, time = 35, oxygen = 13 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
plot(Cl47_md,pos=2)
Cl48_md<-inspect(Cl45_48MD, time = 35, oxygen = 16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
#### inspect bg
Cl1_bg<-inspect(Cl1_4BG, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time") %>%
  summary()
Cl2_bg<-inspect(Cl1_4BG, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl3_bg<-inspect(Cl1_4BG, time = 35, oxygen = 13 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl4_bg<-inspect(Cl1_4BG, time = 35, oxygen = 16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl5_bg<-inspect(Cl5_8BG, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl6_bg<-inspect(Cl5_8BG, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
plot(Cl6_bg,pos=2)
Cl7_bg<-inspect(Cl5_8BG, time = 35, oxygen = 13 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
plot(Cl7_bg, pos=3)
Cl8_bg<-inspect(Cl5_8BG, time = 35, oxygen = 16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
plot(Cl8_bg, pos=3)
Cl9_bg<-inspect(Cl9_12BG, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl10_bg<-inspect(Cl9_12BG, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl11_bg<-inspect(Cl9_12BG, time = 35, oxygen = 13 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl12_bg<-inspect(Cl9_12BG, time = 35, oxygen = 16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl13_bg<-inspect(Cl13_16BG, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl14_bg<-inspect(Cl13_16BG, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl15_bg<-inspect(Cl13_16BG, time = 35, oxygen = 13 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl16_bg<-inspect(Cl13_16BG, time = 35, oxygen =  16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl17_bg<-inspect(Cl17_20BG, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl18_bg<-inspect(Cl17_20BG, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl19_bg<-inspect(Cl17_20BG, time = 35, oxygen = 13 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl20_bg<-inspect(Cl17_20BG, time = 35, oxygen = 16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl21_bg<-inspect(Cl21_24BG, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl22_bg<-inspect(Cl21_24BG, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
plot(Cl22_bg, pos=4)

Cl23_bg<-inspect(Cl21_24BG, time = 35, oxygen = 13, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl24_bg<-inspect(Cl21_24BG, time = 35, oxygen = 16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
plot(Cl24_bg, pos=3)

Cl25_bg<-inspect(Cl25_28BG, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl26_bg<-inspect(Cl25_28BG, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
plot(Cl26_bg, pos=3)
Cl27_bg<-inspect(Cl25_28BG, time = 35, oxygen = 13, plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl28_bg<-inspect(Cl25_28BG, time = 35, oxygen = 16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl29_bg<-inspect(Cl29_32BG, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()

Cl30_bg<-inspect(Cl29_32BG, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl31_bg<-inspect(Cl29_32BG, time = 35, oxygen = 13  , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl32_bg<-inspect(Cl29_32BG, time = 35, oxygen = 16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl33_bg<-inspect(Cl33_36BG, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl34_bg<-inspect(Cl33_36BG, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl35_bg<-inspect(Cl33_36BG, time = 35, oxygen = 13 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl36_bg<-inspect(Cl33_36BG, time = 35, oxygen = 16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl37_bg<-inspect(Cl37_40BG, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl38_bg<-inspect(Cl37_40BG, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl39_bg<-inspect(Cl37_40BG, time = 35, oxygen = 13 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl40_bg<-inspect(Cl37_40BG, time = 35, oxygen = 16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl41_bg<-inspect(Cl41_44BG, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl42_bg<-inspect(Cl41_44BG, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl43_bg<-inspect(Cl41_44BG, time = 35, oxygen = 13 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl44_bg<-inspect(Cl41_44BG, time = 35, oxygen = 16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl45_bg<-inspect(Cl45_48BG, time = 35, oxygen = 7 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl46_bg<-inspect(Cl45_48BG, time = 35, oxygen = 10 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl47_bg<-inspect(Cl45_48BG, time = 35, oxygen = 13 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()
Cl48_bg<-inspect(Cl45_48BG, time = 35, oxygen = 16 , plot = FALSE) %>%
  auto_rate(method="linear",width= 1800, by="time")%>%
  summary()



summarize(Cl45_48BG, 
          mean_tep= mean(Cl45_48BG$`CH5 temp (∞C)`))
###my temperature in the excel sheet didnt seem to be correct for the target measurement of the treatment 22C, hence i used the average background tenperature instead. i will look into it later
rm(list=ls())
###convert rate to mass specific at INT
Trt7_INT <- read_excel("./output/Iqaluit_Trt7.xlsx")
Trt7INT<-convert_rate(
  Trt7_INT $INT_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt7_INT$volume_ml/1000,
  mass = Trt7_INT$mass_whole_g/1000,
  S = Trt7_INT$salinity_ppt,
  t = Trt7_INT$temp_c_int,
  P = 1.013253
) %>%
  summary()
Trt7INT[[7]]
Trt7INT58<-convert_rate(
  Trt7_INT $INT_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt7_INT$volume_ml/1000,
  mass = Trt7_INT$mass_whole_g/1000,
  S = 32,
  t = 7,
  P = 1.013253
) %>%
  summary()
Trt7INT58[[7]]
###Trt 10
Trt10_INT <- read_excel("./output/Iqaluit_Trt10.xlsx")
Trt10INT<-convert_rate(
  Trt10_INT $INT_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt10_INT$volume_ml/1000,
  mass = Trt10_INT$mass_whole_g/1000,
  S = Trt10_INT$salinity_ppt,
  t = Trt10_INT$temp_c_int,
  P = 1.013253
) %>%
  summary()
Trt10_INT <- read_excel("./output/Iqaluit_Trt10.xlsx")
Trt10INT58<-convert_rate(
  Trt10_INT $INT_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt10_INT$volume_ml/1000,
  mass = Trt10_INT$mass_whole_g/1000,
  S = 31.9,
  t = 6.89,
  P = 1.013253
) %>%
  summary()
###TRT13
Trt13_INT <- read_excel("./output/Iqaluit_Trt13.xlsx")
Trt13INT<-convert_rate(
  Trt13_INT $INT_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt13_INT$volume_ml/1000,
  mass = Trt13_INT$mass_whole_g/1000,
  S = Trt13_INT$salinity_ppt,
  t = Trt13_INT$temp_c_int,
  P = 1.013253
) %>%
  summary()
Trt13INT[[7]]
Trt13INT58<-convert_rate(
  Trt13_INT $INT_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt13_INT$volume_ml/1000,
  mass = Trt13_INT$mass_whole_g/1000,
  S = 32,
  t = Trt13_INT$temp_c_int,
  P = 1.013253
) %>%
  summary()
##Trt16
Trt16_INT <- read_excel("./output/Iqaluit_Trt16.xlsx")
Trt16INT<-convert_rate(
  Trt16_INT $INT_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt16_INT$volume_ml/1000,
  mass = Trt16_INT$mass_whole_g/1000,
  S = Trt16_INT$salinity_ppt,
  t = Trt16_INT$temp_c_int,
  P = 1.013253
) %>%
  summary()
Trt16INT[[7]]
Trt16INT58<-convert_rate(
  Trt16_INT $INT_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt16_INT$volume_ml/1000,
  mass = Trt16_INT$mass_whole_g/1000,
  S = 31.7,
  t = 6.6,
  P = 1.013253
) %>%
  summary()
Trt16INT58[[7]]
##Trt19
Trt19_INT <- read_excel("./output/Iqaluit_Trt19.xlsx")
Trt19INT<-convert_rate(
  Trt19_INT $INT_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt19_INT$volume_ml/1000,
  mass = Trt19_INT$mass_whole_g/1000,
  S = Trt19_INT$salinity_ppt,
  t = Trt19_INT$temp_c_int,
  P = 1.013253
) %>%
  summary()
Trt19INT[[7]]

Trt19INT58<-convert_rate(
  Trt19_INT $INT_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt19_INT$volume_ml/1000,
  mass = Trt19_INT$mass_whole_g/1000,
  S = Trt19_INT$salinity_ppt,
  t = 19.1,
  P = 1.013253
) %>%
  summary()
Trt19INT58[[7]]
##Trt22
Trt22_INT <- read_excel("./output/Iqaluit_Trt22.xlsx")
Trt22INT<-convert_rate(
  Trt22_INT $INT_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt22_INT$volume_ml/1000,
  mass = Trt22_INT$mass_whole_g/1000,
  S= 31.5,
  #S = Trt22_INT$salinity_ppt,
  t = Trt22_INT$temp_c_int,
  P = 1.013253
) %>%
  summary()
Trt22INT[[7]]

Trt22INT58<-convert_rate(
  Trt22_INT $INT_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt22_INT$volume_ml/1000,
  mass = Trt22_INT$mass_whole_g/1000,
  S= 31.5,
  #S = Trt22_INT$salinity_ppt,
  t = 7.04,
  P = 1.013253
) %>%
  summary()

rm(list=ls())
### mass specific at Target
## TRt7
Trt7_MD <- read_excel("./output/Iqaluit_Trt7.xlsx")
Trt7MD<-convert_rate(
  Trt7_MD$MD_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt7_MD$volume_ml/1000,
  mass = Trt7_MD$mass_whole_g/1000,
  S = Trt7_MD$salinity_ppt,
  t = Trt7_MD$temp_c_md,
  P = 1.013253
) %>%
  summary()
Trt7MD[[7]]


###Trt 10
Trt10_MD <- read_excel("./output/Iqaluit_Trt10.xlsx")
Trt10MD<-convert_rate(
  Trt10_MD $MD_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt10_MD$volume_ml/1000,
  mass = Trt10_MD$mass_whole_g/1000,
  S = Trt10_MD$salinity_ppt,
  t = Trt10_MD$temp_c_md,
  P = 1.013253
) %>%
  summary()
Trt10_MD[[7]]


###TRT13
Trt13_MD <- read_excel("./output/Iqaluit_Trt13.xlsx")
Trt13MD<-convert_rate(
  Trt13_MD $MD_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt13_MD$volume_ml/1000,
  mass = Trt13_MD$mass_whole_g/1000,
  S = Trt13_MD$salinity_ppt,
  t = Trt13_MD$temp_c_md,
  P = 1.013253
) %>%
  summary()
Trt13MD[[7]]



##Trt16
Trt16_MD <- read_excel("./output/Iqaluit_Trt16.xlsx")
Trt16MD<-convert_rate(
  Trt16_MD$MD_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt16_MD$volume_ml/1000,
  mass = Trt16_MD$mass_whole_g/1000,
  S = Trt16_MD$salinity_ppt,
  t = Trt16_MD$temp_c_md,
  P = 1.013253
) %>%
  summary()
Trt16MD[[7]]

##Trt19
Trt19_MD <- read_excel("./output/Iqaluit_Trt19.xlsx")
Trt19MD<-convert_rate(
  Trt19_MD $MD_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt19_MD$volume_ml/1000,
  mass = Trt19_MD$mass_whole_g/1000,
  S = Trt19_MD$salinity_ppt,
  t = Trt19_MD$temp_c_md,
  P = 1.013253
) %>%
  summary()
Trt19MD[[7]]

  
##Trt22
Trt22_MD <- read_excel("./output/Iqaluit_Trt22.xlsx")
Trt22MD<-convert_rate(
  Trt22_MD $MD_corrected,
  oxy.unit = '%Air',
  time.unit = 's',
  output.unit = 'mg/h/kg',
  volume = Trt22_MD$volume_ml/1000,
  mass = Trt22_MD$mass_whole_g/1000,
  S = Trt22_MD$salinity_ppt,
  t = Trt22_MD$temp_c_md,
  P = 1.013253
) %>%
  summary()



######### Analyze mass specific rate
dt <-read_excel("./output/Iqaluit_convertedrate_raw.xlsx")
dt <- dt %>% mutate( Treatment=Trt)
dt <- dt %>% group_by(Treatment)

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
##not a normal distribution, cant use anova. 

library(emmeans)
#anova(mo1)
summary(mo1)
anova(mo1)

emm_sp <- emmeans(mo1, specs = "Treatment")
emm_sp
c<-pairs(emm_sp)
c<-as.data.frame(c)
c
#### no significance was found
g <-data_delta %>%
  group_by(Treatment) %>%
  get_summary_stats (Delta, type = "mean_sd")%>%
  as.data.frame()
g

g1 <-ggplot(data=data_delta, aes(x= Treatment, y=Delta, colour= Treatment))+
  geom_boxplot()+
  geom_point(alpha=0.5)+
  theme_classic()+
  theme(text = element_text(size=14))+
  labs(
       x = "Treatment",
       y = "Delta_Oxygen_Consumption_mgO2/hour/kg")
g1
g2 <- g1 + scale_colour_manual(values=c("black","black", "black", 'black', "black", "black"))
ggsave(plot= g2, filename = "output/Result_graph/Deltaplot_raw_all_values_points.png")
g2

#### remove outlier in plot not in the analysis
g_no <-ggplot(data=data_delta, aes(x= Treatment, y=Delta, fill= Treatment))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(alpha=0.5)+
  labs(title = "Oxygen Consumption at Elevating Temperatures
Northern population",
       x = "Treatment",
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
  labs(title = "Oxygen Consumption at Elevating Temperatures
Northern population",
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
  labs(title = "Oxygen Consumption at Elevating Temperatures
Northern population",
       x = "Treatments",
       y = "Delta_Oxygen_Consumption_mgO2/hour/kg")
gno_no1
gno_no2 <- gno_no1 + scale_colour_manual(values=c("lightblue","pink", "green", 'orange', "red", "brown"))
ggsave(plot= gno_no2, filename = "output/Result_graph/Deltaplot_raw_all_values_no_geom_no_outliershape.png")


######
DT <-read_excel("./output/Iqaluit_convertedrate_raw.xlsx")
DT <- DT %>% mutate ("7°C"=INT_converted_possitive,
                     Target = MD_converted_posstive,
                     Treatment= Trt)

DT <- DT %>%
  gather(key = "time", value = "Oxygen", "7°C", Target) %>%
  convert_as_factor(Treatment, time)
tail(DT)

#### check normality: VERY good if (p>0.05)

DT %>%
  group_by(time, Treatment) %>%
  shapiro_test(Oxygen)


DT$Treatment <- as.factor(DT$Treatment)
#assess homegenity of variance
leveneTest(Oxygen~Trt, data= DT)

#normality
shapiro.test(DT$Oxygen)



 #not fit the assumption
###does not fit the normal distribution event with data transformation

#m1 <- lm(Oxygen ~ Trt*time, data= DT)
#b <- MASS::boxcox(m1)
#b$x[b$y == max(b$y)]
#DT$Trt <- as.factor(DT$Trt)

str(DT)
###transformation
#DT$logOxygen <- log(DT$Oxygen) 
### asset normal distribution again
    #assess homegenity of variance
#leveneTest(logOxygen~Trt, data=DT)


#normality
#shapiro.test(DT$logOxygen)
### use repeated anova
###identify outliers
#out<- DT %>%
 # group_by(time) %>%
  #identify_outliers(logOxygen)
#out

#DT %>%
 # group_by(time) %>%
 # shapiro_test(logOxygen)
#### CANNOT USE ANOVA METHOD BECAUSE IT"S NOT NORMALLY DISTRIBUTED

##USE LINEAR MODEL WITH INTERACTION


ml1 <- lm(Oxygen ~ Treatment, data=DT)
anova(ml1)
summary(ml1)
emm_sp1 <- emmeans(ml1, specs =c ("Treatment"))
emm_sp1
cp11<-pairs(emm_sp1)
cp11

model <-lm(Oxygen ~ Treatment*time, data=DT)
summary(model, correlation = TRUE)
anova(model)

emm_sp <- emmeans(model, specs =c ("Treatment", "time"))
emm_sp
cp1<-pairs(emm_sp)
cp1
### no significance was found
# pairwise comparisons
library(xlsx)
write.xlsx(file="output/Result_tables/pairwise.xlsx",cp1)

#stat <-dt_trt %>%
  #group_by(Trt,time) %>%
  #get_summary_stats (Oxygen, type = "mean_sd")
#stat

library(ggpubr)
DT <- mutate(DT, Trt = factor(Treatment, levels = c("7", "10", "13", "16", "19", "22")))

###no geom_point, hide outlier shape
bxp <- ggboxplot (DT, x= "time", y = "Oxygen", color= "Treatment", palette= "jco",
                  outlier.shape = NA, add = "jitter"
              )
bxp

bxp<-bxp + labs(
  x = "Time",
  y = "mgO2/hour/kg",
  colour = "Treatment")
bxp
###'

box <- ggplot(data = DT, aes(x = time, y = Oxygen, colour = Treatment)) +
  geom_boxplot(outlier.shape = -1) +
  scale_colour_manual(values=c("grey60","grey50", "grey40", 'grey30', "grey20", "grey1")) +
  geom_point(position=position_dodge(width=0.75),aes(group=Treatment),alpha = 0.6) +
  labs(
    x = "Time",
    y = "mgO2/hour/kg",
    fill = "Treatment") +
  theme_classic()+
  theme(text = element_text(size=14))

box
ggsave(plot=box, filename= "output/Result_graph/before_after_plot.png")

##### with outlier shape
bxp1 <- ggboxplot (DT, x= "time", y = "Oxygen", color= "Treatment", palette= "jco"
)
bxp1

bxp1<-bxp1 + labs(
  x = "Time",
  y = "mgO2/hour/kg",
  colour = "Treatment")
bxp1
ggsave(plot=bxp1, filename= "output/Result_graph/before_after_plot_with_outliershape.png")




temp_dt <-dt %>% group_by(Trt)
summarize(temp_dt,
          mean_INT = mean (temp_c_int),
          sd_INT = sd (temp_c_int),
          mean_MD = mean (temp_c_md),
          sd_MD = sd (temp_c_md))


#### create subset of each treatment
DT <- group_by(DT, Treatment, time)
DT$time <- as.character(DT$time)
trt7 <-DT[which(DT$Treatment=='7'),]
trt10 <-DT[which(DT$Treatment=='10'),]
trt13 <-DT[which(DT$Treatment=='13'),]
trt16 <-DT[which(DT$Treatment=='16'),]
trt19 <-DT[which(DT$Treatment=='19'),]
trt22 <-DT[which(DT$Treatment=='22'),]

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
g7 <-g7 + scale_y_continuous(limits = c(0, 40))
g7<-g7 +
  theme(axis.text=element_text(size=12),                                            axis.title=element_text(size=12),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.tag.position = "top")
g7


g10<-ggplot(data=trt10,aes(x = time, y = Oxygen))  +
  geom_point(aes(group= Clam_ID), shape =21) +
  geom_line(aes(group=Clam_ID))+
  theme_classic()
g10 <-g10 + scale_y_continuous(limits = c(0, 40))
g10<-g10  +
  theme(axis.text=element_text(size=12),                                            axis.title=element_text(size=12),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.tag.position = "top")
g10


g13<-ggplot(data=trt13,aes(x = time, y = Oxygen))  +
  geom_point(aes(group= Clam_ID), shape =21) +
  geom_line(aes(group=Clam_ID))+
  theme_classic()
g13 <-g13 + scale_y_continuous(limits = c(0, 40))
g13<-g13 +
  theme(axis.text=element_text(size=12),                                            axis.title=element_text(size=12),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.tag.position = "top")

g13

g16<-ggplot(data=trt16,aes(x = time, y = Oxygen))  +
  geom_point(aes(group= Clam_ID), shape =21) +
  geom_line(aes(group=Clam_ID))+
  theme_classic()
g16 <- g16 + scale_y_continuous(limits = c(0, 40))
g16<-g16 +
  theme(axis.text=element_text(size=12),                                            axis.title=element_text(size=12),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.tag.position = "top")
g16

g19<-ggplot(data=trt19,aes(x = time, y = Oxygen))  +
  geom_point(aes(group= Clam_ID), shape =21) +
  geom_line(aes(group=Clam_ID))+
  theme_classic()
g19 <- g19 + scale_y_continuous(limits = c(0, 40))
g19<-g19 +
  theme(axis.text=element_text(size=12),                                            axis.title=element_text(size=12),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        plot.tag.position = "top")
g19

g22<-ggplot(data=trt22,aes(x = time, y = Oxygen))  +
  geom_point(aes(group= Clam_ID), shape =21) +
  geom_line(aes(group=Clam_ID))+
  theme_classic()
g22 <- g22 + scale_y_continuous(limits = c(0, 40))
g22<-g22 +
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
                    font.label = list(size = 8, color = "black", face = "bold", family = NULL, position = "top"))
fig

all<-annotate_figure(fig,
                bottom = text_grob("Measuring Period", color = "black", size = 14))
all
ggsave(plot=all, file="./output/Result_graph/Iqaluit_trt_pannels.jpeg", height = 10, width = 6.25, units = "in")


##### test if the dates cause an impact on MO2

c7 <-read_excel("./output/Iqaluit_Trt7_test.xlsx")
c7 <- c7 %>% rename("7°C"= INT_converted,
                    Target = MD_converted,
                    Treatment=Trt)
c7 <- c7 %>%
  gather(key = "time", value = "Oxygen", "7°C", Target) %>%
  convert_as_factor(Treatment, time)


ml <-lm(Oxygen ~ Treatment*time, data=c7)
summary(ml, correlation = TRUE)
anova(ml)

emm_sp <- emmeans(ml, specs =c ("Treatment", "time"))
cpt<-pairs(emm_sp)
cpt


c10 <-read_excel("./output/Iqaluit_Trt10_test.xlsx")
c10 <- c10 %>% rename("7°C"= INT_converted,
                    Target = MD_converted,
                    Treatment=Trt)
c10 <- c10 %>%
  gather(key = "time", value = "Oxygen", "7°C", Target) %>%
  convert_as_factor(Treatment, time)


ml10 <-lm(Oxygen ~ Treatment*time, data=c10)
summary(ml10, correlation = TRUE)
anova(ml10)

emm_sp10 <- emmeans(ml10, specs =c ("Treatment", "time"))
cpt10<-pairs(emm_sp10)
cpt10


c22 <-read_excel("./output/Iqaluit_Trt22_test.xlsx")
c22 <- c22 %>% rename("7°C"= INT_converted,
                    Target = MD_converted,
                    Treatment=Trt)
c22 <- c22 %>%
  gather(key = "time", value = "Oxygen", "7°C", Target) %>%
  convert_as_factor(Treatment, time)


ml22 <-lm(Oxygen ~ Treatment*time, data=c22)
summary(ml22, correlation = TRUE)
anova(ml22)

emm_sp22 <- emmeans(ml22, specs =c ("Treatment", "time"))
cpt22<-pairs(emm_sp22)
cpt22

#### random effect
m <- lmer(Delta ~ Trt + (1| Date_order), data=dt)
arm::display(m)
summary(m)
#### plot background
dt <-read_excel("./output/Iqaluit_convertedrate_raw.xlsx")
dt$Trt <-as.character(dt$Trt)
dt <- dt %>% group_by(Trt)
dt<- mutate(dt, Trt = factor(Trt, levels = c("7", "10", "13", "16", "19", "22")))

box_bg <- ggplot(data = dt, aes(x = Trt, y = (BG_at_target*(-1) ))) +
  geom_boxplot() +
  geom_point(alpha = 0.6) +
  labs(
    x = "Treatment",
    y = "rate",
    colour = "Treatment") +
  theme_classic()
box_bg
ggsave(plot=box_bg, file="./output/Result_graph/Iqaluit_BG_rate.jpeg")





