library(respR)
library(tidyverse)
library(respR)
library(lubridate)
library(tidyverse)
library(dplyr)
library(readxl)
library(janitor)
d118BG <- read.csv("./Data/2022-08-18-C14BG15_raw.csv")
d218BG <- read.csv("./Data/2022-08-18-C58BG15_raw.csv")

d119BG <- read.csv("./Data/2022-08-19-C14BG15_raw.csv")
d219BG <- read.csv("./Data/2022-08-19-C58BG15_raw.csv")

d120BG <- read.csv("./Data/2022-08-20-C14BG15_raw.csv")
d220BG <- read.csv("./Data/2022-08-20-C58BG15_raw.csv")

d121BG <- read.csv("./Data/2022-08-21-C14BG15_raw.csv")
d221BG <- read.csv("./Data/2022-08-21-C58BG15_raw.csv")

d122BG <- read.csv("./Data/2022-08-22-C14BG15_raw.csv")
d222BG <- read.csv("./Data/2022-08-22-C58BG15_raw.csv")

d123BG <- read.csv("./Data/2022-08-23-C14BG15_raw.csv")
d223BG <- read.csv("./Data/2022-08-23-C58BG15_raw.csv")

d124BG <- read.csv("./Data/2022-08-24-C14BG15_raw.csv")
d224BG <- read.csv("./Data/2022-08-24-C58BG15_raw.csv")


####INT
d118INT <- read.csv("./Data/2022-08-18-C14INT15_raw.csv")
d218INT <- read.csv("./Data/2022-08-18-C58INT15_raw.csv")

d119INT <- read.csv("./Data/2022-08-19-C14INT15_raw.csv")
d219INT <- read.csv("./Data/2022-08-19-C58INT15_raw.csv")

d120INT <- read.csv("./Data/2022-08-20-C14INT15_raw.csv")
d220INT <- read.csv("./Data/2022-08-20-C58INT15_raw.csv")

d121INT <- read.csv("./Data/2022-08-21-C14INT15_raw.csv")
d221INT <- read.csv("./Data/2022-08-21-C58INT15_raw.csv")

d122INT <- read.csv("./Data/2022-08-22-C14INT15_raw.csv")
d222INT <- read.csv("./Data/2022-08-22-C58INT15_raw.csv")

d123INT <- read.csv("./Data/2022-08-23-C14INT15_raw.csv")
d223INT <- read.csv("./Data/2022-08-23-C58INT15_raw.csv")

d124INT <- read.csv("./Data/2022-08-24-C14INT15_raw.csv")
d224INT <- read.csv("./Data/2022-08-24-C58INT15_raw.csv")

c118INT<-inspect(d118INT, time = 1, oxygen = c (8)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c218INT<-inspect(d118INT, time = 1, oxygen = c (11)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c318INT<-inspect(d118INT, time = 1, oxygen = c (14)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c418INT<-inspect(d118INT, time = 1, oxygen = c (17)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c518INT<-inspect(d218INT, time = 1, oxygen = c (20)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c618INT<-inspect(d218INT, time = 1, oxygen = c (23)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c718INT<-inspect(d218INT, time = 1, oxygen = c (26)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c818INT<-inspect(d218INT, time = 1, oxygen = c (29)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()
##day19
c119INT<-inspect(d119INT, time = 1, oxygen = c (8)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c219INT<-inspect(d119INT, time = 1, oxygen = c (11)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c319INT<-inspect(d119INT, time = 1, oxygen = c (14)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c419INT<-inspect(d119INT, time = 1, oxygen = c (17)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c519INT<-inspect(d219INT, time = 1, oxygen = c (20)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c619INT<-inspect(d219INT, time = 1, oxygen = c (23)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c719INT<-inspect(d219INT, time = 1, oxygen = c (26)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c819INT<-inspect(d219INT, time = 1, oxygen = c (29)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

#day 20
c120INT<-inspect(d120INT, time = 1, oxygen = c (8)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c220INT<-inspect(d120INT, time = 1, oxygen = c (11)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c320INT<-inspect(d120INT, time = 1, oxygen = c (14)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c420INT<-inspect(d120INT, time = 1, oxygen = c (17)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c520INT<-inspect(d220INT, time = 1, oxygen = c (20)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c620INT<-inspect(d220INT, time = 1, oxygen = c (23)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c720INT<-inspect(d220INT, time = 1, oxygen = c (26)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c820INT<-inspect(d220INT, time = 1, oxygen = c (29)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

##day 21
c121INT<-inspect(d121INT, time = 1, oxygen = c (8)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c221INT<-inspect(d121INT, time = 1, oxygen = c (11)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c321INT<-inspect(d121INT, time = 1, oxygen = c (14)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c421INT<-inspect(d121INT, time = 1, oxygen = c (17)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c521INT<-inspect(d221INT, time = 1, oxygen = c (20)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c621INT<-inspect(d221INT, time = 1, oxygen = c (23)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c721INT<-inspect(d221INT, time = 1, oxygen = c (26)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c821INT<-inspect(d221INT, time = 1, oxygen = c (29)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

##day 22
c122INT<-inspect(d122INT, time = 1, oxygen = c (8)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c222INT<-inspect(d122INT, time = 1, oxygen = c (11)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c322INT<-inspect(d122INT, time = 1, oxygen = c (14)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c422INT<-inspect(d122INT, time = 1, oxygen = c (17)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c522INT<-inspect(d222INT, time = 1, oxygen = c (20)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c622INT<-inspect(d222INT, time = 1, oxygen = c (23)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c722INT<-inspect(d222INT, time = 1, oxygen = c (26)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c822INT<-inspect(d222INT, time = 1, oxygen = c (29)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()
###day 23
c123INT<-inspect(d123INT, time = 1, oxygen = c (8)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c223INT<-inspect(d123INT, time = 1, oxygen = c (11)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c323INT<-inspect(d123INT, time = 1, oxygen = c (14)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c423INT<-inspect(d123INT, time = 1, oxygen = c (17)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c523INT<-inspect(d223INT, time = 1, oxygen = c (20)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c623INT<-inspect(d223INT, time = 1, oxygen = c (23)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c723INT<-inspect(d223INT, time = 1, oxygen = c (26)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c823INT<-inspect(d223INT, time = 1, oxygen = c (29)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

##day 24
c124INT<-inspect(d124INT, time = 1, oxygen = c (8)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c224INT<-inspect(d124INT, time = 1, oxygen = c (11)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c324INT<-inspect(d124INT, time = 1, oxygen = c (14)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c424INT<-inspect(d124INT, time = 1, oxygen = c (17)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c524INT<-inspect(d224INT, time = 1, oxygen = c (20)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c624INT<-inspect(d224INT, time = 1, oxygen = c (23)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c724INT<-inspect(d224INT, time = 1, oxygen = c (26)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()

c824INT<-inspect(d224INT, time = 1, oxygen = c (29)) %>%
  auto_rate(method = "linear", width = 900, by = "time", plot = TRUE) %>%
  summary()


data_INT <-read_csv("./Data/convert_data_INT.csv")
convert_rate(
  data_INT$adjusted_rate,
  oxy.unit = "%Air",
  time.unit = "s",
  output.unit = "mgO2/h/kg",
  volume = data_INT$volume_l,
  mass = data_INT$mass_kg,
  S = data_INT$salinity_ppt,
  t = data_INT$temp_c,
  P = 1.013253) %>%
  summary()

