library(tidyverse)
library(readxl)
library(stringr)
library(corrplot)

# data_SEER_male <- read.csv('C:/Workspace/John Ji/Cancer and climate change/Cl5-data/SEER data/male.csv')
# data_SEER_male$Sex <- 1
# data_SEER_allsex <- read.csv('C:/Workspace/John Ji/Cancer and climate change/Cl5-data/SEER data/female.csv')
# data_SEER_female$Sex <- 2
# data_SEER <- rbind(data_SEER_female,data_SEER_male)
# # Transform the "Year" column into separate variables
# data_SEER_wide <- data_SEER %>%
#   select(-Count,-Population) %>% 
#   spread(key = Age.recode.with..1.year.olds, value = Age.Adjusted.Rate, sep = "")
# cancer_list <- c("All Sites", "Lip","Salivary Gland","Tonsil","Nasopharynx",
#                  "Hypopharynx","Esophagus","Stomach","Colon and Rectum","Liver",
#                  "Gallbladder","Pancreas","Larynx","Lung and Bronchus","Melanoma of the Skin",
#                  "Mesothelioma","Kaposi Sarcoma","Breast","Vulva","Vagina","Cervix Uteri",
#                  "Corpus Uteri","Ovary","Penis","Prostate","Testis","Kidney and Renal Pelvis",
#                  "Urinary Bladder","Brain and Other Nervous System","Thyroid","Hodgkin Lymphoma",
#                  "Non-Hodgkin Lymphoma","Myeloma","Lymphocytic Leukemia")
# filtered_SEER <- data_SEER_wide %>%
#   filter(Site.recode.ICD.O.3.WHO.2008 %in% cancer_list) %>% 
#   rename(CANCER = Site.recode.ICD.O.3.WHO.2008,YEAR = Year.of.diagnosis,
#          N0=`Age.recode.with..1.year.olds00 years`,N0_4=`Age.recode.with..1.year.olds01-04 years`,
#          N5_9=`Age.recode.with..1.year.olds05-09 years`, N10_14=`Age.recode.with..1.year.olds10-14 years`,
#          N15_19=`Age.recode.with..1.year.olds15-19 years`, N20_24=`Age.recode.with..1.year.olds20-24 years`,
#          N25_29=`Age.recode.with..1.year.olds25-29 years`, N30_34=`Age.recode.with..1.year.olds30-34 years`,
#          N35_39=`Age.recode.with..1.year.olds35-39 years`, N40_44=`Age.recode.with..1.year.olds40-44 years`,
#          N45_49=`Age.recode.with..1.year.olds45-49 years`, N50_54=`Age.recode.with..1.year.olds50-54 years`,
#          N55_59=`Age.recode.with..1.year.olds55-59 years`, N60_64=`Age.recode.with..1.year.olds60-64 years`,
#          N65_69=`Age.recode.with..1.year.olds65-69 years`, N70_74=`Age.recode.with..1.year.olds70-74 years`,
#          N75_79=`Age.recode.with..1.year.olds75-79 years`, N80_84=`Age.recode.with..1.year.olds80-84 years`,
#          N85.=`Age.recode.with..1.year.olds85+ years`,N_UNK=Age.recode.with..1.year.oldsUnknown,
#          REGISTRY=State.county)
# unique(filtered_SEER$REGISTRY)
# getwd()
# setwd('C:/Workspace/John Ji/Cancer and climate change/Cl5-data/SEER data')
# save(filtered_SEER, file = "filtered_SEER.Rdata")
######################Merge SEER data#################################
# load('C:/Haowen Wang/Cancer and climate change/SEER data/filtered_SEER.Rdata')
# load('C:/Haowen Wang/Cancer and climate change/SEER data/filtered_SEER_P.Rdata')
# str(filtered_SEER)
data_enviro_US <- read.csv('C:/Haowen Wang/Cancer and climate change/SEER data/Merged-enviro.csv')
load('C:/Haowen Wang/Cancer and climate change/SEER data/data_all_SEER.Rdata')

# 
# filtered_SEER$GEOID <- str_extract(filtered_SEER$REGISTRY, "\\((\\d{5})\\)")
# filtered_SEER$GEOID <- str_replace_all(filtered_SEER$GEOID, "[()]", "")
# filtered_SEER$GEOID <- as.numeric(filtered_SEER$GEOID)
# filtered_SEER_P$GEOID <- str_extract(filtered_SEER$REGISTRY, "\\((\\d{5})\\)")
# filtered_SEER_P$GEOID <- str_replace_all(filtered_SEER$GEOID, "[()]", "")
# filtered_SEER_P$GEOID <- as.numeric(filtered_SEER$GEOID)
# data_all_SEER <- merge(filtered_SEER_P,filtered_SEER,by=c("REGISTRY","YEAR","Sex","CANCER","GEOID"))
# data_all_SEER <- data_all_SEER %>%
#   mutate(across(starts_with("N"), as.numeric))
 

#Change unit
colnames(data_enviro_US)[colnames(data_enviro_US) == "NDVI"] <- "NDVI1"
data_enviro_US$NDVI <- (data_enviro_US$NDVI1)*10
colnames(data_enviro_US)[colnames(data_enviro_US) == "Ozone"] <- "Ozone1"
data_enviro_US$Ozone <- (data_enviro_US$Ozone1)/10
colnames(data_enviro_US)[colnames(data_enviro_US) == "Precipitation"] <- "Precipitation1"
data_enviro_US$Precipitation <- (data_enviro_US$Precipitation1)*1000
colnames(data_enviro_US)[colnames(data_enviro_US) == "skin_temperature"] <- "skin_temperature1"
data_enviro_US$skin_temperature <- (data_enviro_US$skin_temperature1)-273.15

#lag term NDVI
data_enviro_US <- data_enviro_US %>%
  arrange(GEOID, YEAR)

lag1 <- c(NA, data_enviro_US$NDVI[-nrow(data_enviro_US)])
lag2 <- c(NA, lag1[-nrow(data_enviro_US)])
lag3 <- c(NA, lag2[-nrow(data_enviro_US)])
lag4 <- c(NA, lag3[-nrow(data_enviro_US)])
lag5 <- c(NA, lag4[-nrow(data_enviro_US)])
lag6 <- c(NA, lag5[-nrow(data_enviro_US)])
lag7 <- c(NA, lag6[-nrow(data_enviro_US)])
lag8 <- c(NA, lag7[-nrow(data_enviro_US)])
lag9 <- c(NA, lag8[-nrow(data_enviro_US)])
lag10 <- c(NA, lag9[-nrow(data_enviro_US)])
data_enviro_US$lag1 <- lag1
data_enviro_US$lag2 <- lag2
data_enviro_US$lag3 <- lag3
data_enviro_US$lag4 <- lag4
data_enviro_US$lag5 <- lag5
data_enviro_US$lag6 <- lag6
data_enviro_US$lag7 <- lag7
data_enviro_US$lag8 <- lag8
data_enviro_US$lag9 <- lag9
data_enviro_US$lag10 <- lag10

#lag term temp
tlag1 <- c(NA, data_enviro_US$skin_temperature[-nrow(data_enviro_US)])
tlag2 <- c(NA, tlag1[-nrow(data_enviro_US)])
tlag3 <- c(NA, tlag2[-nrow(data_enviro_US)])
tlag4 <- c(NA, tlag3[-nrow(data_enviro_US)])
tlag5 <- c(NA, tlag4[-nrow(data_enviro_US)])
tlag6 <- c(NA, tlag5[-nrow(data_enviro_US)])
tlag7 <- c(NA, tlag6[-nrow(data_enviro_US)])
tlag8 <- c(NA, tlag7[-nrow(data_enviro_US)])
tlag9 <- c(NA, tlag8[-nrow(data_enviro_US)])
tlag10 <- c(NA, tlag9[-nrow(data_enviro_US)])
data_enviro_US$tlag1 <- tlag1
data_enviro_US$tlag2 <- tlag2
data_enviro_US$tlag3 <- tlag3
data_enviro_US$tlag4 <- tlag4
data_enviro_US$tlag5 <- tlag5
data_enviro_US$tlag6 <- tlag6
data_enviro_US$tlag7 <- tlag7
data_enviro_US$tlag8 <- tlag8
data_enviro_US$tlag9 <- tlag9
data_enviro_US$tlag10 <- tlag10

#lagged term precipitation
plag1 <- c(NA, data_enviro_US$Precipitation[-nrow(data_enviro_US)])
plag2 <- c(NA, plag1[-nrow(data_enviro_US)])
plag3 <- c(NA, plag2[-nrow(data_enviro_US)])
plag4 <- c(NA, plag3[-nrow(data_enviro_US)])
plag5 <- c(NA, plag4[-nrow(data_enviro_US)])
plag6 <- c(NA, plag5[-nrow(data_enviro_US)])
plag7 <- c(NA, plag6[-nrow(data_enviro_US)])
plag8 <- c(NA, plag7[-nrow(data_enviro_US)])
plag9 <- c(NA, plag8[-nrow(data_enviro_US)])
plag10 <- c(NA, plag9[-nrow(data_enviro_US)])
data_enviro_US$plag1 <- plag1
data_enviro_US$plag2 <- plag2
data_enviro_US$plag3 <- plag3
data_enviro_US$plag4 <- plag4
data_enviro_US$plag5 <- plag5
data_enviro_US$plag6 <- plag6
data_enviro_US$plag7 <- plag7
data_enviro_US$plag8 <- plag8
data_enviro_US$plag9 <- plag9
data_enviro_US$plag10 <- plag10


#lagged term Ozone
olag1 <- c(NA, data_enviro_US$Ozone[-nrow(data_enviro_US)])
olag2 <- c(NA, olag1[-nrow(data_enviro_US)])
olag3 <- c(NA, olag2[-nrow(data_enviro_US)])
olag4 <- c(NA, olag3[-nrow(data_enviro_US)])
olag5 <- c(NA, olag4[-nrow(data_enviro_US)])
olag6 <- c(NA, olag5[-nrow(data_enviro_US)])
olag7 <- c(NA, olag6[-nrow(data_enviro_US)])
olag8 <- c(NA, olag7[-nrow(data_enviro_US)])
olag9 <- c(NA, olag8[-nrow(data_enviro_US)])
olag10 <- c(NA, olag9[-nrow(data_enviro_US)])
data_enviro_US$olag1 <- olag1
data_enviro_US$olag2 <- olag2
data_enviro_US$olag3 <- olag3
data_enviro_US$olag4 <- olag4
data_enviro_US$olag5 <- olag5
data_enviro_US$olag6 <- olag6
data_enviro_US$olag7 <- olag7
data_enviro_US$olag8 <- olag8
data_enviro_US$olag9 <- olag9
data_enviro_US$olag10 <- olag10

#lagged term solar
slag1 <- c(NA, data_enviro_US$Solar_radiation[-nrow(data_enviro_US)])
slag2 <- c(NA, slag1[-nrow(data_enviro_US)])
slag3 <- c(NA, slag2[-nrow(data_enviro_US)])
slag4 <- c(NA, slag3[-nrow(data_enviro_US)])
slag5 <- c(NA, slag4[-nrow(data_enviro_US)])
slag6 <- c(NA, slag5[-nrow(data_enviro_US)])
slag7 <- c(NA, slag6[-nrow(data_enviro_US)])
slag8 <- c(NA, slag7[-nrow(data_enviro_US)])
slag9 <- c(NA, slag8[-nrow(data_enviro_US)])
slag10 <- c(NA, slag9[-nrow(data_enviro_US)])
data_enviro_US$slag1 <- slag1
data_enviro_US$slag2 <- slag2
data_enviro_US$slag3 <- slag3
data_enviro_US$slag4 <- slag4
data_enviro_US$slag5 <- slag5
data_enviro_US$slag6 <- slag6
data_enviro_US$slag7 <- slag7
data_enviro_US$slag8 <- slag8
data_enviro_US$slag9 <- slag9
data_enviro_US$slag10 <- slag10

#create variable for different time window
baseline <- data_enviro_US %>%
  filter(YEAR >= 1990 & YEAR <= 1999) %>%
  group_by(GEOID) %>%
  summarize(baseline_temperature = mean(skin_temperature, na.rm = TRUE),
            baseline_NDVI = mean(NDVI, na.rm = TRUE),
            baseline_ozone = mean(Ozone, na.rm = TRUE),
            baseline_Precipitation = mean(Precipitation, na.rm = TRUE),
            baseline_Solar = mean(Solar_radiation, na.rm = TRUE))

data_enviro_US_new <- left_join(data_enviro_US, baseline, by = "GEOID")

data_enviro_US_new <- data_enviro_US_new %>%
  arrange(GEOID, YEAR) %>%
  group_by(GEOID) %>%
  mutate(period1 = (NDVI + lag(NDVI, 1)) / 2,
         period2 = (NDVI + lag(NDVI, 1) + lag(NDVI, 2)) / 3,
         period3 = (NDVI + lag(NDVI, 1) + lag(NDVI, 2) + lag(NDVI, 3)) / 4,
         period4 = (NDVI + lag(NDVI, 1) + lag(NDVI, 2) + lag(NDVI, 3) + lag(NDVI, 4)) / 5,
         period5 = (NDVI + lag(NDVI, 1) + lag(NDVI, 2) + lag(NDVI, 3) + lag(NDVI, 4) + lag(NDVI, 5)) / 6,
         period6 = (NDVI + lag(NDVI, 1) + lag(NDVI, 2) + lag(NDVI, 3) + lag(NDVI, 4) + lag(NDVI, 5) + lag(NDVI, 6)) / 7,
         period7 = (NDVI + lag(NDVI, 1) + lag(NDVI, 2) + lag(NDVI, 3) + lag(NDVI, 4) + lag(NDVI, 5) + lag(NDVI, 6) + lag(NDVI, 7)) / 8,
         period8 = (NDVI + lag(NDVI, 1) + lag(NDVI, 2) + lag(NDVI, 3) + lag(NDVI, 4) + lag(NDVI, 5) + lag(NDVI, 6) + lag(NDVI, 7) + lag(NDVI, 8)) / 9,
         period9 = (NDVI + lag(NDVI, 1) + lag(NDVI, 2) + lag(NDVI, 3) + lag(NDVI, 4) + lag(NDVI, 5) + lag(NDVI, 6) + lag(NDVI, 7) + lag(NDVI, 8) + lag(NDVI, 9)) / 10,
         period10 = (NDVI + lag(NDVI, 1) + lag(NDVI, 2) + lag(NDVI, 3) + lag(NDVI, 4) + lag(NDVI, 5) + lag(NDVI, 6) + lag(NDVI, 7) + lag(NDVI, 8) + lag(NDVI, 9) + lag(NDVI, 10)) / 11) %>%
  mutate(tperiod1 = (skin_temperature + lag(skin_temperature, 1)) / 2,
         tperiod2 = (skin_temperature + lag(skin_temperature, 1) + lag(skin_temperature, 2)) / 3,
         tperiod3 = (skin_temperature + lag(skin_temperature, 1) + lag(skin_temperature, 2) + lag(skin_temperature, 3)) / 4,
         tperiod4 = (skin_temperature + lag(skin_temperature, 1) + lag(skin_temperature, 2) + lag(skin_temperature, 3) + lag(skin_temperature, 4)) / 5,
         tperiod5 = (skin_temperature + lag(skin_temperature, 1) + lag(skin_temperature, 2) + lag(skin_temperature, 3) + lag(skin_temperature, 4) + lag(skin_temperature, 5)) / 6,
         tperiod6 = (skin_temperature + lag(skin_temperature, 1) + lag(skin_temperature, 2) + lag(skin_temperature, 3) + lag(skin_temperature, 4) + lag(skin_temperature, 5) + lag(skin_temperature, 6)) / 7,
         tperiod7 = (skin_temperature + lag(skin_temperature, 1) + lag(skin_temperature, 2) + lag(skin_temperature, 3) + lag(skin_temperature, 4) + lag(skin_temperature, 5) + lag(skin_temperature, 6) + lag(skin_temperature, 7)) / 8,
         tperiod8 = (skin_temperature + lag(skin_temperature, 1) + lag(skin_temperature, 2) + lag(skin_temperature, 3) + lag(skin_temperature, 4) + lag(skin_temperature, 5) + lag(skin_temperature, 6) + lag(skin_temperature, 7) + lag(skin_temperature, 8)) / 9,
         tperiod9 = (skin_temperature + lag(skin_temperature, 1) + lag(skin_temperature, 2) + lag(skin_temperature, 3) + lag(skin_temperature, 4) + lag(skin_temperature, 5) + lag(skin_temperature, 6) + lag(skin_temperature, 7) + lag(skin_temperature, 8) + lag(skin_temperature, 9)) / 10,
         tperiod10 = (skin_temperature + lag(skin_temperature, 1) + lag(skin_temperature, 2) + lag(skin_temperature, 3) + lag(skin_temperature, 4) + lag(skin_temperature, 5) + lag(skin_temperature, 6) + lag(skin_temperature, 7) + lag(skin_temperature, 8) + lag(skin_temperature, 9) + lag(skin_temperature, 10)) / 11) %>%
  mutate(operiod1 = (Ozone + lag(Ozone, 1)) / 2,
         operiod2 = (Ozone + lag(Ozone, 1) + lag(Ozone, 2)) / 3,
         operiod3 = (Ozone + lag(Ozone, 1) + lag(Ozone, 2) + lag(Ozone, 3)) / 4,
         operiod4 = (Ozone + lag(Ozone, 1) + lag(Ozone, 2) + lag(Ozone, 3) + lag(Ozone, 4)) / 5,
         operiod5 = (Ozone + lag(Ozone, 1) + lag(Ozone, 2) + lag(Ozone, 3) + lag(Ozone, 4) + lag(Ozone, 5)) / 6,
         operiod6 = (Ozone + lag(Ozone, 1) + lag(Ozone, 2) + lag(Ozone, 3) + lag(Ozone, 4) + lag(Ozone, 5) + lag(Ozone, 6)) / 7,
         operiod7 = (Ozone + lag(Ozone, 1) + lag(Ozone, 2) + lag(Ozone, 3) + lag(Ozone, 4) + lag(Ozone, 5) + lag(Ozone, 6) + lag(Ozone, 7)) / 8,
         operiod8 = (Ozone + lag(Ozone, 1) + lag(Ozone, 2) + lag(Ozone, 3) + lag(Ozone, 4) + lag(Ozone, 5) + lag(Ozone, 6) + lag(Ozone, 7) + lag(Ozone, 8)) / 9,
         operiod9 = (Ozone + lag(Ozone, 1) + lag(Ozone, 2) + lag(Ozone, 3) + lag(Ozone, 4) + lag(Ozone, 5) + lag(Ozone, 6) + lag(Ozone, 7) + lag(Ozone, 8) + lag(Ozone, 9)) / 10,
         operiod10 = (Ozone + lag(Ozone, 1) + lag(Ozone, 2) + lag(Ozone, 3) + lag(Ozone, 4) + lag(Ozone, 5) + lag(Ozone, 6) + lag(Ozone, 7) + lag(Ozone, 8) + lag(Ozone, 9) + lag(Ozone, 10)) / 11) %>%
mutate(pperiod1 = (Precipitation + lag(Precipitation, 1)) / 2,
         pperiod2 = (Precipitation + lag(Precipitation, 1) + lag(Precipitation, 2)) / 3,
         pperiod3 = (Precipitation + lag(Precipitation, 1) + lag(Precipitation, 2) + lag(Precipitation, 3)) / 4,
         pperiod4 = (Precipitation + lag(Precipitation, 1) + lag(Precipitation, 2) + lag(Precipitation, 3) + lag(Precipitation, 4)) / 5,
         pperiod5 = (Precipitation + lag(Precipitation, 1) + lag(Precipitation, 2) + lag(Precipitation, 3) + lag(Precipitation, 4) + lag(Precipitation, 5)) / 6,
         pperiod6 = (Precipitation + lag(Precipitation, 1) + lag(Precipitation, 2) + lag(Precipitation, 3) + lag(Precipitation, 4) + lag(Precipitation, 5) + lag(Precipitation, 6)) / 7,
         pperiod7 = (Precipitation + lag(Precipitation, 1) + lag(Precipitation, 2) + lag(Precipitation, 3) + lag(Precipitation, 4) + lag(Precipitation, 5) + lag(Precipitation, 6) + lag(Precipitation, 7)) / 8,
         pperiod8 = (Precipitation + lag(Precipitation, 1) + lag(Precipitation, 2) + lag(Precipitation, 3) + lag(Precipitation, 4) + lag(Precipitation, 5) + lag(Precipitation, 6) + lag(Precipitation, 7) + lag(Precipitation, 8)) / 9,
         pperiod9 = (Precipitation + lag(Precipitation, 1) + lag(Precipitation, 2) + lag(Precipitation, 3) + lag(Precipitation, 4) + lag(Precipitation, 5) + lag(Precipitation, 6) + lag(Precipitation, 7) + lag(Precipitation, 8) + lag(Precipitation, 9)) / 10,
         pperiod10 = (Precipitation + lag(Precipitation, 1) + lag(Precipitation, 2) + lag(Precipitation, 3) + lag(Precipitation, 4) + lag(Precipitation, 5) + lag(Precipitation, 6) + lag(Precipitation, 7) + lag(Precipitation, 8) + lag(Precipitation, 9) + lag(Precipitation, 10)) / 11) %>%
 mutate(speriod1 = (Solar_radiation + lag(Solar_radiation, 1)) / 2,
         speriod2 = (Solar_radiation + lag(Solar_radiation, 1) + lag(Solar_radiation, 2)) / 3,
         speriod3 = (Solar_radiation + lag(Solar_radiation, 1) + lag(Solar_radiation, 2) + lag(Solar_radiation, 3)) / 4,
         speriod4 = (Solar_radiation + lag(Solar_radiation, 1) + lag(Solar_radiation, 2) + lag(Solar_radiation, 3) + lag(Solar_radiation, 4)) / 5,
         speriod5 = (Solar_radiation + lag(Solar_radiation, 1) + lag(Solar_radiation, 2) + lag(Solar_radiation, 3) + lag(Solar_radiation, 4) + lag(Solar_radiation, 5)) / 6,
         speriod6 = (Solar_radiation + lag(Solar_radiation, 1) + lag(Solar_radiation, 2) + lag(Solar_radiation, 3) + lag(Solar_radiation, 4) + lag(Solar_radiation, 5) + lag(Solar_radiation, 6)) / 7,
         speriod7 = (Solar_radiation + lag(Solar_radiation, 1) + lag(Solar_radiation, 2) + lag(Solar_radiation, 3) + lag(Solar_radiation, 4) + lag(Solar_radiation, 5) + lag(Solar_radiation, 6) + lag(Solar_radiation, 7)) / 8,
         speriod8 = (Solar_radiation + lag(Solar_radiation, 1) + lag(Solar_radiation, 2) + lag(Solar_radiation, 3) + lag(Solar_radiation, 4) + lag(Solar_radiation, 5) + lag(Solar_radiation, 6) + lag(Solar_radiation, 7) + lag(Solar_radiation, 8)) / 9,
         speriod9 = (Solar_radiation + lag(Solar_radiation, 1) + lag(Solar_radiation, 2) + lag(Solar_radiation, 3) + lag(Solar_radiation, 4) + lag(Solar_radiation, 5) + lag(Solar_radiation, 6) + lag(Solar_radiation, 7) + lag(Solar_radiation, 8) + lag(Solar_radiation, 9)) / 10,
         speriod10 = (Solar_radiation + lag(Solar_radiation, 1) + lag(Solar_radiation, 2) + lag(Solar_radiation, 3) + lag(Solar_radiation, 4) + lag(Solar_radiation, 5) + lag(Solar_radiation, 6) + lag(Solar_radiation, 7) + lag(Solar_radiation, 8) + lag(Solar_radiation, 9) + lag(Solar_radiation, 10)) / 11) %>%
 mutate(average0 = (NDVI + lag(NDVI, 1)) / 2,
         average1 = (NDVI + lag(NDVI, 1) + lag(NDVI, 2)) / 3,
         average2 = (lag(NDVI, 1) + lag(NDVI, 2) + lag(NDVI, 3)) / 3,
         average3 = (lag(NDVI, 2) + lag(NDVI, 3) + lag(NDVI, 4)) / 3,
         average4 = (lag(NDVI, 3) + lag(NDVI, 4) + lag(NDVI, 5)) / 3,
         average5 = (lag(NDVI, 4) + lag(NDVI, 5) + lag(NDVI, 6)) / 3,
         average6 = (lag(NDVI, 5) + lag(NDVI, 6) + lag(NDVI, 7)) / 3,
         average7 = (lag(NDVI, 6) + lag(NDVI, 7) + lag(NDVI, 8)) / 3,
         average8 = (lag(NDVI, 7) + lag(NDVI, 8) + lag(NDVI, 9)) / 3,
         average9 = (lag(NDVI, 8) + lag(NDVI, 9) + lag(NDVI, 10)) / 3,
         average10 = (lag(NDVI, 9) + lag(NDVI, 10) + lag(NDVI, 11)) / 3)%>%
  mutate(taverage0 = (skin_temperature + lag(skin_temperature, 1)) / 2,
         taverage1 = (skin_temperature + lag(skin_temperature, 1) + lag(skin_temperature, 2)) / 3,
         taverage2 = (lag(skin_temperature, 1) + lag(skin_temperature, 2) + lag(skin_temperature, 3)) / 3,
         taverage3 = (lag(skin_temperature, 2) + lag(skin_temperature, 3) + lag(skin_temperature, 4)) / 3,
         taverage4 = (lag(skin_temperature, 3) + lag(skin_temperature, 4) + lag(skin_temperature, 5)) / 3,
         taverage5 = (lag(skin_temperature, 4) + lag(skin_temperature, 5) + lag(skin_temperature, 6)) / 3,
         taverage6 = (lag(skin_temperature, 5) + lag(skin_temperature, 6) + lag(skin_temperature, 7)) / 3,
         taverage7 = (lag(skin_temperature, 6) + lag(skin_temperature, 7) + lag(skin_temperature, 8)) / 3,
         taverage8 = (lag(skin_temperature, 7) + lag(skin_temperature, 8) + lag(skin_temperature, 9)) / 3,
         taverage9 = (lag(skin_temperature, 8) + lag(skin_temperature, 9) + lag(skin_temperature, 10)) / 3,
         taverage10 = (lag(skin_temperature, 9) + lag(skin_temperature, 10) + lag(skin_temperature, 11)) / 3)%>%
  mutate(oaverage0 = (Ozone + lag(Ozone, 1)) / 2,
         oaverage1 = (Ozone + lag(Ozone, 1) + lag(Ozone, 2)) / 3,
         oaverage2 = (lag(Ozone, 1) + lag(Ozone, 2) + lag(Ozone, 3)) / 3,
         oaverage3 = (lag(Ozone, 2) + lag(Ozone, 3) + lag(Ozone, 4)) / 3,
         oaverage4 = (lag(Ozone, 3) + lag(Ozone, 4) + lag(Ozone, 5)) / 3,
         oaverage5 = (lag(Ozone, 4) + lag(Ozone, 5) + lag(Ozone, 6)) / 3,
         oaverage6 = (lag(Ozone, 5) + lag(Ozone, 6) + lag(Ozone, 7)) / 3,
         oaverage7 = (lag(Ozone, 6) + lag(Ozone, 7) + lag(Ozone, 8)) / 3,
         oaverage8 = (lag(Ozone, 7) + lag(Ozone, 8) + lag(Ozone, 9)) / 3,
         oaverage9 = (lag(Ozone, 8) + lag(Ozone, 9) + lag(Ozone, 10)) / 3,
         oaverage10 = (lag(Ozone, 9) + lag(Ozone, 10) + lag(Ozone, 11)) / 3)%>%
  mutate(paverage0 = (Precipitation + lag(Precipitation, 1)) / 2,
         paverage1 = (Precipitation + lag(Precipitation, 1) + lag(Precipitation, 2)) / 3,
         paverage2 = (lag(Precipitation, 1) + lag(Precipitation, 2) + lag(Precipitation, 3)) / 3,
         paverage3 = (lag(Precipitation, 2) + lag(Precipitation, 3) + lag(Precipitation, 4)) / 3,
         paverage4 = (lag(Precipitation, 3) + lag(Precipitation, 4) + lag(Precipitation, 5)) / 3,
         paverage5 = (lag(Precipitation, 4) + lag(Precipitation, 5) + lag(Precipitation, 6)) / 3,
         paverage6 = (lag(Precipitation, 5) + lag(Precipitation, 6) + lag(Precipitation, 7)) / 3,
         paverage7 = (lag(Precipitation, 6) + lag(Precipitation, 7) + lag(Precipitation, 8)) / 3,
         paverage8 = (lag(Precipitation, 7) + lag(Precipitation, 8) + lag(Precipitation, 9)) / 3,
         paverage9 = (lag(Precipitation, 8) + lag(Precipitation, 9) + lag(Precipitation, 10)) / 3,
         paverage10 = (lag(Precipitation, 9) + lag(Precipitation, 10) + lag(Precipitation, 11)) / 3)%>%
  mutate(saverage0 = (Solar_radiation + lag(Solar_radiation, 1)) / 2,
         saverage1 = (Solar_radiation + lag(Solar_radiation, 1) + lag(Solar_radiation, 2)) / 3,
         saverage2 = (lag(Solar_radiation, 1) + lag(Solar_radiation, 2) + lag(Solar_radiation, 3)) / 3,
         saverage3 = (lag(Solar_radiation, 2) + lag(Solar_radiation, 3) + lag(Solar_radiation, 4)) / 3,
         saverage4 = (lag(Solar_radiation, 3) + lag(Solar_radiation, 4) + lag(Solar_radiation, 5)) / 3,
         saverage5 = (lag(Solar_radiation, 4) + lag(Solar_radiation, 5) + lag(Solar_radiation, 6)) / 3,
         saverage6 = (lag(Solar_radiation, 5) + lag(Solar_radiation, 6) + lag(Solar_radiation, 7)) / 3,
         saverage7 = (lag(Solar_radiation, 6) + lag(Solar_radiation, 7) + lag(Solar_radiation, 8)) / 3,
         saverage8 = (lag(Solar_radiation, 7) + lag(Solar_radiation, 8) + lag(Solar_radiation, 9)) / 3,
         saverage9 = (lag(Solar_radiation, 8) + lag(Solar_radiation, 9) + lag(Solar_radiation, 10)) / 3,
         saverage10 = (lag(Solar_radiation, 9) + lag(Solar_radiation, 10) + lag(Solar_radiation, 11)) / 3)%>%
  mutate(Solar_change = saverage1/baseline_Solar,
         NDVI_change = average10/baseline_NDVI,
         Pre_change = paverage10/baseline_Precipitation,
         temp_change = taverage10/baseline_temperature,
         Ozone_change = oaverage10/baseline_ozone)%>%
  ungroup()
  

data_all_SEER <- merge(data_all_SEER,data_enviro_US_new, 
                       by=c("GEOID","YEAR"))

str(data_all_SEER)
for (i in 5:25) {
  data_all_SEER[,i] <- as.numeric(data_all_SEER[,i])
}
for (i in 46:48) {
  data_all_SEER[,i] <- as.numeric(data_all_SEER[,i])
}
data_all_SEER[,2] <- as.numeric(data_all_SEER[,2])
geoid_to_remove <- c(15001,15009,15003,15007,15005)
data_all_SEER <- data_all_SEER[!data_all_SEER$GEOID %in% geoid_to_remove, ]
data_all_SEER$N60_74 <- ((4/9)*data_all_SEER$N60_64+(3/9)*data_all_SEER$N65_69+(2/9)*data_all_SEER$N70_74)
data_all_SEER$N45_59 <- ((6000*data_all_SEER$N45_49+5000*data_all_SEER$N50_54+4000*data_all_SEER$N55_59)/15000)
data_all_SEER$N_all <- (12000*data_all_SEER$N0_4+10000*data_all_SEER$N5_9+9000*data_all_SEER$N10_14+9000*data_all_SEER$N15_19+
                       8000*data_all_SEER$N20_24+8000*data_all_SEER$N25_29+6000*data_all_SEER$N30_34+6000*data_all_SEER$N35_39+
                       6000*data_all_SEER$N40_44+6000*data_all_SEER$N45_49+5000*data_all_SEER$N50_54+4000*data_all_SEER$N55_59+
                       4000*data_all_SEER$N60_64+3000*data_all_SEER$N65_69+2000*data_all_SEER$N70_74+2000*data_all_SEER$N75_79)/100000
data_all_SEER$Region <- "America"
setwd('C:/Haowen Wang/Cancer and climate change/SEER data')
save(data_all_SEER, file = "data_all_SEER2.Rdata")



######################Merge CI5 data#################################
load('C:/Haowen Wang/Cancer and climate change/SEER data/CI5_data.Rdata')
load('C:/Haowen Wang/Cancer and climate change/SEER data/CI5_envirodata.Rdata')
colnames(data_enviro)[colnames(data_enviro) == "NDVI_mean"] <- "NDVI_mean1"
data_enviro$NDVI_mean <- (data_enviro$NDVI_mean1)*10
colnames(data_enviro)[colnames(data_enviro) == "Ozone_mean"] <- "Ozone_mean1"
data_enviro$Ozone_mean <- (data_enviro$Ozone_mean1)/10
colnames(data_enviro)[colnames(data_enviro) == "precipitation_mean"] <- "precipitation_mean1"
data_enviro$precipitation_mean <- (data_enviro$precipitation_mean1)*1
colnames(data_enviro)[colnames(data_enviro) == "skin_temp_mean"] <- "skin_temp_mean1"
data_enviro$skin_temp_mean <- (data_enviro$skin_temp_mean1)-273.15
colnames(data_enviro)[colnames(data_enviro) == "solar_radiation_mean"] <- "solar_radiation_mean1"
data_enviro$solar_radiation_mean <- (data_enviro$solar_radiation)
colnames(data_enviro)[colnames(data_enviro) == "latitude"] <- "Latitude"
colnames(data_enviro)[colnames(data_enviro) == "longitude"] <- "Longitude"
#######lag term male##########
data_enviro_male <- data_enviro %>%
  filter(SEX == 1)

baseline <- data_enviro_male %>%
  filter(YEAR >= 1988 & YEAR <= 1997) %>%
  group_by(REGISTRY) %>%
  summarize(baseline_temperature = mean(skin_temp_mean, na.rm = TRUE),
            baseline_NDVI = mean(NDVI_mean, na.rm = TRUE),
            baseline_ozone = mean(Ozone_mean, na.rm = TRUE),
            baseline_Precipitation = mean(precipitation_mean, na.rm = TRUE),
            baseline_Solar = mean(solar_radiation_mean, na.rm = TRUE))

data_enviro_male <- left_join(data_enviro_male, baseline, by = "REGISTRY")
#create variable for different time window
data_enviro_male <- data_enviro_male %>%
  arrange(REGISTRY,YEAR) %>%
  group_by(REGISTRY) %>%
  mutate(period1 = (NDVI_mean + lag(NDVI_mean, 1)) / 2,
         period2 = (NDVI_mean + lag(NDVI_mean, 1) + lag(NDVI_mean, 2)) / 3,
         period3 = (NDVI_mean + lag(NDVI_mean, 1) + lag(NDVI_mean, 2) + lag(NDVI_mean, 3)) / 4,
         period4 = (NDVI_mean + lag(NDVI_mean, 1) + lag(NDVI_mean, 2) + lag(NDVI_mean, 3) + lag(NDVI_mean, 4)) / 5,
         period5 = (NDVI_mean + lag(NDVI_mean, 1) + lag(NDVI_mean, 2) + lag(NDVI_mean, 3) + lag(NDVI_mean, 4) + lag(NDVI_mean, 5)) / 6,
         period6 = (NDVI_mean + lag(NDVI_mean, 1) + lag(NDVI_mean, 2) + lag(NDVI_mean, 3) + lag(NDVI_mean, 4) + lag(NDVI_mean, 5) + lag(NDVI_mean, 6)) / 7,
         period7 = (NDVI_mean + lag(NDVI_mean, 1) + lag(NDVI_mean, 2) + lag(NDVI_mean, 3) + lag(NDVI_mean, 4) + lag(NDVI_mean, 5) + lag(NDVI_mean, 6) + lag(NDVI_mean, 7)) / 8,
         period8 = (NDVI_mean + lag(NDVI_mean, 1) + lag(NDVI_mean, 2) + lag(NDVI_mean, 3) + lag(NDVI_mean, 4) + lag(NDVI_mean, 5) + lag(NDVI_mean, 6) + lag(NDVI_mean, 7) + lag(NDVI_mean, 8)) / 9,
         period9 = (NDVI_mean + lag(NDVI_mean, 1) + lag(NDVI_mean, 2) + lag(NDVI_mean, 3) + lag(NDVI_mean, 4) + lag(NDVI_mean, 5) + lag(NDVI_mean, 6) + lag(NDVI_mean, 7) + lag(NDVI_mean, 8) + lag(NDVI_mean, 9)) / 10,
         period10 = (NDVI_mean + lag(NDVI_mean, 1) + lag(NDVI_mean, 2) + lag(NDVI_mean, 3) + lag(NDVI_mean, 4) + lag(NDVI_mean, 5) + lag(NDVI_mean, 6) + lag(NDVI_mean, 7) + lag(NDVI_mean, 8) + lag(NDVI_mean, 9) + lag(NDVI_mean, 10)) / 11) %>%
  mutate(tperiod1 = (skin_temp_mean + lag(skin_temp_mean, 1)) / 2,
         tperiod2 = (skin_temp_mean + lag(skin_temp_mean, 1) + lag(skin_temp_mean, 2)) / 3,
         tperiod3 = (skin_temp_mean + lag(skin_temp_mean, 1) + lag(skin_temp_mean, 2) + lag(skin_temp_mean, 3)) / 4,
         tperiod4 = (skin_temp_mean + lag(skin_temp_mean, 1) + lag(skin_temp_mean, 2) + lag(skin_temp_mean, 3) + lag(skin_temp_mean, 4)) / 5,
         tperiod5 = (skin_temp_mean + lag(skin_temp_mean, 1) + lag(skin_temp_mean, 2) + lag(skin_temp_mean, 3) + lag(skin_temp_mean, 4) + lag(skin_temp_mean, 5)) / 6,
         tperiod6 = (skin_temp_mean + lag(skin_temp_mean, 1) + lag(skin_temp_mean, 2) + lag(skin_temp_mean, 3) + lag(skin_temp_mean, 4) + lag(skin_temp_mean, 5) + lag(skin_temp_mean, 6)) / 7,
         tperiod7 = (skin_temp_mean + lag(skin_temp_mean, 1) + lag(skin_temp_mean, 2) + lag(skin_temp_mean, 3) + lag(skin_temp_mean, 4) + lag(skin_temp_mean, 5) + lag(skin_temp_mean, 6) + lag(skin_temp_mean, 7)) / 8,
         tperiod8 = (skin_temp_mean + lag(skin_temp_mean, 1) + lag(skin_temp_mean, 2) + lag(skin_temp_mean, 3) + lag(skin_temp_mean, 4) + lag(skin_temp_mean, 5) + lag(skin_temp_mean, 6) + lag(skin_temp_mean, 7) + lag(skin_temp_mean, 8)) / 9,
         tperiod9 = (skin_temp_mean + lag(skin_temp_mean, 1) + lag(skin_temp_mean, 2) + lag(skin_temp_mean, 3) + lag(skin_temp_mean, 4) + lag(skin_temp_mean, 5) + lag(skin_temp_mean, 6) + lag(skin_temp_mean, 7) + lag(skin_temp_mean, 8) + lag(skin_temp_mean, 9)) / 10,
         tperiod10 = (skin_temp_mean + lag(skin_temp_mean, 1) + lag(skin_temp_mean, 2) + lag(skin_temp_mean, 3) + lag(skin_temp_mean, 4) + lag(skin_temp_mean, 5) + lag(skin_temp_mean, 6) + lag(skin_temp_mean, 7) + lag(skin_temp_mean, 8) + lag(skin_temp_mean, 9) + lag(skin_temp_mean, 10)) / 11) %>%
  mutate(operiod1 = (Ozone_mean + lag(Ozone_mean, 1)) / 2,
         operiod2 = (Ozone_mean + lag(Ozone_mean, 1) + lag(Ozone_mean, 2)) / 3,
         operiod3 = (Ozone_mean + lag(Ozone_mean, 1) + lag(Ozone_mean, 2) + lag(Ozone_mean, 3)) / 4,
         operiod4 = (Ozone_mean + lag(Ozone_mean, 1) + lag(Ozone_mean, 2) + lag(Ozone_mean, 3) + lag(Ozone_mean, 4)) / 5,
         operiod5 = (Ozone_mean + lag(Ozone_mean, 1) + lag(Ozone_mean, 2) + lag(Ozone_mean, 3) + lag(Ozone_mean, 4) + lag(Ozone_mean, 5)) / 6,
         operiod6 = (Ozone_mean + lag(Ozone_mean, 1) + lag(Ozone_mean, 2) + lag(Ozone_mean, 3) + lag(Ozone_mean, 4) + lag(Ozone_mean, 5) + lag(Ozone_mean, 6)) / 7,
         operiod7 = (Ozone_mean + lag(Ozone_mean, 1) + lag(Ozone_mean, 2) + lag(Ozone_mean, 3) + lag(Ozone_mean, 4) + lag(Ozone_mean, 5) + lag(Ozone_mean, 6) + lag(Ozone_mean, 7)) / 8,
         operiod8 = (Ozone_mean + lag(Ozone_mean, 1) + lag(Ozone_mean, 2) + lag(Ozone_mean, 3) + lag(Ozone_mean, 4) + lag(Ozone_mean, 5) + lag(Ozone_mean, 6) + lag(Ozone_mean, 7) + lag(Ozone_mean, 8)) / 9,
         operiod9 = (Ozone_mean + lag(Ozone_mean, 1) + lag(Ozone_mean, 2) + lag(Ozone_mean, 3) + lag(Ozone_mean, 4) + lag(Ozone_mean, 5) + lag(Ozone_mean, 6) + lag(Ozone_mean, 7) + lag(Ozone_mean, 8) + lag(Ozone_mean, 9)) / 10,
         operiod10 = (Ozone_mean + lag(Ozone_mean, 1) + lag(Ozone_mean, 2) + lag(Ozone_mean, 3) + lag(Ozone_mean, 4) + lag(Ozone_mean, 5) + lag(Ozone_mean, 6) + lag(Ozone_mean, 7) + lag(Ozone_mean, 8) + lag(Ozone_mean, 9) + lag(Ozone_mean, 10)) / 11) %>%
  mutate(pperiod1 = (precipitation_mean + lag(precipitation_mean, 1)) / 2,
         pperiod2 = (precipitation_mean + lag(precipitation_mean, 1) + lag(precipitation_mean, 2)) / 3,
         pperiod3 = (precipitation_mean + lag(precipitation_mean, 1) + lag(precipitation_mean, 2) + lag(precipitation_mean, 3)) / 4,
         pperiod4 = (precipitation_mean + lag(precipitation_mean, 1) + lag(precipitation_mean, 2) + lag(precipitation_mean, 3) + lag(precipitation_mean, 4)) / 5,
         pperiod5 = (precipitation_mean + lag(precipitation_mean, 1) + lag(precipitation_mean, 2) + lag(precipitation_mean, 3) + lag(precipitation_mean, 4) + lag(precipitation_mean, 5)) / 6,
         pperiod6 = (precipitation_mean + lag(precipitation_mean, 1) + lag(precipitation_mean, 2) + lag(precipitation_mean, 3) + lag(precipitation_mean, 4) + lag(precipitation_mean, 5) + lag(precipitation_mean, 6)) / 7,
         pperiod7 = (precipitation_mean + lag(precipitation_mean, 1) + lag(precipitation_mean, 2) + lag(precipitation_mean, 3) + lag(precipitation_mean, 4) + lag(precipitation_mean, 5) + lag(precipitation_mean, 6) + lag(precipitation_mean, 7)) / 8,
         pperiod8 = (precipitation_mean + lag(precipitation_mean, 1) + lag(precipitation_mean, 2) + lag(precipitation_mean, 3) + lag(precipitation_mean, 4) + lag(precipitation_mean, 5) + lag(precipitation_mean, 6) + lag(precipitation_mean, 7) + lag(precipitation_mean, 8)) / 9,
         pperiod9 = (precipitation_mean + lag(precipitation_mean, 1) + lag(precipitation_mean, 2) + lag(precipitation_mean, 3) + lag(precipitation_mean, 4) + lag(precipitation_mean, 5) + lag(precipitation_mean, 6) + lag(precipitation_mean, 7) + lag(precipitation_mean, 8) + lag(precipitation_mean, 9)) / 10,
         pperiod10 = (precipitation_mean + lag(precipitation_mean, 1) + lag(precipitation_mean, 2) + lag(precipitation_mean, 3) + lag(precipitation_mean, 4) + lag(precipitation_mean, 5) + lag(precipitation_mean, 6) + lag(precipitation_mean, 7) + lag(precipitation_mean, 8) + lag(precipitation_mean, 9) + lag(precipitation_mean, 10)) / 11) %>%
  mutate(speriod1 = (solar_radiation_mean + lag(solar_radiation_mean, 1)) / 2,
         speriod2 = (solar_radiation_mean + lag(solar_radiation_mean, 1) + lag(solar_radiation_mean, 2)) / 3,
         speriod3 = (solar_radiation_mean + lag(solar_radiation_mean, 1) + lag(solar_radiation_mean, 2) + lag(solar_radiation_mean, 3)) / 4,
         speriod4 = (solar_radiation_mean + lag(solar_radiation_mean, 1) + lag(solar_radiation_mean, 2) + lag(solar_radiation_mean, 3) + lag(solar_radiation_mean, 4)) / 5,
         speriod5 = (solar_radiation_mean + lag(solar_radiation_mean, 1) + lag(solar_radiation_mean, 2) + lag(solar_radiation_mean, 3) + lag(solar_radiation_mean, 4) + lag(solar_radiation_mean, 5)) / 6,
         speriod6 = (solar_radiation_mean + lag(solar_radiation_mean, 1) + lag(solar_radiation_mean, 2) + lag(solar_radiation_mean, 3) + lag(solar_radiation_mean, 4) + lag(solar_radiation_mean, 5) + lag(solar_radiation_mean, 6)) / 7,
         speriod7 = (solar_radiation_mean + lag(solar_radiation_mean, 1) + lag(solar_radiation_mean, 2) + lag(solar_radiation_mean, 3) + lag(solar_radiation_mean, 4) + lag(solar_radiation_mean, 5) + lag(solar_radiation_mean, 6) + lag(solar_radiation_mean, 7)) / 8,
         speriod8 = (solar_radiation_mean + lag(solar_radiation_mean, 1) + lag(solar_radiation_mean, 2) + lag(solar_radiation_mean, 3) + lag(solar_radiation_mean, 4) + lag(solar_radiation_mean, 5) + lag(solar_radiation_mean, 6) + lag(solar_radiation_mean, 7) + lag(solar_radiation_mean, 8)) / 9,
         speriod9 = (solar_radiation_mean + lag(solar_radiation_mean, 1) + lag(solar_radiation_mean, 2) + lag(solar_radiation_mean, 3) + lag(solar_radiation_mean, 4) + lag(solar_radiation_mean, 5) + lag(solar_radiation_mean, 6) + lag(solar_radiation_mean, 7) + lag(solar_radiation_mean, 8) + lag(solar_radiation_mean, 9)) / 10,
         speriod10 = (solar_radiation_mean + lag(solar_radiation_mean, 1) + lag(solar_radiation_mean, 2) + lag(solar_radiation_mean, 3) + lag(solar_radiation_mean, 4) + lag(solar_radiation_mean, 5) + lag(solar_radiation_mean, 6) + lag(solar_radiation_mean, 7) + lag(solar_radiation_mean, 8) + lag(solar_radiation_mean, 9) + lag(solar_radiation_mean, 10)) / 11) %>%
  mutate(average0 = (NDVI_mean + lag(NDVI_mean, 1)) / 2,
         average1 = (NDVI_mean + lag(NDVI_mean, 1) + lag(NDVI_mean, 2)) / 3,
         average2 = (lag(NDVI_mean, 1) + lag(NDVI_mean, 2) + lag(NDVI_mean, 3)) / 3,
         average3 = (lag(NDVI_mean, 2) + lag(NDVI_mean, 3) + lag(NDVI_mean, 4)) / 3,
         average4 = (lag(NDVI_mean, 3) + lag(NDVI_mean, 4) + lag(NDVI_mean, 5)) / 3,
         average5 = (lag(NDVI_mean, 4) + lag(NDVI_mean, 5) + lag(NDVI_mean, 6)) / 3,
         average6 = (lag(NDVI_mean, 5) + lag(NDVI_mean, 6) + lag(NDVI_mean, 7)) / 3,
         average7 = (lag(NDVI_mean, 6) + lag(NDVI_mean, 7) + lag(NDVI_mean, 8)) / 3,
         average8 = (lag(NDVI_mean, 7) + lag(NDVI_mean, 8) + lag(NDVI_mean, 9)) / 3,
         average9 = (lag(NDVI_mean, 8) + lag(NDVI_mean, 9) + lag(NDVI_mean, 10)) / 3,
         average10 = (lag(NDVI_mean, 9) + lag(NDVI_mean, 10) + lag(NDVI_mean, 11)) / 3)%>%
  mutate(taverage0 = (skin_temp_mean + lag(skin_temp_mean, 1)) / 2,
         taverage1 = (skin_temp_mean + lag(skin_temp_mean, 1) + lag(skin_temp_mean, 2)) / 3,
         taverage2 = (lag(skin_temp_mean, 1) + lag(skin_temp_mean, 2) + lag(skin_temp_mean, 3)) / 3,
         taverage3 = (lag(skin_temp_mean, 2) + lag(skin_temp_mean, 3) + lag(skin_temp_mean, 4)) / 3,
         taverage4 = (lag(skin_temp_mean, 3) + lag(skin_temp_mean, 4) + lag(skin_temp_mean, 5)) / 3,
         taverage5 = (lag(skin_temp_mean, 4) + lag(skin_temp_mean, 5) + lag(skin_temp_mean, 6)) / 3,
         taverage6 = (lag(skin_temp_mean, 5) + lag(skin_temp_mean, 6) + lag(skin_temp_mean, 7)) / 3,
         taverage7 = (lag(skin_temp_mean, 6) + lag(skin_temp_mean, 7) + lag(skin_temp_mean, 8)) / 3,
         taverage8 = (lag(skin_temp_mean, 7) + lag(skin_temp_mean, 8) + lag(skin_temp_mean, 9)) / 3,
         taverage9 = (lag(skin_temp_mean, 8) + lag(skin_temp_mean, 9) + lag(skin_temp_mean, 10)) / 3,
         taverage10 = (lag(skin_temp_mean, 9) + lag(skin_temp_mean, 10) + lag(skin_temp_mean, 11)) / 3)%>%
  mutate(oaverage0 = (Ozone_mean + lag(Ozone_mean, 1)) / 2,
         oaverage1 = (Ozone_mean + lag(Ozone_mean, 1) + lag(Ozone_mean, 2)) / 3,
         oaverage2 = (lag(Ozone_mean, 1) + lag(Ozone_mean, 2) + lag(Ozone_mean, 3)) / 3,
         oaverage3 = (lag(Ozone_mean, 2) + lag(Ozone_mean, 3) + lag(Ozone_mean, 4)) / 3,
         oaverage4 = (lag(Ozone_mean, 3) + lag(Ozone_mean, 4) + lag(Ozone_mean, 5)) / 3,
         oaverage5 = (lag(Ozone_mean, 4) + lag(Ozone_mean, 5) + lag(Ozone_mean, 6)) / 3,
         oaverage6 = (lag(Ozone_mean, 5) + lag(Ozone_mean, 6) + lag(Ozone_mean, 7)) / 3,
         oaverage7 = (lag(Ozone_mean, 6) + lag(Ozone_mean, 7) + lag(Ozone_mean, 8)) / 3,
         oaverage8 = (lag(Ozone_mean, 7) + lag(Ozone_mean, 8) + lag(Ozone_mean, 9)) / 3,
         oaverage9 = (lag(Ozone_mean, 8) + lag(Ozone_mean, 9) + lag(Ozone_mean, 10)) / 3,
         oaverage10 = (lag(Ozone_mean, 9) + lag(Ozone_mean, 10) + lag(Ozone_mean, 11)) / 3)%>%
  mutate(paverage0 = (precipitation_mean + lag(precipitation_mean, 1)) / 2,
         paverage1 = (precipitation_mean + lag(precipitation_mean, 1) + lag(precipitation_mean, 2)) / 3,
         paverage2 = (lag(precipitation_mean, 1) + lag(precipitation_mean, 2) + lag(precipitation_mean, 3)) / 3,
         paverage3 = (lag(precipitation_mean, 2) + lag(precipitation_mean, 3) + lag(precipitation_mean, 4)) / 3,
         paverage4 = (lag(precipitation_mean, 3) + lag(precipitation_mean, 4) + lag(precipitation_mean, 5)) / 3,
         paverage5 = (lag(precipitation_mean, 4) + lag(precipitation_mean, 5) + lag(precipitation_mean, 6)) / 3,
         paverage6 = (lag(precipitation_mean, 5) + lag(precipitation_mean, 6) + lag(precipitation_mean, 7)) / 3,
         paverage7 = (lag(precipitation_mean, 6) + lag(precipitation_mean, 7) + lag(precipitation_mean, 8)) / 3,
         paverage8 = (lag(precipitation_mean, 7) + lag(precipitation_mean, 8) + lag(precipitation_mean, 9)) / 3,
         paverage9 = (lag(precipitation_mean, 8) + lag(precipitation_mean, 9) + lag(precipitation_mean, 10)) / 3,
         paverage10 = (lag(precipitation_mean, 9) + lag(precipitation_mean, 10) + lag(precipitation_mean, 11)) / 3)%>%
  mutate(saverage0 = (solar_radiation_mean + lag(solar_radiation_mean, 1)) / 2,
         saverage1 = (solar_radiation_mean + lag(solar_radiation_mean, 1) + lag(solar_radiation_mean, 2)) / 3,
         saverage2 = (lag(solar_radiation_mean, 1) + lag(solar_radiation_mean, 2) + lag(solar_radiation_mean, 3)) / 3,
         saverage3 = (lag(solar_radiation_mean, 2) + lag(solar_radiation_mean, 3) + lag(solar_radiation_mean, 4)) / 3,
         saverage4 = (lag(solar_radiation_mean, 3) + lag(solar_radiation_mean, 4) + lag(solar_radiation_mean, 5)) / 3,
         saverage5 = (lag(solar_radiation_mean, 4) + lag(solar_radiation_mean, 5) + lag(solar_radiation_mean, 6)) / 3,
         saverage6 = (lag(solar_radiation_mean, 5) + lag(solar_radiation_mean, 6) + lag(solar_radiation_mean, 7)) / 3,
         saverage7 = (lag(solar_radiation_mean, 6) + lag(solar_radiation_mean, 7) + lag(solar_radiation_mean, 8)) / 3,
         saverage8 = (lag(solar_radiation_mean, 7) + lag(solar_radiation_mean, 8) + lag(solar_radiation_mean, 9)) / 3,
         saverage9 = (lag(solar_radiation_mean, 8) + lag(solar_radiation_mean, 9) + lag(solar_radiation_mean, 10)) / 3,
         saverage10 = (lag(solar_radiation_mean, 9) + lag(solar_radiation_mean, 10) + lag(solar_radiation_mean, 11)) / 3)%>%
  mutate(Solar_change = saverage1/baseline_Solar,
         NDVI_change = average10/baseline_NDVI,
         Pre_change = paverage10/baseline_Precipitation,
         temp_change = taverage10/baseline_temperature,
         Ozone_change = oaverage10/baseline_ozone)%>%
  ungroup()
data_enviro_male <- data_enviro_male %>%
  arrange(CODE,YEAR)
lag1 <- c(NA, data_enviro_male$NDVI_mean[-nrow(data_enviro_male)])
lag2 <- c(NA, lag1[-nrow(data_enviro_male)])
lag3 <- c(NA, lag2[-nrow(data_enviro_male)])
lag4 <- c(NA, lag3[-nrow(data_enviro_male)])
lag5 <- c(NA, lag4[-nrow(data_enviro_male)])
lag6 <- c(NA, lag5[-nrow(data_enviro_male)])
lag7 <- c(NA, lag6[-nrow(data_enviro_male)])
lag8 <- c(NA, lag7[-nrow(data_enviro_male)])
lag9 <- c(NA, lag8[-nrow(data_enviro_male)])
lag10 <- c(NA, lag9[-nrow(data_enviro_male)])
data_enviro_male$lag1 <- lag1
data_enviro_male$lag2 <- lag2
data_enviro_male$lag3 <- lag3
data_enviro_male$lag4 <- lag4
data_enviro_male$lag5 <- lag5
data_enviro_male$lag6 <- lag6
data_enviro_male$lag7 <- lag7
data_enviro_male$lag8 <- lag8
data_enviro_male$lag9 <- lag9
data_enviro_male$lag10 <- lag10

#lag term temp
tlag1 <- c(NA, data_enviro_male$skin_temp_mean[-nrow(data_enviro_male)])
tlag2 <- c(NA, tlag1[-nrow(data_enviro_male)])
tlag3 <- c(NA, tlag2[-nrow(data_enviro_male)])
tlag4 <- c(NA, tlag3[-nrow(data_enviro_male)])
tlag5 <- c(NA, tlag4[-nrow(data_enviro_male)])
tlag6 <- c(NA, tlag5[-nrow(data_enviro_male)])
tlag7 <- c(NA, tlag6[-nrow(data_enviro_male)])
tlag8 <- c(NA, tlag7[-nrow(data_enviro_male)])
tlag9 <- c(NA, tlag8[-nrow(data_enviro_male)])
tlag10 <- c(NA, tlag9[-nrow(data_enviro_male)])
data_enviro_male$tlag1 <- tlag1
data_enviro_male$tlag2 <- tlag2
data_enviro_male$tlag3 <- tlag3
data_enviro_male$tlag4 <- tlag4
data_enviro_male$tlag5 <- tlag5
data_enviro_male$tlag6 <- tlag6
data_enviro_male$tlag7 <- tlag7
data_enviro_male$tlag8 <- tlag8
data_enviro_male$tlag9 <- tlag9
data_enviro_male$tlag10 <- tlag10

#lagged term precipitation
plag1 <- c(NA, data_enviro_male$precipitation_mean[-nrow(data_enviro_male)])
plag2 <- c(NA, plag1[-nrow(data_enviro_male)])
plag3 <- c(NA, plag2[-nrow(data_enviro_male)])
plag4 <- c(NA, plag3[-nrow(data_enviro_male)])
plag5 <- c(NA, plag4[-nrow(data_enviro_male)])
plag6 <- c(NA, plag5[-nrow(data_enviro_male)])
plag7 <- c(NA, plag6[-nrow(data_enviro_male)])
plag8 <- c(NA, plag7[-nrow(data_enviro_male)])
plag9 <- c(NA, plag8[-nrow(data_enviro_male)])
plag10 <- c(NA, plag9[-nrow(data_enviro_male)])
data_enviro_male$plag1 <- plag1
data_enviro_male$plag2 <- plag2
data_enviro_male$plag3 <- plag3
data_enviro_male$plag4 <- plag4
data_enviro_male$plag5 <- plag5
data_enviro_male$plag6 <- plag6
data_enviro_male$plag7 <- plag7
data_enviro_male$plag8 <- plag8
data_enviro_male$plag9 <- plag9
data_enviro_male$plag10 <- plag10


#lagged term Ozone
olag1 <- c(NA, data_enviro_male$Ozone_mean[-nrow(data_enviro_male)])
olag2 <- c(NA, olag1[-nrow(data_enviro_male)])
olag3 <- c(NA, olag2[-nrow(data_enviro_male)])
olag4 <- c(NA, olag3[-nrow(data_enviro_male)])
olag5 <- c(NA, olag4[-nrow(data_enviro_male)])
olag6 <- c(NA, olag5[-nrow(data_enviro_male)])
olag7 <- c(NA, olag6[-nrow(data_enviro_male)])
olag8 <- c(NA, olag7[-nrow(data_enviro_male)])
olag9 <- c(NA, olag8[-nrow(data_enviro_male)])
olag10 <- c(NA, olag9[-nrow(data_enviro_male)])
data_enviro_male$olag1 <- olag1
data_enviro_male$olag2 <- olag2
data_enviro_male$olag3 <- olag3
data_enviro_male$olag4 <- olag4
data_enviro_male$olag5 <- olag5
data_enviro_male$olag6 <- olag6
data_enviro_male$olag7 <- olag7
data_enviro_male$olag8 <- olag8
data_enviro_male$olag9 <- olag9
data_enviro_male$olag10 <- olag10

#lagged term solar
slag1 <- c(NA, data_enviro_male$solar_radiation_mean[-nrow(data_enviro_male)])
slag2 <- c(NA, slag1[-nrow(data_enviro_male)])
slag3 <- c(NA, slag2[-nrow(data_enviro_male)])
slag4 <- c(NA, slag3[-nrow(data_enviro_male)])
slag5 <- c(NA, slag4[-nrow(data_enviro_male)])
slag6 <- c(NA, slag5[-nrow(data_enviro_male)])
slag7 <- c(NA, slag6[-nrow(data_enviro_male)])
slag8 <- c(NA, slag7[-nrow(data_enviro_male)])
slag9 <- c(NA, slag8[-nrow(data_enviro_male)])
slag10 <- c(NA, slag9[-nrow(data_enviro_male)])
data_enviro_male$slag1 <- slag1
data_enviro_male$slag2 <- slag2
data_enviro_male$slag3 <- slag3
data_enviro_male$slag4 <- slag4
data_enviro_male$slag5 <- slag5
data_enviro_male$slag6 <- slag6
data_enviro_male$slag7 <- slag7
data_enviro_male$slag8 <- slag8
data_enviro_male$slag9 <- slag9
data_enviro_male$slag10 <- slag10

#####lag term NDVI female###############

data_enviro_female <- data_enviro %>%
  filter(SEX == 2)
baseline <- data_enviro_female %>%
  filter(YEAR >= 1988 & YEAR <= 1997) %>%
  group_by(REGISTRY) %>%
  summarize(baseline_temperature = mean(skin_temp_mean, na.rm = TRUE),
            baseline_NDVI = mean(NDVI_mean, na.rm = TRUE),
            baseline_ozone = mean(Ozone_mean, na.rm = TRUE),
            baseline_Precipitation = mean(precipitation_mean, na.rm = TRUE),
            baseline_Solar = mean(solar_radiation_mean, na.rm = TRUE))

data_enviro_female <- left_join(data_enviro_female, baseline, by = "REGISTRY")
data_enviro_female <- data_enviro_female %>%
  arrange(REGISTRY,YEAR) %>%
  group_by(REGISTRY) %>%
  mutate(average0 = (NDVI_mean + lag(NDVI_mean, 1)) / 2,
         average1 = (NDVI_mean + lag(NDVI_mean, 1) + lag(NDVI_mean, 2)) / 3,
         average2 = (lag(NDVI_mean, 1) + lag(NDVI_mean, 2) + lag(NDVI_mean, 3)) / 3,
         average3 = (lag(NDVI_mean, 2) + lag(NDVI_mean, 3) + lag(NDVI_mean, 4)) / 3,
         average4 = (lag(NDVI_mean, 3) + lag(NDVI_mean, 4) + lag(NDVI_mean, 5)) / 3,
         average5 = (lag(NDVI_mean, 4) + lag(NDVI_mean, 5) + lag(NDVI_mean, 6)) / 3,
         average6 = (lag(NDVI_mean, 5) + lag(NDVI_mean, 6) + lag(NDVI_mean, 7)) / 3,
         average7 = (lag(NDVI_mean, 6) + lag(NDVI_mean, 7) + lag(NDVI_mean, 8)) / 3,
         average8 = (lag(NDVI_mean, 7) + lag(NDVI_mean, 8) + lag(NDVI_mean, 9)) / 3,
         average9 = (lag(NDVI_mean, 8) + lag(NDVI_mean, 9) + lag(NDVI_mean, 10)) / 3,
         average10 = (lag(NDVI_mean, 9) + lag(NDVI_mean, 10) + lag(NDVI_mean, 11)) / 3)%>%
  mutate(taverage0 = (skin_temp_mean + lag(skin_temp_mean, 1)) / 2,
         taverage1 = (skin_temp_mean + lag(skin_temp_mean, 1) + lag(skin_temp_mean, 2)) / 3,
         taverage2 = (lag(skin_temp_mean, 1) + lag(skin_temp_mean, 2) + lag(skin_temp_mean, 3)) / 3,
         taverage3 = (lag(skin_temp_mean, 2) + lag(skin_temp_mean, 3) + lag(skin_temp_mean, 4)) / 3,
         taverage4 = (lag(skin_temp_mean, 3) + lag(skin_temp_mean, 4) + lag(skin_temp_mean, 5)) / 3,
         taverage5 = (lag(skin_temp_mean, 4) + lag(skin_temp_mean, 5) + lag(skin_temp_mean, 6)) / 3,
         taverage6 = (lag(skin_temp_mean, 5) + lag(skin_temp_mean, 6) + lag(skin_temp_mean, 7)) / 3,
         taverage7 = (lag(skin_temp_mean, 6) + lag(skin_temp_mean, 7) + lag(skin_temp_mean, 8)) / 3,
         taverage8 = (lag(skin_temp_mean, 7) + lag(skin_temp_mean, 8) + lag(skin_temp_mean, 9)) / 3,
         taverage9 = (lag(skin_temp_mean, 8) + lag(skin_temp_mean, 9) + lag(skin_temp_mean, 10)) / 3,
         taverage10 = (lag(skin_temp_mean, 9) + lag(skin_temp_mean, 10) + lag(skin_temp_mean, 11)) / 3)%>%
  mutate(oaverage0 = (Ozone_mean + lag(Ozone_mean, 1)) / 2,
         oaverage1 = (Ozone_mean + lag(Ozone_mean, 1) + lag(Ozone_mean, 2)) / 3,
         oaverage2 = (lag(Ozone_mean, 1) + lag(Ozone_mean, 2) + lag(Ozone_mean, 3)) / 3,
         oaverage3 = (lag(Ozone_mean, 2) + lag(Ozone_mean, 3) + lag(Ozone_mean, 4)) / 3,
         oaverage4 = (lag(Ozone_mean, 3) + lag(Ozone_mean, 4) + lag(Ozone_mean, 5)) / 3,
         oaverage5 = (lag(Ozone_mean, 4) + lag(Ozone_mean, 5) + lag(Ozone_mean, 6)) / 3,
         oaverage6 = (lag(Ozone_mean, 5) + lag(Ozone_mean, 6) + lag(Ozone_mean, 7)) / 3,
         oaverage7 = (lag(Ozone_mean, 6) + lag(Ozone_mean, 7) + lag(Ozone_mean, 8)) / 3,
         oaverage8 = (lag(Ozone_mean, 7) + lag(Ozone_mean, 8) + lag(Ozone_mean, 9)) / 3,
         oaverage9 = (lag(Ozone_mean, 8) + lag(Ozone_mean, 9) + lag(Ozone_mean, 10)) / 3,
         oaverage10 = (lag(Ozone_mean, 9) + lag(Ozone_mean, 10) + lag(Ozone_mean, 11)) / 3)%>%
  mutate(paverage0 = (precipitation_mean + lag(precipitation_mean, 1)) / 2,
         paverage1 = (precipitation_mean + lag(precipitation_mean, 1) + lag(precipitation_mean, 2)) / 3,
         paverage2 = (lag(precipitation_mean, 1) + lag(precipitation_mean, 2) + lag(precipitation_mean, 3)) / 3,
         paverage3 = (lag(precipitation_mean, 2) + lag(precipitation_mean, 3) + lag(precipitation_mean, 4)) / 3,
         paverage4 = (lag(precipitation_mean, 3) + lag(precipitation_mean, 4) + lag(precipitation_mean, 5)) / 3,
         paverage5 = (lag(precipitation_mean, 4) + lag(precipitation_mean, 5) + lag(precipitation_mean, 6)) / 3,
         paverage6 = (lag(precipitation_mean, 5) + lag(precipitation_mean, 6) + lag(precipitation_mean, 7)) / 3,
         paverage7 = (lag(precipitation_mean, 6) + lag(precipitation_mean, 7) + lag(precipitation_mean, 8)) / 3,
         paverage8 = (lag(precipitation_mean, 7) + lag(precipitation_mean, 8) + lag(precipitation_mean, 9)) / 3,
         paverage9 = (lag(precipitation_mean, 8) + lag(precipitation_mean, 9) + lag(precipitation_mean, 10)) / 3,
         paverage10 = (lag(precipitation_mean, 9) + lag(precipitation_mean, 10) + lag(precipitation_mean, 11)) / 3)%>%
  mutate(saverage0 = (solar_radiation_mean + lag(solar_radiation_mean, 1)) / 2,
         saverage1 = (solar_radiation_mean + lag(solar_radiation_mean, 1) + lag(solar_radiation_mean, 2)) / 3,
         saverage2 = (lag(solar_radiation_mean, 1) + lag(solar_radiation_mean, 2) + lag(solar_radiation_mean, 3)) / 3,
         saverage3 = (lag(solar_radiation_mean, 2) + lag(solar_radiation_mean, 3) + lag(solar_radiation_mean, 4)) / 3,
         saverage4 = (lag(solar_radiation_mean, 3) + lag(solar_radiation_mean, 4) + lag(solar_radiation_mean, 5)) / 3,
         saverage5 = (lag(solar_radiation_mean, 4) + lag(solar_radiation_mean, 5) + lag(solar_radiation_mean, 6)) / 3,
         saverage6 = (lag(solar_radiation_mean, 5) + lag(solar_radiation_mean, 6) + lag(solar_radiation_mean, 7)) / 3,
         saverage7 = (lag(solar_radiation_mean, 6) + lag(solar_radiation_mean, 7) + lag(solar_radiation_mean, 8)) / 3,
         saverage8 = (lag(solar_radiation_mean, 7) + lag(solar_radiation_mean, 8) + lag(solar_radiation_mean, 9)) / 3,
         saverage9 = (lag(solar_radiation_mean, 8) + lag(solar_radiation_mean, 9) + lag(solar_radiation_mean, 10)) / 3,
         saverage10 = (lag(solar_radiation_mean, 9) + lag(solar_radiation_mean, 10) + lag(solar_radiation_mean, 11)) / 3)%>%
  mutate(period1 = (NDVI_mean + lag(NDVI_mean, 1)) / 2,
         period2 = (NDVI_mean + lag(NDVI_mean, 1) + lag(NDVI_mean, 2)) / 3,
         period3 = (NDVI_mean + lag(NDVI_mean, 1) + lag(NDVI_mean, 2) + lag(NDVI_mean, 3)) / 4,
         period4 = (NDVI_mean + lag(NDVI_mean, 1) + lag(NDVI_mean, 2) + lag(NDVI_mean, 3) + lag(NDVI_mean, 4)) / 5,
         period5 = (NDVI_mean + lag(NDVI_mean, 1) + lag(NDVI_mean, 2) + lag(NDVI_mean, 3) + lag(NDVI_mean, 4) + lag(NDVI_mean, 5)) / 6,
         period6 = (NDVI_mean + lag(NDVI_mean, 1) + lag(NDVI_mean, 2) + lag(NDVI_mean, 3) + lag(NDVI_mean, 4) + lag(NDVI_mean, 5) + lag(NDVI_mean, 6)) / 7,
         period7 = (NDVI_mean + lag(NDVI_mean, 1) + lag(NDVI_mean, 2) + lag(NDVI_mean, 3) + lag(NDVI_mean, 4) + lag(NDVI_mean, 5) + lag(NDVI_mean, 6) + lag(NDVI_mean, 7)) / 8,
         period8 = (NDVI_mean + lag(NDVI_mean, 1) + lag(NDVI_mean, 2) + lag(NDVI_mean, 3) + lag(NDVI_mean, 4) + lag(NDVI_mean, 5) + lag(NDVI_mean, 6) + lag(NDVI_mean, 7) + lag(NDVI_mean, 8)) / 9,
         period9 = (NDVI_mean + lag(NDVI_mean, 1) + lag(NDVI_mean, 2) + lag(NDVI_mean, 3) + lag(NDVI_mean, 4) + lag(NDVI_mean, 5) + lag(NDVI_mean, 6) + lag(NDVI_mean, 7) + lag(NDVI_mean, 8) + lag(NDVI_mean, 9)) / 10,
         period10 = (NDVI_mean + lag(NDVI_mean, 1) + lag(NDVI_mean, 2) + lag(NDVI_mean, 3) + lag(NDVI_mean, 4) + lag(NDVI_mean, 5) + lag(NDVI_mean, 6) + lag(NDVI_mean, 7) + lag(NDVI_mean, 8) + lag(NDVI_mean, 9) + lag(NDVI_mean, 10)) / 11) %>%
  mutate(tperiod1 = (skin_temp_mean + lag(skin_temp_mean, 1)) / 2,
         tperiod2 = (skin_temp_mean + lag(skin_temp_mean, 1) + lag(skin_temp_mean, 2)) / 3,
         tperiod3 = (skin_temp_mean + lag(skin_temp_mean, 1) + lag(skin_temp_mean, 2) + lag(skin_temp_mean, 3)) / 4,
         tperiod4 = (skin_temp_mean + lag(skin_temp_mean, 1) + lag(skin_temp_mean, 2) + lag(skin_temp_mean, 3) + lag(skin_temp_mean, 4)) / 5,
         tperiod5 = (skin_temp_mean + lag(skin_temp_mean, 1) + lag(skin_temp_mean, 2) + lag(skin_temp_mean, 3) + lag(skin_temp_mean, 4) + lag(skin_temp_mean, 5)) / 6,
         tperiod6 = (skin_temp_mean + lag(skin_temp_mean, 1) + lag(skin_temp_mean, 2) + lag(skin_temp_mean, 3) + lag(skin_temp_mean, 4) + lag(skin_temp_mean, 5) + lag(skin_temp_mean, 6)) / 7,
         tperiod7 = (skin_temp_mean + lag(skin_temp_mean, 1) + lag(skin_temp_mean, 2) + lag(skin_temp_mean, 3) + lag(skin_temp_mean, 4) + lag(skin_temp_mean, 5) + lag(skin_temp_mean, 6) + lag(skin_temp_mean, 7)) / 8,
         tperiod8 = (skin_temp_mean + lag(skin_temp_mean, 1) + lag(skin_temp_mean, 2) + lag(skin_temp_mean, 3) + lag(skin_temp_mean, 4) + lag(skin_temp_mean, 5) + lag(skin_temp_mean, 6) + lag(skin_temp_mean, 7) + lag(skin_temp_mean, 8)) / 9,
         tperiod9 = (skin_temp_mean + lag(skin_temp_mean, 1) + lag(skin_temp_mean, 2) + lag(skin_temp_mean, 3) + lag(skin_temp_mean, 4) + lag(skin_temp_mean, 5) + lag(skin_temp_mean, 6) + lag(skin_temp_mean, 7) + lag(skin_temp_mean, 8) + lag(skin_temp_mean, 9)) / 10,
         tperiod10 = (skin_temp_mean + lag(skin_temp_mean, 1) + lag(skin_temp_mean, 2) + lag(skin_temp_mean, 3) + lag(skin_temp_mean, 4) + lag(skin_temp_mean, 5) + lag(skin_temp_mean, 6) + lag(skin_temp_mean, 7) + lag(skin_temp_mean, 8) + lag(skin_temp_mean, 9) + lag(skin_temp_mean, 10)) / 11) %>%
  mutate(operiod1 = (Ozone_mean + lag(Ozone_mean, 1)) / 2,
         operiod2 = (Ozone_mean + lag(Ozone_mean, 1) + lag(Ozone_mean, 2)) / 3,
         operiod3 = (Ozone_mean + lag(Ozone_mean, 1) + lag(Ozone_mean, 2) + lag(Ozone_mean, 3)) / 4,
         operiod4 = (Ozone_mean + lag(Ozone_mean, 1) + lag(Ozone_mean, 2) + lag(Ozone_mean, 3) + lag(Ozone_mean, 4)) / 5,
         operiod5 = (Ozone_mean + lag(Ozone_mean, 1) + lag(Ozone_mean, 2) + lag(Ozone_mean, 3) + lag(Ozone_mean, 4) + lag(Ozone_mean, 5)) / 6,
         operiod6 = (Ozone_mean + lag(Ozone_mean, 1) + lag(Ozone_mean, 2) + lag(Ozone_mean, 3) + lag(Ozone_mean, 4) + lag(Ozone_mean, 5) + lag(Ozone_mean, 6)) / 7,
         operiod7 = (Ozone_mean + lag(Ozone_mean, 1) + lag(Ozone_mean, 2) + lag(Ozone_mean, 3) + lag(Ozone_mean, 4) + lag(Ozone_mean, 5) + lag(Ozone_mean, 6) + lag(Ozone_mean, 7)) / 8,
         operiod8 = (Ozone_mean + lag(Ozone_mean, 1) + lag(Ozone_mean, 2) + lag(Ozone_mean, 3) + lag(Ozone_mean, 4) + lag(Ozone_mean, 5) + lag(Ozone_mean, 6) + lag(Ozone_mean, 7) + lag(Ozone_mean, 8)) / 9,
         operiod9 = (Ozone_mean + lag(Ozone_mean, 1) + lag(Ozone_mean, 2) + lag(Ozone_mean, 3) + lag(Ozone_mean, 4) + lag(Ozone_mean, 5) + lag(Ozone_mean, 6) + lag(Ozone_mean, 7) + lag(Ozone_mean, 8) + lag(Ozone_mean, 9)) / 10,
         operiod10 = (Ozone_mean + lag(Ozone_mean, 1) + lag(Ozone_mean, 2) + lag(Ozone_mean, 3) + lag(Ozone_mean, 4) + lag(Ozone_mean, 5) + lag(Ozone_mean, 6) + lag(Ozone_mean, 7) + lag(Ozone_mean, 8) + lag(Ozone_mean, 9) + lag(Ozone_mean, 10)) / 11) %>%
  mutate(pperiod1 = (precipitation_mean + lag(precipitation_mean, 1)) / 2,
         pperiod2 = (precipitation_mean + lag(precipitation_mean, 1) + lag(precipitation_mean, 2)) / 3,
         pperiod3 = (precipitation_mean + lag(precipitation_mean, 1) + lag(precipitation_mean, 2) + lag(precipitation_mean, 3)) / 4,
         pperiod4 = (precipitation_mean + lag(precipitation_mean, 1) + lag(precipitation_mean, 2) + lag(precipitation_mean, 3) + lag(precipitation_mean, 4)) / 5,
         pperiod5 = (precipitation_mean + lag(precipitation_mean, 1) + lag(precipitation_mean, 2) + lag(precipitation_mean, 3) + lag(precipitation_mean, 4) + lag(precipitation_mean, 5)) / 6,
         pperiod6 = (precipitation_mean + lag(precipitation_mean, 1) + lag(precipitation_mean, 2) + lag(precipitation_mean, 3) + lag(precipitation_mean, 4) + lag(precipitation_mean, 5) + lag(precipitation_mean, 6)) / 7,
         pperiod7 = (precipitation_mean + lag(precipitation_mean, 1) + lag(precipitation_mean, 2) + lag(precipitation_mean, 3) + lag(precipitation_mean, 4) + lag(precipitation_mean, 5) + lag(precipitation_mean, 6) + lag(precipitation_mean, 7)) / 8,
         pperiod8 = (precipitation_mean + lag(precipitation_mean, 1) + lag(precipitation_mean, 2) + lag(precipitation_mean, 3) + lag(precipitation_mean, 4) + lag(precipitation_mean, 5) + lag(precipitation_mean, 6) + lag(precipitation_mean, 7) + lag(precipitation_mean, 8)) / 9,
         pperiod9 = (precipitation_mean + lag(precipitation_mean, 1) + lag(precipitation_mean, 2) + lag(precipitation_mean, 3) + lag(precipitation_mean, 4) + lag(precipitation_mean, 5) + lag(precipitation_mean, 6) + lag(precipitation_mean, 7) + lag(precipitation_mean, 8) + lag(precipitation_mean, 9)) / 10,
         pperiod10 = (precipitation_mean + lag(precipitation_mean, 1) + lag(precipitation_mean, 2) + lag(precipitation_mean, 3) + lag(precipitation_mean, 4) + lag(precipitation_mean, 5) + lag(precipitation_mean, 6) + lag(precipitation_mean, 7) + lag(precipitation_mean, 8) + lag(precipitation_mean, 9) + lag(precipitation_mean, 10)) / 11) %>%
  mutate(speriod1 = (solar_radiation_mean + lag(solar_radiation_mean, 1)) / 2,
         speriod2 = (solar_radiation_mean + lag(solar_radiation_mean, 1) + lag(solar_radiation_mean, 2)) / 3,
         speriod3 = (solar_radiation_mean + lag(solar_radiation_mean, 1) + lag(solar_radiation_mean, 2) + lag(solar_radiation_mean, 3)) / 4,
         speriod4 = (solar_radiation_mean + lag(solar_radiation_mean, 1) + lag(solar_radiation_mean, 2) + lag(solar_radiation_mean, 3) + lag(solar_radiation_mean, 4)) / 5,
         speriod5 = (solar_radiation_mean + lag(solar_radiation_mean, 1) + lag(solar_radiation_mean, 2) + lag(solar_radiation_mean, 3) + lag(solar_radiation_mean, 4) + lag(solar_radiation_mean, 5)) / 6,
         speriod6 = (solar_radiation_mean + lag(solar_radiation_mean, 1) + lag(solar_radiation_mean, 2) + lag(solar_radiation_mean, 3) + lag(solar_radiation_mean, 4) + lag(solar_radiation_mean, 5) + lag(solar_radiation_mean, 6)) / 7,
         speriod7 = (solar_radiation_mean + lag(solar_radiation_mean, 1) + lag(solar_radiation_mean, 2) + lag(solar_radiation_mean, 3) + lag(solar_radiation_mean, 4) + lag(solar_radiation_mean, 5) + lag(solar_radiation_mean, 6) + lag(solar_radiation_mean, 7)) / 8,
         speriod8 = (solar_radiation_mean + lag(solar_radiation_mean, 1) + lag(solar_radiation_mean, 2) + lag(solar_radiation_mean, 3) + lag(solar_radiation_mean, 4) + lag(solar_radiation_mean, 5) + lag(solar_radiation_mean, 6) + lag(solar_radiation_mean, 7) + lag(solar_radiation_mean, 8)) / 9,
         speriod9 = (solar_radiation_mean + lag(solar_radiation_mean, 1) + lag(solar_radiation_mean, 2) + lag(solar_radiation_mean, 3) + lag(solar_radiation_mean, 4) + lag(solar_radiation_mean, 5) + lag(solar_radiation_mean, 6) + lag(solar_radiation_mean, 7) + lag(solar_radiation_mean, 8) + lag(solar_radiation_mean, 9)) / 10,
         speriod10 = (solar_radiation_mean + lag(solar_radiation_mean, 1) + lag(solar_radiation_mean, 2) + lag(solar_radiation_mean, 3) + lag(solar_radiation_mean, 4) + lag(solar_radiation_mean, 5) + lag(solar_radiation_mean, 6) + lag(solar_radiation_mean, 7) + lag(solar_radiation_mean, 8) + lag(solar_radiation_mean, 9) + lag(solar_radiation_mean, 10)) / 11) %>%
  mutate(Solar_change = saverage1/baseline_Solar,
         NDVI_change = average10/baseline_NDVI,
         Pre_change = paverage10/baseline_Precipitation,
         temp_change = taverage10/baseline_temperature,
         Ozone_change = oaverage10/baseline_ozone)%>%
  ungroup()
data_enviro_female <- data_enviro_female %>%
  arrange(CODE,YEAR)

lag1 <- c(NA, data_enviro_female$NDVI_mean[-nrow(data_enviro_female)])
lag2 <- c(NA, lag1[-nrow(data_enviro_female)])
lag3 <- c(NA, lag2[-nrow(data_enviro_female)])
lag4 <- c(NA, lag3[-nrow(data_enviro_female)])
lag5 <- c(NA, lag4[-nrow(data_enviro_female)])
lag6 <- c(NA, lag5[-nrow(data_enviro_female)])
lag7 <- c(NA, lag6[-nrow(data_enviro_female)])
lag8 <- c(NA, lag7[-nrow(data_enviro_female)])
lag9 <- c(NA, lag8[-nrow(data_enviro_female)])
lag10 <- c(NA, lag9[-nrow(data_enviro_female)])
data_enviro_female$lag1 <- lag1
data_enviro_female$lag2 <- lag2
data_enviro_female$lag3 <- lag3
data_enviro_female$lag4 <- lag4
data_enviro_female$lag5 <- lag5
data_enviro_female$lag6 <- lag6
data_enviro_female$lag7 <- lag7
data_enviro_female$lag8 <- lag8
data_enviro_female$lag9 <- lag9
data_enviro_female$lag10 <- lag10

#lag term temp
tlag1 <- c(NA, data_enviro_female$skin_temp_mean[-nrow(data_enviro_female)])
tlag2 <- c(NA, tlag1[-nrow(data_enviro_female)])
tlag3 <- c(NA, tlag2[-nrow(data_enviro_female)])
tlag4 <- c(NA, tlag3[-nrow(data_enviro_female)])
tlag5 <- c(NA, tlag4[-nrow(data_enviro_female)])
tlag6 <- c(NA, tlag5[-nrow(data_enviro_female)])
tlag7 <- c(NA, tlag6[-nrow(data_enviro_female)])
tlag8 <- c(NA, tlag7[-nrow(data_enviro_female)])
tlag9 <- c(NA, tlag8[-nrow(data_enviro_female)])
tlag10 <- c(NA, tlag9[-nrow(data_enviro_female)])
data_enviro_female$tlag1 <- tlag1
data_enviro_female$tlag2 <- tlag2
data_enviro_female$tlag3 <- tlag3
data_enviro_female$tlag4 <- tlag4
data_enviro_female$tlag5 <- tlag5
data_enviro_female$tlag6 <- tlag6
data_enviro_female$tlag7 <- tlag7
data_enviro_female$tlag8 <- tlag8
data_enviro_female$tlag9 <- tlag9
data_enviro_female$tlag10 <- tlag10

#lagged term precipitation
plag1 <- c(NA, data_enviro_female$precipitation_mean[-nrow(data_enviro_female)])
plag2 <- c(NA, plag1[-nrow(data_enviro_female)])
plag3 <- c(NA, plag2[-nrow(data_enviro_female)])
plag4 <- c(NA, plag3[-nrow(data_enviro_female)])
plag5 <- c(NA, plag4[-nrow(data_enviro_female)])
plag6 <- c(NA, plag5[-nrow(data_enviro_female)])
plag7 <- c(NA, plag6[-nrow(data_enviro_female)])
plag8 <- c(NA, plag7[-nrow(data_enviro_female)])
plag9 <- c(NA, plag8[-nrow(data_enviro_female)])
plag10 <- c(NA, plag9[-nrow(data_enviro_female)])
data_enviro_female$plag1 <- plag1
data_enviro_female$plag2 <- plag2
data_enviro_female$plag3 <- plag3
data_enviro_female$plag4 <- plag4
data_enviro_female$plag5 <- plag5
data_enviro_female$plag6 <- plag6
data_enviro_female$plag7 <- plag7
data_enviro_female$plag8 <- plag8
data_enviro_female$plag9 <- plag9
data_enviro_female$plag10 <- plag10


#lagged term Ozone
olag1 <- c(NA, data_enviro_female$Ozone_mean[-nrow(data_enviro_female)])
olag2 <- c(NA, olag1[-nrow(data_enviro_female)])
olag3 <- c(NA, olag2[-nrow(data_enviro_female)])
olag4 <- c(NA, olag3[-nrow(data_enviro_female)])
olag5 <- c(NA, olag4[-nrow(data_enviro_female)])
olag6 <- c(NA, olag5[-nrow(data_enviro_female)])
olag7 <- c(NA, olag6[-nrow(data_enviro_female)])
olag8 <- c(NA, olag7[-nrow(data_enviro_female)])
olag9 <- c(NA, olag8[-nrow(data_enviro_female)])
olag10 <- c(NA, olag9[-nrow(data_enviro_female)])
data_enviro_female$olag1 <- olag1
data_enviro_female$olag2 <- olag2
data_enviro_female$olag3 <- olag3
data_enviro_female$olag4 <- olag4
data_enviro_female$olag5 <- olag5
data_enviro_female$olag6 <- olag6
data_enviro_female$olag7 <- olag7
data_enviro_female$olag8 <- olag8
data_enviro_female$olag9 <- olag9
data_enviro_female$olag10 <- olag10

#lagged term solar
slag1 <- c(NA, data_enviro_female$solar_radiation_mean[-nrow(data_enviro_female)])
slag2 <- c(NA, slag1[-nrow(data_enviro_female)])
slag3 <- c(NA, slag2[-nrow(data_enviro_female)])
slag4 <- c(NA, slag3[-nrow(data_enviro_female)])
slag5 <- c(NA, slag4[-nrow(data_enviro_female)])
slag6 <- c(NA, slag5[-nrow(data_enviro_female)])
slag7 <- c(NA, slag6[-nrow(data_enviro_female)])
slag8 <- c(NA, slag7[-nrow(data_enviro_female)])
slag9 <- c(NA, slag8[-nrow(data_enviro_female)])
slag10 <- c(NA, slag9[-nrow(data_enviro_female)])
data_enviro_female$slag1 <- slag1
data_enviro_female$slag2 <- slag2
data_enviro_female$slag3 <- slag3
data_enviro_female$slag4 <- slag4
data_enviro_female$slag5 <- slag5
data_enviro_female$slag6 <- slag6
data_enviro_female$slag7 <- slag7
data_enviro_female$slag8 <- slag8
data_enviro_female$slag9 <- slag9
data_enviro_female$slag10 <- slag10

#####lag term NDVI all sex###############

data_enviro_allsex <- data_enviro %>%
  filter(SEX == 3)

baseline <- data_enviro_allsex %>%
  filter(YEAR >= 1988 & YEAR <= 1997) %>%
  group_by(REGISTRY) %>%
  summarize(baseline_temperature = mean(skin_temp_mean, na.rm = TRUE),
            baseline_NDVI = mean(NDVI_mean, na.rm = TRUE),
            baseline_ozone = mean(Ozone_mean, na.rm = TRUE),
            baseline_Precipitation = mean(precipitation_mean, na.rm = TRUE),
            baseline_Solar = mean(solar_radiation_mean, na.rm = TRUE))

data_enviro_allsex <- left_join(data_enviro_allsex, baseline, by = "REGISTRY")
data_enviro_allsex <- data_enviro_allsex %>%
  arrange(REGISTRY,YEAR) %>%
  group_by(REGISTRY) %>%
  mutate(average0 = (NDVI_mean + lag(NDVI_mean, 1)) / 2,
         average1 = (NDVI_mean + lag(NDVI_mean, 1) + lag(NDVI_mean, 2)) / 3,
         average2 = (lag(NDVI_mean, 1) + lag(NDVI_mean, 2) + lag(NDVI_mean, 3)) / 3,
         average3 = (lag(NDVI_mean, 2) + lag(NDVI_mean, 3) + lag(NDVI_mean, 4)) / 3,
         average4 = (lag(NDVI_mean, 3) + lag(NDVI_mean, 4) + lag(NDVI_mean, 5)) / 3,
         average5 = (lag(NDVI_mean, 4) + lag(NDVI_mean, 5) + lag(NDVI_mean, 6)) / 3,
         average6 = (lag(NDVI_mean, 5) + lag(NDVI_mean, 6) + lag(NDVI_mean, 7)) / 3,
         average7 = (lag(NDVI_mean, 6) + lag(NDVI_mean, 7) + lag(NDVI_mean, 8)) / 3,
         average8 = (lag(NDVI_mean, 7) + lag(NDVI_mean, 8) + lag(NDVI_mean, 9)) / 3,
         average9 = (lag(NDVI_mean, 8) + lag(NDVI_mean, 9) + lag(NDVI_mean, 10)) / 3,
         average10 = (lag(NDVI_mean, 9) + lag(NDVI_mean, 10) + lag(NDVI_mean, 11)) / 3)%>%
  mutate(taverage0 = (skin_temp_mean + lag(skin_temp_mean, 1)) / 2,
         taverage1 = (skin_temp_mean + lag(skin_temp_mean, 1) + lag(skin_temp_mean, 2)) / 3,
         taverage2 = (lag(skin_temp_mean, 1) + lag(skin_temp_mean, 2) + lag(skin_temp_mean, 3)) / 3,
         taverage3 = (lag(skin_temp_mean, 2) + lag(skin_temp_mean, 3) + lag(skin_temp_mean, 4)) / 3,
         taverage4 = (lag(skin_temp_mean, 3) + lag(skin_temp_mean, 4) + lag(skin_temp_mean, 5)) / 3,
         taverage5 = (lag(skin_temp_mean, 4) + lag(skin_temp_mean, 5) + lag(skin_temp_mean, 6)) / 3,
         taverage6 = (lag(skin_temp_mean, 5) + lag(skin_temp_mean, 6) + lag(skin_temp_mean, 7)) / 3,
         taverage7 = (lag(skin_temp_mean, 6) + lag(skin_temp_mean, 7) + lag(skin_temp_mean, 8)) / 3,
         taverage8 = (lag(skin_temp_mean, 7) + lag(skin_temp_mean, 8) + lag(skin_temp_mean, 9)) / 3,
         taverage9 = (lag(skin_temp_mean, 8) + lag(skin_temp_mean, 9) + lag(skin_temp_mean, 10)) / 3,
         taverage10 = (lag(skin_temp_mean, 9) + lag(skin_temp_mean, 10) + lag(skin_temp_mean, 11)) / 3)%>%
  mutate(oaverage0 = (Ozone_mean + lag(Ozone_mean, 1)) / 2,
         oaverage1 = (Ozone_mean + lag(Ozone_mean, 1) + lag(Ozone_mean, 2)) / 3,
         oaverage2 = (lag(Ozone_mean, 1) + lag(Ozone_mean, 2) + lag(Ozone_mean, 3)) / 3,
         oaverage3 = (lag(Ozone_mean, 2) + lag(Ozone_mean, 3) + lag(Ozone_mean, 4)) / 3,
         oaverage4 = (lag(Ozone_mean, 3) + lag(Ozone_mean, 4) + lag(Ozone_mean, 5)) / 3,
         oaverage5 = (lag(Ozone_mean, 4) + lag(Ozone_mean, 5) + lag(Ozone_mean, 6)) / 3,
         oaverage6 = (lag(Ozone_mean, 5) + lag(Ozone_mean, 6) + lag(Ozone_mean, 7)) / 3,
         oaverage7 = (lag(Ozone_mean, 6) + lag(Ozone_mean, 7) + lag(Ozone_mean, 8)) / 3,
         oaverage8 = (lag(Ozone_mean, 7) + lag(Ozone_mean, 8) + lag(Ozone_mean, 9)) / 3,
         oaverage9 = (lag(Ozone_mean, 8) + lag(Ozone_mean, 9) + lag(Ozone_mean, 10)) / 3,
         oaverage10 = (lag(Ozone_mean, 9) + lag(Ozone_mean, 10) + lag(Ozone_mean, 11)) / 3)%>%
  mutate(paverage0 = (precipitation_mean + lag(precipitation_mean, 1)) / 2,
         paverage1 = (precipitation_mean + lag(precipitation_mean, 1) + lag(precipitation_mean, 2)) / 3,
         paverage2 = (lag(precipitation_mean, 1) + lag(precipitation_mean, 2) + lag(precipitation_mean, 3)) / 3,
         paverage3 = (lag(precipitation_mean, 2) + lag(precipitation_mean, 3) + lag(precipitation_mean, 4)) / 3,
         paverage4 = (lag(precipitation_mean, 3) + lag(precipitation_mean, 4) + lag(precipitation_mean, 5)) / 3,
         paverage5 = (lag(precipitation_mean, 4) + lag(precipitation_mean, 5) + lag(precipitation_mean, 6)) / 3,
         paverage6 = (lag(precipitation_mean, 5) + lag(precipitation_mean, 6) + lag(precipitation_mean, 7)) / 3,
         paverage7 = (lag(precipitation_mean, 6) + lag(precipitation_mean, 7) + lag(precipitation_mean, 8)) / 3,
         paverage8 = (lag(precipitation_mean, 7) + lag(precipitation_mean, 8) + lag(precipitation_mean, 9)) / 3,
         paverage9 = (lag(precipitation_mean, 8) + lag(precipitation_mean, 9) + lag(precipitation_mean, 10)) / 3,
         paverage10 = (lag(precipitation_mean, 9) + lag(precipitation_mean, 10) + lag(precipitation_mean, 11)) / 3)%>%
  mutate(saverage0 = (solar_radiation_mean + lag(solar_radiation_mean, 1)) / 2,
         saverage1 = (solar_radiation_mean + lag(solar_radiation_mean, 1) + lag(solar_radiation_mean, 2)) / 3,
         saverage2 = (lag(solar_radiation_mean, 1) + lag(solar_radiation_mean, 2) + lag(solar_radiation_mean, 3)) / 3,
         saverage3 = (lag(solar_radiation_mean, 2) + lag(solar_radiation_mean, 3) + lag(solar_radiation_mean, 4)) / 3,
         saverage4 = (lag(solar_radiation_mean, 3) + lag(solar_radiation_mean, 4) + lag(solar_radiation_mean, 5)) / 3,
         saverage5 = (lag(solar_radiation_mean, 4) + lag(solar_radiation_mean, 5) + lag(solar_radiation_mean, 6)) / 3,
         saverage6 = (lag(solar_radiation_mean, 5) + lag(solar_radiation_mean, 6) + lag(solar_radiation_mean, 7)) / 3,
         saverage7 = (lag(solar_radiation_mean, 6) + lag(solar_radiation_mean, 7) + lag(solar_radiation_mean, 8)) / 3,
         saverage8 = (lag(solar_radiation_mean, 7) + lag(solar_radiation_mean, 8) + lag(solar_radiation_mean, 9)) / 3,
         saverage9 = (lag(solar_radiation_mean, 8) + lag(solar_radiation_mean, 9) + lag(solar_radiation_mean, 10)) / 3,
         saverage10 = (lag(solar_radiation_mean, 9) + lag(solar_radiation_mean, 10) + lag(solar_radiation_mean, 11)) / 3)%>%
  mutate(period1 = (NDVI_mean + lag(NDVI_mean, 1)) / 2,
         period2 = (NDVI_mean + lag(NDVI_mean, 1) + lag(NDVI_mean, 2)) / 3,
         period3 = (NDVI_mean + lag(NDVI_mean, 1) + lag(NDVI_mean, 2) + lag(NDVI_mean, 3)) / 4,
         period4 = (NDVI_mean + lag(NDVI_mean, 1) + lag(NDVI_mean, 2) + lag(NDVI_mean, 3) + lag(NDVI_mean, 4)) / 5,
         period5 = (NDVI_mean + lag(NDVI_mean, 1) + lag(NDVI_mean, 2) + lag(NDVI_mean, 3) + lag(NDVI_mean, 4) + lag(NDVI_mean, 5)) / 6,
         period6 = (NDVI_mean + lag(NDVI_mean, 1) + lag(NDVI_mean, 2) + lag(NDVI_mean, 3) + lag(NDVI_mean, 4) + lag(NDVI_mean, 5) + lag(NDVI_mean, 6)) / 7,
         period7 = (NDVI_mean + lag(NDVI_mean, 1) + lag(NDVI_mean, 2) + lag(NDVI_mean, 3) + lag(NDVI_mean, 4) + lag(NDVI_mean, 5) + lag(NDVI_mean, 6) + lag(NDVI_mean, 7)) / 8,
         period8 = (NDVI_mean + lag(NDVI_mean, 1) + lag(NDVI_mean, 2) + lag(NDVI_mean, 3) + lag(NDVI_mean, 4) + lag(NDVI_mean, 5) + lag(NDVI_mean, 6) + lag(NDVI_mean, 7) + lag(NDVI_mean, 8)) / 9,
         period9 = (NDVI_mean + lag(NDVI_mean, 1) + lag(NDVI_mean, 2) + lag(NDVI_mean, 3) + lag(NDVI_mean, 4) + lag(NDVI_mean, 5) + lag(NDVI_mean, 6) + lag(NDVI_mean, 7) + lag(NDVI_mean, 8) + lag(NDVI_mean, 9)) / 10,
         period10 = (NDVI_mean + lag(NDVI_mean, 1) + lag(NDVI_mean, 2) + lag(NDVI_mean, 3) + lag(NDVI_mean, 4) + lag(NDVI_mean, 5) + lag(NDVI_mean, 6) + lag(NDVI_mean, 7) + lag(NDVI_mean, 8) + lag(NDVI_mean, 9) + lag(NDVI_mean, 10)) / 11) %>%
  mutate(tperiod1 = (skin_temp_mean + lag(skin_temp_mean, 1)) / 2,
         tperiod2 = (skin_temp_mean + lag(skin_temp_mean, 1) + lag(skin_temp_mean, 2)) / 3,
         tperiod3 = (skin_temp_mean + lag(skin_temp_mean, 1) + lag(skin_temp_mean, 2) + lag(skin_temp_mean, 3)) / 4,
         tperiod4 = (skin_temp_mean + lag(skin_temp_mean, 1) + lag(skin_temp_mean, 2) + lag(skin_temp_mean, 3) + lag(skin_temp_mean, 4)) / 5,
         tperiod5 = (skin_temp_mean + lag(skin_temp_mean, 1) + lag(skin_temp_mean, 2) + lag(skin_temp_mean, 3) + lag(skin_temp_mean, 4) + lag(skin_temp_mean, 5)) / 6,
         tperiod6 = (skin_temp_mean + lag(skin_temp_mean, 1) + lag(skin_temp_mean, 2) + lag(skin_temp_mean, 3) + lag(skin_temp_mean, 4) + lag(skin_temp_mean, 5) + lag(skin_temp_mean, 6)) / 7,
         tperiod7 = (skin_temp_mean + lag(skin_temp_mean, 1) + lag(skin_temp_mean, 2) + lag(skin_temp_mean, 3) + lag(skin_temp_mean, 4) + lag(skin_temp_mean, 5) + lag(skin_temp_mean, 6) + lag(skin_temp_mean, 7)) / 8,
         tperiod8 = (skin_temp_mean + lag(skin_temp_mean, 1) + lag(skin_temp_mean, 2) + lag(skin_temp_mean, 3) + lag(skin_temp_mean, 4) + lag(skin_temp_mean, 5) + lag(skin_temp_mean, 6) + lag(skin_temp_mean, 7) + lag(skin_temp_mean, 8)) / 9,
         tperiod9 = (skin_temp_mean + lag(skin_temp_mean, 1) + lag(skin_temp_mean, 2) + lag(skin_temp_mean, 3) + lag(skin_temp_mean, 4) + lag(skin_temp_mean, 5) + lag(skin_temp_mean, 6) + lag(skin_temp_mean, 7) + lag(skin_temp_mean, 8) + lag(skin_temp_mean, 9)) / 10,
         tperiod10 = (skin_temp_mean + lag(skin_temp_mean, 1) + lag(skin_temp_mean, 2) + lag(skin_temp_mean, 3) + lag(skin_temp_mean, 4) + lag(skin_temp_mean, 5) + lag(skin_temp_mean, 6) + lag(skin_temp_mean, 7) + lag(skin_temp_mean, 8) + lag(skin_temp_mean, 9) + lag(skin_temp_mean, 10)) / 11) %>%
  mutate(operiod1 = (Ozone_mean + lag(Ozone_mean, 1)) / 2,
         operiod2 = (Ozone_mean + lag(Ozone_mean, 1) + lag(Ozone_mean, 2)) / 3,
         operiod3 = (Ozone_mean + lag(Ozone_mean, 1) + lag(Ozone_mean, 2) + lag(Ozone_mean, 3)) / 4,
         operiod4 = (Ozone_mean + lag(Ozone_mean, 1) + lag(Ozone_mean, 2) + lag(Ozone_mean, 3) + lag(Ozone_mean, 4)) / 5,
         operiod5 = (Ozone_mean + lag(Ozone_mean, 1) + lag(Ozone_mean, 2) + lag(Ozone_mean, 3) + lag(Ozone_mean, 4) + lag(Ozone_mean, 5)) / 6,
         operiod6 = (Ozone_mean + lag(Ozone_mean, 1) + lag(Ozone_mean, 2) + lag(Ozone_mean, 3) + lag(Ozone_mean, 4) + lag(Ozone_mean, 5) + lag(Ozone_mean, 6)) / 7,
         operiod7 = (Ozone_mean + lag(Ozone_mean, 1) + lag(Ozone_mean, 2) + lag(Ozone_mean, 3) + lag(Ozone_mean, 4) + lag(Ozone_mean, 5) + lag(Ozone_mean, 6) + lag(Ozone_mean, 7)) / 8,
         operiod8 = (Ozone_mean + lag(Ozone_mean, 1) + lag(Ozone_mean, 2) + lag(Ozone_mean, 3) + lag(Ozone_mean, 4) + lag(Ozone_mean, 5) + lag(Ozone_mean, 6) + lag(Ozone_mean, 7) + lag(Ozone_mean, 8)) / 9,
         operiod9 = (Ozone_mean + lag(Ozone_mean, 1) + lag(Ozone_mean, 2) + lag(Ozone_mean, 3) + lag(Ozone_mean, 4) + lag(Ozone_mean, 5) + lag(Ozone_mean, 6) + lag(Ozone_mean, 7) + lag(Ozone_mean, 8) + lag(Ozone_mean, 9)) / 10,
         operiod10 = (Ozone_mean + lag(Ozone_mean, 1) + lag(Ozone_mean, 2) + lag(Ozone_mean, 3) + lag(Ozone_mean, 4) + lag(Ozone_mean, 5) + lag(Ozone_mean, 6) + lag(Ozone_mean, 7) + lag(Ozone_mean, 8) + lag(Ozone_mean, 9) + lag(Ozone_mean, 10)) / 11) %>%
  mutate(pperiod1 = (precipitation_mean + lag(precipitation_mean, 1)) / 2,
         pperiod2 = (precipitation_mean + lag(precipitation_mean, 1) + lag(precipitation_mean, 2)) / 3,
         pperiod3 = (precipitation_mean + lag(precipitation_mean, 1) + lag(precipitation_mean, 2) + lag(precipitation_mean, 3)) / 4,
         pperiod4 = (precipitation_mean + lag(precipitation_mean, 1) + lag(precipitation_mean, 2) + lag(precipitation_mean, 3) + lag(precipitation_mean, 4)) / 5,
         pperiod5 = (precipitation_mean + lag(precipitation_mean, 1) + lag(precipitation_mean, 2) + lag(precipitation_mean, 3) + lag(precipitation_mean, 4) + lag(precipitation_mean, 5)) / 6,
         pperiod6 = (precipitation_mean + lag(precipitation_mean, 1) + lag(precipitation_mean, 2) + lag(precipitation_mean, 3) + lag(precipitation_mean, 4) + lag(precipitation_mean, 5) + lag(precipitation_mean, 6)) / 7,
         pperiod7 = (precipitation_mean + lag(precipitation_mean, 1) + lag(precipitation_mean, 2) + lag(precipitation_mean, 3) + lag(precipitation_mean, 4) + lag(precipitation_mean, 5) + lag(precipitation_mean, 6) + lag(precipitation_mean, 7)) / 8,
         pperiod8 = (precipitation_mean + lag(precipitation_mean, 1) + lag(precipitation_mean, 2) + lag(precipitation_mean, 3) + lag(precipitation_mean, 4) + lag(precipitation_mean, 5) + lag(precipitation_mean, 6) + lag(precipitation_mean, 7) + lag(precipitation_mean, 8)) / 9,
         pperiod9 = (precipitation_mean + lag(precipitation_mean, 1) + lag(precipitation_mean, 2) + lag(precipitation_mean, 3) + lag(precipitation_mean, 4) + lag(precipitation_mean, 5) + lag(precipitation_mean, 6) + lag(precipitation_mean, 7) + lag(precipitation_mean, 8) + lag(precipitation_mean, 9)) / 10,
         pperiod10 = (precipitation_mean + lag(precipitation_mean, 1) + lag(precipitation_mean, 2) + lag(precipitation_mean, 3) + lag(precipitation_mean, 4) + lag(precipitation_mean, 5) + lag(precipitation_mean, 6) + lag(precipitation_mean, 7) + lag(precipitation_mean, 8) + lag(precipitation_mean, 9) + lag(precipitation_mean, 10)) / 11) %>%
  mutate(speriod1 = (solar_radiation_mean + lag(solar_radiation_mean, 1)) / 2,
         speriod2 = (solar_radiation_mean + lag(solar_radiation_mean, 1) + lag(solar_radiation_mean, 2)) / 3,
         speriod3 = (solar_radiation_mean + lag(solar_radiation_mean, 1) + lag(solar_radiation_mean, 2) + lag(solar_radiation_mean, 3)) / 4,
         speriod4 = (solar_radiation_mean + lag(solar_radiation_mean, 1) + lag(solar_radiation_mean, 2) + lag(solar_radiation_mean, 3) + lag(solar_radiation_mean, 4)) / 5,
         speriod5 = (solar_radiation_mean + lag(solar_radiation_mean, 1) + lag(solar_radiation_mean, 2) + lag(solar_radiation_mean, 3) + lag(solar_radiation_mean, 4) + lag(solar_radiation_mean, 5)) / 6,
         speriod6 = (solar_radiation_mean + lag(solar_radiation_mean, 1) + lag(solar_radiation_mean, 2) + lag(solar_radiation_mean, 3) + lag(solar_radiation_mean, 4) + lag(solar_radiation_mean, 5) + lag(solar_radiation_mean, 6)) / 7,
         speriod7 = (solar_radiation_mean + lag(solar_radiation_mean, 1) + lag(solar_radiation_mean, 2) + lag(solar_radiation_mean, 3) + lag(solar_radiation_mean, 4) + lag(solar_radiation_mean, 5) + lag(solar_radiation_mean, 6) + lag(solar_radiation_mean, 7)) / 8,
         speriod8 = (solar_radiation_mean + lag(solar_radiation_mean, 1) + lag(solar_radiation_mean, 2) + lag(solar_radiation_mean, 3) + lag(solar_radiation_mean, 4) + lag(solar_radiation_mean, 5) + lag(solar_radiation_mean, 6) + lag(solar_radiation_mean, 7) + lag(solar_radiation_mean, 8)) / 9,
         speriod9 = (solar_radiation_mean + lag(solar_radiation_mean, 1) + lag(solar_radiation_mean, 2) + lag(solar_radiation_mean, 3) + lag(solar_radiation_mean, 4) + lag(solar_radiation_mean, 5) + lag(solar_radiation_mean, 6) + lag(solar_radiation_mean, 7) + lag(solar_radiation_mean, 8) + lag(solar_radiation_mean, 9)) / 10,
         speriod10 = (solar_radiation_mean + lag(solar_radiation_mean, 1) + lag(solar_radiation_mean, 2) + lag(solar_radiation_mean, 3) + lag(solar_radiation_mean, 4) + lag(solar_radiation_mean, 5) + lag(solar_radiation_mean, 6) + lag(solar_radiation_mean, 7) + lag(solar_radiation_mean, 8) + lag(solar_radiation_mean, 9) + lag(solar_radiation_mean, 10)) / 11) %>%
  mutate(Solar_change = saverage1/baseline_Solar,
         NDVI_change = average10/baseline_NDVI,
         Pre_change = paverage10/baseline_Precipitation,
         temp_change = taverage10/baseline_temperature,
         Ozone_change = oaverage10/baseline_ozone)%>%
  ungroup()
data_enviro_allsex <- data_enviro_allsex %>%
  arrange(CODE,YEAR)

lag1 <- c(NA, data_enviro_allsex$NDVI_mean[-nrow(data_enviro_allsex)])
lag2 <- c(NA, lag1[-nrow(data_enviro_allsex)])
lag3 <- c(NA, lag2[-nrow(data_enviro_allsex)])
lag4 <- c(NA, lag3[-nrow(data_enviro_allsex)])
lag5 <- c(NA, lag4[-nrow(data_enviro_allsex)])
lag6 <- c(NA, lag5[-nrow(data_enviro_allsex)])
lag7 <- c(NA, lag6[-nrow(data_enviro_allsex)])
lag8 <- c(NA, lag7[-nrow(data_enviro_allsex)])
lag9 <- c(NA, lag8[-nrow(data_enviro_allsex)])
lag10 <- c(NA, lag9[-nrow(data_enviro_allsex)])
data_enviro_allsex$lag1 <- lag1
data_enviro_allsex$lag2 <- lag2
data_enviro_allsex$lag3 <- lag3
data_enviro_allsex$lag4 <- lag4
data_enviro_allsex$lag5 <- lag5
data_enviro_allsex$lag6 <- lag6
data_enviro_allsex$lag7 <- lag7
data_enviro_allsex$lag8 <- lag8
data_enviro_allsex$lag9 <- lag9
data_enviro_allsex$lag10 <- lag10

#lag term temp
tlag1 <- c(NA, data_enviro_allsex$skin_temp_mean[-nrow(data_enviro_allsex)])
tlag2 <- c(NA, tlag1[-nrow(data_enviro_allsex)])
tlag3 <- c(NA, tlag2[-nrow(data_enviro_allsex)])
tlag4 <- c(NA, tlag3[-nrow(data_enviro_allsex)])
tlag5 <- c(NA, tlag4[-nrow(data_enviro_allsex)])
tlag6 <- c(NA, tlag5[-nrow(data_enviro_allsex)])
tlag7 <- c(NA, tlag6[-nrow(data_enviro_allsex)])
tlag8 <- c(NA, tlag7[-nrow(data_enviro_allsex)])
tlag9 <- c(NA, tlag8[-nrow(data_enviro_allsex)])
tlag10 <- c(NA, tlag9[-nrow(data_enviro_allsex)])
data_enviro_allsex$tlag1 <- tlag1
data_enviro_allsex$tlag2 <- tlag2
data_enviro_allsex$tlag3 <- tlag3
data_enviro_allsex$tlag4 <- tlag4
data_enviro_allsex$tlag5 <- tlag5
data_enviro_allsex$tlag6 <- tlag6
data_enviro_allsex$tlag7 <- tlag7
data_enviro_allsex$tlag8 <- tlag8
data_enviro_allsex$tlag9 <- tlag9
data_enviro_allsex$tlag10 <- tlag10

#lagged term precipitation
plag1 <- c(NA, data_enviro_allsex$precipitation_mean[-nrow(data_enviro_allsex)])
plag2 <- c(NA, plag1[-nrow(data_enviro_allsex)])
plag3 <- c(NA, plag2[-nrow(data_enviro_allsex)])
plag4 <- c(NA, plag3[-nrow(data_enviro_allsex)])
plag5 <- c(NA, plag4[-nrow(data_enviro_allsex)])
plag6 <- c(NA, plag5[-nrow(data_enviro_allsex)])
plag7 <- c(NA, plag6[-nrow(data_enviro_allsex)])
plag8 <- c(NA, plag7[-nrow(data_enviro_allsex)])
plag9 <- c(NA, plag8[-nrow(data_enviro_allsex)])
plag10 <- c(NA, plag9[-nrow(data_enviro_allsex)])
data_enviro_allsex$plag1 <- plag1
data_enviro_allsex$plag2 <- plag2
data_enviro_allsex$plag3 <- plag3
data_enviro_allsex$plag4 <- plag4
data_enviro_allsex$plag5 <- plag5
data_enviro_allsex$plag6 <- plag6
data_enviro_allsex$plag7 <- plag7
data_enviro_allsex$plag8 <- plag8
data_enviro_allsex$plag9 <- plag9
data_enviro_allsex$plag10 <- plag10


#lagged term Ozone
olag1 <- c(NA, data_enviro_allsex$Ozone_mean[-nrow(data_enviro_allsex)])
olag2 <- c(NA, olag1[-nrow(data_enviro_allsex)])
olag3 <- c(NA, olag2[-nrow(data_enviro_allsex)])
olag4 <- c(NA, olag3[-nrow(data_enviro_allsex)])
olag5 <- c(NA, olag4[-nrow(data_enviro_allsex)])
olag6 <- c(NA, olag5[-nrow(data_enviro_allsex)])
olag7 <- c(NA, olag6[-nrow(data_enviro_allsex)])
olag8 <- c(NA, olag7[-nrow(data_enviro_allsex)])
olag9 <- c(NA, olag8[-nrow(data_enviro_allsex)])
olag10 <- c(NA, olag9[-nrow(data_enviro_allsex)])
data_enviro_allsex$olag1 <- olag1
data_enviro_allsex$olag2 <- olag2
data_enviro_allsex$olag3 <- olag3
data_enviro_allsex$olag4 <- olag4
data_enviro_allsex$olag5 <- olag5
data_enviro_allsex$olag6 <- olag6
data_enviro_allsex$olag7 <- olag7
data_enviro_allsex$olag8 <- olag8
data_enviro_allsex$olag9 <- olag9
data_enviro_allsex$olag10 <- olag10

#lagged term solar
slag1 <- c(NA, data_enviro_allsex$solar_radiation_mean[-nrow(data_enviro_allsex)])
slag2 <- c(NA, slag1[-nrow(data_enviro_allsex)])
slag3 <- c(NA, slag2[-nrow(data_enviro_allsex)])
slag4 <- c(NA, slag3[-nrow(data_enviro_allsex)])
slag5 <- c(NA, slag4[-nrow(data_enviro_allsex)])
slag6 <- c(NA, slag5[-nrow(data_enviro_allsex)])
slag7 <- c(NA, slag6[-nrow(data_enviro_allsex)])
slag8 <- c(NA, slag7[-nrow(data_enviro_allsex)])
slag9 <- c(NA, slag8[-nrow(data_enviro_allsex)])
slag10 <- c(NA, slag9[-nrow(data_enviro_allsex)])
data_enviro_allsex$slag1 <- slag1
data_enviro_allsex$slag2 <- slag2
data_enviro_allsex$slag3 <- slag3
data_enviro_allsex$slag4 <- slag4
data_enviro_allsex$slag5 <- slag5
data_enviro_allsex$slag6 <- slag6
data_enviro_allsex$slag7 <- slag7
data_enviro_allsex$slag8 <- slag8
data_enviro_allsex$slag9 <- slag9
data_enviro_allsex$slag10 <- slag10

data_enviro <- rbind(data_enviro_male, data_enviro_allsex,data_enviro_female)
CI5_all <- merge(data_enviro,data_IC,
                  by=c("REGISTRY","YEAR","SEX"))
CI5_all <- CI5_all%>%
  filter(YEAR >= 1998)
summary(CI5_all)
colnames(CI5_all)[colnames(CI5_all) == "GDP per capita"] <- "GDP"
colnames(CI5_all)[colnames(CI5_all) == "REGISTRY"] <- "GEOID"
colnames(CI5_all)[colnames(CI5_all) == "PM25_mean"] <- "PM25"
colnames(CI5_all)[colnames(CI5_all) == "solar_radiation_mean"] <- "Solar_radiation"
colnames(CI5_all)[colnames(CI5_all) == "precipitation_mean"] <- "Precipitation"
colnames(CI5_all)[colnames(CI5_all) == "Ozone_mean"] <- "Ozone"
colnames(CI5_all)[colnames(CI5_all) == "NDVI"] <- "NDVI2"
colnames(CI5_all)[colnames(CI5_all) == "NDVI_mean"] <- "NDVI"
colnames(CI5_all)[colnames(CI5_all) == "CANCER"] <- "cancer_code"
colnames(CI5_all)[colnames(CI5_all) == "name"] <- "CANCER"
colnames(CI5_all)[colnames(CI5_all) == "NAME"] <- "REGISTRY"
colnames(CI5_all)[colnames(CI5_all) == "skin_temp_mean"] <- "skin_temperature"
colnames(CI5_all)[colnames(CI5_all) == "TOTAL.x"] <- "P_all"
getwd()
setwd('C:/Haowen Wang/Cancer and climate change/SEER data')
save(CI5_all, file = "CI5_clean.Rdata")


# cor_matrix <- cor(data_male_all[, c("NDVI", "Ozone", "Precipitation", "skin_temperature", "Solar_radiation", 
#                                "PM25")])
# print(cor_matrix)
# png("heatmap.png", width = 1200, height = 1200, res = 150)
# corrmap <- corrplot(cor_matrix, method = "color", type = "full", order = "hclust",
#                     addCoef.col = "black", # Add correlation coefficients to the plot
#                     tl.col = "black", # Change text color of variable names to black
#                     tl.srt = 45,
#                     number.cex = 0.8, # Font size of correlation coefficients# Font size of correlation coefficients
#                     tl.cex = 0.8) # Rotate variable names by 45 degrees
# dev.off()