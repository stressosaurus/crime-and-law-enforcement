oakland2012 <- read_xlsx("Datasets/OAKLAND data/2012-Use-of-Force-for-PRRs.xlsx")
oakland2013 <- read_xlsx("Datasets/OAKLAND data/2013-Use-of-Force-for-PRRs.xlsx")
oakland2014 <- read_xlsx("Datasets/OAKLAND data/2014-Use-of-Force-for-PRRs.xlsx")
oakland2015 <- read_xlsx("Datasets/OAKLAND data/2015-Use-of-Force-for-PRRs.xlsx")
oakland2016 <- read_xlsx("Datasets/OAKLAND data/2016-Use-of-Force-for-PRRs.xlsx")
oakland2017 <- read_xlsx("Datasets/OAKLAND data/2017-Use-of-Force-for-PRRs.xlsx")
oakland2018 <- read_xlsx("Datasets/OAKLAND data/2018-Use-of-Force-for-PRRs.xlsx")
oakland2019 <- read_xlsx("Datasets/OAKLAND data/2019-Use-of-Force-for-PRRs.xlsx")

remove(oakland2012_20)

oakland_mpv <- new %>% filter(ori == "CA0010900")


#okle data wrangling 

LEAcrosswalk_CA %>% group_by(AGCYTYPE) %>% summarise(agency = n())
ca_sj <- LEAcrosswalk_CA %>% filter(AGCYTYPE == "(006) Special jurisdiction")
intersect(ca_sj$ORI9, ca_mpv$ori)

LEAcrosswalk_CA <- rename(LEAcrosswalk_CA, ori = ORI9)

ca_mpv_2 <- merge(ca_mpv, LEAcrosswalk_CA, by = "ori", all.x = TRUE)
ca_mpv_2 %>% group_by(AGCYTYPE) %>% summarise(agency = n())

# I feel New should have the attributes of LEAcrosswalk

new <- merge(new, LEAcrosswalk_CA, by = "ori", all.x = TRUE)

ca_mpv[114,13] = "CA0072300"


#Exploring sethi's Data

sethi_S3_ca %>% filter(ORI7 == "-1") 


# LEOKA Data

library(haven)
leoka_monthly_2020 <- 
  read_dta("Datasets/LEOKA/ucr_leoka_monthly_1960_2020_dta/leoka_monthly_2020.dta")
leoka_monthly_2019 <- 
  read_dta("Datasets/LEOKA/ucr_leoka_monthly_1960_2020_dta/leoka_monthly_2019.dta")
leoka_monthly_2018 <- 
  read_dta("Datasets/LEOKA/ucr_leoka_monthly_1960_2020_dta/leoka_monthly_2018.dta")
leoka_monthly_2017 <- 
  read_dta("Datasets/LEOKA/ucr_leoka_monthly_1960_2020_dta/leoka_monthly_2017.dta")
leoka_monthly_2016 <- 
  read_dta("Datasets/LEOKA/ucr_leoka_monthly_1960_2020_dta/leoka_monthly_2016.dta")
leoka_monthly_2015 <- 
  read_dta("Datasets/LEOKA/ucr_leoka_monthly_1960_2020_dta/leoka_monthly_2015.dta")
leoka_monthly_2014 <- 
  read_dta("Datasets/LEOKA/ucr_leoka_monthly_1960_2020_dta/leoka_monthly_2014.dta")
leoka_monthly_2013 <- 
  read_dta("Datasets/LEOKA/ucr_leoka_monthly_1960_2020_dta/leoka_monthly_2013.dta")


CA13 <- leoka_monthly_2013 %>% 
  filter(state_abb == "CA")
CA14 <- leoka_monthly_2014 %>% 
  filter(state_abb == "CA")
CA15 <- leoka_monthly_2015 %>% 
  filter(state_abb == "CA")
CA16 <- leoka_monthly_2016 %>% 
  filter(state_abb == "CA")
CA17 <- leoka_monthly_2017 %>% 
  filter(state_abb == "CA")
CA18 <- leoka_monthly_2018 %>% 
  filter(state_abb == "CA")
CA19 <- leoka_monthly_2019 %>% 
  filter(state_abb == "CA")
CA20 <- leoka_monthly_2020 %>% 
  filter(state_abb == "CA")

CA13_20_raw <- rbind(CA13, CA14, CA15, CA16, CA17, CA18, CA19, CA20)

remove(leoka_monthly_2013, leoka_monthly_2014, leoka_monthly_2015, 
       leoka_monthly_2016, leoka_monthly_2017, leoka_monthly_2018, 
       leoka_monthly_2019, leoka_monthly_2020)


df <- StageThreeDatax %>% group_by(ori) %>%  arrange(Rdate, .by_group = TRUE)

df %>% group_by(ori) %>% summarise(n = n()) %>% filter(n!= 84)


# finding if more than one agency shares the same PLACE


unique(StageFourData$ori) #321

unique(StageFourData$GEOID) #320

StageFourData %>% group_by(ori) %>% summarise(n = n())

StageFourData %>% group_by(GEOID) %>% summarise(n = n()) %>% filter(n == 168)

# we have two agencies sharing same GEOID

install.packages("googlesheets4")
library("googlesheets4")
