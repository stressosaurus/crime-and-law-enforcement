### draft script

library(zoo)
library(slider)
library(tibble)
library(lubridate, warn.conflicts = FALSE)
library(tidyverse)

# load data set
df <- read_csv("stage5.csv")

# corrections for fresno police department
df$actual_index_property[df$ori == "CA0100500" & df$Rdate == '2019-11-15'] <- NA #1446
df$actual_index_violent[df$ori == "CA0100500" & df$Rdate == '2019-11-15'] <- NA #238
df$actual_index_total[df$ori == "CA0100500" & df$Rdate == '2019-11-15'] <- 1684
df$actual_index_property[df$ori == "CA0100500" & df$Rdate == '2019-12-15'] <- NA #1622
df$actual_index_violent[df$ori == "CA0100500" & df$Rdate == '2019-12-15'] <- NA #274
df$actual_index_total[df$ori == "CA0100500" & df$Rdate == '2019-12-15'] <- 1896

# computing rate per months
df <- df %>%
  group_by(ori, Rdate) %>% 
  mutate(m_property_crime_rate = ceiling((actual_index_property/U_TPOP)*100000),
         m_violent_crime_rate = ceiling((actual_index_violent/U_TPOP)*100000), 
         m_index_crime_rate = ceiling((actual_index_total/U_TPOP)*100000)) %>%
  ungroup()

# function to estimate slope
slope <- function(data){
  sequence <- seq_along(data)
  estimate <- lm(formula = data ~ sequence, na.action=na.omit)
  return(summary(estimate)$coefficients["sequence","Estimate"])
}

# lags vector
lags_vect <- seq(2,24)

# main data frame
df_MAIN <- tibble(GEOID = df$GEOID,
                  ori = df$ori,
                  fips_state_county_code = df$fips_state_county_code,
                  crosswalk_agency_name = df$crosswalk_agency_name,
                  Rdate = df$Rdate,
                  OISperMonth = df$OISperMonth,
                  m_index_crime_rate = df$m_index_crime_rate)

for (lag in lags_vect) {
  
  # set lag
  #lag <- 2 # months
  new_col1 <- paste0("slope_",lag)
  
  # extract sliding windows and apply slope function
  df_slides <- df %>%
    # group by agency
    group_by(GEOID) %>%
    # sort dates
    arrange(Rdate,.by_group = TRUE) %>%
    summarise(# start date for the lag
              start = rollapply(Rdate,width=lag,by=1,min,align="left"),
              # end date for the lag
              end = rollapply(Rdate,width=lag,by=1,max,align="left"),
              # apply SLR to estimate slope of the previous l months
              !!new_col1 := rollapply(m_index_crime_rate,width=lag,by=1,
                                slope,
                                align="left"),
              # add in the crime rates and the OIS on the next month of end month
              OIS_month = add_with_rollback(end, months(1)),
              .groups = "drop") %>%
    select(-start,-end)
  
  # join the data to the MAIN data frame
  df_X <- df %>% 
    select(GEOID,ori,fips_state_county_code,crosswalk_agency_name,Rdate,OISperMonth,m_index_crime_rate) %>%
    left_join(df_slides,by=c("GEOID" = "GEOID","Rdate" = "OIS_month"))
  df_MAIN <- full_join(df_MAIN,df_X,by=c("GEOID"="GEOID",
                                         "ori" = "ori",
                                         "fips_state_county_code" = "fips_state_county_code",
                                         "crosswalk_agency_name" = "crosswalk_agency_name",
                                         "Rdate"="Rdate",
                                         "OISperMonth"="OISperMonth",
                                         "m_index_crime_rate"="m_index_crime_rate"))
}

# save the resulting data frame as csv
write.csv(df_MAIN,"lagSlopesOIS.csv",row.names = FALSE)
