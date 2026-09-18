# Daylight hours ----------------------------------------------------------

library(dplyr)
library(tidyr)
library(lubridate)

setwd("~/Library/CloudStorage/OneDrive-JamesCookUniversity/Projects/THESIS_PHD_NLJ_2024/CH5_PREForaging")


tFinal <- readRDS("./src_outputs/tidesFinal.rds") 

datePlace<-data.frame(date=seq(min(tFinal$dateTimeUTC), max(tFinal$dateTimeUTC)+days(1), 'days'),
                      lat=mean(tFinal$y),lon=mean(tFinal$x))%>%
  mutate(date=date(date))

# Get times (come as UTC) and convert to AEST
lightTimes<-getSunlightTimes(data=datePlace, keep = c("nauticalDawn","nauticalDusk"))%>%
  mutate(nauticalDawn=ymd_hms(nauticalDawn)%>%with_tz("Australia/Brisbane"),
         nauticalDusk=ymd_hms(nauticalDusk)%>%with_tz("Australia/Brisbane"))%>%
  distinct(date, .keep_all = TRUE)

moonLight<-getMoonIllumination(date=datePlace$date)%>%
  mutate(moonFraction=fraction)%>%
  dplyr::select(date,moonFraction)

lightPRE<-readRDS("./src_outputs/tidesFinal.rds") %>%
  mutate(date=date(dateTimeAEST))%>%
  left_join(., lightTimes) %>%
  left_join(., moonLight) %>%
  mutate(timeSinceDawn = as.numeric(difftime(dateTimeAEST, nauticalDawn, units = "hours")),
         timeSinceDusk = as.numeric(difftime(dateTimeAEST, nauticalDusk, units = "hours")),
         lightPeriod = ifelse(dateTimeAEST > nauticalDawn & dateTimeAEST < nauticalDusk, "Light","Dark"),
         TOD=hour(dateTimeAEST))%>%
  dplyr::select(-c(lat,lon))

# Persist so 6.BenthosType.R can pick this up (previously computed in-memory only,
# relying on an already-saved copy of this exact object being present)
saveRDS(lightPRE, "./src_outputs/lightFinal.rds")












