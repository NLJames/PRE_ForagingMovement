

library(tidyverse)
library(sf)
library(fuzzyjoin )
library(zoo)

setwd("~/Library/CloudStorage/OneDrive-JamesCookUniversity/Projects/THESIS_PHD_NLJ_2024/CH5_PREForaging")


# read in data frame from previous prep stage
benthosPoints<-readRDS("./data/DFs/benthosFinal.rds")%>%
  mutate(hour = hour(dateTimeAEST),
         date = date(dateTimeAEST))%>%
  st_drop_geometry()


# Get temperature data from both lagoon and reef slope

# Takes a very long time to run so better off just
# taking the saved data frames below

# source("./src/AIMSloggerData.R")
# time stamps are in UTC

# 1m depth reef flat
fTemp<-readRDS("./src_outputs/AIMStempReefFlat.rds")%>%
  dplyr::select(time, cal_val)%>%
  mutate(time=time%>%with_tz("Australia/Brisbane"),
         hour = hour(time),
         date = date(time))%>%
  dplyr::rename(flatTemp = cal_val)%>%
  dplyr::filter(!is.na(flatTemp))%>%
  distinct(date, hour, .keep_all = TRUE)%>%
  complete(date, hour) %>%  # Ensure all date-hour combinations exist
  arrange(date, hour) %>%  # Ensure data is ordered by time
  mutate(flatTemp = na.approx(flatTemp, na.rm = FALSE))%>%
  dplyr::filter(!is.na(flatTemp))%>%
  dplyr::select(-time)


# 8.5m depth slope
sTemp<-readRDS("./src_outputs/AIMStempReefSlope.rds")%>%
  dplyr::select(time, cal_val)%>%
  mutate(time=time%>%with_tz("Australia/Brisbane"),
         hour = hour(time),
         date = date(time))%>%
  dplyr::rename(slopeTemp = cal_val)%>%
  dplyr::filter(!is.na(slopeTemp))%>%
  distinct(date, hour, .keep_all = TRUE)%>%
  complete(date, hour) %>%  # Ensure all date-hour combinations exist
  arrange(date, hour) %>%  # Ensure data is ordered by time
  mutate(slopeTemp = na.approx(slopeTemp, na.rm = FALSE))%>%
  dplyr::filter(!is.na(slopeTemp))%>%
  dplyr::select(-time)



# Perform a fuzzy join based on closest time difference
flatTemp_joined <- left_join(benthosPoints, fTemp)
slopeTemp_joined <- left_join(benthosPoints, sTemp)


# skim(flatTemp_joined)
# skim(slopeTemp_joined)


all_joined <- left_join(slopeTemp_joined, flatTemp_joined)%>%
  mutate(month = month(dateTimeAEST),
         season = case_when(
           month %in% c(12, 1, 2) ~ "Summer",
           month %in%  3:5  ~ "Autumn",
           month %in%  6:8  ~ "Winter",
           month %in%  9:11  ~ "Spring"))%>%
  group_by(month)%>%
  mutate(slopeTemp = ifelse(is.na(slopeTemp), mean(slopeTemp, na.rm=TRUE), slopeTemp),
         flatTemp = ifelse(is.na(flatTemp), mean(flatTemp, na.rm=TRUE), flatTemp),
         month = month(dateTimeAEST))%>%
  ungroup%>%
  dplyr::select(-c(hour, totalHI, totalOTI, rDT))

# Persist so 8.colonyDistances.R / 8.forgKDE.R can pick this up (previously
# computed in-memory only, relying on an already-saved copy being present)
saveRDS(all_joined, "./src_outputs/finalTemp.rds")

# skim(all_joined)







  

