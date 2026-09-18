## TIDE HEIGHTS (BOM)

library(tidyverse)
library(bayesbio)
library(lubridate)
library(sf)
library(skimr)
library(suncalc)

setwd("~/Library/CloudStorage/OneDrive-JamesCookUniversity/Projects/THESIS_PHD_NLJ_2024/CH5_PREForaging")


fHI<-read.csv("./data/BOM_TIDE/tides_HI.csv")
names(fHI)<-c("dateTime", "HeronTide")
  
fOTI<-read.csv("./data/BOM_TIDE/tides_OTI.csv")
names(fOTI)<-c("dateTime", "OneTreeTide")


tides<-left_join(fHI, fOTI)%>%
  as.data.frame%>%
  mutate(dateTimeTide = ymd_hms(dateTime,tz = "Australia/Brisbane"),
         date=as.character(date(dateTimeTide)))%>%
  dplyr::select(-dateTime)%>%
  dplyr::arrange(HeronTide)%>%
  dplyr::group_by(date)%>%
  dplyr::mutate(tideRankHI = 1:n(),
                totalHI = n())%>%
  dplyr::arrange(OneTreeTide)%>%
  dplyr::group_by(date)%>%
  dplyr::mutate(tideRankOTI = 1:n(),
                totalOTI = n())%>%
  ungroup%>%
  dplyr::arrange(dateTimeTide)%>%
  dplyr::mutate(
    TideCatHI = case_when(
      tideRankHI <= 6 ~ "Low Tide",
      tideRankHI >= (totalHI - 6) ~ "High Tide",
      TRUE ~ "Mid Tide"
    ),
    TideCatOTI = case_when(
      tideRankOTI <= 6 ~ "Low Tide",
      tideRankOTI >= (totalOTI - 6) ~ "High Tide",
      TRUE ~ "Mid Tide"
    )
  )

  
skim(tides)

depthPoints<-readRDS("./src_outputs/finalDepth.rds")%>%
  mutate(# round to 30 mins for tide
         rDT=round_date(dateTimeAEST, "30 minutes"),
         #rDT = as.POSIXct(rDT, format="%Y-%m-%d %H:%M:%S"),
         x = unlist(map(.$geometry,1)),
         y = unlist(map(.$geometry,2)))%>%
  st_drop_geometry()


moonPhase<-getMoonIllumination(date=depthPoints$dateTimeUTC)%>%
  mutate(moonPhase=phase, dateTimeUTC=date,
         neapSpring = case_when(
           moonPhase < 0.125 | moonPhase > 0.375 & moonPhase < 0.625 | moonPhase > 0.875  ~ "Spring Tide",  # New Moon & Full Moon
           moonPhase >= 0.125 & moonPhase <= 0.375 | moonPhase >= 0.625 & moonPhase <= 0.875 ~ "Neap Tide",  # First & Last Quarter
           TRUE ~ NA_character_))%>%
  dplyr::select(dateTimeUTC,neapSpring, moonPhase)%>%
  distinct()

tidepts<-left_join(depthPoints, tides, by=join_by("rDT"=="dateTimeTide"))%>%
  left_join(., moonPhase)%>%
  mutate(nearestTide = ifelse(LOCBANDED=="HER", HeronTide, OneTreeTide),
         nearestTideCat = ifelse(LOCBANDED=="HER", TideCatHI, TideCatOTI),
         nearestTideRank = ifelse(LOCBANDED=="HER", tideRankHI, tideRankOTI))%>%
  # Tide assumed as distance from low tide mark (where depth measurement made to)
  mutate(actualDepth = (nearestTide-(finalElevation))*-1,
         date=date(dateTimeAEST),
         TOD = hour(dateTimeAEST))%>%
  dplyr::select(-c(TideCatHI, TideCatOTI, tideRankHI, tideRankOTI,HeronTide, OneTreeTide))

# Persist so 5.LightDark.R can pick this up (previously computed in-memory only,
# relying on an already-saved copy of this exact object being present)
saveRDS(tidepts, "./src_outputs/tidesFinal.rds")





