library(tidyverse)
library(readxl)
library(skimr)

setwd("~/Library/CloudStorage/OneDrive-JamesCookUniversity/Projects/THESIS_PHD_NLJ_2024/CH5_PREForaging/")


# Import and clean - Heron Island -----------------------------------------

oti_files <- list.files("./data/egretTracks/OTI", pattern = "GPS.csv$", recursive = TRUE, full.names = TRUE)
oti_indivs <- lapply(oti_files, function(f) {
  read.csv(f)
})

oti_ID<-str_extract(oti_files, "(?<=OTI/)[0-9]+")

for (i in 1:length(oti_indivs)){
  oti_indivs[[i]]$trackerID<-oti_ID[[i]]
}

# All times are in UTC from the text file
# GPS Datum: WGS-84


oti_dfind<-do.call('rbind', oti_indivs)%>%
  mutate_all(trimws) %>%
  dplyr::rename(y='Latitude', x='Longitude', altitude='Alt.m.') %>%
  unite("dateTimeUTC", c("Date","Time"), sep = " ",remove = TRUE )%>%
  mutate(x=as.numeric(x), y=as.numeric(y),
         dateTimeUTC = lubridate::parse_date_time(dateTimeUTC, orders = c("dmy HMS", "dmY HMS", "ymd HMS"), tz = "UTC"),
         dateTimeAEST = lubridate::with_tz(dateTimeUTC, "Australia/Brisbane")) %>%
  dplyr::filter(#CRC!="Fail",
                dateTimeAEST<ymd_hms("2022-01-31 18:21:20"),	
                dateTimeAEST>ymd_hms("2020-11-18 18:36:16"),
                y>(-23.52),
                y<(-23.4),
                x<152.11,
                x>152)


# Diagnostic QC plot only - requires `gbr_feat` (an sf object of GBR features)
# to already be loaded in the session. Commented out so this script can run
# standalone without erroring.
# ggplot()+
#   geom_sf(data=gbr_feat)+
#   geom_point(data=oti_dfind, aes(x=x,y=y,col=CRC))+
#   ylim(c(-24,-23.17))+
#   xlim(c(151.5,152.5))+
#   facet_grid(~CRC)

# ARGOS PTT codes from OTI to be converted
# 54543 => 84972
# 54544 => 84973
# 54545 => 84974
# 54546 => 84975
# 54547 => 84976

egSex<-read_excel("./data/SexingData/egretFeatherBlood.xlsx", sheet = "EgretsFeatherAndBlood", trim_ws = TRUE)%>%
  dplyr::select(BandNumber, SEX=Sex) 

PRE_ID<-read_excel("./data/DFs/PRE_ID.xlsx", trim_ws = TRUE)

PRE_IDsex<-left_join(PRE_ID, egSex)%>%
  dplyr::rename(LOCBANDED=location, MORPH=morph)%>%
  mutate(
    PTTID = case_when(
      PTTID == 54543 ~ 84972,
      PTTID == 54544 ~ 84973,
      PTTID == 54545 ~ 84974,
      PTTID == 54546 ~ 84975,
      PTTID == 54547 ~ 84976,
      TRUE ~ PTTID  # Keep other values unchanged
    )
  )
  

bNum<-PRE_IDsex%>%
  dplyr::select(PTTID, BandNumber, DateBanded, TimeBanded, SEX, LOCBANDED, MORPH) %>%
  dplyr::rename(trackerID = PTTID) %>%
  mutate(trackerID = as.factor(trackerID),
         TimeBanded = as.character(format(TimeBanded, "%H:%M:%S")),
         DateBanded = as.character(DateBanded),
         dateTimeAESTBanded = ymd_hms(paste0(DateBanded," ", TimeBanded)),
         BandNumber=as.factor(BandNumber)) %>%
  dplyr::select(trackerID, BandNumber, dateTimeAESTBanded, SEX, LOCBANDED, MORPH)

skim(bNum)


oti_pre<-oti_dfind%>%
  left_join(., bNum) %>%
  filter(dateTimeAEST>dateTimeAESTBanded) %>%
  mutate(x=as.numeric(x), y=as.numeric(y))%>%
  dplyr::select(dateTimeUTC:x,trackerID:MORPH)

# Persist so 2.1combineTracks.R can pick this up (previously computed
# in-memory only, relying on an already-saved copy of this exact object
# being present)
saveRDS(oti_pre, "./src_outputs/preClean_OTI.rds")



