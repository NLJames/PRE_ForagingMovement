

library(tidyverse)
library(readxl)
library(janitor)
library(skimr)

setwd("~/Library/CloudStorage/OneDrive-JamesCookUniversity/Projects/THESIS_PHD_NLJ_2024/CH5_PREForaging/")

# Import and clean - Heron Island -----------------------------------------
options(digits = 15)


gfiles <- list.files("./data/egretTracks/processed_Argos_04032025", pattern = "g.txt$", recursive = TRUE, full.names = TRUE)
gindivs <- lapply(gfiles, function(f) {
  #f<-gfiles[[1]]
  read.delim(f, header = TRUE, sep = "\t", stringsAsFactors = FALSE, fileEncoding = "ASCII")
})

gdf<-do.call('rbind', gindivs)%>%
  clean_names()



ID<-str_extract(gfiles, "(?<=/)(\\d{5})(?=/)")

for (i in 1:length(gindivs)){
  gindivs[[i]]$trackerID<-ID[[i]]
}

# All times are in UTC from the text file
# GPS Datum: WGS-84
dfind<-do.call('rbind', gindivs)%>%
  mutate_all(trimws) %>%
  dplyr::rename(y='Latitude.N.', x='Longitude.E.', altitude='Altitude.m.', dateTimeUTC='Date.Time') %>%
  mutate(x=as.numeric(as.character(x)), y=as.numeric(as.character(y)),
         dateTimeAEST=ymd_hm(dateTimeUTC, tz="UTC")%>%with_tz(., "Australia/Brisbane")) %>%
  filter(., altitude!="no fix", y!=0, x>149, x<152.05)

head(dfind)
unique(dfind$altitude)

# Diagnostic QC plot only - requires `gbrFeat` (an sf object of GBR features)
# to already be loaded in the session (e.g. from CH5_PRE_SI_Tables.Rmd's setup
# chunk). Commented out so this script can run standalone without erroring.
# ggplot()+
#   geom_sf(data=gbrFeat)+
#   geom_point(data=dfind, aes(x=x,y=y))+
#   ylim(c(-25,-23.17))+
#   xlim(c(150,152.5))



egSex<-read_excel("./data/SexingData/egretFeatherBlood.xlsx", sheet = "EgretsFeatherAndBlood", trim_ws = TRUE)%>%
  dplyr::select(BandNumber, SEX=Sex) 

PRE_ID<-read_excel("./data/DFs/PRE_ID.xlsx", trim_ws = TRUE)

PRE_IDsex<-left_join(PRE_ID, egSex)%>%
  dplyr::rename(LOCBANDED=location, MORPH=morph)

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

# Note that one tracker was re-used (84961) by birds:
# 101-18502 (started 1-12-2019 23:45, stopped 21:47 14-12-2020) and 
# 101-18518 (started 21:44 16-12-2020)

# BandNumber 101-18501 is from Lizard Island

chgTrkr <- dfind %>%
  dplyr::filter(trackerID == "84961") %>%
  dplyr::mutate(
    BandNumber = dplyr::case_when(
      dateTimeUTC < ymd_hms("2020-12-14 21:47:00") ~ "101-18502",
      dateTimeUTC > ymd_hms("2020-12-16 21:44:00") ~ "101-18518",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::left_join(bNum) %>%
  dplyr::filter(!is.na(BandNumber))
  

pre<-dfind%>%
  filter(trackerID!="84961") %>%
  left_join(., bNum) %>%
  filter(dateTimeAEST>dateTimeAESTBanded) %>%
  rbind(., chgTrkr) %>%
  filter(dateTimeAEST>ymd("1970-01-03")) %>%
  mutate(x=as.numeric(x), y=as.numeric(y),
         dateTimeUTC = ymd_hm(dateTimeUTC))%>%
  filter(x!=0, y<(-20))%>%
  dplyr::select(dateTimeUTC:x,trackerID:MORPH)

# Persist so 2.1combineTracks.R can pick this up (previously computed
# in-memory only, relying on an already-saved copy of this exact object
# being present)
saveRDS(pre, "./src_outputs/preClean_HI.rds")


