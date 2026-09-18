# Get ACA substrate and other categories for each fix

library(tidyverse)
library(geojsonsf)
library(ggplot2)
library(tictoc)
library(parallel)
library(doParallel)
library("rmapshaper")
library(sf)

setwd("~/Library/CloudStorage/OneDrive-JamesCookUniversity/Projects/THESIS_PHD_NLJ_2024/CH5_PREForaging")


lightPoints<-readRDS("./src_outputs/lightFinal.rds")%>%
  mutate(lat = y, lon = x)%>%
  st_as_sf(coords = c(x='x', y='y'), crs=4326)


isBuff100<-st_read("./data/GBR_FEATURES/egFeats.shp", quiet=TRUE)%>%
  dplyr::filter(FEAT_NAME %in% c("Island","Cay"))%>%
  dplyr::select(GBR_NAME, FEAT_NAME, geometry)%>%
  st_buffer(100)%>%
  st_transform(4326)%>%
  st_make_valid() %>%
  st_crop(., lightPoints) %>%
  st_make_valid() 

# needs a minute...
# reducing this as much as possible to reduce processing time when
# extracting data from intersection
benth <- geojson_sf("./data/ACA_HERON/Benthic-Map/benthic.geojson")%>%
  st_make_valid()%>%
  ms_simplify() %>%
  st_make_valid() %>%
  st_crop(., lightPoints) %>%
  st_make_valid() 



geo <- geojson_sf("./data/ACA_HERON/Geomorphic-Map/geomorphic.geojson")%>%
  st_make_valid()%>%
  ms_simplify() %>%
  st_make_valid() %>%
  st_crop(., lightPoints) %>%
  st_make_valid()


islandDat <-st_join(lightPoints, isBuff100)%>%dplyr::rename(featType=FEAT_NAME, islandName = GBR_NAME )

benthDat <- st_join(islandDat, benth)%>%dplyr::rename(benthicCover=class)%>%
  mutate(benthicCover = ifelse(is.na(benthicCover), featType, benthicCover))

geodat <-st_join(benthDat, geo)%>%dplyr::rename(geomorphicCover=class)%>%
  mutate(geomorphicCover = ifelse(is.na(geomorphicCover), featType, geomorphicCover))

table(geodat$islandName)
table(geodat$benthicCover)
table(geodat$geomorphicCover)

# Persist so 7.waterTemps.R can pick this up (previously computed in-memory only,
# relying on an already-saved copy of this exact object being present)
saveRDS(geodat, "./data/DFs/benthosFinal.rds")

# Take points near Heron and OTI that are NA and class them as on island









