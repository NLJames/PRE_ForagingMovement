

library(raster)
library(sf)
library(skimr)
library(terra)
library(tidyverse)


setwd("~/Library/CloudStorage/OneDrive-JamesCookUniversity/Projects/THESIS_PHD_NLJ_2024/CH5_PREForaging")


# Get depth values at 30m resolution raster
trackPoints<-readRDS("./src_outputs/cleanTracks.rds")
table(trackPoints$BandNumber)
table(trackPoints$BandNumber)|>mean()
table(trackPoints$BandNumber)|>sd()

tracks_sf<-st_as_sf(trackPoints, coords = c(x='x', y='y'), crs=4326)

e<-extent(min(trackPoints$x) -0.5, max(trackPoints$x) +0.5, min(trackPoints$y) -0.5, max(trackPoints$y) +0.5)

# This is a composite depth at 10m res from the ACA units are in CM
# https://allencoralatlas.org/methods/
# demACA<-raster("./data/ACA_HERON/Bathymetry_compositeDepth/bathymetry_0.tif")%>%
#   crop(., e)
# demHI<-rast("./data/govtDEM/QLD Government/DEM/1 Metre/HeronIsland_2009_Is_SW_388000_7406000_2K_DEM_1m.tif")%>%
#   project(., "EPSG:4326")
# demHI<-rast("./data/govtDEM/QLD Government/DEM/1 Metre/HeronIsland_2009_Is_SW_388000_7406000_2K_DEM_1m.tif")%>%
#   project(., "EPSG:4326")
# demOTI<-rast("./data/govtDEM/QLD Government/DEM/1 Metre/OneTreeIsland_2009_Is_SW_406000_7400000_2K_DEM_1m.tif")%>%
#   project(., "EPSG:4326")
# demInter2022<-rast("./data/govtDEM/Digital Earth Australia/DEM/10 Metre Intertidal/2022 Digital Earth Australia Intertidal Digital Elevation Model 10m.tif")%>%
#   project(., "EPSG:4326")
# demGBR2020<-rast("./data/GBR_DEM30m/Great_Barrier_Reef_D_2020_30m_MSL_cog.tif")%>%
#   project(., "EPSG:4326")

# grab the already transformed files to make faster
demACA<-rast("./data/demTransformed/demACA_4326.tif")
demInter2022<-rast("./data/demTransformed/demInter2022_4326.tif")
demGBR2020<-rast("./data/demTransformed/demGBR2020_4326.tif")
demOTI<-rast("./data/demTransformed/demOTI_4326.tif")
demHI<-rast("./data/demTransformed/demHI_4326.tif")

# Extract longitude (x) from the sf geometry column
extrDEM <- tracks_sf %>%
  mutate(
    year=year(dateTimeAEST),
    x = st_coordinates(.)[,1],
    y = st_coordinates(.)[,2],# Extract longitude
    elevACA = (raster::extract(demACA, .)%>%.[,2]/100)*-1, 
    elevHI = raster::extract(demHI, .)%>%.[,2],
    elevOTI = raster::extract(demOTI, .)%>%.[,2],
    elevInter2022 = raster::extract(demInter2022, .)%>%.[,2],
    elevGRB2020 = raster::extract(demGBR2020, .)%>%.[,2],
    finalElevation = coalesce(elevACA,elevHI, elevOTI, elevInter2022,elevGRB2020))%>%
  dplyr::select(BandNumber, dateTimeUTC,dateTimeAEST, year, SEX, LOCBANDED, MORPH, finalElevation, x,y,geometry)

# Persist so 4.tides.R can pick this up (previously computed in-memory only,
# relying on an already-saved copy of this exact object being present)
saveRDS(extrDEM, "./src_outputs/finalDepth.rds")




        

