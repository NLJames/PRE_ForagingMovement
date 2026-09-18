# Import and clean PRE data

library(readxl)
library(openxlsx)
library(tidyverse)
library(rgdal)
library(rgeos)
library(viridis)
library(raster)
library(amt)
library(purrr)
library(adehabitatHR)
library(Polychrome)
library(lubridate)
library(dataaimsr)
#library(gisaimsr) # for reef layers
library(sf)

# consider time spent foraging (on the reef versus time spent on island or high points)
#' does this correlate with time of day or tide height?
#' Calulate the weekly or monthly foraging area per individual
#' Calculate the distance travelled (weekly/monthly) - these should both vary with prey availability
#' Theory states time spent searching should decrease with increased prey abundance [@HollingBuckingham]
#' note that prey consumption will increase to the point where other factors such as digestion limit consumption
#' Predator density will also play a role in how much effort a forager exerts
#' diversity of prey may contribute
#' hunger can also contribute to the rate of prey consumption
#' there must be certain types of habitat that allow them to catch fish easier - maybe rocks for little pools?
rm(list = ls())

#' Notes on fix data 
#' https://d9-wret.s3.us-west-2.amazonaws.com/assets/palladium/production/s3fs-public/atoms/files/MTI_Solar_Argos-GPS_PTT-100_Manual.pdf
#' 
#' failed fixes give no lat/lon altitude='no fix', 'batt drain', 'low volt'
#' in engineering data (with 'e') 
#' activity indicates bird is moving when fix was recorded
#' GPS fix time = time to get fix
#' satellite count = number of satelites for fix
#' 
#' 
#' 

# get capture data
# recap trackerID 84966 23:00 
PRE_ID <- read_excel("data/DFs/PRE_ID.xlsx")




files<-list.files("./data/egretTracks/Argos_Processed/", pattern="g.txt$", recursive = T, full.names = T)
indivs<-lapply(files, read_delim)
  
  

head(indivs[[1]], 10)
names(indivs[[1]])

files2<-list.files("./data/egretTracks/Argos_Processed/", pattern="e.txt$", recursive = T, full.names = T)
ed<-lapply(files2,  read.csv2, header=T, sep = "\t",fileEncoding = "Latin1", check.names = F)
head(ed)
names(ed[[1]])

ID<-substr(files, 37, 41)

names(indivs) = ID
names(ed) = ID


for (i in 1:length(indivs)){
  
  indivs[[i]]$ID<-ID[[i]]
  ed[[i]]$ID<-ID[[i]]
  
}

names(ed[[1]])
names(indivs[[1]])

# tracker ID 84966 was reused
dfind<-do.call('rbind', indivs)%>%
  mutate(across(where(is.character), str_trim))%>%
  rename(y="Latitude(N)", x="Longitude(E)") %>%
  filter( x>147)

# Engineer pts appear to be wacko!!!
dfed<-do.call('rbind', ed[1:10])%>%
  rename(y="Latest Latitude(N)", x="Latest Longitude(E)") %>%
  mutate(across(where(is.character), str_trim),
         y=as.numeric(y), x=as.numeric(x))%>%
  filter( y>(-40), x>120, x<170)

ggplot(dfed, aes(x=x,y=y))+
  geom_point()
  
range(dfind$y);range(dfind$x)

sfind<-st_as_sf(dfind, coords=c(x='x', y='y')) 
sfed<-st_as_sf(dfed, coords=c(x='x', y='y')) 

ggplot(sfind)+
  geom_sf() 

ggplot(sfed)+
  geom_sf()

# save a shp for the points
st_write(sfind, paste0("./data/DFs/egTracks.shp"), driver = "ESRI Shapefile", delete_layer = T)

# how many points from each individual
table(dfind$ID)

feats<-st_read("~/Library/CloudStorage/OneDrive-JamesCookUniversity/Projects/CH2_BNForaging/data/GBR_FEATURES/SiteFeatures/allFeatures.shp") %>%
  st_transform(4326)

e <- extent(min(dfind$x)-0.1,max(dfind$x)+0.1, min(dfind$y)-0.1,  max(dfind$y)+0.1)

dem<-raster("/Users/jc754314/Library/CloudStorage/OneDrive-JamesCookUniversity/Projects/CH5_PREForaging/data/GBR_DEM30m/Great_Barrier_Reef_D_2020_30m_MSL_cog.tif")%>%
  crop(., e)

plot(dem)

points(dfind$x, dfind$y)


ggplot(data=dfdem, aes(y=y, x=x)) +
  geom_raster(aes(fill=values)) +
  geom_point(data=dfind, aes(x=x, y=y, col=ID), size=0.1) +
  theme_bw() +
  coord_equal() +
  scale_fill_gradient("Depth (mm/yr)", limits=c(-3946.685, 959.9044)) +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        legend.key = element_blank()
  )


ggplot() +
  geom_raster(data=dfdem, aes(x=x, y=y, fill=values)) +
  geom_sf(data = feats, mapping = aes(fill = FEAT_NAME), lwd = 0.01) +
  geom_point(data=dfind, aes(x=x, y=y, col=ID), size=0.1) +
  #scale_fill_manual(values=c('brown', 'brown', 'brown', 'brown', 'brown')) +
  scale_fill_viridis_c() +
  theme_classic() +
  coord_sf()




ggplot(dem) + 
  geom_tile(aes(x, y, fill = value)) +
  facet_wrap(~ variable) +
  scale_fill_gradientn(colours = rev(terrain.colors(225))) +
  coord_equal()
  
  

# Can't trust this as there are are still duplicate matches happening
edid<-left_join(dfind, dfed, by=join_by(Date.Time==Tx.Date.Time, ID==ID))%>%
  filter(Lat1.N.<(-22.5))







sf::sf_use_s2(FALSE)

ft<-st_read("./data/AIMS_GIS/Great_Barrier_Reef_Features.shp")%>%
  st_crop(., c(xmin=150.583, xmax=152.566, ymin=-24.084, ymax=-22.375))# %>%
 # st_transform(32756) %>%
#  st_write(., "./data/GBR_FEATURES/SiteFeatures/allFeatures.shp", delete_dsn = TRUE)

sf::sf_use_s2(TRUE)

## Still cant find a way of identifying fix accuracy/reliability - awaiting response from email

ggplot() +
  geom_sf(data=geo, aes(fill = class)) + guides(fill = guide_none()) +
  geom_sf(data=ft, aes(fill=FEAT_NAME)) +
  geom_point(data=edid, aes(y=Lat1.N., x=Long1.E., col=(Fix))) 



















trackerID<-unlist(files)%>%
  strsplit(., "/") %>% 
  do.call('rbind',.)%>%
  data.frame%>%
  dplyr::select(tID=X5)%>%
  .$tID


indivsID<-mapply(function(x,y){
  x%>%
    dplyr::filter(., Altitude.m.!="no fix")%>%
    dplyr::filter(., Latitude.N.!=0)%>%
    dplyr::filter(., Latitude.N.!=0)%>%
    dplyr::filter(., Latitude.N.> -40 , Longitude.E.> 0)%>%
    dplyr::mutate(Altitude.m. = ifelse(Altitude.m.=="2D fix" | Altitude.m.=="neg alt"|
                                         Altitude.m.=="low volt"|Altitude.m.=="batt drain [1]"|
                                         Altitude.m.=="batt drain [2]"|Altitude.m.=="batt drain [3]"|
                                         Altitude.m.=="batt drain [4]"
                                       , "NA", Altitude.m.))%>%
    dplyr::mutate(trackerID=y)%>%
    dplyr::mutate(trackerID=as.factor(trackerID))%>%
    # mutate(Date.Time= as.POSIXct(Date.Time))%>%
    dplyr::rename(y=Latitude.N.
                  ,x=Longitude.E.)%>%
    dplyr::select(c(x,y, trackerID, Date.Time))%>%
    mutate(Date.Time = ymd_hm(Date.Time))
  
}, x = indivs,  y = trackerID, SIMPLIFY = FALSE )

# Add bandNumbers

# Note that one tracker was re-used (84961) by birds:
# 101-18502 (started 1-12-2019 23:45, stopped 21:47 14-12-2020) and 
# 101-18518 (started 21:44 16-12-2020)

# BandNumber 101-18501 is from Lizard Island


bNum<-read_excel("./data/PRE_ID.xlsx", trim_ws = TRUE)%>%
  dplyr::select(PTTID, BandNumber, DateBanded, TimeBanded) %>%
  rename(trackerID = PTTID) %>%
  mutate(trackerID = as.factor(trackerID),
         TimeBanded = as.character(TimeBanded),
         TimeBanded = trimws(str_extract(TimeBanded, " .*")),
         TimeBanded = ymd_hms(paste(DateBanded,TimeBanded))) %>%
  dplyr::select(trackerID, BandNumber, TimeBanded)

# Issue here, can't manage to split the date correctly for the tracker that was changed over
# current code is not correct!!
pre<-do.call(rbind, indivsID)%>%
  mutate(chgdt = ifelse(Date.Time<ymd_hms("2020-12-14 21:47:00"), "101-18502", NA),
         chgdt = ifelse(Date.Time>ymd_hms("2020-12-16 21:44:00"), "101-18518", chgdt)) %>%
  left_join(., bNum) %>%
  filter(BandNumber!="101-18501") %>%
  filter(Date.Time > ymd_hms("2019-04-12 08:52:00")) %>%
  mutate(chgdt = ifelse(Date.Time>ymd_hms("2020-12-14 21:47:00") && chgdt=="101-18502", NA, chgdt),
         BandNumber = ifelse(trackerID==84961, chgdt, BandNumber))



sf::sf_use_s2(FALSE)
features<- st_crop(gbr_feat, c(xmin=151.8, xmax=152.2, ymin=-23, ymax=-23.6))
sf::sf_use_s2(TRUE)


ggplot(data=pre) +
  geom_sf(data=features) +
  geom_point(aes(x=x,y=y)) +
  ylim(c(-23.6, -23.35))
  

ggplot(data=pre) +
  geom_histogram(aes(x=Date.Time)) +
  facet_wrap(vars(BandNumber), ncol = 1)

ggplot(data=pre) +
  geom_histogram(aes(x=Date.Time)) +
  facet_wrap(vars(trackerID), ncol = 1)



data.xy = chk[c("x","y")]
#Creates class Spatial Points for all locations
xysp <- SpatialPoints(data.xy)

proj4string(xysp) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84")

#Creates a Spatial Data Frame from all locations
sppt<-data.frame(xysp)

#Creates a spatial data frame of ID
idsp<-data.frame(chk[["id"]])

#Merges ID data frame with GPS locations data frame
#Data frame is called "idsp" comparable to the "relocs" from puechabon dataset
coordinates(idsp)<-sppt

#First we need to create utilization distributions for each panther
p14<-createPalette(14, c("#c0c0c0", "#00ff00","#af0087")) # For 9 colours
swatch(p14)

ud <- kernelUD(idsp[,1], grid = 300)
image(ud)

ver<-getverticeshr(ud, 95)

projExtent <- extent(151.83,152, -23.5,-23.42)
heron.is<-extent(151.911329, 151.918836,-23.442713, -23.441252)
wistari.rf<-extent(151.843950, 151.903885, -23.495931, -23.449615)
heron.rf<-extent(151.902486, 151.996545, -23.471942, -23.424542)

plot(projExtent)
plot(ver, col=alpha(p9, 0.5), add=T)
plot(projExtent, add=T)
plot(heron.is, add=T)
plot(wistari.rf, add=T)
plot(heron.rf, add=T)


#HR overlap
OL<-kerneloverlaphr(ud, method="UDOI")

round(OL, digits = 3)










#


%>%
  SpatialPointsDataFrame(coords = cbind(.$x, .$y)
                         , data = .,proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))


ltraj->BRB->estUDm->overlap

as.ltraj(chk, xy=cbind(chk$x, chk$y), date=Date.Time, id=id)

UDs<-kernelUD(chk)


?kernelUD

UDs<-lapply(indivsID, function(x){
  kernelUD(x)
  
})


projExtent <- extent(151.83,152, -23.5,-23.42)
plot(projExtent)

lapply(UDs, function(x){
  plot(x, add=T)
  
})
plot(UDs[[1]], add=T)

warnings()

kernelUD

kerneloverlaphr(UDs, method="UDOI", percent=95)




class(UDs)


class(UDs)



dat<-do.call(rbind, indivsID)%>%
  make_track(x, y, Date.Time, id=ID, crs= CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))


dat1 <- dat %>%
  nest(data = -c(id))

dat1
dat1$data[[1]]

hr1 <- dat1 %>%
  dplyr::mutate(
    hr_mcp = map(data, hr_mcp),
    hr_kde = map(data, hr_kde),
    #hr_locoh = map(data, ~ hr_locoh(., n = ceiling(sqrt(nrow(.))))),
    hr_akde_iid = map(data, ~ hr_akde(., fit_ctmm(., "iid"))),
    #hr_akde_ou = map(data, ~ hr_akde(., fit_ctmm(., "ou")))
  )





#
























##Make amt track
trks<-do.call(rbind, indivsID)%>%
  make_track(x, y, Date.Time, id=ID, crs= CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))

#Divide track into different birds
trk.lst<-trks%>%
  nest(data=c(x_, y_, t_))

#Check for duplicate date times for each individual
lapply(trk.lst$data, function(x){
  #x%>%select("x_")%>%duplicated(.)%>%unique(.)%>%print(.)
  x%>%select("y_")%>%duplicated(.)%>%unique(.)%>%print(.)
  #x%>%select("t_")%>%duplicated(.)%>%unique(.)%>%print(.)
})

hr1<-lapply(trk.lst$data, function(x){
  hr_mcp=hr_mcp(x)
  hr_kde=hr_kde(x)
  hr_locoh=hr_locoh(x, n = ceiling(sqrt(nrow(x))))
})



hr1 <- trk.lst %>%
  mutate(
    hr_mcp = map(data, hr_mcp),
    hr_kde = map(data, hr_kde),
    hr_locoh = map(data, ~ hr_locoh(., n = ceiling(sqrt(nrow(.))))),
    hr_akde_iid = map(data, ~ hr_akde(., fit_ctmm(., "iid"))),
    hr_akde_ou = map(data, ~ hr_akde(., fit_ctmm(., "ou")))
  )






#par(mfrow = c(1, 1))

colours<-list("#003f5c", "#2f4b7c", "#665191", "#a05195","#d45087"
                       , "#f95d6a","#ff6c43","#ffa600","#00e4b8")

projExtent <- extent(151.83,152, -23.5,-23.42)
plot(projExtent, add=T)

# Overlay the points for each indiv
mapply(function(x,y){
  plot(x, add=T, col=y)
  
}, x = chk,  y = colours, SIMPLIFY = FALSE )

# Overlay the MCP for each indiv
hullz<-lapply(chk, function(x){
  gConvexHull(x)
})

mapply(function(x,y){
  plot(x, add=T, col=scales::alpha(y, 0.2),border=y)
  
}, x = hullz,  y = colours, SIMPLIFY = FALSE )

##Further ideas
# what is the proportion of overlap for each home range with another individual?
# Does division of the months show a different story for overlap?
# Does finding the core area of the UD tell a different story for overlap?
