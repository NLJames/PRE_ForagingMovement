rm(list = ls())

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

setwd('/Users/jc754314/OneDrive - James Cook University/Projects/Chapter 2 - Foraging Strategy/Data/TracksEgret/Processed2');
files<-list.files(pattern="g.txt$", recursive = T)
indivs<-lapply(files, read.delim)
head(indivs)

lapply(indivs, function(x){
  #unique(x$Latitude.N.)
  #unique(x$Longitude.E.)
  unique(x$Altitude.m.)
  #unique(x$Speed)
  #unique(x$Course)
  #unique(x$Date.Time)
})

# Cleaning up the fixes, we NA all weird altitude values
# Remove rows with 'no fix', 0 lat or 0 long
# bird 4 seems to be in totally other location that the rest? Not on heron??
b1<-indivs[[1]]
b4<-indivs[[4]]
indivs[[1]]<-NULL
indivs[[3]]<-NULL #Change to 3 because the first is deleted

indivsID<-mapply(function(x,y){
  x%>%
    dplyr::filter(., Altitude.m.!="no fix")%>%
    dplyr::filter(., Latitude.N.!=0)%>%
    dplyr::filter(., Latitude.N.!=0)%>%
    dplyr::filter(., Latitude.N.< -19.4 & Longitude.E.>147)%>%
  dplyr::mutate(Altitude.m. = ifelse(Altitude.m.=="2D fix" | Altitude.m.=="neg alt"|
                                Altitude.m.=="low volt"|Altitude.m.=="batt drain [1]"|
                                Altitude.m.=="batt drain [2]"|Altitude.m.=="batt drain [3]"|
                                Altitude.m.=="batt drain [4]"
                              , "NA", Altitude.m.))%>%
    dplyr::mutate(id=y)%>%
    dplyr::mutate(id=as.factor(id))%>%
   # mutate(Date.Time= as.POSIXct(Date.Time))%>%
    dplyr::rename(y=Latitude.N.
           ,x=Longitude.E.)%>%
    dplyr::select(c(x,y, id))
    #filter(x!=y)

  #SpatialPointsDataFrame(coords = cbind(.$x, .$y)
   #                      , data = .,proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))

}, x = indivs,  y = 1:length(indivs), SIMPLIFY = FALSE )


chk<-do.call(rbind, indivsID)
chk

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
