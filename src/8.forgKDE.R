
library(amt)
library(ggplot2)
library(tidygraph)
library(ggraph)
library(sf)
library(parallel) # needed for mclapply (parallel)
library(rrapply)

# Data setup --------------------------------------------------------------


# get tracking data - still contains all states
tempPoints<-readRDS("./src_outputs/finalTemp.rds")%>%
  mutate(MonYr=substr(dateTimeAEST, 1, 7),
         x=lon, y=lat,
         week = floor_date(dateTimeAEST, "week")) %>%
  dplyr::select(BandNumber, MonYr, week, dateTimeAEST, x, y, lon, lat) %>%
  distinct(BandNumber, week, MonYr, dateTimeAEST, x, y, .keep_all = TRUE) %>%
  st_as_sf(., coords = c('x', 'y'), crs = st_crs(4326))%>%
  # OK - amt coords need to be in UTM format for other functions
  st_transform(32756) %>%
  mutate(UTMx = unlist(map(.$geometry,1)),
         UTMy = unlist(map(.$geometry,2)))  %>%
  st_drop_geometry() %>%
  droplevels()


table(tempPoints$MonYr)
table(tempPoints$week)

# Find months with fewer than 2 points per individual
# otherwise it causes issues when nesting by bID
keepers<-tempPoints %>%
  group_by(BandNumber, week) %>%
  dplyr::summarise(n=n()) %>%
  filter(n>2) %>%
  distinct(week) %>%
  .$week

# Filter out >2  point bouts
# then, create tracks to use in function below
filtTempPoints<- tempPoints %>%
  dplyr::filter(week %in% keepers) %>%
  mutate(ID=BandNumber, Latitude=lat, Longitude=lon, DateTime=dateTimeAEST)

# Make list of all BandNumbers to run iteratively in function below
ls.bn<-distinct(filtTempPoints, BandNumber)%>%
  .$BandNumber


library(track2KBA)

colony <- filtTempPoints%>%
  group_by(BandNumber)%>%
  dplyr::summarise(Latitude = as.numeric(names(which.max(table(lat)))),
                   Longitude = as.numeric(names(which.max(table(lon)))))%>%
  dplyr::rename(ID=BandNumber)

trips <- tripSplit(filtTempPoints,
                   nests=TRUE,
                   colony=colony,
                   innerBuff=0.1,
                   returnBuff=0.1,
                   duration=0.05,
                   rmNonTrip = FALSE)

tracks <- projectTracks( dataGroup = trips, projType = 'azim', custom=TRUE )
class(tracks)


tracks <- tracks[tracks$ColDist > 0.5, ] # remove trip start and end points near colony

KDE <- estSpaceUse(
  tracks = tracks, 
  scale = 1, 
  levelUD = 95,
  polyOut = TRUE
)

mapKDE(KDE = KDE$UDPolygons, colony = colony)


## Visualize trips
mapTrips(Trips, colony) # add colony location to each facet
mapTrips(Trips, colony, colorBy = "trip") # color trips by their order


estSpaceUse(trips, scale=1, levelUD=0.95, res = 500, polyOut = TRUE)













cores=detectCores()
cl <- makeCluster(8)

# Export necessary objects and functions
clusterExport(cl, varlist = c("ls.bn", "make_trast", "hr_kde","filtTempPoints")) 

# Load required packages on each worker
clusterEvalQ(cl, {
  library(amt)
  library(tidyverse)
})


# Get KDE for all Locations -----------------------------------------------


ls.kde.raw<-parLapply(cl, ls.bn, function(x){
  
  # Select individual track
  trk<-filter(filtTempPoints, BandNumber==paste0(x))%>%
    make_track(., .x=UTMx, .y=UTMy, .t=dateTimeAEST, id=BandNumber, crs=32756,  all_cols = T)
  
  # Make template raster
  tRast<-make_trast(trk, res=100)

  trk %>% 
    nest(data = -week) %>% 
    mutate(kde = map(data, hr_kde, trast = tRast, levels = c(0.95)))
  
  
})

stopCluster(cl) 


ls.monthlyKDE<-lapply(ls.kde.raw, function(x){
  
  all<-rrapply(x, how='melt')%>%
    filter(L1=="MonYr"|L3=="BandNumber"|L3=="h")%>%
    distinct
  
  my<-filter(all, L1=="MonYr")%>%
    dplyr::select(value)%>%
    .$value%>%unlist
  
  bn<-filter(all, L3=="BandNumber" & L2==1)%>%
    dplyr::select(value)%>%
    .$value%>%.[[1]]%>%.[1]
  
  kde<-all%>%
    filter(L3=="h")%>%
    mutate(MonYr = my, BandNumber=bn, value=unlist(value)[ c(TRUE,FALSE) ])%>%
    dplyr::select(kde=value, MonYr, BandNumber)
  
  return(kde)
  
})

pts<-readRDS("./data/DFs/prePointsPA.rds")%>%
  # use presence only points
  filter(PA==1) %>%
  dplyr::select(BandNumber, SEX, LOCBANDED, MORPH)%>%
  distinct

monthlyKDE<-do.call('rbind',ls.monthlyKDE)%>%
  mutate(MonYr = as.Date(paste(MonYr, "-01", sep="")),
         kde=as.numeric(kde)) %>%
  left_join(.,pts) %>%
  mutate(TIME=as.numeric(MonYr)/1000,
         BandNumber=as.factor(BandNumber),
         SEX=as.factor(SEX))

table(monthlyKDE$MonYr)

# Save
saveRDS(monthlyKDE, "./data/DFs/monthlyAllLocsKDE.RDS")

# From here - monthly KDE, all locs -------------------------------------------------


monthlyKDE<-readRDS("./data/DFs/monthlyAllLocsKDE.RDS")


# Ok, we have a clear pattern between males and females
# Would be interesting to link this to an environmental factor
# at least show a map of these distributions

ggplot() +
  geom_point(data=monthlyKDE, aes(x=MonYr, y=kde, col=SEX)) +
  labs(title=("Pacific Reef Egrets - Monthly 95% KDE per sex"),
       x = "Date", y = "KDE (m^2)") +
  geom_jitter()

ggplot() +
  geom_boxplot(data=monthlyKDE, aes(x=SEX, y=kde, col=SEX), alpha=0) +
  geom_point(data=monthlyKDE, aes(x=SEX, y=kde, col=SEX), position = "jitter") +
  labs(title=("Pacific Reef Egrets - Monthly 95% KDE per sex"),
       x = "Date", y = "KDE (m^2)") 

# We also see there are seasonal peaks during the winter months
# this is expected as per noddy research which indicates there
# might be productivity maximums during the summer hence synch breeding
ggplot() +
  geom_smooth(data=monthlyKDE, aes(x=MonYr, y=kde, col=SEX), method="gam", formula = y~s(x) ) +
  labs(title=("Pacific Reef Egrets - Monthly 95% KDE per sex"),
       x = "Date", y = "KDE (km^2)") +
  geom_jitter()

ggplot() +
  geom_point(data=monthlyKDE, aes(x=MonYr, y=kde, col=MORPH)) +
  labs(title=("Pacific Reef Egrets - Monthly 95% KDE per morph"),
       x = "Date", y = "KDE (m^2)")

library(mgcv)
library(tidyverse)

names(monthlyKDE)


m <- gam(kde ~   s(TIME, bs='fs'),
         family=gaussian, data=monthlyKDE, method = "REML")

summary(m)

plot(m)

# !! Need to find a way to plot the Male Vs Female trend lines
# as group averages rather than each individual.


# Also some evidence that it varies between morphs
ggplot() +
  # geom_smooth(data=monthlyKDE, aes(x=MonYr, y=kde))
  geom_point(data=monthlyKDE, aes(x=MonYr, y=kde, col=MORPH)) +
  labs(title=("Pacific Reef Egrets - Monthly 95% KDE per morph"),
      x = "Date", y = "KDE (km^2)")







# Get data for Reef Locations ----------------------------------------------
# Need to run the first section to set up data - then this


# get tracking data - still contains all states
tripsReef<-readRDS("./data/DFs/prePoints.RDS")%>%
  mutate(MonYr=substr(Date.Time, 1, 7)) %>%
  #filter(state==2) %>%
  dplyr::select(BandNumber, MonYr, Date.Time, x, y) %>%
  distinct(BandNumber, MonYr, Date.Time, x, y) %>%
  st_as_sf(., coords = c('x', 'y'), crs = st_crs(4326))%>%
  # identify whether over reef, cay, island etc...
  st_intersection(., feats) %>%
  filter(FEAT_NAME=="Reef") %>%
  mutate(UTMx = unlist(map(.$geometry,1)),
         UTMy = unlist(map(.$geometry,2)))  %>%
  # OK - amt coords need to be in UTM format for other functions
  st_transform(32756) %>%
  st_drop_geometry() %>%
  #filter(Year==2020) %>%
  left_join(., bIDs) %>%
  droplevels()

saveRDS(tripsReef, "./data/DFs/prePointsReef.rds")

# pts data frame is created later
# it is just the BandNumber and individual data
# such as SEX and MORPH
chkreef<-left_join(tripsReef, pts)

ggplot()+
  geom_point(data=chkreef, aes(x=UTMx, y=UTMy, col=BandNumber)) +
  facet_wrap(~SEX)


# Find months with fewer than 2 points per individual
# otherwise it causes issues when nesting by bID
keepersReef<-tripsReef %>%
  group_by(BandNumber, MonYr) %>%
  dplyr::summarise(n=n()) %>%
  filter(n>2) %>%
  distinct(MonYr) %>%
  .$MonYr

# Filter out >2  point bouts
# then, create tracks to use in function below
trksReef<- tripsReef %>%
  filter(MonYr %in% keepersReef) %>%
  make_track(., .x=UTMx, .y=UTMy, .t=Date.Time, id=BandNumber, crs=32756,  all_cols = T)

# Make list of all BandNumbers to run iteratively in function below
ls.bnReef<-distinct(trksReef, BandNumber)%>%
  .$BandNumber

# Can we add more threads per core...?
# doesn't actually run more processes simultaneously
# library(h2o)
# h2o.init(nthreads = 4)

# Get KDE for Reef Locations ----------------------------------------------

cores=detectCores()
cl <- makeCluster(cores[1]) 
clusterExport(cl, varlist = c("trksReef", "ls.bnReef")) #Export data frames that are used in the code

# Didn't find a faster way than running one for each package that is used in the code
clusterEvalQ(cl, library(amt))
clusterEvalQ(cl, library(tidyverse))


ls.kde.reef.raw<-parLapply(cl, ls.bn, function(x){
  
  # Select individual track
  trkreef<-filter(trksReef, BandNumber==paste0(x))
  # Make template raster
  tRastReef<-make_trast(trkreef, res=50)

# kde for this individual  
  trkreef%>% 
    nest(data = -MonYr) %>% 
    mutate(kde = map(data, hr_kde, trast = tRastReef, levels = c(0.95)))
  
})

stopCluster(cl) 

library(rrapply)

ls.monthlyReefKDE<-lapply(ls.kde.reef.raw, function(x){
  
  all<-rrapply(x, how='melt')%>%
    filter(L1=="MonYr"|L3=="BandNumber"|L3=="h")%>%
    distinct
  
  my<-filter(all, L1=="MonYr")%>%
    dplyr::select(value)%>%
    .$value%>%unlist
  
  bn<-filter(all, L3=="BandNumber" & L2==1)%>%
    dplyr::select(value)%>%
    .$value%>%.[[1]]%>%.[1]
  
  kde<-all%>%
    filter(L3=="h")%>%
    mutate(MonYr = my, BandNumber=bn, value=unlist(value)[ c(TRUE,FALSE) ])%>%
    dplyr::select(kde=value, MonYr, BandNumber)
  
  return(kde)
  
})

pts<-readRDS("./data/DFs/prePointsPA.rds")%>%
  # use presence only points
  filter(PA==1) %>%
  dplyr::select(BandNumber, SEX, LOCBANDED, MORPH)%>%
  distinct

monthlyReefKDE<-do.call('rbind', ls.monthlyReefKDE)%>%
  mutate(MonYr = as.Date(paste(MonYr, "-01", sep="")),
         kde=as.numeric(kde)) %>%
  left_join(.,pts) %>%
  mutate(TIME=as.numeric(MonYr)/1000,
         BandNumber=as.factor(BandNumber),
         SEX=as.factor(SEX))



# Save
saveRDS(monthlyReefKDE, "./data/monthlyReefLocsKDE.RDS")



# From here - monthly KDE, reef locs --------------------------------------
monthlyReefKDE<-readRDS("./data/monthlyReefLocsKDE.RDS")

table(monthlyReefKDE$MonYr)

# Reef Plots --------------------------------------------------------------



# Pattern is less clear between males and females when it comes
#' to reef only locations. This could be caused by the females spending 
#' more even amounts of time on the island

ggplot() +
  geom_point(data=monthlyReefKDE, aes(x=MonYr, y=kde, col=SEX)) +
  labs(title=("Pacific Reef Egrets - Monthly 95% KDE per sex"),
       x = "Date", y = "KDE (km^2)") +
  geom_jitter()

ggplot() +
  geom_boxplot(data=monthlyReefKDE, aes(x=SEX, y=kde, col=SEX), alpha=0) +
  geom_point(data=monthlyReefKDE, aes(x=SEX, y=kde, col=SEX), position = "jitter") +
  labs(title=("Pacific Reef Egrets (Reef only locations) - Monthly 95% KDE per sex"),
       x = "Date", y = "KDE (m^2)") 

# We also see there are seasonal peaks during the winter months
# this is expected as per noddy research which indicates there
# might be productivity maximums during the summer hence synch breeding
ggplot() +
  geom_smooth(data=monthlyReefKDE, aes(x=MonYr, y=kde, col=SEX), method="gam", formula = y~s(x) ) +
  labs(title=("Pacific Reef Egrets (Reef locations only) - Monthly 95% KDE per sex"),
       x = "Date", y = "KDE (m^2)") +
  geom_jitter()



library(mgcv)
library(tidyverse)



mreef <- gam(kde ~   s(TIME, BandNumber, bs='fs'),
         family=gaussian, data=monthlyReefKDE, method = "REML")

summary(mreef)

plot(mreef)

# !! Need to find a way to plot the Male Vs Female trend lines
# as group averages rather than each individual.






# Time spent at the island per sex ----------------------------------------

sexIsland<-readRDS("./data/DFs/prePoints.RDS")%>%
  mutate(MonYr=substr(Date.Time, 1, 7)) %>%
  #filter(state==2) %>%
  dplyr::select(BandNumber, MonYr, Date.Time, x, y) %>%
  distinct(BandNumber, MonYr, Date.Time, x, y) %>%
  st_as_sf(., coords = c('x', 'y'), crs = st_crs(4326))%>%
  # identify whether over reef, cay, island etc...
  st_intersection(., feats) %>%
  mutate(UTMx = unlist(map(.$geometry,1)),
         UTMy = unlist(map(.$geometry,2)))  %>%
  # OK - amt coords need to be in UTM format for other functions
  st_transform(32756) %>%
  st_drop_geometry() %>%
  left_join(., bIDs) %>%
  droplevels()%>%
  left_join(.,pts)%>%
  dplyr::count(BandNumber, SEX, FEAT_NAME)

ptpct<-sexIsland%>%
  group_by(BandNumber) %>%
  dplyr::summarise(totpts=sum(n))%>%
  left_join(., sexIsland)%>%
  mutate(pct=n/totpts)


# save
saveRDS(ptpct, "./data/DFs/islandPct.rds")


# No real changes in the proportion of time that each sex
# spent on the island...next check distances and standard deviation
# from the island?

ptpct<-readRDS("./data/DFs/islandPct.rds")



ggplot(ptpct, aes(x=FEAT_NAME, y=pct, fill=SEX))+
  geom_boxplot()+
  geom_point(position='jitter')+
  labs(title="Pacific Reef Egret percentage time spent on island Vs Reef per sex",
       y="Percentage of time", x="Location")


# What about distance to the colony?

HI <- st_read("/Users/jc754314/Library/CloudStorage/OneDrive-JamesCookUniversity/Projects/CH4_BN_ShortComms/data/GBR_Features/HI.shp")

dist<-left_join(trksReef, pts)%>%
  data.frame%>%
  st_as_sf(., coords=c(x="x_", y="y_"), crs=4326)%>%
  st_transform(32756)%>%
  mutate(distColony = as.vector(st_distance(., st_union(HI))),
         distColony = as.numeric(distColony)/1000)%>%
  group_by(BandNumber, SEX, MORPH)%>%
  dplyr::summarise(mean=mean(distColony), sd=sd(distColony))

saveRDS(dist, "./data/DFs/distPRE.rds")

dist<-readRDS("./data/DFs/distPRE.rds")

# BAM. Ladies are generally having to forage further away from the island
# which is increasing their foraging KDEs AND they are more consistent in the
# distances that they forage from the island

ggplot(data=dist, aes(x=SEX, y=mean))+
  geom_boxplot()+
  labs(title="Pacific Reef Egret distance from colony (reef locations only)",
       y="Distance (km)")

ggplot(data=dist, aes(x=SEX, y=sd))+
  geom_boxplot()+
  labs(title="Pacific Reef Egret variation in distance from colony (reef locations only)",
       y="SD")

# per morph

distm<-left_join(trksReef, pts)%>%
  data.frame%>%
  st_as_sf(., coords=c(x="x_", y="y_"), crs=4326)%>%
  st_transform(32756)%>%
  mutate(distColony = as.vector(st_distance(., st_union(HI))),
         distColony = as.numeric(distColony)/1000)%>%
  group_by(BandNumber, MORPH)%>%
  dplyr::summarise(mean=mean(distColony), sd=sd(distColony))

# save
saveRDS(distm, "./data/DFs/distMorph.rds")

distm<-readRDS("./data/DFs/distMorph.rds")

ggplot(data=distm, aes(x=MORPH, y=mean))+
  geom_boxplot()+
  labs(title="Pacific Reef Egret distance from colony (reef locations only) per morph",
       y="Distance (km)")

ggplot(data=distm, aes(x=MORPH, y=mean))+
  geom_boxplot()+
  labs(title="Pacific Reef Egret variation in distance from colony (reef locations only) per morph",
       y="SD")



# Get quarterly KDE for all Locations -----------------------------------------------

head(monthlyKDE)

# calculate at 3 month intervals
# need grouping variables for each quarter

qKDE<-monthlyKDE%>%
  mutate(quarter = quarter(MonYr, with_year = TRUE))%>%
  group_by(BandNumber, quarter)%>%
  dplyr::summarise(qKDE = mean(kde))

saveRDS(qKDE, "./data/DFs/quarterlyKDE.rds")

