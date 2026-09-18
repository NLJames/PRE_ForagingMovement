# calculate foraging area at weekly and monthly time intervals
#'
#'
#'
#'
#'
# Calculate KDE for weekly and monthly points per individual
# create a summarised table for these two areas
# NOTE this should be done where shore and island points are included, then again with only reef/sea locations
# given turbidity data is available quarterly, it would also be useful to calculate at this time interval as well
# the dates are 
# First quarter, Q1: 1 January – 31 March (90 days or 91 days in leap years)
# Second quarter, Q2: 1 April – 30 June (91 days)
# Third quarter, Q3: 1 July – 30 September (92 days)
# Fourth quarter, Q4: 1 October – 31 December (92 days)

# CROP TO NON-ISLAND POINTS

# feature layer for egrets
feats<-readRDS("./data/GBR_FEATURES/egFeats.rds")%>%
  dplyr::select(FEAT_NAME)
plot(feats)

# pre points 
pre<-readRDS("./data/DFs/prePoints.rds")%>%
  st_as_sf(coords=c('x', 'y'), crs=4326)%>%
  # identify whether over reef, cay, island etc...
  st_intersection(., feats)




# WEEKLY, MONTHLY FORAGIN KDEs

library(move)
login<-movebankLogin()

# returns a MoveStack object from the specified study
getMovebankData(study="	Kivi Kuaka [ID_PROG 1099]", login=login) 


animalID

?as_move

library(amt)

?make_track
predf<-pre%>%
  mutate(x = unlist(map(.$geometry,1)),
         y = unlist(map(.$geometry,2)))%>%
  st_drop_geometry()%>%
  arrange(Date.Time)%>%
  rename(tag_id=BandNumber, timestamp=Date.Time, location.lat=y, location.long = x)

tel<-as.telemetry(predf)
plot(tel,col=rainbow(length(tel)))

b1<-tel$`101-18504`

SVF <- variogram(b1)
level <- c(0.5,0.95) # 50% and 95% CIs
xlim <- c(0,12 %#% "hour") # 0-12 hour window
plot(SVF,xlim=xlim,level=level)
title("zoomed in")
plot(SVF,fraction=0.65,level=level)
title("zoomed out")

m.iid <- ctmm(sigma=3 %#% "km^2")
m.ou <- ctmm(sigma=3 %#% "km^2",tau=5 %#% "hour")
plot(SVF,CTMM=m.iid,fraction=0.65,level=level,col.CTMM="red")
title("Independent and identically distributed data")
plot(SVF,CTMM=m.ou,fraction=0.65,level=level,col.CTMM="purple")
title("Ornstein-Uhlenbeck movement")

m.ouf <- ctmm(sigma=3 %#% "km^2",tau=c(5 %#% "hour"))
plot(SVF,CTMM=m.ou,level=level,col.CTMM="purple",xlim=xlim)
title("Ornstein-Uhlenbeck movement")
plot(SVF,CTMM=m.ouf,level=level,col.CTMM="blue",xlim=xlim)
title("Ornstein-Uhlenbeck-F movement")

plot(SVF,CTMM=m.ou,fraction=0.65,level=level,col.CTMM="purple")
title("Ornstein-Uhlenbeck movement")
plot(SVF,CTMM=m.ouf,fraction=0.65,level=level,col.CTMM="blue")
title("Ornstein-Uhlenbeck-F movement")


SVF4 <- lapply(tel,variogram)
SVF4 <- mean(SVF4)
plot(SVF4,fraction=0.35,level=level)
title("Population variogram")

m.iid <- ctmm(sigma=1.3 %#% "km^2")
m.ou <- ctmm(sigma=1.3 %#% "km^2",tau=5 %#% "hour")
plot(SVF4,CTMM=m.iid,fraction=0.65,level=level,col.CTMM="red")
title("Independent and identically distributed data")
plot(SVF4,CTMM=m.ou,fraction=0.65,level=level,col.CTMM="purple")
title("Ornstein-Uhlenbeck movement")

m.ouf <- ctmm(sigma=1.3 %#% "km^2",tau=c(5 %#% "hour"))
plot(SVF4,CTMM=m.ou,level=level,col.CTMM="purple",xlim=xlim)
title("Ornstein-Uhlenbeck movement")
plot(SVF4,CTMM=m.ouf,level=level,col.CTMM="blue",xlim=xlim)
title("Ornstein-Uhlenbeck-F movement")

plot(SVF4,CTMM=m.ou,fraction=0.65,level=level,col.CTMM="purple")
title("Ornstein-Uhlenbeck movement")
plot(SVF4,CTMM=m.ouf,fraction=0.65,level=level,col.CTMM="blue")
title("Ornstein-Uhlenbeck-F movement")

b1<tel$`101-18502`

M.IID <- ctmm.fit(b1) # no autocorrelation timescales
GUESS <- ctmm.guess(b1,interactive=FALSE) # automated model guess
M.OUF <- ctmm.fit(b1,GUESS) # in general, use ctmm.select instead

KDE <- akde(b1,M.IID) # KDE
AKDE <- akde(b1,M.OUF) # AKDE

wAKDE <- akde(b1,M.OUF,weights=TRUE) # weighted AKDE

# calculate one extent for all UDs
EXT <- extent(list(KDE,AKDE,wAKDE),level=0.95)

plot(b1,UD=KDE,xlim=EXT$x,ylim=EXT$y)
title(expression("IID KDE"["C"]))
plot(b1,UD=AKDE,xlim=EXT$x,ylim=EXT$y)
title(expression("OUF AKDE"["C"]))
plot(b1,UD=wAKDE,xlim=EXT$x,ylim=EXT$y)
title(expression("weighted OUF AKDE"["C"]))

summary(KDE)
summary(wAKDE)

mv<-as_move(data=tel)

names(predf)

trks<-mk_track(predf, crs=4326, .x=x, .y=y, .t="Date.Time", id=BandNumber, SEX=SEX, MORPH=MORPH)
class(trks)

mv<-as_move(pre)
