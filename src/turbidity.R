



library(raster)
library(tidyverse)

## Turbidity


rm(list=ls())

# Prep turbidity rasters --------------------------------------------------

#first import all files in a single folder as a list 
rastlist <- list.files(path = "./data/ACA_HERON/ACA_TURBIDITY", pattern='.tif$', full.names=T, recursive = T)

quarter<-rastlist%>%
  data.frame()%>%
  dplyr::rename(quarter=".")%>%
  mutate(quarter=substr(quarter, 43,48))%>%
  separate(col=quarter, into=c("year","quarter"))%>%
  unite("quarter", c("quarter", "year"), sep=".")%>%
  mutate(quarter = paste0("Q", quarter))%>%
  .$quarter

# crop to extent of know range
points<-read_rds("./data/DFs/prePointsPA.rds")%>%
  filter(PA==1)

e<-extent(min(points$x) -0.1, max(points$x) +0.1, min(points$y) -0.1, max(points$y) + 0.1)

r <- stack(rastlist)%>%
  crop(., e)

# change names in the raster
names(r)<-quarter

r_df <- as.data.frame(r, xy = TRUE) %>% 
  tidyr::pivot_longer(cols = !c(x, y), 
                      names_to = 'variable', 
                      values_to = 'value')


ggplot() + 
  geom_raster(data = r_df%>%filter(variable%in%c("Q2020.1")), 
              aes(x = x, y = y, fill = value)) +
  geom_point(data=points, mapping=aes(x=x,y=y)) +
  facet_wrap(~ variable, nrow = 3) +
  scale_fill_gradientn(colours = rev(terrain.colors(225))) +
  coord_equal() + 
  theme_minimal() 


#
## next 4
#ggplot() + 
#  geom_raster(data = r_df%>%filter(variable%in%vars[5:8]), 
#              aes(x = x, y = y, fill = value)) +
#  facet_wrap(~ variable, nrow = 3) +
#  scale_fill_gradientn(colours = rev(terrain.colors(225))) +
#  coord_equal() + 
#  theme_minimal()
#
## last 6
#ggplot() + 
#  geom_raster(data = r_df%>%filter(variable%in%vars[9:14]), 
#              aes(x = x, y = y, fill = value)) +
#  facet_wrap(~ variable, nrow = 3) +
#  scale_fill_gradientn(colours = rev(terrain.colors(225))) +
#  coord_equal() + 
#  theme_minimal()
#

# Calculate average values per quarter ------------------------------------

# calculate average turbidity value per quarter
# takes a minute

mn <- data.frame(mean=cellStats(r, "mean")) 
sd <- data.frame(sd=cellStats(r, "sd")) 


mnsd<-cbind(quarter, mn, sd)%>%
  mutate(quarter=substr(quarter, 2, nchar(quarter)),
         qNum = substr(quarter, nchar(quarter), nchar(quarter)),
         qNum= as.numeric(qNum),
         year = substr(quarter, 1, nchar(quarter)-2))


head(mnsd)

ggplot() +
  geom_point(data=mnsd, aes(x=qNum, y=mean))+
  geom_line(data=mnsd, aes(x=qNum, y=mean, group=year, col=year))






# get quarterly KDEs
qKDE<-readRDS("./data/DFs/quarterlyKDE.rds")%>%
  mutate(quarter=as.character(quarter))%>%
  group_by(quarter)%>%
  dplyr::summarise(qKDE = mean(qKDE))%>%
  ungroup


head(qKDE)

qdf<-left_join(qKDE, mnsd, by=join_by(quarter))%>%
  mutate(quarter=as.numeric(quarter),
         qNum = substr(quarter, nchar(quarter), nchar(quarter)),
         qNum= as.numeric(qNum),
         year = substr(quarter, 1, nchar(quarter)-2))%>%
  dplyr::filter(!is.na(mean))


head(qdf)

ggplot() +
  geom_point(data=qdf, aes(x=qNum, y=scale(qKDE)))+
  geom_line(data=qdf, aes(x=qNum, y=scale(qKDE), group=year), col="black")+
  geom_point(data=qdf, aes(x=qNum, y=scale(mean)))+
  geom_line(data=qdf, aes(x=qNum, y=scale(mean), group=year), col="lightblue")



saveRDS(qdf, "./data/qdfTurb.rds")

qdf<-readRDS("./data/qdfTurb.rds")

# is there a lag effect?
ggplot()+
  geom_smooth(qdf, mapping=aes(x=as.numeric(qNum), y=scale(qKDE), linetype=year), se=F)+
  geom_smooth(qdf, mapping=aes(x=as.numeric(qNum), y=scale(mean),  linetype=year), col='black', se=F)

# WTF, lower turbidity correlates with higher KDEs??
ggplot()+
  geom_point(qdf, mapping=aes(x=as.numeric(qNum), y=scale(mean)))+
  geom_line(qdf, mapping=aes(x=as.numeric(qNum), y=scale(mean)), col='grey')+
  geom_point(qdf, mapping=aes(x=as.numeric(qNum), y=scale(qKDE)))+
  geom_line(qdf, mapping=aes(x=as.numeric(qNum), y=scale(qKDE)), col='black')+
  facet_grid(~year)+
  geom_hline(yintercept=c(-0.2, 0.5), linetpe='dashed', col='red')+
  ggtitle("Black = KDE, Grey = Turbidity")



# Clear trend in another way....
ggplot()+
  geom_smooth(qdf, mapping=aes(x=mean, y=qKDE), method='lm')+
  geom_point(qdf, mapping=aes(x=mean, y=qKDE))


 #' Need to be careful that this is not just a coincidence but the actual cause of the 
 #' changes in foraging efforts. For example temperature and chlorophyl (both related)
 #' to productivity are also likely to correlate in the same way...


# Extracting turbidity spatiotemporally -----------------------------------
library(sf)
library("zoo")
library(terra)
library(plyr)

# import locations with date times
pts<-readRDS( "./data/DFs/benthosFinal.rds")%>%
  # create the quarter column to match the names of the
  # raster layers
  mutate(x = unlist(map(.$geometry,1)),
         y = unlist(map(.$geometry,2)))


table(pts$PA)

str(pts)

names(pts)





# Join random points to the original data frame


# layer needs to be the index of the raster layer
index<-data.frame(index=as.integer(1:length(unique(r_df$variable))), Q=sort(unique(r_df$variable)))%>%
  mutate(Q=substr(Q, 2, nchar(Q)))%>%
  left_join(pts, .)%>%
  filter(!is.na(index))%>%
  .$index
  

str(index)
skimr::skim(index)

# use extract to get turbidity at location and time
pts$turbidity <- terra::extract(r, pts[, c("x", "y")], layer = index)

turb2<-pts%>%
  mutate(TurFNU=as.numeric(turbidity/10))%>%
  dplyr::select(-turbidity)

table(turb2$Q)
table(turb2$TurFNU)

names(turb2)

saveRDS(turb2, "./data/DFs/finalTurbidity.rds")

ggplot(pts, aes(x=Q, y=turbidity))+
  geom_point()+
  geom_boxplot()

?extract

