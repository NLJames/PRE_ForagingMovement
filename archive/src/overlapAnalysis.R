


library(amt)
library(ggplot2)
library(tidygraph)
library(tidyverse)
library(ggraph)
library(sf)
library(parallel) 

rm(list=ls())




# Data prep ---------------------------------------------------------------


## Distances between individuals for all time stamps
dd<-readRDS("~/Library/CloudStorage/OneDrive-JamesCookUniversity/Projects/THESIS_PHD_NLJ_2024/CH5_PREForaging/src_outputs/finalTemp.rds")%>%
  mutate(x=lon,y=lat)%>%
  st_as_sf(coords=c(x='x',y='y'), crs=4326)%>%
  st_transform(32756)%>%
  mutate(UTMx = unlist(map(.$geometry,1)),
         UTMy = unlist(map(.$geometry,2)),
         YM = paste0(year(dateTimeAEST),"_",month(dateTimeAEST)))%>%
  st_drop_geometry()



# Second attempt at overlap estimations (using amt)

# 1.Estimate overlaps between an individual's trips

# get boutIDs from CH3 data
bIDs<-dd%>%
  dplyr::select(dateTimeAEST, BandNumber)



# Quick duplicate checks
chk<-dd %>% 
  add_count(BandNumber, dateTimeAEST) 


# Find bouts with fewer than 2 points
# otherwise it causes issues when nesting by bID
keepers<-dd %>%
  group_by(BandNumber, YM) %>%
  dplyr::summarise(n=n()) %>%
  filter(n>2) %>%
  distinct(BandNumber, YM) %>%
  ungroup

# Filter out >2  point bouts
# then, create tracks to use in function below
trks<- keepers %>%
  left_join(., dd)%>%
  make_track(., .x=UTMx, .y=UTMy, .t=dateTimeAEST, id=BandNumber, crs=32756,  all_cols = T)

chk<-distinct(dd, BandNumber, YM)





# Site fidelity analysis (annual) -----------------------------------------



# Make list of all BandNumbers to run iteratively in function below
ls.bn<-distinct(trks, BandNumber)%>%
  .$BandNumber

cores=detectCores()
cl <- makeCluster(cores[1]-1) # Leave one core free
clusterExport(cl, varlist = c("trks", "ls.bn")) #Export data frames that are used in the code

# Didn't find a faster way than runing one for each package that is used in the code
clusterEvalQ(cl, library(amt))
clusterEvalQ(cl, library(tidyverse))



## Run site fidelity per month 95% KDE - Parallel cluster ===============

siteFidelity <- parLapply(cl, ls.bn, function(x) {
  
  bn <- x
  
  # Select individual track
  trk <- dplyr::filter(trks, BandNumber == bn)
  
  # Make template raster
  tRast <- make_trast(trk, res = 500)
  
  # Make YM vector for ID later
  YM <- trk %>%
    data.frame() %>%
    dplyr::distinct(YM) %>%
    dplyr::mutate(YMid = 1:dplyr::n())
  
  nested <- trk %>% 
    amt::nest(.data = ., .by = YM) %>% 
    dplyr::mutate(kde = purrr::map(data, ~ tryCatch(hr_kde(.x, trast = tRast, levels = c(0.95, 0.5)), error = function(e) NULL))) %>%
    dplyr::filter(!purrr::map_lgl(kde, is.null))
  
  if (nrow(nested) < 2) return(NULL)  # Skip if less than 2 KDEs
  
  OL1 <- hr_overlap(nested$kde, type = "ba", conditional = TRUE, which = "all") %>%
    dplyr::mutate(BandNumber = bn)
  
  OL2 <- dplyr::left_join(OL1, YM, by = dplyr::join_by(from == YMid))
  OL3 <- dplyr::left_join(OL2, YM, by = dplyr::join_by(to == YMid))
  
  return(OL3)
})

 stopCluster(cl) 

# Save, running this takes forever
saveRDS(siteFidelity, "./data/monthlySiteFidelity1000.RDS")


## Plots on Site Fiedlity ====

siteFidelity<-readRDS( "./data/monthlySiteFidelity200.RDS")


sfdf<-do.call('rbind', siteFidelity)%>%
  left_join(., dd%>%distinct(BandNumber,SEX))
  

## matching months of the year ====
sm<-sfdf%>%
  mutate(M1=substr(YM.x, 5, nchar(YM.x)),
         M2=substr(YM.y, 5, nchar(YM.y)),
         Y=substr(YM.y, 1, 4))%>%
  filter(M1==M2)%>%
  arrange(M1)%>%
  mutate(
    SEASON = case_when(
      M1 %in% c(12, 1, 2) ~ "Summer",
      M1 %in%  3:5  ~ "Autumn",
      M1 %in%  6:8  ~ "Winter",
      M1 %in%  9:11  ~ "Spring"),
    WS = case_when(
      M1 %in% c(10,11, 12, 1, 2, 3) ~ "Summer",
      M1 %in%  c(4,5,6,7,8,9)  ~ "Winter"
    ))


sm%>%
  dplyr::filter(levels==0.95)%>%
  group_by(BandNumber)%>%
  dplyr::summarise(meanOL=mean(overlap),
                   sd=sd(overlap))

## At individual levels ====
ggplot(sm, aes(x=as.numeric(M1), y=overlap, col=Y))+
  geom_point()+
  geom_smooth(se = F)+
  facet_wrap(~BandNumber)

ggplot(sm, aes(x=as.numeric(M1), y=overlap, group=M1))+
  geom_boxplot()

ggplot(sm, aes(x=SEASON, y=overlap, group=SEASON))+
  geom_boxplot()

ggplot(sm, aes(x=WS, y=overlap, group=WS))+
  geom_boxplot()


## average overlap per month, per individual ====
# not sure how this stacks up with sexes and
# morphs?
sfdf%>%
  mutate(M1=substr(YM.x, 5, nchar(YM.x)),
         M2=substr(YM.y, 5, nchar(YM.y)),
         Y=substr(YM.y, 1, 4))%>%
  ggplot(., aes(x=as.numeric(M1), y=overlap))+
  geom_point() +
  facet_wrap(~BandNumber)





# Territoriality ----------------------------------------------------------



# Make list of all BandNumbers to run iteratively in function below
ls.ym<-distinct(trks, YM)%>%
  .$YM

cores=detectCores()
cl <- makeCluster(cores[1]-1) # Leave one core free
clusterExport(cl, varlist = c("trks", "ls.ym")) #Export data frames that are used in the code

# Didn't find a faster way than runing one for each package that is used in the code
clusterEvalQ(cl, library(amt))
clusterEvalQ(cl, library(tidyverse))



## Run site fidelity per month 95% KDE - Parallel cluster ===============

lsTerries<-parLapply(cl, ls.ym, function(x){
  
  ym<-x
  
  # Select individual track
  trk<-filter(trks, YM==ym)
  
  # Make template raster
  tRast<-make_trast(trk, res=250)
  
  # Make YM vector for ID later
  BandNumber<-trk%>%
    data.frame()%>%
    distinct(BandNumber)%>%
    mutate(BandNumberID=1:n())
  
  nested <- trk %>% 
    amt::nest(.data=., .by =BandNumber) %>% 
    mutate(kde = map(data, hr_kde, trast = tRast, levels = c(0.95)))
  
  # Currently, there are three options for calculating overlap among multiple 
  # instances: which = "all" calculates overlap for each pair of home ranges, 
  # which = "one_to_all" calculates overlap between the first element in the 
  # list and all others, and which = "consecutive" will calculate overlap between 
  # consecutive elements in the list.
  
  OL1<-hr_overlap(nested$kde, type = "ba", conditional = TRUE, which="all")%>%
    mutate(YM = ym)
  
  OL2<-left_join(OL1, BandNumber, by=join_by(from==BandNumberID))
  
  OL3<-left_join(OL2, BandNumber, by=join_by(to==BandNumberID))
  
  return(OL3)
  
})

stopCluster(cl) 

dfterry <- do.call('rbind', lsTerries) %>%
  dplyr::filter(from != to) %>%
  tidyr::separate(YM, into = c("Y", "M"), sep = "_", convert = TRUE) %>%
  dplyr::mutate(
    BNY = paste0(Y, "-", BandNumber.y),
    DATE = as.POSIXct(paste0(Y, "-", M, "-01"), format = "%Y-%m-%d")
  )%>%
  left_join(.,dd%>%distinct(BandNumber, SEX, LOCBANDED), by = c("BandNumber.x" = "BandNumber"))%>%
  left_join(.,dd%>%distinct(BandNumber, SEX, LOCBANDED), by = c("BandNumber.y" = "BandNumber"))

saveRDS(dfterry, "./data/DFs/dfterries250res.rds")
dfterry<-readRDS("./data/DFs/dfterries250res.rds")

dfterry$M <- as.numeric(as.character(dfterry$M))  # If M was a factor

dfterryOTI<-dfterry%>%dplyr::filter(LOCBANDED.x=="OTI"&LOCBANDED.y=="OTI")
dfterryHER<-dfterry%>%dplyr::filter(LOCBANDED.x=="HER"&LOCBANDED.y=="HER")


## Plot territory overlaps ===============
ggplot(dfterryOTI, aes(x=DATE, y=overlap, group=BandNumber.y, col=BandNumber.y))+
  geom_point()+
  geom_line() +
  facet_wrap(~BandNumber.x)+
  labs(x="Date", y="Percentage overlap")

ggplot(dfterryHER, aes(x=DATE, y=overlap, group=BandNumber.y, col=BandNumber.y))+
  geom_point()+
  geom_line() +
  facet_wrap(~BandNumber.x)+
  labs(x="Date", y="Percentage overlap")

ggplot()+
  geom_point(data=dfterryOTI, aes(x=DATE, y=overlap))+
  geom_smooth(data=dfterryOTI, aes(x=DATE, y=overlap), method = 'gam') +
  #facet_wrap(~BandNumber.x)+
  labs(x="Date", y="Percentage overlap")

ggplot()+
  geom_point(data=dfterryHER, aes(x=DATE, y=overlap))+
  geom_smooth(data=dfterryHER, aes(x=DATE, y=overlap), method = 'gam') +
  #facet_wrap(~BandNumber.x)+
  labs(x="Date", y="Percentage overlap")

ggplot()+
  geom_point(data=dfterryOTI, aes(x=M, y=overlap))+
  geom_smooth(data=dfterryOTI, aes(x=M, y=overlap), method = 'gam') +
  labs(x="Date", y="Percentage overlap")

ggplot()+
  geom_point(data=dfterryHER, aes(x=M, y=overlap))+
  geom_smooth(data=dfterryHER, aes(x=M, y=overlap), method = 'gam') +
  labs(x="Date", y="Percentage overlap")

# Plot territories per month ----------------------------------------------



# Find bouts with fewer than 2 points
# otherwise it causes issues when nesting by bID
keepers<-dd %>%
  group_by(BandNumber, YM) %>%
  summarise(n=n()) %>%
  filter(n>2) %>%
  distinct(BandNumber, YM) 

library(smoothr)


polys<- keepers %>%
  left_join(., dd)%>%
  mutate(BNYM = paste0(BandNumber, YM))%>%
  st_as_sf(., coords = c(x="UTMx", y="UTMy"), crs = 32756)%>%
  group_by(BNYM, BandNumber, YM)%>% 
  dplyr::summarise(.groups = "keep")%>%
  sf::st_convex_hull()%>%
  st_cast("POLYGON") %>% ungroup()
  # %>%
  # mutate(DATE = paste0(YEAR,"-", MONTH,"-", "01"),
  #        DATE = as.POSIXct(DATE))

sm<-smooth(polys, method = "ksmooth")

library(gganimate)
library(transformr)
library(plotly)
library(gapminder)
library(ggmap)
library(animation)

knitr::opts_chunk$set(dev = "ragg_png")

p<-ggplot()+
  geom_sf(data=sm, mapping= aes(col=BandNumber), size=5, fill=NA, lwd=1) +
  theme_classic()

p

anim<- p + 
  transition_states(DATE,
                    transition_length = 0.1,
                    state_length = 0.01)+
  ease_aes('cubic-in-out')+
  ggtitle('Date {closest_state}')

#anim


animate(anim, fps=8)




