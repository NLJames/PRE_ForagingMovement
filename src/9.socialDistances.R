
#' Data restructure required:
#' 
#' 1. Finalise how we treat Island and off island points
#'    - strict removal of island to off island distances?
#' 
#' 2. Make distance measurements only for nearest timestamp with each 
#'    individual ~ 2 hours tolerance 
#'    
#' 3. Covariate distance to colony (centre locbanded?)

bandSex<-readRDS("./src_outputs/finalTemp.rds")%>%
  mutate(featType=ifelse(is.na(featType), "offIsland",featType))%>%
  distinct(BandNumber, SEX, LOCBANDED,featType)

tempPoints<-readRDS("./src_outputs/finalTemp.rds")%>%
  mutate(x=lon,y=lat)%>%
  st_as_sf(coords=c(x='x',y='y'), crs=4326)


isBuff100<-st_read("./data/GBR_FEATURES/egFeats.shp", quiet=TRUE)%>%
  dplyr::filter(FEAT_NAME %in% c("Island","Cay"))%>%
  dplyr::select(GBR_NAME, FEAT_NAME, geometry)%>%
  st_buffer(100)%>%
  st_transform(4326)%>%
  st_make_valid() %>%
  st_crop(., tempPoints) %>%
  st_make_valid() 

## Distances between individuals for all time stamps
dd<-tempPoints%>%
  st_join(isBuff100)%>%
  st_transform(32756)%>%
  mutate(UTMx = unlist(map(.$geometry,1)),
         UTMy = unlist(map(.$geometry,2)))%>%
  st_drop_geometry()%>%
  # create a water level difference var
  # convert herontide to cm
  mutate(# convert quarter to seasons
    season = case_when(
      month %in% c(12, 1, 2) ~ "Summer",
      month %in%  3:5  ~ "Autumn",
      month %in%  6:8  ~ "Winter",
      month %in%  9:11  ~ "Spring"))%>%
  mutate(featType=ifelse(is.na(featType), "offIsland",featType))




# Create separate DF for each BandNumber sequentially.
# then join the data frame without the original BandNumber
# join by date time. Make sure that XY are kept so we can 
# get distance for each value in new column.


BNs<-unique(dd$BandNumber)

# Get bare min
ddPrep<-dd%>%
  dplyr::select(dateTimeAEST, BandNumber, UTMx, UTMy, featType)


lsDist<-lapply(BNs, function(x){
  
  df1<-ddPrep%>%
    filter(BandNumber %in% x)
  
  df2<-ddPrep%>%
    filter(!(BandNumber %in% x))%>%
    dplyr::rename(UTMx2=UTMx, UTMy2=UTMy, BandNumber2=BandNumber)
  
  jn<-left_join(df1, df2)%>%
    filter(!is.na(BandNumber2))
  
  jn$dist<-as.numeric(st_distance(st_as_sf(jn, coords = c(x='UTMx', y='UTMy'), crs = st_crs(32756)), 
                    st_as_sf(jn, coords = c(x='UTMx2', y='UTMy2'), crs = st_crs(32756)),
                    by_element = T))
  
  return(jn)
})


# Average distance to each individual
ddist <- do.call('rbind', lsDist) %>%
  dplyr::mutate(month = lubridate::month(dateTimeAEST)) %>%
  dplyr::left_join(bandSex, by = "BandNumber") %>%
  dplyr::rename(sex1 = SEX) %>%
  dplyr::left_join(bandSex, by = c("BandNumber2" = "BandNumber")) %>%
  dplyr::rename(sex2 = SEX) %>%
  dplyr::mutate(couple = paste0(sex1, sex2),
                couple = gsub("MF","FM", couple),
                siteCross = paste0(LOCBANDED.x, LOCBANDED.y))%>%
  dplyr::filter(siteCross!="HEROTI")%>%
  dplyr::filter(siteCross!="OTIHER")
  
unique(ddist$couple)
saveRDS(ddist, "./src_outputs/socialDist_all.rds")
 
# group_by(BandNumber2)%>%
 # summarise(mean(dist))
ggplot(ddist, aes(x = dist)) +
  geom_histogram(fill = "skyblue", color = "black") +  
  scale_x_log10() 

ggplot(ddist, aes(x = dist, fill = couple)) +
  geom_histogram(binwidth = 0.1, position = "identity", alpha = 0.5, color = "black") +
  scale_x_log10() +
  facet_wrap(~couple) +
  labs(x = "Distance (log10 m)", y = "Count", title = "Pairwise Distances by Pair Type")+ 
  geom_vline(xintercept = c(250, 5000), linetype = "dashed", color = "red")+
  facet_wrap(~siteCross)


ggplot(ddist, aes(x = dist, color = couple)) +
  geom_density() +
  scale_x_log10() +
  labs(x = "Distance (log10 m)", y = "Density", title = "Distance Density by Pair Type")



ggplot(ddist, aes(x=month, y=dist,group=month))+
  geom_boxplot()+
  facet_wrap(~couple+siteCross, ncol=2)


ddist2<-ddist%>%mutate(month=as.factor(month))

library(glmmTMB)
library(effects)

m1<-glmmTMB(formula = dist ~ month+couple+siteCross + (1|BandNumber),
        data=ddist2,
        REML = TRUE)
m2<-glmmTMB(formula = dist ~ month*couple+siteCross + (1|BandNumber),
            data=ddist2,
            REML = TRUE)
m3<-glmmTMB(formula = dist ~ month*couple*siteCross + (1|BandNumber),
            data=ddist2,
            REML = TRUE)
m4<-glmmTMB(formula = dist ~ month+couple*siteCross + (1|BandNumber),
            data=ddist2,
            REML = TRUE)
m5<-glmmTMB(formula = dist ~ month+siteCross + (1|BandNumber),
            data=ddist2,
            REML = TRUE)
m6<-glmmTMB(formula = dist ~ couple*siteCross + (1|BandNumber),
            data=ddist2,
            REML = TRUE)
m7<-glmmTMB(formula = dist ~ month*couple + (1|BandNumber),
            data=ddist2,
            REML = TRUE)

AIC(m1,m2,m3,m4,m5,m6,m7)%>%
  cbind(c('m1','m2','m3','m4','m5','m6','m7'))%>%
  arrange(AIC)

summary(m3)
car::Anova(m3)
plot(allEffects(m3))


#' Findings:
#' 1 there is bimodal distribution of distances between birds on both islands
#' probably owing to island Vs reef locations, heron distances bigger probably due to greater foraging area
#' distance to favourite foraging habitat i.e. reef crest probably important here
#' 
#' 2. both islands show the same seasonal clustering in the summer months and being more spread out 
#' over the winter months
#' 
#' 3. Female and male clustering pulses align with AKDE pulses indicating they spread out (maybe) to avoid each
#' other when not breeding
#' 
#' 4. Breeding most likely happens in the warmer months, males cluster before females - territoriality?
#' 
#' 5. We may be able to find breeding pairs via distances that are very small over a certain sustained period - see
#' OTI FM plot, unexpecrted dips may be brought down by a breeding pair breeding in winter?
#' 
#' 6. May need to separate non island points to look at foraging responses more closely






