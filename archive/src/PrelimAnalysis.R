

library(glmmTMB)
library(ggplot2)
library(skimr)
library(effects)
library(lubridate)
library(tidyverse)
library(sf)

dd<-readRDS("./src_outputs/finalTemps.rds")%>%
  st_drop_geometry()%>%
  # create a water level difference var
  # convert herontide to cm
  mutate(MONTH=as.numeric(substr(DTIME, 6, 7)),
         # convert quarter to seasons
    SEASON = case_when(
      MONTH %in% c(12, 1, 2) ~ "Summer",
      MONTH %in%  3:5  ~ "Autumn",
      MONTH %in%  6:8  ~ "Winter",
      MONTH %in%  9:11  ~ "Spring"))


skim(dd)




# Foraging effort ---------------------------------------------------------



# Data exploration and prelim models
ggplot(data=dd, aes(x=WLEVEL))+
  geom_histogram()

ggplot(data=dd, aes(x=BENTHIC, group=SEASON, fill=as.factor(SEASON)))+
  geom_density(alpha=0.5)+
  facet_grid(~PA)+
  scale_x_discrete(guide = guide_axis(angle = 90)) 

ggplot(data=dd, aes(x=GEOMORPHIC, group=SEASON, fill=as.factor(SEASON)))+
  geom_density(alpha=0.5)+
  facet_grid(~PA)+
  scale_x_discrete(guide = guide_axis(angle = 90)) 

ggplot(data=dd, aes(x=SEASON, y=TurFNU, fill=as.factor(PA)))+
  geom_boxplot()+
  facet_grid(~SEX)


# How does slope and flat temp influence kde and distances?? 
# or the type of substrate used??

m.sltemp.benth<-glmmTMB(PA~scale(SLOPETEMP)*BENTHIC+(1|BandNumber),
               data=dd)

summary(m.sltemp.benth)

plot(allEffects(m.sltemp.benth))

m.fltemp.benth<-glmmTMB(PA~scale(FLATTEMP)*BENTHIC+(1|BandNumber),
                  data=dd)

summary(m.fltemp.benth)

plot(allEffects(m.fltemp.benth))



# geomoprhic -
# as temp increases in flat, reef crest and maybe outer reef flat are used less

m.sltemp.geo<-glmmTMB(PA~scale(SLOPETEMP)*GEOMORPHIC+(1|BandNumber),
                        data=dd)

summary(m.sltemp.geo)

plot(allEffects(m.sltemp.geo))

m.fltemp.geo<-glmmTMB(PA~scale(FLATTEMP)*GEOMORPHIC+(1|BandNumber),
                        data=dd)

summary(m.fltemp.geo)

plot(allEffects(m.fltemp.geo))


# Important to remove the non-reef locations
# from the turbidity values as they are not
# really foraging locations

rfOnly<-dd%>%filter(!is.na(BENTHIC))

ggplot(data=rfOnly, aes(x=x, y=y, col=TurFNU))+
  geom_point()+
  facet_grid(~PA)


# Foraging selection of less turbid water
m.tur<-glmmTMB(PA~scale(TurFNU)+(1|BandNumber),
           data=rfOnly)

summary(m.tur)

plot(allEffects(m.tur))

shalrfOnly<-rfOnly%>%
  filter(DEPTHACA<100)

# In depths less that 1m, low turbidity locations selected
m.tur.dep<-glmmTMB(PA~scale(TurFNU)*scale(DEPTHACA)+(1|BandNumber),
               data=shalrfOnly)

summary(m.tur.dep)

plot(allEffects(m.tur.dep))


## Geomorphic Vs Tide ---------------------------------------------------------
library(ggeffects)
library(car)


morph<-glmmTMB(PA~scale(HeronTide)*GEOMORPHIC+(1|BandNumber),
           data=dd)

morph
Anova(morph)



morphm<- ggemmeans(morph, c( "GEOMORPHIC", "HeronTide [1,100,200,300,400,500]"), ci.lvl = 0.95, type="random")

morphplot<-ggplot() +
  geom_point(data=morphm, mapping=aes(x=x, y=predicted, fill = group, col=group), position=position_dodge(.9))+
  geom_errorbar(data=morphm, mapping=aes(ymin = conf.low , ymax = conf.high, x=x, y=predicted, col=group ), position=position_dodge(.9)) + 
  labs(
    x = "Geomorphic Type", 
    y = "Probability of selection", 
    title = "",
    colour = "Tide Height (cm)",
    fill = "Tide Height (cm)") +
  ggtitle('Probability of selecting geomorphic type under changing tide heights') +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_jitter()

morphplot







## Benthic Vs Tide ---------------------------------------------------------

benth<-glmmTMB(PA~scale(HeronTide)*BENTHIC+(1|BandNumber),
               data=dd)

benthm<- ggemmeans(benth, c( "BENTHIC", "HeronTide [1,100,200,300,400,500]"), ci.lvl = 0.95, type="random")

benthplot<-ggplot() +
  geom_point(data=benthm, mapping=aes(x=x, y=predicted, fill = group, col=group), position=position_dodge(.9))+
  geom_errorbar(data=benthm, mapping=aes(ymin = conf.low , ymax = conf.high, x=x, y=predicted, col=group ), position=position_dodge(.9)) + 
  labs(
    x = "Benthic Type", 
    y = "Probability of selection", 
    title = "",
    colour = "Tide Height (cm)",
    fill = "Tide Height (cm)") +
  ggtitle('Probability of selecting benthic type under changing tide heights') +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_jitter()

benthplot




# what about monthly changes in KDE? ---------------------------------------------------------
library(sjmisc)
library(janitor)

# monthly kde for both island and reef locations
# month in date is the month that KDE was calculated for

mkde<-readRDS("./data/DFs/monthlyAllLocsKDE.RDS")%>%
  mutate(MONTH=as.numeric(substr(MonYr, 6, 7)),
         YEAR=as.numeric(substr(MonYr, 1, 4)))%>%
  clean_names(case="all_caps")

head(mkde)

names(dd)

# average out monthly values to match KDEs?
# weekly KDEs?
# may need dummy variables to add for habitat types
mdd<-dd%>%
  to_dummy(BENTHIC, GEOMORPHIC, suffix = "label")%>%
  bind_cols(dd)%>%
  filter(PA==1, !is.na(BENTHIC))%>%
  mutate(DEPTHACA=as.numeric(DEPTHACA))%>%
  group_by(BandNumber, YEAR, MONTH)%>%
  dplyr::summarise(across(c("DEPTHACA", "SLOPETEMP","FLATTEMP", "WLEVEL", "TurFNU"), ~mean(.x, na.rm = TRUE)),
            across("BENTHIC_Coral/Algae":"GEOMORPHIC_Sheltered Reef Slope", ~sum(.x, na.rm = TRUE)))%>%
  ungroup%>%
  clean_names(case="all_caps")
  
head(mdd)

kdedf<-left_join(mkde, mdd, by=join_by(MONTH, YEAR, BAND_NUMBER))%>%
  dplyr::select(-c(BAND_NUMBER:YEAR))%>%
  pivot_longer(cols=DEPTHACA:GEOMORPHIC_SHELTERED_REEF_SLOPE)

head(kdedf)

saveRDS(kdedf, "./data/DFs/kdedf.rds")


kdedf<-readRDS("./data/DFs/kdedf.rds")

# Plots for trends!!!
names(kdedf)
head(kdedf)

# greater use of geo reef crest and benthic coral algae when kde is increased
# foraging KDE decreases with lower turbidities
ggplot()+
  geom_smooth(data=kdedf, aes(x=KDE, y=value), method='lm')+
  geom_point(data=kdedf, aes(x=KDE, y=value))+
  facet_wrap(~name, scales = 'free', nrow=3)

kdedf%>%
  filter(name=="GEOMORPHIC_REEF_CREST")%>%
ggplot()+
  geom_smooth( aes(x=KDE, y=value), method='lm')+
  geom_point(aes(x=KDE, y=value))+
  theme_classic()

kdedf%>%
  filter(name=="BENTHIC_CORAL_ALGAE")%>%
  ggplot()+
  geom_smooth( aes(x=KDE, y=value), method='lm')+
  geom_point(aes(x=KDE, y=value))+
  theme_classic()

kdedf%>%
  filter(name=="TUR_FNU")%>%
  ggplot()+
  geom_smooth(aes(y=KDE, x=value), method='lm')+
  geom_point(aes(y=KDE, x=value))+
  labs(x="Turbidity (FNU)")+
  theme_classic()



# What influences foraging depth? ---------------------------------------------

dd1<-dd%>%
  filter(PA==1, !is.na(BENTHIC), DEPTHACA<200)%>%
  mutate()

saveRDS(dd1, "./data/DFs/depthTurb.rds")


range(dd1$HeronTide, na.rm=T)
range(dd1$DEPTHACA, na.rm=T)

dd1<-readRDS("./data/DFs/depthTurb.rds")

ggplot(dd1, aes(y=DEPTHACA, x=HeronTide))+
  geom_point()+
  geom_smooth(method='lm')

mean(dd1$DEPTHACA)


ggplot(dd1, aes(y=DEPTHACA, group=SEASON, col=SEASON))+
  geom_boxplot()

# Turbidity VS depth. After 7.5 TFNU birds seem to seek out
# deeper water
ggplot(dd1, aes(y=DEPTHACA, x=TurFNU))+
  geom_point()+
  geom_smooth()

# Difference between tide and depth increases
ggplot(dd1, aes(y=WLEVEL, x=TurFNU))+
  geom_point()+
  geom_smooth()

ggplot(dd1, aes(y=WLEVEL, x=TurbCAT, group=TurbCAT, col=TurbCAT))+
  geom_boxplot()

ggplot(dd1, aes(y=DEPTHACA, group=Q))+
  geom_boxplot()



