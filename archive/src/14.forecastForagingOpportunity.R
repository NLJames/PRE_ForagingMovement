library(dplyr)
library(tidyr)
library(ggplot2)
library(ggridges)

# foraging analysis:

#' climate scenarios increase water depth i.e. substract XXcm from topography
#'  to emulate predicted climate scenarios then use current model predict to estimate
#'  foraging opportunities/time windows. restricted to island edge foraging when reef
#'  creast is clearly preffered a compelling story

# 2. Simulate hourly tide heights and apply sea level rise scenarios
fHI<-read.csv("./data/BOM_TIDE/tides_HI.csv")
names(fHI)<-c("dateTime", "heronTide")

fOTI<-read.csv("./data/BOM_TIDE/tides_OTI.csv")
names(fOTI)<-c("dateTime", "OneTreeTide")


tides<-left_join(fHI, fOTI)%>%
dplyr::filter(heronTide!="Prediction")%>%
  mutate(heronTide=as.numeric(heronTide),
         OneTreeTide=as.numeric(OneTreeTide),
    heronTide_0.5m=heronTide+0.5,
         OneTreeTide_0.5m=OneTreeTide+0.5,
         heronTide_1.9m=heronTide+1.9,
         OneTreeTide_1.9m=OneTreeTide+1.9)%>%
  pivot_longer(cols=c(OneTreeTide, heronTide,heronTide_0.5m,OneTreeTide_0.5m,
                      heronTide_1.9m, OneTreeTide_1.9m), values_to = "tideHeight", names_to = "scenario")%>%
  mutate(scenario=factor(as.factor(scenario), levels=rev(c('heronTide', 'OneTreeTide','heronTide_0.5m',
                                                       'OneTreeTide_0.5m', 'heronTide_1.9m', 'OneTreeTide_1.9m' ))))

ggplot(tides, aes(y=scenario, x=tideHeight, fill=after_stat(x)))+
  geom_density_ridges_gradient()+
  scale_fill_viridis_c(option = "C",name = "Tide Height")+
  geom_vline(xintercept = 1.37, linetype='dotted')+
  geom_vline(xintercept = 1.66, linetype='dashed')+
  geom_vline(xintercept = 2.68)+
  theme_classic()


