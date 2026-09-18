

library(ggplot2)
library(mgcv)
library(Ecfun)
library(mgcv)      #for GAMs
library(gratia)    #for GAM plots
library(emmeans)   #for marginal means etc
library(broom)     #for tidy output
library(MuMIn)     #for model selection and AICc
library(lubridate) #for processing dates
library(tidyverse) #for data wrangling
library(DHARMa)    #for residuals diagnostics
#library(performance) #for residual disagnostics
#library(see)        # to visualize residual diagnostics
library(patchwork) #for grids of plots
library(lubridate)
library(ggeffects)




# Foraging effort per season ----------------------------------------------



# TIME variable already converted as numeric for dates
monthlyKDE<-readRDS("./data/DFs/monthlyKDE.rds")%>%
  mutate(MORPH=as.factor(MORPH),
         MONTH = as.numeric(substr(MonYr, 6,7)),
         YEAR  = as.factor(substr(MonYr, 1,4)))%>%
  filter(kde<0.01)

str(monthlyKDE)

ggplot(monthlyKDE, aes(x=MonYr, y=kde, group=SEX, col=SEX))+
  geom_smooth(method='gam')+
  geom_point()+
  labs(x="Date", y='Foraging area')

# SEX ratios - only one white female...
# F	Grey	3		
# F	White	1		
# M	Grey	2		
# M	White	4	

csexGamma<-gam(kde ~ s(MONTH, by=SEX, bs = "cc") + 
                 s(TIME, by=SEX) + 
                 s(BandNumber, bs = 're'), 
               data = monthlyKDE,
               family=Gamma(link='log'),
               method = "REML")

draw(csexGamma, residuals = T)


csexGaus<-gam(kde ~ s(MONTH, by=SEX, bs = "cc") + 
                s(TIME, by=SEX) + 
                s(BandNumber, bs = 're'), 
              data = monthlyKDE,
              family=gaussian(link='log'),
              method = "REML")

sexGaus<-gam(kde ~ s(MONTH, by=SEX, bs = "cc") +
               s(MONTH, by=SEX, bs = "cc") +
               s(BandNumber, bs = 're'), 
             data = monthlyKDE,
             family=gaussian(link='log'),
             method = "REML")

sexGaussimp<-gam(kde ~ s(MONTH, by=SEX, bs='cc') + 
                   s(TIME, by=SEX, bs='cs', k=40) + 
                 s(BandNumber, bs = 're'), 
                 
                 data = monthlyKDE,
                 family=gaussian(link='log'),
                 method = "REML",
                 fx = TRUE)

gam1<-gam(kde ~  s(TIME, bs='cr') + s(TIME, by=SEX, bs='cs') + s(BandNumber, bs = 're'),  #+ s(MONTH, by=SEX, m=1)
                 
                 data = monthlyKDE,
                 family=gaussian(link='log'),
                 method = "REML")

draw(gam1, residuals = T)

gam2<-gam(kde ~  s(MONTH, by=SEX, bs='cc') + s(BandNumber, bs = 're'),
          
          data = monthlyKDE,
          family=gaussian(link='log'),
          method = "REML")

gam3<-gam(kde ~  SEX + s(MONTH, by=SEX, bs='cc')+ s(BandNumber, bs = 're'),
          
          data = monthlyKDE,
          #family=gaussian(link='log'),
          method = "REML")

draw(gam3)

gam4<-gam(kde ~  SEX + s(TIME, by=SEX, bs='cr') + s(BandNumber, bs = c("re")),
          
          data = monthlyKDE,
          family=gaussian(),
          method = "REML")


draw(gam4, residuals = T)

gam5<-gam(kde ~  SEX + s(MONTH, by=SEX, bs='cc') + s(BandNumber, bs = 're'),
          
          data = monthlyKDE,
          family=gaussian(),
          method = "REML")

draw(gam5, residuals = T)


gam6<-gam(kde ~  SEX + s(TIME, by=SEX, bs='cr')  + s(YEAR, BandNumber, bs = c("re")),
          
          data = monthlyKDE,
          family=gaussian(),
          method = "REML")


draw(gam6, residuals = T)


AICc(sexGaus, csexGaus,sexGaussimp, gam1, gam2, gam3, gam4, gam5, gam6)

appraise(gam6)
k.check(gam6)
sexres<-simulateResiduals(gam4,  plot=TRUE)
testDispersion(sexres)


sexres2 = recalculateResiduals(sexres , group = monthlyKDE$BandNumber)
testDispersion(sexres2, plot=T)


prels = with(monthlyKDE,
             list(TIME=seq(min(TIME), max(TIME), len=6000),
                  BandNumber=sample(BandNumber, size=6000, replace=T),
                  YEAR=sample(YEAR, size=6000, replace=T),
                  SEX=sample(SEX, size=6000, replace=T)))

summary(gam6)

preds<-predict.gam(gam6, newdata = prels, 
                   type = 'response', 
                   exclude = c("s(YEAR,BandNumber)"),
                   se.fit=TRUE)

# Female time range
range(monthlyKDE%>%filter(SEX=='F')%>%.$TIME)

# Male time range
range(monthlyKDE%>%filter(SEX=='M')%>%.$TIME)

preDF<-prels%>%
  data.frame()%>%
  cbind(.,preds)%>%
  filter(SEX=='M' & TIME<=19.205 | SEX=='F' & TIME<=19.448)

# Male time range
range(preDF%>%filter(SEX=='M')%>%.$TIME)
# Female time range
range(preDF%>%filter(SEX=='F')%>%.$TIME)

head(preDF,20)

library(wesanderson)

pal<-wes_palette("Royal2")
# Keep working from here with this one...
ggplot() +
  geom_ribbon(data=preDF, aes(x=TIME, group=SEX, ymin=fit-se.fit, ymax=fit+se.fit), fill = "grey70", alpha=0.3) +
  geom_line(data=preDF, aes(y=fit, x=TIME, group=SEX, col=SEX), size=1)+
  geom_point(data=monthlyKDE, aes(x=TIME, y=kde, col=SEX)) +
  scale_color_manual(values = c(pal[c(3,5)])) +
  labs() +
  theme_classic()+
  #scale_x_continuous(breaks = 1:12)+
  labs(y="95% KDE (km^2)", x="Date", col="Sex")



pal<-wes_palette("Royal2")
# Keep working from here with this one...
ggplot() +
  #geom_ribbon(data=preDF, aes(y=fit, x=MONTH, group=interaction(YEAR, SEX),ymin=fit-se.fit, ymax=fit+se.fit), fill = "grey70", alpha=0.3) +
  geom_line(data=preDF, aes(y=fit, x=MONTH, group=interaction(YEAR, SEX), linetype=YEAR, col=SEX), size=1)+ #group = interaction(replicate, lane),colour = lane
  scale_color_manual(values = c(pal[c(3,5)])) +
  labs() +
  theme_classic()+
  scale_x_continuous(breaks = 1:12)+
  labs(y="95% KDE (km^2)", x="Month")





# Foraging effort with turbidity ------------------------------------------


gam4<-gam(kde ~  SEX + s(TIME, by=SEX, bs='cc') + s(BandNumber, bs = c("re")) + BandNumber,
          data = dd,
          family=gaussian(),
          method = "REML")

prels = with(dd,
             list(MONTH=seq(min(MONTH), max(MONTH), len=6000),
                  BandNumber=sample(BandNumber, size=6000, replace=T),
                  SEX=sample(SEX, size=6000, replace=T)))

preds<-predict.gam(gam4, newdata = prels, 
                   type = 'response', 
                   exclude = c("s(BandNumber)"),
                   se.fit=TRUE)

ggplot() +
  geom_ribbon(data=preDF, aes(x=MONTH, group=SEX, ymin=fit-se.fit, ymax=fit+se.fit), fill = "grey70", alpha=0.3) +
  geom_line(data=preDF, aes(y=fit, x=TIME, group=SEX, col=SEX), size=1)+
  geom_point(data=dd, aes(x=MONTH, y=kde, col=SEX)) +
  scale_color_manual(values = c(pal[c(3,5)])) +
  labs() +
  theme_classic()+
  labs(y="95% KDE (km^2)", x="Date")


pal<-wesanderson::wes_palette("Royal2")


gam3<-gam(kde ~  SEX + s(TIME, by=SEX, bs='cr') + s(BandNumber, bs = c("re")) + BandNumber,
          
          data = dd,
          family=gaussian(),
          method = "REML")

prels = with(dd,
             list(TIME=seq(min(TIME), max(TIME), len=6000),
                  BandNumber=sample(BandNumber, size=6000, replace=T),
                  SEX=sample(SEX, size=6000, replace=T)))

preds<-predict.gam(gam3, newdata = prels, 
                   type = 'response', 
                   exclude = c("s(BandNumber)"),
                   se.fit=TRUE)

ggplot() +
  geom_ribbon(data=preDF, aes(x=TIME, group=SEX, ymin=fit-se.fit, ymax=fit+se.fit), fill = "grey70", alpha=0.3) +
  geom_line(data=preDF, aes(y=fit, x=TIME, group=SEX, col=SEX), size=1)+
  geom_point(data=dd, aes(x=MONTH, y=kde, col=SEX)) +
  scale_color_manual(values = c(pal[c(3,5)])) +
  labs() +
  theme_classic()+
  labs(y="95% KDE (km^2)", x="Date")