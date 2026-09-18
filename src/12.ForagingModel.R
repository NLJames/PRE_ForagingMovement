library(glmmTMB)
library(effects)
library(MuMIn)
library(splines)
library(tidyverse)

library(effects)
library(ggplot2)
library(dplyr)
library(DHARMa)

setwd("~/Library/CloudStorage/OneDrive-JamesCookUniversity/Projects/THESIS_PHD_NLJ_2024/CH5_PREForaging")


prePoints <- readRDS("./src_outputs/finalColDists.rds")%>%
  mutate(tideValueCats = cut(nearestTide, breaks=seq(0,3.6,by=0.6)),
         moonBrightness = cut(moonFraction, breaks=seq(0,1,by=0.25)),
         onColony = ifelse(is.na(featType),0,1),
         moonPhase_rad = moonPhase * 2 * pi,
         moon_cos = cos(moonPhase_rad),
         moon_sin = sin(moonPhase_rad),
         season = factor(season),
         lightPeriod = factor(lightPeriod),
         LOCBANDED = factor(LOCBANDED))

#skimr::skim(prePoints)
cor(prePoints[, c("moonFraction", "moon_cos", "moon_sin")], use = "complete.obs")

prePoints%>%
  ggplot()+
  geom_histogram(aes(x=moonFraction))+
  facet_wrap(~onColony)

prePoints%>%
  ggplot()+
  geom_histogram(aes(x=moonPhase))+
  facet_wrap(~onColony)

m1 <- glmmTMB(
  onColony ~ moonFraction*lightPeriod + (1 | BandNumber),
  data = prePoints,
  family = "binomial"
)

m2 <- glmmTMB(
  onColony ~ moonFraction*moonPhase + (1 | BandNumber),
  data = prePoints,
  family = "binomial"
)

m3 <- glmmTMB(
  onColony ~ moonFraction*lightPeriod+tideValueCats + (1 | BandNumber),
  data = prePoints,
  family = "binomial"
)

m4 <- glmmTMB(
  onColony ~ moonFraction*lightPeriod*tideValueCats + (1 | BandNumber),
  data = prePoints,
  family = "binomial"
)

m5 <- glmmTMB(
  onColony ~ moonFraction*lightPeriod* ns(nearestTide, df = 5) + flatTemp + (1 | BandNumber),
  data = prePoints,
  family = "binomial"
)

m6 <- glmmTMB(
  onColony ~ moonFraction*lightPeriod* ns(nearestTide, df = 5) +season + (1 | BandNumber),
  data = prePoints,
  family = "binomial"
)

m7 <- glmmTMB(
  onColony ~ moonFraction*lightPeriod* ns(nearestTide, df = 5) +ns(nearestTide, df = 5)*LOCBANDED + (1 | BandNumber),
  data = prePoints,
  family = "binomial"
)

m8 <- glmmTMB(
  onColony ~ moonFraction*ns(TOD, df=3)* ns(nearestTide, df = 5) +ns(nearestTide, df = 5)*LOCBANDED + (1 | BandNumber),
  data = prePoints,
  family = "binomial"
)


m9 <- glmmTMB(
  onColony ~ SEX*moonFraction*lightPeriod* ns(nearestTide, df = 5) +ns(nearestTide, df = 5)*LOCBANDED,
  data = prePoints,
  family = "binomial"
)

m10 <- glmmTMB(
  onColony ~ moonFraction*lightPeriod* ns(nearestTide, df = 5) +ns(nearestTide, df = 5)*SEX + (1 | BandNumber),
  data = prePoints,
  family = "binomial"
)

m11 <- glmmTMB(
  onColony ~ moonFraction*lightPeriod* ns(nearestTide, df = 5) +ns(nearestTide, df = 5)*MORPH + (1 | BandNumber),
  data = prePoints,
  family = "binomial"
)

m12 <- glmmTMB(
  onColony ~ moonFraction*lightPeriod* ns(nearestTide, df = 5) +ns(nearestTide, df = 5)*LOCBANDED+season + (1 | BandNumber),
  data = prePoints,
  family = "binomial"
)

m13 <- glmmTMB(
  onColony ~ moonFraction*lightPeriod* ns(nearestTide, df = 5)*season +ns(nearestTide, df = 5)*LOCBANDED  + (1 | BandNumber),
  data = prePoints,
  family = binomial
)


m14 <- glmmTMB(
  onColony ~ moonFraction*lightPeriod* ns(nearestTide, df = 5) +ns(nearestTide, df = 5)*LOCBANDED*season + (1 | BandNumber),
  data = prePoints,
  family = binomial
)

m15 <- glmmTMB(
  onColony ~ moonFraction*lightPeriod* ns(nearestTide, df = 5) +ns(nearestTide, df = 5)*season + (1 | BandNumber),
  data = prePoints,
  family = binomial
)

m16 <- glmmTMB(
  onColony ~ moonFraction*lightPeriod* ns(nearestTide, df = 5) +ns(nearestTide, df = 5)*flatTemp + (1 | BandNumber),
  data = prePoints,
  family = binomial
)

m17 <- glmmTMB(
  onColony ~ moonFraction*lightPeriod*moonPhase*ns(nearestTide, df = 5) +ns(nearestTide, df = 5)*flatTemp + (1 | BandNumber),
  data = prePoints,
  family = binomial
)

m18 <- glmmTMB(
  onColony ~ (moon_cos + moon_sin) * lightPeriod * ns(nearestTide, df = 5)
  + ns(nearestTide, df = 5) * flatTemp
  + (1 | BandNumber),
  data = prePoints,
  family = binomial,
  control = glmmTMBControl(optCtrl = list(iter.max = 1e4, eval.max = 1e4))
)


m19 <- glmmTMB(
  onColony ~ lightPeriod*season*ns(nearestTide, df = 5) +ns(nearestTide, df = 5)*flatTemp + (1 | BandNumber),
  data = prePoints,
  family = binomial
)

m20 <- glmmTMB(
  onColony ~ moonFraction*lightPeriod* ns(nearestTide, df = 5)*season +ns(nearestTide, df = 5)*neapSpring  + (1 | BandNumber),
  data = prePoints,
  family = binomial
)


model_list <- list(
  m1, m2, m3, m4, m5, m6, m7, m8, m9, m10,
  m11, m12, m13, m14, m15, m16, m17, m18, m19, m20
)
names(model_list) <- paste0("m", 1:20)

# Step 2: Compare AICc across models
aic_table <- MuMIn::model.sel(model_list) %>%
  as.data.frame() %>%
  rownames_to_column("model") %>%
  arrange(delta)

# Step 3: Save best model
best_model_name <- aic_table$model[1]
best_model <- model_list[[best_model_name]]


# Save best model
saveRDS(model_list,file = "./src_outputs/allForgMods.rds")
saveRDS(best_model, file = "./src_outputs/bestForgMod.rds")



# Save best model
model_list<-readRDS(file = "./src_outputs/allForgMods.rds")
best_model<-readRDS(file = "./src_outputs/bestForgMod.rds")


library(broom.mixed)
m11[["call"]]

modTab<-lapply(model_list, function(x){
  
  covars<-x[["call"]]%>%
    as.character()%>%as.vector()%>%
    stringr::str_subset("^onColony")
  
  data.frame(Covariates = covars, AIC = AIC(x))
  
})



# Save the final table in the MS folder for compiling
modTab_df<-do.call('rbind', modTab) %>% 
  distinct()%>%
  mutate_if(is.numeric, round, 3) %>%
  arrange((AIC))%>%
  mutate(AICdelta = AIC-min(AIC),
         Covariates=gsub("LOCBANDED", "site", Covariates))%>%
  mutate(Covariates=gsub("MORPH", "morph", Covariates))%>%
  mutate(Covariates=gsub("SEX", "sex", Covariates))%>%
  mutate(Covariates=gsub("onColony", "on_island", Covariates))

saveRDS(modTab_df,"./src_outputs/candidateForgMods.rds")



 plot(
  allEffects(m13), 
             multiline = TRUE, 
             #ci.style = "bands", 
             xlevels = list(nearestTide = seq(0.2, 3.6, length.out = 100)))


car::Anova(m13)

summary(m13)


# Simulate residuals
sim_res <- simulateResiduals(fittedModel = m13, n = 1000)

# Plot diagnostic summary
plot(sim_res)

# Optional: test for uniformity, dispersion, outliers
testUniformity(sim_res)
testDispersion(sim_res)
testZeroInflation(sim_res)


plotResiduals(sim_res, prePoints$nearestTide)
plotResiduals(sim_res, prePoints$moonFraction)
plotResiduals(sim_res, prePoints$lightPeriod)
plotResiduals(sim_res, prePoints$season)




# emmeans values ----------------------------------------------------------



library(emmeans)
library(ggeffects)

# Perform a test that averages over all levels of factors (year in this case)
emmeans(m13, pairwise ~ moonFraction, 
        at = list(SST_3d = c(0,0.25,0.5,0.75,1)), 
        type = "response")


## chla X distBanks -------------------------------------------------------------
# Compute predicted values on the logit scale for CHLA_60d
# The effect of CHLA X distBanks on foraging prob changes greatly between years
# in the 'good years' there was almost no effect while in 'bad' years we found a sizeable effect
# this interaction was not included in the model

moonFractionvals<-c(0,0.25,0.5,0.75,1)

# we can look at marginal means
predict_response(m13, terms = c("moonFraction [moonFractionvals]"), margin = "marginalmeans")%>%plot()

# or adjusted aka mean referenced predictions
predict_response(m13, terms = c("moonFraction [moonFractionvals]"), margin = "mean_reference")%>%plot()

# or counterfactual response predictions (decided not to use in this paper - can't get CIs??)
#predict_response(finModRSF, terms = c("distBanks [distbanksvals]","CHLA_60d [0.2,2]"), margin = "empirical", ci_level = 0.95)%>%plot()

# Moon effect at NIGHT, in SUMMER, at 0.5 m tide, Heron
emmeans(m13, ~ moonFraction | lightPeriod + season + nearestTide + LOCBANDED,
        at = list(
          moonFraction = c(0, 0.25, 0.5, 0.75, 1),
          lightPeriod = "Dark",
          season = c("Summer","Winter"),
          nearestTide = c(0.5,2),
          LOCBANDED = c("HER","OTI")
        ),
        type = "response") %>%
  summary(infer = TRUE) 

# Moon effect at NIGHT, in SUMMER, at 0.5 m tide, Heron
emmeans(m13, ~ moonFraction | lightPeriod + season + nearestTide + LOCBANDED,
        at = list(
          moonFraction = c(0.5),
          lightPeriod = "Light",
          season = c("Summer"),
          nearestTide = seq(0,3.6,0.1),
          LOCBANDED = c("HER","OTI")
        ),
        type = "response") %>%
  summary(infer = TRUE) 



