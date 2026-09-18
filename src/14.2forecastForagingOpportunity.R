library(tidyverse)
library(ggplot2)
library(glmmTMB)
library(splines)

forgModFinal<-readRDS(file = "./src_outputs/bestForgMod.rds")

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

# Create new tide range for prediction (e.g. up to 5 m)
pred_tide_0m <- expand.grid(
  nearestTide = seq(0, 3.6, length.out = 400),
  moonFraction = 0.5,
  lightPeriod = factor(c("Dark", "Light"), levels = levels(prePoints$lightPeriod)),
  season = factor("Summer", levels = levels(prePoints$season)),
  LOCBANDED = factor(c("HER", "OTI"), levels = levels(prePoints$LOCBANDED)),
  BandNumber = NA  # keep NA since we're excluding random effects
)%>%data.frame()

# Create new tide range for prediction (e.g. up to 5 m)
pred_tide_0.5m <- pred_tide_0m%>%
  mutate(nearestTide = nearestTide+0.5)%>%
  data.frame()

# Make sure you're using the same levels as in the model data
pred_tide_1.9m <- pred_tide_0m%>%
  mutate(nearestTide = nearestTide+1.9)%>%
  data.frame()

# Predict using final model
pred_tide_0m$pred_0m <- predict(forgModFinal, newdata = pred_tide_0m, type = "response", re.form = NA)
pred_tide_0m$pred_0.5m <- predict(forgModFinal, newdata = pred_tide_0.5m, type = "response", re.form = NA)
pred_tide_0m$pred_1.9m <- predict(forgModFinal, newdata = pred_tide_1.9m, type = "response", re.form = NA)

pred_tide_0m%>%
  pivot_longer(cols = c(pred_0m,pred_0.5m,pred_1.9m), names_to = "scenario", values_to = "probOnIsland")%>%
  # Plot predicted onColony probability vs tide height
  ggplot(., aes(x = nearestTide, y = probOnIsland, group=scenario, col=scenario)) +
  geom_line() +
  labs(
    x = "Tide Height (m)",
    y = "P(onColony)",
    title = "Predicted Probability of Being On Colony by Tide Height",
    subtitle = "SLR projected out to 5 m | MoonFraction = 0.5 | Dark | Summer | HER"
  ) +
  ylim(0, 1)+
  facet_wrap(~lightPeriod+LOCBANDED)


