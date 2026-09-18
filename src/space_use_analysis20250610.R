# generate kdes per month
# model seasonal trends (GAMM)

# Generate kde fidelity
# model seasonal and time series trends





library(amt)
library(ggplot2)
library(tidygraph)
library(tidyverse)
library(ggraph)
library(sf)
library(parallel) 
library(mgcv)
library(DHARMa)
library(emmeans)

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

siteFidelity <- parLapply(cl, ls.bn, function(x) {
  
  bn <- x
  
  # Select individual track
  trk <- dplyr::filter(trks, BandNumber == bn)
  
  # Make template raster
  tRast <- make_trast(trk, res = 250)
  
  # Make YM vector for ID later
  YM <- trk %>%
    data.frame() %>%
    dplyr::distinct(YM) %>%
    dplyr::mutate(YMid = 1:dplyr::n())
  
  # Nest and compute KDE
  nested <- trk %>% 
    amt::nest(.data = ., .by = YM) %>% 
    dplyr::mutate(kde = purrr::map(data, ~ tryCatch(hr_kde(.x, trast = tRast, levels = c(0.95)), error = function(e) NULL))) %>%
    dplyr::filter(!purrr::map_lgl(kde, is.null))
  
  if (nrow(nested) < 2) return(NULL)  # Skip if less than 2 KDEs
  
  # Compute area for 95% isopleth
  nested <- nested %>%
    dplyr::mutate(kde_area = purrr::map_dbl(kde, ~ hr_area(.x)[[1, "area"]])) %>% 
    dplyr::mutate(YMid = 1:n())  # Ensure YMid aligns
  
  # Calculate overlap
  OL1 <- hr_overlap(nested$kde, type = "ba", conditional = TRUE, which = "all") %>%
    dplyr::mutate(BandNumber = bn)
  
  # Join KDE area back to each pair
  OL2 <- dplyr::left_join(OL1, nested %>% dplyr::select(YMid, area_from = kde_area), by = c("from" = "YMid"))
  OL3 <- dplyr::left_join(OL2, nested %>% dplyr::select(YMid, area_to = kde_area),   by = c("to" = "YMid"))
  
  # Add month identifiers
  OL4 <- dplyr::left_join(OL3, YM, by = dplyr::join_by(from == YMid)) %>%
    dplyr::rename(month_from = YM) %>%
    dplyr::left_join(YM, by = dplyr::join_by(to == YMid)) %>%
    dplyr::rename(month_to = YM)
  
  return(OL4)
})

stopCluster(cl)

# Save, running this takes forever
saveRDS(siteFidelity, "./data/monthlySiteFidelity250.RDS")


## Plots on Site Fiedlity ====

siteFidelity<-readRDS( "./data/monthlySiteFidelity250.RDS")


sfdf<-do.call('rbind', siteFidelity)%>%
  left_join(., dd%>%distinct(BandNumber,SEX))


## matching months of the year ====

monthlyDayTides<-readRDS("./src_outputs/tidesLight.rds")%>%
  dplyr::filter(lightPeriod=="Light")%>%
  mutate(YM=paste0(year,"_", month(dateTimeAEST)))%>%
  group_by(YM)%>%# Can we add average daylight, tide heights?
dplyr::summarise(meanDayTide = mean(nearestTide),
                 sdDayTide = sd(nearestTide))

sm<-sfdf%>%
  arrange(month_from)%>%
  left_join(monthlyDayTides, by = c("month_from" = "YM"))%>%
  mutate(M1=as.numeric(substr(month_from, 6, nchar(month_from))),
         M2=as.numeric(substr(month_to, 6, nchar(month_to))),
         Y1=substr(month_from, 1, 4),
         Y2=substr(month_to, 1, 4),
         date1=ymd(paste0(Y1,"-",M1,"-",15)),
         date2=ymd(paste0(Y2,"-",M2,"-",15)),
         monthsApart = as.numeric(round(abs(difftime(date2,date1, units = 'days')/30))),
         BandNumber=as.factor(BandNumber),
         SEX=as.factor(SEX),
         drift=as.factor(ifelse(BandNumber%in%c("101-18502","101-18505","101-18507"),
                                 "Drifting","Stationary")))%>%
  arrange(M1,Y1)%>%
  mutate(
    SEASON = case_when(
      M1 %in% c(12, 1, 2) ~ "Summer",
      M1 %in%  3:5  ~ "Autumn",
      M1 %in%  6:8  ~ "Winter",
      M1 %in%  9:11  ~ "Spring"),
    WS = case_when(
      M1 %in% c(10,11, 12, 1, 2, 3) ~ "Summer",
      M1 %in%  c(4,5,6,7,8,9)  ~ "Winter"
    ))%>%
  mutate(
    SEASON = factor(SEASON, levels = c("Summer", "Autumn", "Spring", "Winter"))
  )

# Neutral or negative relationshp between kde and overlap within individual!?
ggplot()+
  geom_point(data=sm, aes(x=area_from, y= overlap,group=BandNumber,col=BandNumber))+
  geom_smooth(data=sm, aes(x=area_from, y= overlap,group=BandNumber,col=BandNumber), method='lm')

# Neutral or negative relationshp between kde and overlap within individual!?
ggplot()+
  geom_point(data=sm, aes(x=sdDayTide, y= area_from,group=BandNumber,col=BandNumber))+
  geom_smooth(data=sm, aes(x=sdDayTide, y= area_from,group=BandNumber,col=BandNumber), method='lm')+
  geom_smooth(data=sm, aes(x=sdDayTide, y= area_from),col='black', method='lm')

ggplot()+
  geom_point(data=sm, aes(x=meanDayTide, y= area_from,group=BandNumber,col=BandNumber))+
  geom_smooth(data=sm, aes(x=meanDayTide, y= area_from,group=BandNumber,col=BandNumber), method='lm')+
  geom_smooth(data=sm, aes(x=meanDayTide, y= area_from),col='black', method='lm')

# Neutral or negative relationshp between kde and overlap within individual!?
ggplot()+
  geom_point(data=sm, aes(x=M1, y= area_from,group=BandNumber,col=BandNumber))+
  geom_smooth(data=sm, aes(x=M1, y= area_from,group=BandNumber,col=BandNumber), method='gam')+
  geom_smooth(data=sm, aes(x=M1, y= area_from),col='black', method='gam')


ggplot()+
  geom_point(data=sm, aes(x=monthsApart, y= overlap,group=BandNumber,col=BandNumber))+
  geom_smooth(data=sm, aes(x=monthsApart, y= overlap,group=BandNumber,col=BandNumber), method='gam')



# Run KDE model -----------------------------------------------------------


kde_sm<-sm%>%
  distinct(BandNumber, month_from, .keep_all = TRUE)%>%
  dplyr::select(BandNumber, M1, month_from, SEX,meanDayTide, sdDayTide,area_from)

# Model with cyclic smooth over month
gam_kde_Sex <- gam(
  area_from ~  
    s(M1,bs = "cc",  k = 4) +
    s(M1, by = SEX, bs = "cc", k = 4) +  
    s(meanDayTide, k = 6) +  
    s(BandNumber, bs = "re"),
  data = kde_sm,
  family = Gamma(link = "log"), 
  method = "REML"
)

summary(gam_kde_Sex)
gam.check(gam_kde_Sex)
appraise(gam_kde_Sex)
concurvity(gam_kde_Sex, full = TRUE)


# KDE mod eval ------------------------------------------------------------

kdeModResiduals<-simulateResiduals(gam_kde_Sex, plot = TRUE)

testResiduals(kdeModResiduals)



# KDE plot 1 prep -----------------------------------------------------------


ilink <- gam_kde_Sex$family$linkinv
new_month_kde <- expand.grid(
  M1 = seq(1, 12, length.out = 200),
  SEX = levels(kde_sm$SEX),
  meanDayTide = mean(kde_sm$meanDayTide, na.rm = TRUE),
  BandNumber = kde_sm$BandNumber[1]
)

new_month_kde$SEX <- factor(new_month_kde$SEX, levels = levels(kde_sm$SEX))
new_month_kde$BandNumber <- factor(new_month_kde$BandNumber, levels = levels(kde_sm$BandNumber))

pred_month_kde <- predict(gam_kde_Sex, newdata = new_month_kde, type = "link", se.fit = TRUE)

pred_df_month <- cbind(new_month_kde, fit = pred_month_kde$fit, se = pred_month_kde$se.fit) %>%
  mutate(
    fitted = ilink(fit),
    lwr = ilink(fit - 2 * se),
    upr = ilink(fit + 2 * se)
  )

kdeMonthlyPlot <- ggplot(pred_df_month, aes(x = M1, y = fitted, colour = SEX, fill = SEX)) +
  geom_jitter(data = kde_sm, aes(x = M1, y = area_from, col = SEX),
              alpha = 0.2, width = 0.4, height = 0) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, colour = NA) +
  geom_line(size = 1.2) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(
    x = "Month",
    colour="Sex", fill="Sex") +
  scale_y_continuous(
    name = expression(bold("Predicted KDE Area (km"^2*")")),
    labels = function(x) x / 1e6
  )+
  theme_minimal()

# +
#   #labs(y = "Predicted KDE area (km$^2$)")+
#   ylab(TeX('\\textbf{ KDE area (km$^2$)}'))#+
# #ylab(expression(bold("Coefficient estimate for (km"^2*") FPRS variable")))


# KDE plot 2 prep ---------------------------------------------------------
new_tide_kde <- expand.grid(
  meanDayTide = seq(min(kde_sm$meanDayTide, na.rm = TRUE),
                    max(kde_sm$meanDayTide, na.rm = TRUE),
                    length.out = 200),
  M1 = 6,
  SEX = "F",
  BandNumber = kde_sm$BandNumber[1]
)

new_tide_kde$SEX <- factor(new_tide_kde$SEX, levels = levels(kde_sm$SEX))
new_tide_kde$BandNumber <- factor(new_tide_kde$BandNumber, levels = levels(kde_sm$BandNumber))

pred_tide <- predict(gam_kde_Sex, newdata = new_tide_kde, type = "link", se.fit = TRUE)

pred_df_tide <- cbind(new_tide_kde, fit = pred_tide$fit, se = pred_tide$se.fit) %>%
  mutate(
    fitted = ilink(fit),
    lwr = ilink(fit - 2 * se),
    upr = ilink(fit + 2 * se)
  )

kdeTidePlot <- ggplot(pred_df_tide, aes(x = meanDayTide, y = fitted)) +
  geom_jitter(data = kde_sm, aes(x = meanDayTide, y = area_from), alpha = 0.2, width = 0, height = 0) +
  geom_line(size = 1.2, colour = "navy") +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, fill = "navy") +
  labs(
    x = "Mean Tide Height (m)") +
  scale_y_continuous(
    name = expression(bold("Predicted KDE Area (km"^2*")")),
    labels = function(x) x / 1e6
  ) +
  theme_minimal()

kdeMonthlyPlot / kdeTidePlot

saveRDS(kdeMonthlyPlot,"./src_outputs/kdeMonthlyPlot_GAMM.rds")
saveRDS(kdeTidePlot,"./src_outputs/kdeTidePlot_GAMM.rds")


influence <- influence.gam(gam_kde_Sex)
plot(influence) 

sim <- simulateResiduals(gam_kde_Sex)
plot(sim)

set.seed(123)
train_idx <- sample(seq_len(nrow(kde_sm)), size = 0.8 * nrow(kde_sm))
train <- kde_sm[train_idx, ]
test <- kde_sm[-train_idx, ]

mod_cv <- gam(
  area_from ~ s(M1, by = SEX, bs = "cc", k = 4) + 
    s(meanDayTide, k = 6) + 
    s(BandNumber, bs = "re"),
  data = train, method = "REML"
)

preds <- predict(mod_cv, newdata = test)
cor(preds, test$area_from)  # simple validation check


# KDE emmeans -------------------------------------------------------------



# Create a grid of months and sexes, setting a fixed BandNumber to exclude the random effect
pred_grid <- expand.grid(
  M1 = seq(1, 12, by = 0.5),
  SEX = c("F", "M"),
  BandNumber = NA  # Random effect placeholder
)

# Use emmeans on the fitted GAM model
em_results <- emmeans(gam_kde_Sex, ~ SEX | M1,
                      at = list(month = seq(1, 12, by = 1)),
                      type = "response")

# Summarize results with confidence intervals
summary(em_results, infer = TRUE)

# test Male Vs Females
emmeans(gam_kde_Sex, pairwise ~ SEX | M1, 
        at = list(month = seq(1, 12, 1)),
        type = "response") %>%
  summary(infer = TRUE)



# Generate emmeans estimates across months for each sex
em_grid <- emmeans(
  gam_kde_Sex, 
  ~ SEX | M1,
  at = list(M1 = seq(1, 12.9, by = 0.1)),  # finer resolution for accurate peak/min
  type = "response"
)

# Convert to data frame for filtering
em_df <- as.data.frame(em_grid)

# Get the peak and min for each sex
extremes <- em_df %>%
  group_by(SEX) %>%
  dplyr::summarise(
    peak_month = M1[which.max(response)],
    peak_value = max(response),
    peak_lwr = lower.CL [which.max(response)],
    peak_upr = upper.CL[which.max(response)],
    
    min_month = M1[which.min(response)],
    min_value = min(response),
    min_lwr = lower.CL [which.min(response)],
    min_upr = upper.CL[which.min(response)]
  )

extremes

extremes_km2 <- extremes %>%
  mutate(
    peak_value = peak_value / 1e6,
    peak_lwr   = peak_lwr / 1e6,
    peak_upr   = peak_upr / 1e6,
    min_value  = min_value / 1e6,
    min_lwr    = min_lwr / 1e6,
    min_upr    = min_upr / 1e6
  )


extremes_km2

allMonthsKDEemm <- em_df %>%
  dplyr::filter(dplyr::near(M1 %% 1, 0))
  
allMonthsKDEemm_km2 <- allMonthsKDEemm %>%
  dplyr::mutate(
    response = response / 1e6,
    lower.CL = lower.CL / 1e6,
    upper.CL = upper.CL / 1e6
  )


# Contrast sexes (F vs M) within each month
sex_contrasts <- emmeans(gam_kde_Sex, pairwise ~ SEX | M1,
                         at = list(M1 = 1:12),
                         type = "response")

# Extract the pairwise comparison results
sex_diff_df <- summary(sex_contrasts$contrasts, infer = TRUE)

# Convert estimates and CIs to km²
sex_diff_km2 <- sex_diff_df %>%
  dplyr::mutate(
    lower.CL = lower.CL / 1e6,
    upper.CL = upper.CL / 1e6
  )

# Estimate means
emm <- emmeans(gam_kde_Sex, ~ SEX | M1, at = list(M1 = 1:12), type = "response")

# Contrast as differences
contrast(emm, method = "revpairwise", adjust = "none") %>%
  summary(infer = TRUE)

# Site Fidelity - prep and model ---------------------------------



smSub<-sm%>%
  dplyr::filter(monthsApart<24)

# 2. Fit the Beta GAM with random intercepts
gam_fidelity_Sex <- gam(
  overlap ~
    s(monthsApart, by=SEX, k = 7) +
    s(M1, by=SEX, bs = "cc", k = 4) +
    s(BandNumber, bs = "re"),
  family = betar(link = "logit"),
  data = smSub,
  method = "REML"
)

# Site Fidelity - Checks ---------------------------------

gam.check(gam_fidelity_Sex)
AIC(gam_fidelity_Sex)
summary(gam_fidelity_Sex)
# Plot smooth to visualise cyclic behaviour
#plot(gam_fidelity_Sex)


# Site Fidelity - plot prep ---------------------------------

# 1. Generate prediction data frames
new_months <- expand.grid(
  monthsApart = seq(min(smSub$monthsApart), max(smSub$monthsApart), length.out = 200),
  M1 = median(smSub$M1),  # fix M1
  SEX = levels(smSub$SEX),
  BandNumber = smSub$BandNumber[1]  # dummy
)

new_season <- expand.grid(
  monthsApart = median(smSub$monthsApart),  # fix monthsApart
  M1 = seq(1, 12, length.out = 200),
  SEX = levels(smSub$SEX),
  BandNumber = smSub$BandNumber[1]
)

# 2. Predict on link scale with SE
pred_months <- predict(gam_fidelity_Sex, newdata = new_months, type = "link", se.fit = TRUE)
pred_season <- predict(gam_fidelity_Sex, newdata = new_season, type = "link", se.fit = TRUE)

# 3. Combine and inverse link
ilink <- gam_fidelity_Sex$family$linkinv

months_df <- cbind(new_months, fit = pred_months$fit, se = pred_months$se.fit) %>%
  dplyr::mutate(
    fitted = ilink(fit),
    lwr = ilink(fit - 2 * se),
    upr = ilink(fit + 2 * se)
  )

season_df <- cbind(new_season, fit = pred_season$fit, se = pred_season$se.fit) %>%
  dplyr::mutate(
    fitted = ilink(fit),
    lwr = ilink(fit - 2 * se),
    upr = ilink(fit + 2 * se)
  )

# 1. Plot for monthsApart (fidelity interval)
fidelityIntervalPlot <- ggplot(months_df, aes(x = monthsApart, y = fitted, colour = SEX, fill = SEX)) +
  geom_jitter(data = smSub, aes(x = monthsApart, y = overlap, col = SEX),
              alpha = 0.1, width = 0.4, height = 0) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, colour = NA) +
  geom_line(size = 1.2) +
  labs(x = "Time interval (months)", y = "Predicted Overlap", col = "Sex", fill = "Sex") +
  theme_minimal()

# 2. Plot for seasonal month (M1)
fidelitySeasonPlot <- ggplot(season_df, aes(x = M1, y = fitted, colour = SEX, fill = SEX)) +
  geom_jitter(data = smSub, aes(x = M1, y = overlap, col = SEX),
              alpha = 0.1, width = 0.4, height = 0) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, colour = NA) +
  geom_line(size = 1.2) +
  labs(x = "Month", y = "Predicted Overlap", col = "Sex", fill = "Sex") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  theme_minimal()

fidelityIntervalPlot|fidelitySeasonPlot


saveRDS(fidelityIntervalPlot,"./src_outputs/fidelityIntervalPlot_GAMM.rds")
saveRDS(fidelitySeasonPlot,"./src_outputs/fidelitySeasonPlot_GAMM.rds")


# Site Fidelity - emmeans -------------------------------------------------

# Step 1: Create a properly structured data frame
newdata <- expand.grid(
  monthsApart = seq(1, 24, by=3),
  M1 = c(5, 11),
  SEX = c("M", "F")
)

# Step 2: Use `ref_grid()` instead of `at` in `emmeans()` directly
# Because `at = newdata` expects named vectors for each variable, not a data frame

# Create a reference grid for desired values
rg <- ref_grid(gam_fidelity_Sex, 
               at = list(
                 monthsApart = seq(1, 24, by=3),
                 M1 = c(6,12),
                 SEX = c("M", "F")
               ))

# Step 3: Get emmeans (marginal means) for each combination
emm <- emmeans(rg, ~ monthsApart | SEX + M1, type = "response")

contrast(emm, method = "pairwise")



# Site Fidelity - seasonal emmeans ----------------------------------------

# Step 1: Create reference grid for each month (1–12), fixed monthsApart, both sexes
rg <- ref_grid(
  gam_fidelity_Sex,
  at = list(
    M1 = 1:12,                  # Calendar months
    monthsApart = 12,           # Fixed interval (e.g. 6 months)
    SEX = c("M", "F")
  ),
  cov.reduce = list(BandNumber = function(x) NA)  # exclude random effect
)

# Step 2: Get predictions on the response scale
emm_preds <- emmeans(rg, ~ M1 | SEX, type = "response")
pred_df <- as.data.frame(emm_preds)

# Step 3: Identify min and max month for each sex
summary_df <- pred_df %>%
  group_by(SEX) %>%
  summarise(
    min_month = M1[which.min(response)],
    min_fidelity = min(response),
    max_month = M1[which.max(response)],
    max_fidelity = max(response),
    .groups = "drop"
  )

summary_df




# Step 1: Create reference grid
rg <- ref_grid(
  gam_fidelity_Sex,
  at = list(
    M1 = c(2, 8),                           # e.g., February and August
    monthsApart = c(1, 6, 12, 18, 24),     # selected intervals
    SEX = c("M", "F")
  ),
  cov.reduce = list(BandNumber = function(x) NA)  # Exclude random intercepts
)

# Step 2: Get emmeans across both M1 and monthsApart combinations for each sex
emm <- emmeans(rg, ~ monthsApart * M1 | SEX, type = "response")

# Step 3: Compute pairwise contrasts across all combinations of M1 and monthsApart within each sex
pairwise_contrasts <- contrast(emm, method = "pairwise", adjust = "tukey")

# Step 4: View results
pairwise_contrasts

summary(emm) 

