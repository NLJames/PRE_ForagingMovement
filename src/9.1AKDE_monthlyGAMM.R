library(mgcv)
library(gratia)
library(MuMIn)
library(gratia)
library(DHARMa)

setwd("~/Library/CloudStorage/OneDrive-JamesCookUniversity/Projects/THESIS_PHD_NLJ_2024/CH5_PREForaging/")


# Analysis - AKDE weekly and monthly per sex and morph
bandSexMo<-readRDS("~/Library/CloudStorage/OneDrive-JamesCookUniversity/Projects/THESIS_PHD_NLJ_2024/CH5_PREForaging/src_outputs/finalTemp.rds")%>%
  distinct(BandNumber, SEX, MORPH, LOCBANDED)

# SEX   MORPH     n
# F     Grey      3
# F     White     3
# M     Grey      2
# M     White     6


akdeDFmonthly<-readRDS("./src_outputs/akde_monthlyFinal.rds")%>%
  mutate(month=as.numeric(month(date)),
         BandNumber = as.factor(str_replace(BandNumber, "^X(\\d+)\\.(\\d+)$", "\\1-\\2")))%>%
  left_join(bandSexMo)%>%
  dplyr::mutate(SEX = factor(SEX, levels = c("F", "M")),
                LOCBANDED = as.factor(LOCBANDED),
                BandNumber = as.factor(BandNumber)) %>% # "F" becomes reference
  dplyr::filter(!is.na(CI_Estimate))%>%
  arrange(month)

skimr::skim(akdeDFmonthly)

chk<-akdeDFmonthly%>%
  group_by(SEX,month)%>%
  dplyr::summarise(n=n())

# Fit GAM with separate cyclic smooths
gam_sex <- mgcv::gam(
  CI_Estimate ~ 
    s(month, by = SEX, bs = "cc", k=12)+
    s(BandNumber, bs = "re"),                 
  data = akdeDFmonthly,
  family = Gamma(link = "log"),
  method = "REML"
)

gam_sex2 <- mgcv::gam(
  CI_Estimate ~ 
    s(month, bs = "cc", k=12)+
    s(month, by = SEX, bs = "cc", k=12)+
    s(BandNumber, bs = "re"),                 
  data = akdeDFmonthly,
  family = Gamma(link = "log"),
  method = "REML"
)

gam_SexSite <- mgcv::gam(
  CI_Estimate ~ 
    s(month, by = SEX, bs = "cc", k=12)+
    s(BandNumber, bs = "re",k=12)+
    LOCBANDED,                 
  data = akdeDFmonthly,
  family = Gamma(link = "log"),
  method = "REML"
)

gam_SexSiteSpl <- mgcv::gam(
  CI_Estimate ~ 
    s(month, by = LOCBANDED, bs = "cc", k=12) +
    s(month, by = SEX, bs = "cc", k=12) +
    s(BandNumber, bs = "re"),
  data = akdeDFmonthly,
  family = Gamma(link = "log"),
  method = "REML"
)


AICc(gam_sex, gam_sex2, gam_SexSite, gam_SexSiteSpl)

appraise(gam_sex2)
k.check(gam_sex2)



model_listAKDE <- list(gam_sex, gam_sex2, gam_SexSite, gam_SexSiteSpl)
names(model_listAKDE) <- c("gam_sex", "gam_sex2", "gam_SexSite", "gam_SexSiteSpl")

# Step 2: Compare AICc across models
aic_tableAKDE <- MuMIn::model.sel(model_listAKDE) %>%
  as.data.frame() %>%
  rownames_to_column("model") %>%
  arrange(delta)

# Step 3: Save best model
best_model_nameAKDE <- aic_tableAKDE$model[1]
best_modelAKDE <- model_listAKDE[[best_model_nameAKDE]]


# Save best model
saveRDS(model_listAKDE,file = "./src_outputs/allAKDEMods.rds")
saveRDS(best_modelAKDE, file = "./src_outputs/bestAKDEMod.rds")



gam_sex2

modTabAKDE<-lapply(model_listAKDE, function(x){
  
  covars<-x[["formula"]]%>%
    as.character()%>%as.vector()
  
  data.frame(Covariates = covars, AIC = AIC(x))
  
})



# Save the final table in the MS folder for compiling
modTabAKDE_df<-do.call('rbind', modTabAKDE) %>% 
  distinct()%>%
  dplyr::filter(str_detect(Covariates, "month"))%>%
  mutate_if(is.numeric, round, 3) %>%
  arrange((AIC))%>%
  mutate(AICdelta = AIC-min(AIC),
         Covariates=gsub("LOCBANDED", "site", Covariates))%>%
  mutate(Covariates=gsub("MORPH", "morph", Covariates))%>%
  mutate(Covariates=gsub("SEX", "sex", Covariates))%>%
  mutate(Covariates=paste0("AKDE ~ ", Covariates))

saveRDS(modTabAKDE_df,"./src_outputs/candidateAKDEMods.rds")



sexres<-simulateResiduals(gam_sex2)
#testDispersion(sexres)

summary(gam_sex2)
gratia::draw(gam_sex2, residuals = T)

summary(gam_sex2)$s.table

newgrid <- expand.grid(
  month = seq(1, 12, length.out = 100),
  SEX = levels(akdeDFmonthly$SEX),
  LOCBANDED = levels(akdeDFmonthly$LOCBANDED),
  BandNumber="101-18502"
) %>%
  dplyr::mutate(
    month = as.numeric(month),
    BandNumber = factor(BandNumber),
    SEX = factor(SEX, levels = levels(akdeDFmonthly$SEX))
  )

# Predict without the random smooth
preds <- predict(
  gam_sex2,
  newdata = newgrid,
  type = "response",
  exclude = c("s(BandNumber)"),
  se.fit = TRUE
)

summary(preds$fit)


# Add predictions to newgrid
newgrid <- newgrid %>%
  dplyr::mutate(
    fit = preds$fit,
    se = preds$se.fit,
    lower = fit - 2 * se,
    upper = fit + 2 * se
  )



# emmeans values ----------------------------------------------------------

library(emmeans)

# Create a grid of months and sexes, setting a fixed BandNumber to exclude the random effect
pred_grid <- expand.grid(
  month = seq(1, 12, by = 0.5),
  SEX = c("F", "M"),
  BandNumber = NA  # Random effect placeholder
)

# Use emmeans on the fitted GAM model
em_results <- emmeans(gam_sex2, ~ SEX | month,
                      at = list(month = seq(1, 12, by = 1)),
                      type = "response")

# Summarize results with confidence intervals
summary(em_results, infer = TRUE)

# test Male Vs Females
emmeans(gam_sex2, pairwise ~ SEX | month, 
        at = list(month = seq(1, 12, 1)),
        type = "response") %>%
  summary(infer = TRUE)
































