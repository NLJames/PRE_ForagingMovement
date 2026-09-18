# Analysis - AKDE weekly and monthly per sex and morph
bandSexMo<-all_joined%>%
  distinct(BandNumber, SEX, MORPH, LOCBANDED)

# SEX   MORPH     n
# F     Grey      3
# F     White     3
# M     Grey      2
# M     White     6

akdeDFweekly<-readRDS("./src_outputs/akde_weeklyFinal.rds")%>%
  mutate(month=as.numeric(month(date)),
         BandNumber = as.factor(str_replace(BandNumber, "^X(\\d+)\\.(\\d+)$", "\\1-\\2")))%>%
  left_join(bandSexMo)%>%
  dplyr::mutate(SEX = factor(SEX, levels = c("F", "M")),
                LOCBANDED = as.factor(LOCBANDED),
                BandNumber = as.factor(BandNumber)) %>% # "F" becomes reference
  dplyr::filter(!is.na(CI_Estimate))%>%
  arrange(month)

skimr::skim(akdeDFweekly)

chk<-akdeDFweekly%>%
  group_by(SEX,month)%>%
  dplyr::summarise(n=n())

# Fit GAM with separate cyclic smooths
gam_sex <- mgcv::gam(
  CI_Estimate ~ 
    s(month, by = SEX, bs = "cc", k=12)+
    s(BandNumber, bs = "re"),                 
  data = akdeDFweekly,
  family = Gamma(link = "log"),
  method = "REML"
)

gam_sex2 <- mgcv::gam(
  CI_Estimate ~ 
    s(month, bs = "cc", k=12)+
    s(month, by = SEX, bs = "cc", k=12)+
    s(BandNumber, bs = "re"),                 
  data = akdeDFweekly,
  family = Gamma(link = "log"),
  method = "REML"
)

gam_SexSite <- mgcv::gam(
  CI_Estimate ~ 
    s(month, by = SEX, bs = "cc", k=12)+
    s(BandNumber, bs = "re",k=12)+
    LOCBANDED,                 
  data = akdeDFweekly,
  family = Gamma(link = "log"),
  method = "REML"
)

gam_SexSiteSpl <- mgcv::gam(
  CI_Estimate ~ 
    s(month, by = LOCBANDED, bs = "cc", k=12) +
    s(month, by = SEX, bs = "cc", k=12) +
    s(BandNumber, bs = "re"),
  data = akdeDFweekly,
  family = Gamma(link = "log"),
  method = "REML"
)


AICc(gam_sex, gam_sex2, gam_SexSite, gam_SexSiteSpl)

appraise(gam_sex2)
k.check(gam_sex2)

sexres<-simulateResiduals(gam_sex2,  plot=TRUE)
testDispersion(sexres)

summary(gamSite)
gratia::draw(gamSite, residuals = T)

summary(gam_sex2)$s.table

newgrid <- expand.grid(
  month = seq(1, 12, length.out = 100),
  SEX = levels(akdeDFweekly$SEX),
  LOCBANDED = levels(akdeDFweekly$LOCBANDED),
  BandNumber="101-18502"
) %>%
  dplyr::mutate(
    month = as.numeric(month),
    BandNumber = factor(BandNumber),
    SEX = factor(SEX, levels = levels(akdeDFweekly$SEX))
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

# Plot
ggplot(newgrid, aes(x = month, y = fit, color = SEX, fill = SEX)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(
    title = "Monthly AKDE Area by Sex and Site",
    x = "Month",
    y = "Estimated AKDE (km²)",
    color = "Sex",
    fill = "Sex"
  ) +
  theme_minimal(base_size = 14)









