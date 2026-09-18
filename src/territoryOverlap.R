
dfterry<-readRDS("./data/DFs/dfterries500res.rds")

dfterry$M <- as.numeric(as.character(dfterry$M))  # If M was a factor

dfterryOTI<-dfterry %>%
  dplyr::filter(LOCBANDED.x == "OTI" & LOCBANDED.y == "OTI") %>%
  dplyr::mutate(
    SEXES = purrr::map2_chr(SEX.x, SEX.y, ~ paste0(sort(c(.x, .y)), collapse = "")),
    BandNumber.x=as.factor(BandNumber.x),
    terry="OTI"
  )

dfterryHER<-dfterry%>%dplyr::filter(LOCBANDED.x=="HER"&LOCBANDED.y=="HER")%>%
  dplyr::mutate(
    SEXES = purrr::map2_chr(SEX.x, SEX.y, ~ paste0(sort(c(.x, .y)), collapse = "")),
    BandNumber.x=as.factor(BandNumber.x),
    terry="HER"
  )



terryOTI <-  mgcv::gam(overlap ~ 
                          s(M, bs = "cc", k = 12) +  
                         # s(M, by = SEXES, bs = "cc", k=12),
                          s(BandNumber.x, bs = "re"), 
                        family = Gamma(link = "log"),
                        data = dfterryOTI,
                        method = "REML")

draw(terryOTI)


terryHER <-  mgcv::gam(overlap ~ 
                         s(M, bs = "cc", k = 12) +  
                         # s(M, by = SEXES, bs = "cc", k=12),
                         s(BandNumber.x, bs = "re"), 
                       family = Gamma(link = "log"),
                       data = dfterryHER,
                       method = "REML")

draw(terryHER)


allTerry<-rbind(dfterryOTI, dfterryHER)%>%
  mutate(terry=as.factor(terry))

# 1. Ensure all variables are correct and consistent
allTerry$terry <- factor(allTerry$terry)
allTerry$M <- as.numeric(allTerry$M)
allTerry$BandNumber.x <- factor(allTerry$BandNumber.x)

# 2. Refit model cleanly if needed
terryAll <- mgcv::gam(overlap ~ 
                        s(M, bs = "cc", k = 12) +  
                        s(M, by = terry, bs = "cc", k = 12) +
                        s(BandNumber.x, bs = "re"), 
                      family = Gamma(link = "log"),
                      data = allTerry,
                      method = "REML")

# 3. Construct new data for prediction
new_data <- expand.grid(
  M = seq(1, 12, length.out = 200),
  terry = levels(allTerry$terry)
)

# 4. Add dummy factor level for random effect (required but ignored during prediction)
new_data$BandNumber.x <- allTerry$BandNumber.x[1]  # Pick one real level

# 5. Ensure factor levels match
new_data$terry <- factor(new_data$terry, levels = levels(allTerry$terry))
new_data$BandNumber.x <- factor(new_data$BandNumber.x, levels = levels(allTerry$BandNumber.x))

# 6. Predict
pred <- predict(terryAll, newdata = new_data, type = "link", se.fit = TRUE)
ilink <- family(terryAll)$linkinv

# 7. Format output
pred_df <- cbind(new_data, fit = pred$fit, se = pred$se.fit)
pred_df <- dplyr::mutate(pred_df,
                         fitted = ilink(fit),
                         lwr_ci = ilink(fit - 2 * se),
                         upr_ci = ilink(fit + 2 * se))

territoryOverlap_GAMMplot<-ggplot(pred_df, aes(x = M, y = fitted, colour = terry)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lwr_ci, ymax = upr_ci, fill = terry), alpha = 0.2, colour = NA) +
  geom_point(data = allTerry, aes(x = M, y = overlap, colour = terry), alpha = 0.4, inherit.aes = FALSE) +
  labs(x = "Month", y = "Overlap", title = "Predicted Seasonal Overlap by Territory") +
  theme_minimal()

territoryOverlap_GAMMplot

saveRDS(territoryOverlap_GAMMplot,"./src_outputs/territoryOverlap_GAMMplot.rds")
