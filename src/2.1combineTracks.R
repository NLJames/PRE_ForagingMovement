# Combine Heron Island and One Tree Island cleaned tracks into the single
# `cleanTracks.rds` object used by 3.demData.R onwards.
#
# This step previously existed only as commented-out code inside
# WriteUp/MS/CH5_PRE_SI_Tables.Rmd (the `indivTable` chunk), with the
# analysis pipeline instead relying on an already-saved copy of
# ./src_outputs/cleanTracks.rds being present on disk. That file's schema and
# per-BandNumber fix counts were verified to exactly match the output of the
# logic below (cross-checked against tables/CH6_AppTab1_trackingData.csv),
# so it is reproduced here as an explicit, runnable script.
#
# Run 1.egTracks_HI_Wist.R and 2.egTracks_OTI.R first (they save
# ./src_outputs/preClean_HI.rds and ./src_outputs/preClean_OTI.rds).

library(tidyverse)

setwd("~/Library/CloudStorage/OneDrive-JamesCookUniversity/Projects/THESIS_PHD_NLJ_2024/CH5_PREForaging/")

pre <- readRDS("./src_outputs/preClean_HI.rds")
oti_pre <- readRDS("./src_outputs/preClean_OTI.rds")

preDat <- rbind(pre, oti_pre) %>%
  dplyr::mutate(BandNumber = as.character(BandNumber)) %>%
  dplyr::filter(x > 151.6, x < 152.3, y > -23.7, y < -23.3) %>%
  dplyr::distinct(BandNumber, dateTimeAEST, .keep_all = TRUE) %>%
  droplevels() %>%
  dplyr::arrange(BandNumber, dateTimeUTC) %>%
  dplyr::group_by(BandNumber) %>%
  dplyr::mutate(tDiff = difftime(lead(dateTimeUTC), dateTimeUTC, units = "hours"),
                lagdateTimeUTC = lag(dateTimeUTC)) %>%
  ungroup()

# Persist so 3.demData.R can pick this up
saveRDS(preDat, "./src_outputs/cleanTracks.rds")
