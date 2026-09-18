# Load necessary libraries
library(amt)
library(sf)
library(tidyverse)
library(parallel)
library(lubridate)
library(pbmcapply)
library(furrr)
library(ctmm)
library(move)


# Load and process tracking points, grouped by week
tempPoints <- readRDS("./src_outputs/finalTemp.rds") %>%
  dplyr::mutate(
    week = lubridate::floor_date(dateTimeAEST, unit = "month"),
    weekID = paste0(BandNumber, "_", as.character(week)),
    month = lubridate::floor_date(dateTimeAEST, unit = "month"),
    monthID = paste0(BandNumber, "_", as.character(week)),
    x = lon,
    y = lat
  ) %>%
  dplyr::select(BandNumber, LOCBANDED, week, weekID, month, monthID, dateTimeUTC, dateTimeAEST, x, y, lon, lat) %>%
  dplyr::distinct(BandNumber, week, dateTimeAEST, x, y, .keep_all = TRUE) %>%
  sf::st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  sf::st_transform(32756) %>%
  dplyr::mutate(
    UTMx = purrr::map_dbl(geometry, 1),
    UTMy = purrr::map_dbl(geometry, 2)
  ) %>%
  sf::st_drop_geometry() %>%
  droplevels() %>%
  arrange(BandNumber, dateTimeAEST) %>%
  as.data.frame()

# complains but it works
tempPoints_with_time_gaps <- tempPoints %>%
  group_by(monthID) %>%
  arrange(dateTimeUTC) %>%  # Ensure the data is sorted by time
  mutate(time_gap = as.numeric(difftime(lead(dateTimeUTC), dateTimeUTC, units = "hours"))) %>%
  group_by(monthID) %>%
  dplyr::summarize(
    min_time_gap = min(time_gap, na.rm = TRUE),  # Minimum time gap, excluding NAs
    max_time_gap = max(time_gap, na.rm = TRUE),  # Maximum time gap, excluding NAs
    mean_time_gap = mean(time_gap, na.rm = TRUE) # Mean time gap, excluding NAs
  )

rownames(tempPoints) <- NULL  # just in case
rownames(tempPoints) <- 1:nrow(tempPoints)  # assign proper rownames

# Create Move object from full dataset
mv <- move(
  x = tempPoints$UTMx,
  y = tempPoints$UTMy,
  time = tempPoints$dateTimeUTC,
  data = tempPoints,
  proj = CRS("+proj=utm +zone=56 +south +ellps=WGS84 +datum=WGS84 +units=m"),
  animal = tempPoints$monthID
)

# Convert Move object to telemetry
tel <- as.telemetry(mv, projection = "+proj=utm +zone=56 +south +datum=WGS84", timezone = "UTC", keep = TRUE)
tel <- tel[sapply(tel, nrow) > 10]  # Only keep telemetry data for individuals with >10 points

svf<-variogram(tel[[100]])
ctmm::plot(svf,level = 0.95, xlim = c(0,60*60*24*35))


# Make the guess for the movement model
allGUESS <- lapply(1:length(tel), function(b) ctmm.guess(tel[[b]], interactive = FALSE))

# Handle insufficient data and add error for all
for (guessIndex in 1:length(allGUESS)) {
  # Assign error = TRUE if there are issues with the guess
  allGUESS[[guessIndex]]$error <- TRUE
}

# Fit the movement models with the optimizer
allFITS <- lapply(c(1:length(tel)), function(i){
  
  ctmm.select(tel[[i]], allGUESS[[i]], trace = TRUE, cores = 7, method = 'pHREML')

})

allFits<-allFITS

# Save all fits
save(allFits, file = './src_outputs/ctmmFitsMonthly_models.Rdata')
save(tel, file = './src_outputs/ctmmTelemetryMonthly.Rdata')

load('./src_outputs/ctmmFitsMonthly_models.Rdata')
load('./src_outputs/ctmmTelemetryMonthly.Rdata')

# Check the structure of the first fit result
str(allFits[[1]])
allFits[[1]]@info
# Print summary of a specific fit
summary(allFits[[1]])

# Compute AKDE for each individual/week
allAKDE <- lapply(1:length(allFits), function(i) {
  akde(tel[[i]], allFits[[i]], weights = TRUE)
})

save(allAKDE, file = './src_outputs/allAKDE_monthly.Rdata')
load(file = './src_outputs/allAKDE_monthly.Rdata')

rownames(summary(allAKDE[[113]])$CI)

monthIDs<-names(tel)

areaRows <- lapply(allAKDE, function(x) {
  # Check if CI summary exists and if not, return a row of "NULL"
  if (inherits(x, "UD")) {  # Check if x is a valid 'UD' object
    tryCatch({
      ci_values <- summary(x)$CI
      # Return a row with the CI values: low, est, and high
      return(c(rownames(ci_values), ci_values[1, "low"], ci_values[1, "est"], ci_values[1, "high"]))
    }, error = function(e) {
      # If there's an error, return "NULL" values
      return(c("NULL", "NULL", "NULL", "NULL"))
    })
  } else {
    # If x is not a valid 'UD' object, return "NULL"
    return(c("NULL", "NULL", "NULL", "NULL"))
  }
})


areaDF <- do.call(rbind, areaRows)%>%data.frame
colnames(areaDF) <- c("units", "CI_Lower", "CI_Estimate", "CI_Upper")

akdeDF <- cbind(monthIDs, areaDF) %>%
  data.frame() %>%
  tidyr::separate_wider_delim(
    cols = monthIDs,
    delim = "_",
    names = c("BandNumber", "date")
  ) %>%
  dplyr::mutate(
    date = lubridate::ymd(date),
    CI_Lower = as.numeric(CI_Lower),
    CI_Estimate = as.numeric(CI_Estimate),
    CI_Upper = as.numeric(CI_Upper),
    
    # Convert units to square kilometers
    CI_Lower = dplyr::case_when(
      grepl("square centimeters", units) ~ CI_Lower / 1e10,
      grepl("square meters", units) ~ CI_Lower / 1e6,
      grepl("hectares", units) ~ CI_Lower / 100,
      TRUE ~ CI_Lower
    ),
    CI_Estimate = dplyr::case_when(
      grepl("square centimeters", units) ~ CI_Estimate / 1e10,
      grepl("square meters", units) ~ CI_Estimate / 1e6,
      grepl("hectares", units) ~ CI_Estimate / 100,
      TRUE ~ CI_Estimate
    ),
    CI_Upper = dplyr::case_when(
      grepl("square centimeters", units) ~ CI_Upper / 1e10,
      grepl("square meters", units) ~ CI_Upper / 1e6,
      grepl("hectares", units) ~ CI_Upper / 100,
      TRUE ~ CI_Upper
    ),
    
    # Update units column
    units = dplyr::case_when(
      grepl("square centimeters", units) ~ "area (square kilometers)",
      grepl("square meters", units) ~ "area (square kilometers)",
      grepl("hectares", units) ~ "area (square kilometers)",
      TRUE ~ units
    )
  ) %>%
  dplyr::arrange(BandNumber, date)




ggplot()+
  geom_point(data=akdeDF, aes(x=date, y=CI_Estimate))+
  facet_wrap(~BandNumber, ncol = 1)

saveRDS(akdeDF, "./src_outputs/akde_monthlyFinal.rds")



# Overlap -----------------------------------------------------------------
