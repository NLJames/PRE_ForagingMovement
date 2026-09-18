

setwd("~/Library/CloudStorage/OneDrive-JamesCookUniversity/Projects/THESIS_PHD_NLJ_2024/CH5_PREForaging")

# Read and prepare temp points
tempsf <- readRDS("./src_outputs/finalTemp.rds") %>%
  mutate(x = lon, y = lat) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  st_transform(32756)

# Define Heron Island (HI) location and convert to sf
HI <- data.frame(x = 151.91486247498617, y = -23.4423242469914) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  st_transform(32756)

# Define Heron Island (HI) location and convert to sf
OTI <- data.frame(x = 152.09168322302472, y = -23.50771122695176) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  st_transform(32756)

# Calculate distances (in meters)
tempsf$dist_to_HI_m <- st_distance(tempsf, HI) %>% as.numeric()
tempsf$dist_to_OTI_m <- st_distance(tempsf, OTI) %>% as.numeric()

distsCol<-tempsf%>%
  mutate(colSpecDist=ifelse(LOCBANDED=="HER", dist_to_HI_m, dist_to_OTI_m),
         colDist = ifelse(geomorphicCover=="Island",0,colSpecDist),
         onColony=ifelse(colDist==0, 1, 0))%>%
  dplyr::select(-c(colSpecDist))%>%
  st_drop_geometry()


saveRDS(distsCol, "./src_outputs/finalColDists.rds")
