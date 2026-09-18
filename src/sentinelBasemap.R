
# Load libraries
library(sf)
library(raster)
library(terra)
library(ggplot2)
library(gganimate)
library(rgee)
library(reticulate)


# Remove old credentials completely
rgee::ee_clean_user_credentials()
rgee::ee_clean_pyenv()
# Re-authenticate
rgee::ee_Authenticate()
rgee::ee_Initialize(user = "nlj987@gmail.com")
rgee::ee_check()

# Define study area
region <- ee$Geometry$Rectangle(c(151.6, -23.7, 152.2, -23.35))

# Load Sentinel-2 data with a unique name to avoid conflicts
sentinel_collection <- ee$ImageCollection("COPERNICUS/S2_SR")$
  filterBounds(region)$
  filterDate("2024-06-01", "2024-09-01")  # Adjust date range

# Print the object to confirm it works
print(sentinel_collection)# Adjust date range
  
mask_clouds_shadows <- function(img) {
  scl <- img$select("SCL")  # Select Scene Classification (SCL) band
  mask <- scl$neq(3)$And(scl$neq(8))$And(scl$neq(9))  # Mask out shadows & clouds
  return(img$updateMask(mask))  # Apply mask
}

# Apply cloud and shadow mask
sentinel_filtered <- sentinel_collection$
  map(mask_clouds_shadows)$  # Apply mask
  filter(ee$Filter$lt("CLOUDY_PIXEL_PERCENTAGE", 5))$  # Filter low-cloud images
  map(function(img) { img$select(c("B4", "B3", "B2")) })$  # Select RGB bands
  median()  # Merge clean images

# Check if Sentinel-2 processing is working
bands_final <- sentinel_filtered$bandNames()$getInfo()
print(bands_final)  # Should return ["B4", "B3", "B2"]


# Define local save path
local_tif_path <- "./data/sentinel_data/sentinel_clean.tif"

# Download the processed image as GeoTIFF
sentinel_raster <- ee_as_rast(
  image = sentinel_filtered,
  region = region,
  scale = 10,
  via = "drive",  # Alternative: "gcs" for Google Cloud Storage
  dsn = local_tif_path
)

# Load the cloud-free Sentinel-2 image
sentinel_raster <- rast(local_tif_path)

# Normalize bands between 0-1 for better visualization
sentinel_raster <- (sentinel_raster - min(values(sentinel_raster))) / 
  (max(values(sentinel_raster)) - min(values(sentinel_raster)))

# Convert raster to dataframe for ggplot
sentinel_df <- as.data.frame(sentinel_raster, xy = TRUE)
colnames(sentinel_df) <- c("x", "y", "Red", "Green", "Blue")

ggplot() +
  geom_raster(data = sentinel_df, aes(x = x, y = y, fill = rgb(Red, Green, Blue, maxColorValue = 1))) +
  scale_fill_identity() +
  theme_minimal() +
  labs(title = "Cloud & Shadow-Free Sentinel-2 True Color Composite")







library(imager)
# Load Sentinel-2 TIFF
sentinel_raster <- rast("./data/sentinel_data/sentinel_composite.tif")

# Normalize bands between 0 and 1
sentinel_raster <- (sentinel_raster - min(values(sentinel_raster))) / 
  (max(values(sentinel_raster)) - min(values(sentinel_raster)))

# Convert raster to array for image processing
sentinel_array <- as.array(sentinel_raster)

# Convert array to cimg format (needed for imager functions)
sentinel_cimg <- as.cimg(sentinel_array)

# Apply histogram equalization to enhance contrast
sentinel_eq <- hist(sentinel_cimg)

# Apply CLAHE (Contrast Limited Adaptive Histogram Equalization)
sentinel_clahe <- CLAHE(sentinel_eq, ntiles = c(8, 8), clip.limit = 0.02)

# Convert back to raster
sentinel_raster <- rast(as.array(sentinel_clahe))

# Convert raster to dataframe for ggplot
sentinel_df <- as.data.frame(sentinel_raster, xy = TRUE)

# Rename bands for clarity
colnames(sentinel_df) <- c("x", "y", "Red", "Green", "Blue")

# Apply unsharp mask for sharpening
sentinel_sharpened <- focal(sentinel_raster, w = matrix(c(-1,-1,-1,-1,9,-1,-1,-1,-1), 3, 3))

# Convert to dataframe
sentinel_df <- as.data.frame(sentinel_sharpened, xy = TRUE)
colnames(sentinel_df) <- c("x", "y", "Red", "Green", "Blue")

