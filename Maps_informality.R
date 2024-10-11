# Load required libraries
library(gadm)  # For downloading administrative boundary data
library(sf)    # For handling spatial data
library(terra) # For raster manipulation
library(ggplot2) # For plotting
library(dplyr)  # For data manipulatio

#### Step 1: Get the Peru shapefile
shapefile <- gadm(country = "PER", level = 1, path = tempdir()) %>%
  st_as_sf()

#### Step 2: Download the nighttime lights raster (VNP46A3 is monthly product)
r <- bm_raster(roi_sf = shapefile,
               product_id = "VNP46A4",   # Nighttime lights product
               date = "2014-01-01",      # Specify the date
               bearer = bearer)

# Convert RasterLayer to SpatRaster
r <- rast(r)

# Save it as a GeoTIFF file
writeRaster(r, "my_raster.tif", overwrite=TRUE)

# Load the saved SpatRaster file
r_loaded <- rast("my_raster.tif")

#### Step 3: Loop through each region and map them
# Create a directory to store maps if needed
output_dir <- "nighttime_lights_maps"
if (!dir.exists(output_dir)) dir.create(output_dir)

# Iterate through each region (department)
regions <- unique(shapefile$NAME_1)  # Assuming 'NAME_1' is the column with department names

for (region in regions) {
  # Filter the shapefile for the current region
  region_shape <- shapefile %>% filter(NAME_1 == region)
  
  # Ensure the CRS matches the raster
  region_shape <- st_transform(region_shape, crs(r))
  
  # Mask the raster with the current region's shapefile
  region_raster <- terra::mask(r, region_shape)
  
  # Apply log transformation (if required)
  region_raster[] <- log(region_raster[] + 1)
  
  # Plot the nighttime lights map for the current region
  p <- ggplot() +
    geom_spatraster(data = region_raster) +
    scale_fill_gradient2(low = "black",
                         mid = "yellow",
                         high = "red",
                         midpoint = 4.5,
                         na.value = "transparent") +
    labs(title = paste("Luces Nocturnas:", region, "Enero 2014")) +
    coord_sf() +
    theme_void() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          legend.position = "none")
  
  # Display the map
  print(p)
  
  # Save the map to file (optional)
  ggsave(filename = paste0(output_dir, "/", region, "_nighttime_lights.png"), plot = p)
}
################################################################################
#### Step 3: Loop through each region and map them individually
output_dir <- "nighttime_lights_maps_anual"
if (!dir.exists(output_dir)) dir.create(output_dir)

# Iterate through each region (department)
regions <- unique(shapefile$NAME_1)  # Assuming 'NAME_1' is the column with department names

for (region in regions) {
  # Filter the shapefile for the current region
  region_shape <- shapefile %>% filter(NAME_1 == region)
  
  # Ensure the CRS matches the raster
  region_shape <- st_transform(region_shape, crs(r))
  
  # Mask the raster with the current region's shapefile (clip to region)
  region_raster <- terra::mask(r, region_shape)
  
  # Crop the raster to the region extent to focus the plot on the region
  region_raster <- terra::crop(region_raster, region_shape)
  
  # Apply log transformation (if required)
  region_raster[] <- log(region_raster[] + 1)
  
  # Get the bounding box of the region to zoom into the plot
  region_bbox <- st_bbox(region_shape)
  
  # Plot the nighttime lights map for the current region, zoomed in
  p <- ggplot() +
    geom_spatraster(data = region_raster) +
    geom_sf(data = region_shape, fill = NA, color = "white", size = 0.5) +  # Plot the region boundary
    scale_fill_gradient2(low = "black",
                         mid = "yellow",
                         high = "red",
                         midpoint = 4.5,
                         na.value = "transparent") +
    labs(title = paste("Luces Nocturnas:", region, "2014")) +
    coord_sf(xlim = c(region_bbox["xmin"], region_bbox["xmax"]),
             ylim = c(region_bbox["ymin"], region_bbox["ymax"])) +  # Focus on region's extent
    theme_void() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          legend.position = "none")
  
  # Display the map
  print(p)
  
  # Save the map to file (optional)
  ggsave(filename = paste0(output_dir, "/", region, "_nighttime_lights.png"), plot = p)
}

################################################################################
#### Step 1: Get the Peru shapefile
shapefile <- gadm(country = "PER", level = 1, path = tempdir()) %>%
  st_as_sf()

#### Step 2: Download the nighttime lights raster (VNP46A4 is yearly product)
r <- bm_raster(roi_sf = shapefile,
               product_id = "VNP46A4",   # Nighttime lights product
               date = "2023-01-01",      # Specify the date
               bearer = bearer)

# Convert RasterLayer to SpatRaster
r <- rast(r)
#### Step 3: Loop through each region and map them individually
output_dir <- "nighttime_lights_maps_2023"
if (!dir.exists(output_dir)) dir.create(output_dir)

# Iterate through each region (department)
regions <- unique(shapefile$NAME_1)  # Assuming 'NAME_1' is the column with department names

for (region in regions) {
  # Filter the shapefile for the current region
  region_shape <- shapefile %>% filter(NAME_1 == region)
  
  # Ensure the CRS matches the raster
  region_shape <- st_transform(region_shape, crs(r))
  
  # Mask the raster with the current region's shapefile (clip to region)
  region_raster <- terra::mask(r, region_shape)
  
  # Crop the raster to the region extent to focus the plot on the region
  region_raster <- terra::crop(region_raster, region_shape)
  
  # Apply log transformation (if required)
  region_raster[] <- log(region_raster[] + 1)
  
  # Get the bounding box of the region to zoom into the plot
  region_bbox <- st_bbox(region_shape)
  
  # Plot the nighttime lights map for the current region, zoomed in
  p <- ggplot() +
    geom_spatraster(data = region_raster) +
    geom_sf(data = region_shape, fill = NA, color = "white", size = 0.5) +  # Plot the region boundary
    scale_fill_gradient2(low = "black",
                         mid = "yellow",
                         high = "red",
                         midpoint = 4.5,
                         na.value = "transparent") +
    labs(title = paste("Luces Nocturnas:", region, "2023")) +
    coord_sf(xlim = c(region_bbox["xmin"], region_bbox["xmax"]),
             ylim = c(region_bbox["ymin"], region_bbox["ymax"])) +  # Focus on region's extent
    theme_void() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          legend.position = "none")
  
  # Display the map
  print(p)
  
  # Save the map to file (optional)
  ggsave(filename = paste0(output_dir, "/", region, "_nighttime_lights.png"), plot = p)
}
############################ Trends over time ##################################
#### Extract annual data
ntl_df <- bm_extract(roi_sf = shapefile,
                     product_id = "VNP46A4",
                     date = 2014:2023,
                     bearer = bearer)
ntl <- rast(ntl_df)
# Save it as a GeoTIFF file
writeRaster(ntl_df, "my_raster.tif", overwrite=TRUE)

# Load the saved SpatRaster file
ntl_loaded <- rast("my_raster.tif")

#### Trends over time
ntl_df |>
  ggplot() +
  geom_col(aes(x = date,
               y = ntl_mean),
           fill = "darkorange") +
  facet_wrap(~NAME_1, scales = "free_y") +  # Allows different y-axis scales for each region
    labs(x = NULL,
       y = "NTL Luminosity",
       title = "Peru Nivel Administrativo 1: Promedio Anual de Luces Nocturnas") +
  scale_x_continuous(labels = seq(2015, 2023, 4),
                     breaks = seq(2015, 2023, 4)) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))
