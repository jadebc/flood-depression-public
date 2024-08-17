#############################################
# CRADLE depression and flooding analysis

# Calculate percent of surface water   
#############################################

rm(list=ls())
source(paste0(here::here(), '/0-config.R'))

library(purrr)
library(sp)
library(lubridate)
library(geosphere)
library(sf)
library(raster)
library(terra)
#install.packages("pracma")
library(pracma)
library(dplyr)
# library(matrixStats)
library(haven)


# # find study area boundaries 
bgd_adm4_shp <- st_read("/Users/suhi/downloads/bgd_shp_files/bgd_admbnda_adm4_bbs_20201113.shp") %>% 
  filter(ADM2_EN == "Sirajganj" | ADM2_EN == "Tangail")

#bgd_adm4_shp <- st_read(paste0(box_shapefile_path, "bgd_shp_files/bgd_admbnda_adm4_bbs_20201113.shp")) %>% 
#  filter(ADM2_EN == "Sirajganj" | ADM2_EN == "Tangail")

coords <- as.data.frame(st_coordinates(bgd_adm4_shp))

max(coords$X) # 90.30083
min(coords$X) # 89.24715
max(coords$Y) # 24.78787
min(coords$Y) # 23.96359

# # read in seasonality raster for Bangladesh
seasonality_raster = terra::rast("/Users/suhi/Downloads/seasonality_raw.tif") # data resolution 29.48906m * 29.48906m

# seasonality_raster = terra::rast(paste0(box_shapefile_path, "seasonality_raw.tif"))

## crop seasonality raster to study area
crop_extent <- extent(88, 92, 22, 26)
seasonality_raster = crop(seasonality_raster, crop_extent)

# # determine the appropriate UTM zone for the point
#utm_zone <- floor((-75 + 180) / 6) + 1
utm_zone <- floor((mean(coords$X) + 180) / 6) + 1
utm_crs <- paste0("EPSG:", 32600 + utm_zone)

# # transform the point and raster to the UTM projected CRS
seasonality_raster <- project(seasonality_raster, utm_crs)

# seasonality_raster[seasonality_raster == 255] <- NA # the Bangladesh seasonality raster does not have 255 values

seasonal_raster = seasonality_raster
permanent_raster = seasonality_raster
permanent_raster[permanent_raster < 12] <- 0
seasonal_raster[seasonal_raster > 6] <- 0
# 
writeRaster(permanent_raster, "/Users/suhi/Downloads/permanent_raster_epsg.tif", 
                                      filetype="GTiff", overwrite=TRUE)

writeRaster(seasonal_raster, "/Users/suhi/Downloads/seasonal_raster_epsg.tif", 
            filetype="GTiff", overwrite=TRUE)

#writeRaster(permanent_raster, paste0(box_shapefile_path, "rasters/permanent_raster_epsg.tif"), 
#            filetype="GTiff", overwrite=TRUE)

#writeRaster(seasonal_raster, paste0(box_shapefile_path, "rasters/seasonal_raster_epsg.tif"), 
#            filetype="GTiff", overwrite=TRUE)


perm_raster <- rast("/Users/suhi/Downloads/permanent_raster_epsg.tif")
seasonal_raster <- rast("/Users/suhi/Downloads/seasonal_raster_epsg.tif")

#perm_raster <- rast(paste0(box_shapefile_path, "rasters/permanent_raster_epsg.tif"))
#seasonal_raster <- rast(paste0(box_shapefile_path, "rasters/seasonal_raster_epsg.tif"))

#seasonal_points = as.points(seasonal_raster)
#seasonal_sppoints = SpatialPoints(raster_points[, 1:2], proj4string=CRS('+proj=longlat +datum=WGS84'))


###########################################################
########### Detect Proportion of Surface Water ############
###########################################################

ll <- read_dta("/Users/suhi/Downloads/CRADLE_Baseline_data.dta") %>% 
  dplyr::select(dataid, gpslatitudedegrees, gpslongitudedegrees, q1_3) %>% 
  rename(hhcode = dataid, lat = gpslatitudedegrees, long = gpslongitudedegrees, union = q1_3)


# Store original coordinates
ll <- ll %>%
  mutate(orig_long = long, orig_lat = lat)





detect_water_proportion <- function(ll_row, radii, water_type, water_raster) {
  lon <- as.numeric(ll_row["orig_long"])
  lat <- as.numeric(ll_row["orig_lat"])
  
  longlat <- cbind(lon, lat)
  
  point <- vect(longlat, crs = "+proj=longlat +datum=WGS84")
  point <- project(point, "+proj=utm +zone=45 +datum=WGS84")
  
  hhcode <- ll_row[["hhcode"]]
  union <- ll_row[["union"]]
  
  #this_row <- data.frame(hhcode = hhcode, union = union)
  # Include original coordinates in the output
  this_row <- data.frame(hhcode = hhcode, union = union, orig_long = lon, orig_lat = lat)
  #print(this_row)
  
  
  for (radius in radii) {
    buffer <- buffer(point, width = radius)
    
    tryCatch({
      cropped_raster <- crop(water_raster, buffer)
      
      non_water_cells <- sum(values(cropped_raster) == 0, na.rm = TRUE)
      total_cells <- sum(!is.na(values(cropped_raster)))
      
      if (total_cells > 0) {
        proportion <- 1 - (non_water_cells / total_cells)
      } else {
        proportion <- 0
      }
      
      proportion_df <- data.frame(proportion)
      colnames(proportion_df) <- paste("prop", water_type, "water", radius, sep = "_")
      
      this_row <- cbind(this_row, proportion_df)
      
    }, error = function(e) {
      message("Error at radius: ", radius, " - ", e$message)
      proportion_df <- data.frame(NA)
      colnames(proportion_df) <- paste("prop", water_type, "water", radius, sep = "_")
      
      this_row <- cbind(this_row, proportion_df)
    })
  }
  
  return(this_row)
}


num_hh <- length(ll$orig_long)
radii <- c(10, 25, 50, 75, 100, 250, 500, 1000)
perm_water_df <- map_dfr(1:num_hh, 
                         ~ detect_water_proportion(ll_row = ll[.x, ],
                                                   radii = radii,
                                                   water_type = "perm",
                                                   water_raster = perm_raster))

seasonal_water_df <- map_dfr(1:num_hh, 
                             ~ detect_water_proportion(ll_row = ll[.x, ],
                                                       radii = radii,
                                                       water_type = "seasonal",
                                                       water_raster = seasonal_raster))

sw_df <- left_join(perm_water_df, seasonal_water_df, by = c("union", "hhcode"))

saveRDS(sw_df, "/Users/suhi/Downloads/analysis_prop_surface_water.RDS")
#saveRDS(sw_df, paste0(here::here(), "/data/analysis_prop_surface_water.RDS"))

###########################################################
########### Detect Distance to Surface Water ############
###########################################################

detect_water_distance <- function(ll_row, water_type, water_raster) {
  lon <- as.numeric(ll_row["orig_long"])
  lat <- as.numeric(ll_row["orig_lat"])
  
  longlat <- cbind(lon, lat)
  
  point <- vect(longlat, crs = "+proj=longlat +datum=WGS84")
  point <- project(point, "+proj=utm +zone=45 +datum=WGS84")
  
  hhcode <- ll_row[["hhcode"]]
  union <- ll_row[["union"]]
  
  #correct projection
  # print(crs(point))
  # print(crs(water_raster))
  
  
  this_row <- data.frame(hhcode = hhcode, union = union, orig_long = lon, orig_lat = lat)
  
  # initialize distance variable
  distance_to_water <- Inf
  
  # define buffer zones (up to 5000 meters in 30-meter increments)
  radii <- seq(30, 10000, by = 30)
  
  for (radius in radii) {
    buffer <- buffer(point, width = radius)
    
    cropped_raster <- crop(water_raster, buffer)
    #print(values(cropped_raster))
    
    # Debugging: Print buffer extent and raster extent
    #print(paste("Radius:", radius))
    #print(ext(buffer))
    #print(ext(water_raster))
    
    # print(paste("Number of total cells:", nrow(cropped_raster)))
    # if (nrow(cropped_raster) == 0){
    #    print(paste("LL row:", ll_row))
    # }
    # 
    # 
    # #debugging: Check number of water cells
    # water_cells <- which(values(cropped_raster) != 0, arr.ind = TRUE)
    # print(paste("Number of water cells:", nrow(water_cells)))
     
    #Find the nearest water cell
    water_cells <- which(values(cropped_raster) != 0, arr.ind = TRUE)
    if (nrow(water_cells) > 0) {
      
      # Extract coordinates for the water cells
      cell_indices <- which(values(cropped_raster) != 0)
      water_coords <- xyFromCell(cropped_raster, cell_indices)
      #water_coords <- xyFromCell(cropped_raster, cell = cellFromRowCol(cropped_raster, water_cells[,1], water_cells[,2]))
      water_points <- vect(water_coords, crs = crs(cropped_raster))
      
      # print(water_coords)
      # print(ext(buffer))
      # print(point)
      
      #print(crs(point))
      #print(crs(water_points))
      
      distances <- terra::distance(point, water_points)
      #print(distances)
      
      # check if there are valid distances
      if (length(distances) > 0 && all(!is.na(distances))) {
        min_distance <- min(distances, na.rm = TRUE)
        distance_to_water <- min(distance_to_water, min_distance)
      }
      # add distance to water column
      this_row[[paste("distance_to", water_type, "water", sep = "_")]] <- distance_to_water
      
      return(this_row)
    }
  }
  
  this_row[[paste("distance_to", water_type, "water", sep = "_")]] <- distance_to_water
  
  return(this_row)
  
  
}


## test
# Run the detect_water_distance function on a debugging row
# test_result <- detect_water_distance(ll_row = ll[30, ],
#                                      water_type = "seasonal",
#                                      water_raster = seasonal_raster)
# 

# test_result <- detect_water_distance(ll_row = ll[16, ],
#                                     water_type = "perm",
#                                     water_raster = perm_raster)
# 
# manual_distance <- sqrt((784184.7 - 785051.8)^2 + (2664125 - 2664812)^2)
# print(manual_distance)


## test


seasonal_water_distance <- map_dfr(seq_len(nrow(ll)), 
                                           ~ detect_water_distance(ll_row = ll[.x, ],
                                                                   water_type = "seasonal",
                                                                   water_raster = seasonal_raster))

perm_water_distance <- map_dfr(seq_len(nrow(ll)), 
                                       ~ detect_water_distance(ll_row = ll[.x, ],
                                                               water_type = "perm",
                                                               water_raster = perm_raster))

#print(max(perm_water_distance$distance_to_perm_water))
#print(sum(is.na(perm_water_distance$distance_to_perm_water)))


sw_distance <- left_join(perm_water_distance, seasonal_water_distance, by = c("union", "hhcode"))

saveRDS(sw_distance, "/Users/suhi/Downloads/analysis_water_distance.RDS")
#saveRDS(sw_distance, paste0(here::here(), "/data/analysis_water_distance.RDS"))


