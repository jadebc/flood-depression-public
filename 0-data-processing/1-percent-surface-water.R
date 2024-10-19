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
library(readstata13)


# # find study area boundaries 
# bgd_adm4_shp <- st_read("/Users/suhi/downloads/bgd_shp_files/bgd_admbnda_adm4_bbs_20201113.shp") %>% 
#   filter(ADM2_EN == "Sirajganj" | ADM2_EN == "Tangail")

bgd_adm4_shp <- st_read(paste0(box_shapefile_path, "bgd_shp_files/bgd_admbnda_adm4_bbs_20201113.shp")) %>%
 filter(ADM2_EN == "Sirajganj" | ADM2_EN == "Tangail")

coords <- as.data.frame(st_coordinates(bgd_adm4_shp))

max(coords$X) # 90.30083
min(coords$X) # 89.24715
max(coords$Y) # 24.78787
min(coords$Y) # 23.96359

# # read in seasonality raster for Bangladesh
seasonality_raster = terra::rast("/Users/suhi/Downloads/seasonality_raw.tif") # data resolution 29.48906m * 29.48906m
#seasonality_raster = terra::rast(paste0(box_shapefile_path, "seasonality_raw.tif"))

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

# writeRaster(permanent_raster, "/Users/suhi/Downloads/permanent_raster_epsg.tif", 
#                                       filetype="GTiff", overwrite=TRUE)

# writeRaster(seasonal_raster, "/Users/suhi/Downloads/seasonal_raster_epsg.tif", 
#             filetype="GTiff", overwrite=TRUE)

writeRaster(permanent_raster, paste0(box_shapefile_path, "rasters/permanent_raster_epsg.tif"),
           filetype="GTiff", overwrite=TRUE)

writeRaster(seasonal_raster, paste0(box_shapefile_path, "rasters/seasonal_raster_epsg.tif"),
           filetype="GTiff", overwrite=TRUE)


# perm_raster <- rast("/Users/suhi/Downloads/permanent_raster_epsg.tif")
# seasonal_raster <- rast("/Users/suhi/Downloads/seasonal_raster_epsg.tif")

perm_raster <- rast(paste0(box_shapefile_path, "rasters/permanent_raster_epsg.tif"))
seasonal_raster <- rast(paste0(box_shapefile_path, "rasters/seasonal_raster_epsg.tif"))

# seasonal_points = as.points(seasonal_raster)
# seasonal_sppoints = SpatialPoints(raster_points[, 1:2], proj4string=CRS('+proj=longlat +datum=WGS84'))


###########################################################
########### Detect Proportion of Surface Water ############
###########################################################

ll <- read.dta13("/Users/suhi/Downloads/CRADLE_Baseline_data.dta", convert.factors=F) %>%
  dplyr::select(dataid, gpslatitudedegrees, gpslongitudedegrees, q1_3) %>%
  rename(hhcode = dataid, lat = gpslatitudedegrees, long = gpslongitudedegrees, union = q1_3)

# ll <- read.dta13(paste0(box_path_cradle_data,"Baseline/CRADLE_Baseline_data.dta"), convert.factors=F) %>%
#   dplyr::select(dataid, gpslatitudedegrees, gpslongitudedegrees, q1_3) %>%
#   rename(hhcode = dataid, lat = gpslatitudedegrees, long = gpslongitudedegrees, union = q1_3)

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

sw_df <- left_join(perm_water_df, seasonal_water_df, by = c("union", "hhcode")) %>% 
  dplyr::select(hhcode, union, starts_with("prop_")) %>% 
  rename(dataid = hhcode)

#saveRDS(sw_df, "/Users/suhi/Downloads/analysis_prop_surface_water.RDS")
saveRDS(sw_df, paste0(data_dir, "analysis_prop_surface_water.RDS"))


# drop 10 for perm water 

