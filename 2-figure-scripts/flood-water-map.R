#########################################
# CRADLE depression and flooding analysis

# regression analysis of flooding and depression
#########################################
rm(list=ls())
source(paste0(here::here(), '/0-config.R'))
library(sp)
library(sf)
library(terra)
library(leaflet)
library(htmlwidgets)
library(raster)
library(RColorBrewer)

baseline = readRDS(paste0(data_dir, "baseline_clean.RDS"))
#baseline = readRDS("/Users/suhi/Downloads/baseline_clean.RDS")


# min(baseline$gpslatitudedegrees) #24.02507
# max(baseline$gpslatitudedegrees) #24.37621
# min(baseline$gpslongitudedegrees) #89.65873
# max(baseline$gpslongitudedegrees) #89.88138

baseline <- baseline %>% mutate(
  flood_cat = as.factor(case_when(
    flood_compound == 1 ~ "Flooded compound",
    flood_union == 1 ~ "Flooded union",
    TRUE ~ "No flood"
  ))
) %>%
  mutate(flood_cat = factor(flood_cat, levels = c(
    "No flood", "Flooded compound", "Flooded union"
  )))


baseline <- baseline %>% mutate(
  flood_cat_compound = as.factor(case_when(
    flood_compound == 1 ~ "Flooded compound",
    TRUE ~ "No flood"
  ))
) %>% 
  mutate(flood_cat_compound = factor(flood_cat_compound, levels = c(
    "No flood", "Flooded compound"
  )))

baseline <- baseline %>% mutate(
  flood_cat_union = as.factor(case_when(
    flood_union == 1 ~ "Flooded union",
    TRUE ~ "No flood"
  ))
) %>% 
  mutate(flood_cat_union = factor(flood_cat_union, levels = c(
    "No flood", "Flooded union"
  )))


# ----------------------------------------
# map
# ----------------------------------------
# Spatial points dataframe
baseline_SPDF <- SpatialPointsDataFrame(
  coords = baseline[, c("gpslongitudedegrees", "gpslatitudedegrees")],
  data = baseline[, c("flood_cat","dist_to_perm_water",
                      "dist_to_seasonal_water","EPDS","depression","month",
                      "gpslongitudedegrees", "gpslatitudedegrees")],
  proj4string = CRS("+init=epsg:4326")
)

# Filter the baseline data frame once
baseline_filtered_compound <- baseline[!is.na(baseline$flood_cat_compound), ]

baseline_flooded_SPDF_c <- SpatialPointsDataFrame(
  coords = baseline_filtered_compound[, c("gpslongitudedegrees", "gpslatitudedegrees")],
  data = baseline_filtered_compound[, c("flood_cat_compound", "dist_to_perm_water",
                                        "dist_to_seasonal_water", "EPDS", "depression",
                                        "gpslongitudedegrees", "gpslatitudedegrees")],
  proj4string = CRS("+init=epsg:4326")
)

# Filter the baseline data frame once
baseline_filtered_union <- baseline[!is.na(baseline$flood_cat_union), ]

baseline_flooded_SPDF_u <- SpatialPointsDataFrame(
  coords = baseline_filtered_union[, c("gpslongitudedegrees", "gpslatitudedegrees")],
  data = baseline_filtered_union[, c("flood_cat_union", "dist_to_perm_water",
                                     "dist_to_seasonal_water", "EPDS", "depression",
                                     "gpslongitudedegrees", "gpslatitudedegrees")],
  proj4string = CRS("+init=epsg:4326")
)


# Read in Sirajganj district shapefile -------------------------------------
bgd_adm4_shp <- st_read(paste0(box_shapefile_path, "bgd_shp_files/bgd_admbnda_adm4_bbs_20201113.shp"))%>%
  filter(ADM2_EN == "Sirajganj" | ADM2_EN == "Tangail")

# bgd_adm4_shp <- st_read("/Users/suhi/Downloads/bgd_shp_files/bgd_admbnda_adm4_bbs_20201113.shp")%>% 
#   filter(ADM2_EN == "Sirajganj" | ADM2_EN == "Tangail")

# center for each union
chauhali_shp = bgd_adm4_shp %>% filter(ADM3_EN == "Chauhali")
nagarpur_shp = bgd_adm4_shp %>% filter(ADM3_EN == "Nagarpur") %>% filter(ADM4_EN == "Bhara" | ADM4_EN == "Salimabad")
belkuchi_shp = bgd_adm4_shp %>% filter(ADM3_EN == "Belkuchi") %>% filter(ADM4_EN == "Bara Dhul")

comb_shp = rbind(chauhali_shp, nagarpur_shp, belkuchi_shp)
centers <- data.frame(rgeos::gCentroid(as(comb_shp, 'Spatial'), byid = TRUE))
centers$union <- comb_shp$ADM4_EN

# Define color palette -------------------------------------

flood_compound_palette <- colorFactor(palette = c( "white", "red"),
                             domain = baseline$flood_cat_compound)

flood_union_palette <- colorFactor(palette = c( "white", "red"),
                                      domain = baseline$flood_cat_union)


dist_perm_palette <- colorNumeric(palette = "Oranges", domain = baseline$dist_to_perm_water, reverse = TRUE)

dist_seas_palette <- colorNumeric(palette = "Oranges", domain = baseline$dist_to_seasonal_water, reverse=T)

EPDS_palette <- colorBin(
  palette = c("#91CF60", "#FFFFBF", "#FC8D59", "#D73027"),
  domain = c(0, 30),
  bins = c(0, 9, 10, 20, 30),
  na.color = "transparent"
)

# Map of permanent water -------------------------------------

# read in the dry season raster
raster_spat <- rast(file.path(box_shapefile_path, "rasters/flood_depression_dry_season.tif"))

# reduce raster resolution
raster_spat_reduced <- aggregate(raster_spat, fact = 2) 

# convert to RasterLayer
raster_layer_reduced <- raster(raster_spat_reduced)

map_perm_water <- leaflet(baseline_SPDF,
                            options = leafletOptions(zoomControl = T)) %>%
  
  # add the dry season raster to the map
  addRasterImage(raster_layer_reduced, 
                 colors = colorNumeric("Blues", values(raster_layer_reduced), na.color = "transparent"), 
                 opacity = 0.8) %>%
  
  # indicate study household locations color by distance to water
  addCircleMarkers(data = baseline_SPDF,
                 color = ~dist_perm_palette(dist_to_perm_water),
                 weight = 3.0,
                 fillOpacity = 1,
                 radius = 3) %>%
  
  # add scale bar)
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = FALSE)) %>%

  addLegend("bottomright",
            pal = dist_perm_palette,
            values = ~baseline_SPDF$dist_to_perm_water,
            title = "km",
            opacity = 1
  )


# save map as html file
saveWidget(map_perm_water, file = paste0(figure_path, "map_perm_water.html"), selfcontained = TRUE)



# Map of seasonal water -------------------------------------

# read in the dry season raster
raster_spat_w <- rast(file.path(box_shapefile_path, "rasters/flood_depression_wet_season.tif"))

# reduce raster resolution
raster_spat_w_reduced <- aggregate(raster_spat_w, fact = 2) 

# convert to RasterLayer
raster_layer_w_reduced <- raster(raster_spat_w_reduced)


map_seas_water <- leaflet(baseline_SPDF,
                          options = leafletOptions(zoomControl = TRUE)) %>%
  
  # add the wet season raster to the map
  addRasterImage(raster_layer_w_reduced, 
                 colors = colorNumeric("Blues", values(raster_layer_w_reduced), na.color = "transparent"), 
                 opacity = 0.8) %>%
  
  # indicate study household locations color by distance to water
  addCircleMarkers(data = baseline_SPDF,
                   color = ~dist_seas_palette(dist_to_seasonal_water),
                   weight = 3.0,
                   fillOpacity = 1,
                   radius = 3) %>%
  
  # add scale bar)
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = FALSE)) %>%
  
  addLegend("bottomright",
            pal = dist_seas_palette,
            values = ~baseline_SPDF$dist_to_seasonal_water,
            title = "km",
            opacity = 1
  )

# save map as html file
saveWidget(map_seas_water, file = paste0(figure_path, "map_seas_water.html"), selfcontained = TRUE)




# Map of compound flooding -------------------------------------
map_flood_points_c <- leaflet(baseline_flooded_SPDF_c,
                          options = leafletOptions(zoomControl = TRUE)) %>%

  # add the dry season raster to the map
  addRasterImage(raster_layer_reduced, 
                 colors = colorNumeric("Blues", values(raster_layer_reduced), na.color = "transparent"), 
                 opacity = 0.8) %>%


  # indicate study household locations color by distance to water
  addCircleMarkers(data = baseline_flooded_SPDF_c,
                   color = ~flood_compound_palette(flood_cat_compound),
                   weight = 3.0,
                   fillOpacity = 0.7,
                   radius = 3) %>%
  
  # add scale bar)
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = FALSE)) %>%

  addLegend("bottomright",
            pal = flood_compound_palette,
            values = ~baseline_flooded_SPDF_c$flood_cat_compound,
            title = "Flooding",
            opacity = 1
  )

# save map as html file
saveWidget(map_flood_points_c, file = paste0(figure_path, "map_flood_points_c.html"), selfcontained = TRUE)




# Map of union flooding -------------------------------------
map_flood_points_u <- leaflet(baseline_flooded_SPDF_u,
                              options = leafletOptions(zoomControl = TRUE)) %>%
  
  # add the dry season raster to the map
  addRasterImage(raster_layer_reduced, 
                 colors = colorNumeric("Blues", values(raster_layer_reduced), na.color = "transparent"), 
                 opacity = 0.8) %>%
  
  
  # indicate study household locations color by distance to water
  addCircleMarkers(data = baseline_flooded_SPDF_u,
                   color = ~flood_union_palette(flood_cat_union),
                   weight = 3.0,
                   fillOpacity = 0.7,
                   radius = 3) %>%
  
  # add scale bar)
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = FALSE)) %>%
  
  addLegend("bottomright",
            pal = flood_union_palette,
            values = ~baseline_flooded_SPDF_u$flood_cat_union,
            title = "Flooding",
            opacity = 1
  )

# save map as html file
saveWidget(map_flood_points_u, file = paste0(figure_path, "map_flood_points_u.html"), selfcontained = TRUE)


# Map of depression -------------------------------------
map_depression <- leaflet(baseline_SPDF,
                            options = leafletOptions(zoomControl = TRUE)) %>%
  
  # add the dry season raster to the map
  addRasterImage(raster_layer_reduced, 
                 colors = colorNumeric("Blues", values(raster_layer_reduced), na.color = "transparent"), 
                 opacity = 0.8) %>%
  
  # indicate study household locations color by EPDS
  addCircleMarkers(data = baseline_SPDF,
                   color = ~EPDS_palette(EPDS),
                   weight = 3.0,
                   fillOpacity = 1,
                   radius = 3) %>%
  
  # add scale bar)
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = FALSE)) %>%
  
  addLegend("bottomright", 
            pal = EPDS_palette, 
            values = ~baseline_SPDF$EPDS,
            title = "EPDS",
            opacity = 1
  )

# save map as html file
saveWidget(map_depression, file = paste0(figure_path, "map_depression.html"), selfcontained = TRUE)





