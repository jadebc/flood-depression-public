#########################################
# CRADLE depression and flooding analysis

# regression analysis of flooding and depression
#########################################
rm(list=ls())
source(paste0(here::here(), '/0-config.R'))
library(sp)
library(sf)
library(leaflet)
library(htmlwidgets)

baseline = readRDS(paste0(data_dir, "baseline_clean.RDS"))
#baseline = readRDS("/Users/suhi/Downloads/baseline_clean.RDS")

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

baseline_flooded_SPDF <- SpatialPointsDataFrame(
  coords = baseline[baseline$flood_cat!="No flood" & !is.na(baseline$flood_cat),
                    c("gpslongitudedegrees", "gpslatitudedegrees")],
  data = baseline[baseline$flood_cat!="No flood" & !is.na(baseline$flood_union),
                  c("flood_cat","dist_to_perm_water",
                    "dist_to_seasonal_water","EPDS","depression",
                    "gpslongitudedegrees", "gpslatitudedegrees")],
  proj4string = CRS("+init=epsg:4326")
)


# Read in Sirajganj district shapefile -------------------------------------
# bgd_adm4_shp <- st_read(paste0(box_shapefile_path, "bgd_shp_files/bgd_admbnda_adm4_bbs_20201113.shp"))%>% 
#   filter(ADM2_EN == "Sirajganj" | ADM2_EN == "Tangail")

bgd_adm4_shp <- st_read("/Users/suhi/Downloads/bgd_shp_files/bgd_admbnda_adm4_bbs_20201113.shp")%>% 
  filter(ADM2_EN == "Sirajganj" | ADM2_EN == "Tangail")

# center for each union
chauhali_shp = bgd_adm4_shp %>% filter(ADM3_EN == "Chauhali")
nagarpur_shp = bgd_adm4_shp %>% filter(ADM3_EN == "Nagarpur") %>% filter(ADM4_EN == "Bhara" | ADM4_EN == "Salimabad")
belkuchi_shp = bgd_adm4_shp %>% filter(ADM3_EN == "Belkuchi") %>% filter(ADM4_EN == "Bara Dhul")

comb_shp = rbind(chauhali_shp, nagarpur_shp, belkuchi_shp)
centers <- data.frame(rgeos::gCentroid(as(comb_shp, 'Spatial'), byid = TRUE))
centers$union <- comb_shp$ADM4_EN

# Define color palette -------------------------------------
flood_palette <- colorFactor(palette = c( "#cccbca", "#e4a9fc", "white"), 
                             domain = baseline$flood_cat)
dist_perm_palette <- colorNumeric(palette = "Blues", domain = baseline$dist_to_perm_water, reverse=T)
dist_seas_palette <- colorNumeric(palette = "Blues", domain = baseline$dist_to_seasonal_water, reverse=T)

EPDS_palette <- colorBin(
  palette = c("#91CF60", "#FFFFBF", "#FC8D59", "#D73027"),
  domain = c(0, 30),
  bins = c(0, 9, 10, 20, 30),
  na.color = "transparent"
)

# Map of permanent water -------------------------------------
map_perm_water <- leaflet(baseline_SPDF,
                            options = leafletOptions(zoomControl = TRUE)) %>%
  
  # add Esri World Imagery tiles
  addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
           attribution = "Esri World Imagery") %>% 


  # indicate study household locations color by distance to water
  addCircleMarkers(data = baseline_SPDF,
                 color = ~dist_perm_palette(dist_to_perm_water),
                 weight = 3.0,
                 fillOpacity = 1,
                 radius = 0.5) %>%

  addLegend("bottomright",
            pal = dist_perm_palette,
            values = ~baseline_SPDF$dist_to_perm_water,
            title = "km",
            opacity = 1
  )

# save map as html file
saveWidget(map_perm_water, file = paste0(figure_path, "map_perm_water.html"), selfcontained = TRUE)


# Map of seasonal water -------------------------------------
map_seas_water <- leaflet(baseline_SPDF,
                          options = leafletOptions(zoomControl = TRUE)) %>%
  
  # add Esri World Imagery tiles
  addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
           attribution = "Esri World Imagery") %>% 
  
  
  # indicate study household locations color by distance to water
  addCircleMarkers(data = baseline_SPDF,
                   color = ~dist_seas_palette(dist_to_seasonal_water),
                   weight = 3.0,
                   fillOpacity = 1,
                   radius = 0.5) %>%
  
  addLegend("bottomright",
            pal = dist_seas_palette,
            values = ~baseline_SPDF$dist_to_seasonal_water,
            title = "km",
            opacity = 1
  )

# save map as html file
saveWidget(map_seas_water, file = paste0(figure_path, "map_seas_water.html"), selfcontained = TRUE)

# Map of flooding -------------------------------------
map_flood_points <- leaflet(baseline_flooded_SPDF,
                          options = leafletOptions(zoomControl = TRUE)) %>%
  
  # add Esri World Imagery tiles
  addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
           attribution = "Esri World Imagery") %>% 
  
  
  # indicate study household locations color by distance to water
  addCircleMarkers(data = baseline_flooded_SPDF,
                   color = ~flood_palette(flood_cat),
                   weight = 3.0,
                   fillOpacity = 0.7,
                   radius = 0.5) %>%
  
  addLegend("bottomright",
            pal = flood_palette,
            values = ~baseline_flooded_SPDF$flood_cat,
            title = "Flooding",
            opacity = 1
  )

# save map as html file
saveWidget(map_flood_points, file = paste0(figure_path, "map_flood_points.html"), selfcontained = TRUE)


# Map of depression -------------------------------------
map_depression <- leaflet(baseline_SPDF,
                            options = leafletOptions(zoomControl = TRUE)) %>%
  
  # add Esri World Imagery tiles
  addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
           attribution = "Esri World Imagery") %>% 
  
  # indicate study household locations color by EPDS
  addCircleMarkers(data = baseline_SPDF,
                   color = ~EPDS_palette(EPDS),
                   weight = 3.0,
                   fillOpacity = 1,
                   radius = 0.5) %>%
  
  addLegend("bottomright", 
            pal = EPDS_palette, 
            values = ~baseline_SPDF$EPDS,
            title = "EPDS",
            opacity = 1
  )

# save map as html file
saveWidget(map_depression, file = paste0(figure_path, "map_depression.html"), selfcontained = TRUE)





