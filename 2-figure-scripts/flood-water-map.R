#################################################
# CRADLE depression and flooding analysis
# Creates maps of compound flooding 
# and household's distance to surface water 
################################################
rm(list=ls())
source(paste0(here::here(), '/0-config.R'))
library(sp)
library(sf)
library(terra)
library(leaflet)
library(htmlwidgets)
library(raster)
library(RColorBrewer)
#install.packages("webshot2")
library(webshot2)
#install.packages("magick")
library(magick)

baseline = readRDS(paste0(data_dir, "baseline_clean.RDS"))

baseline <- baseline %>% mutate(
  flood_compound = factor(
    ifelse(flood_compound == 1, "Flooded compound", "No flood"),
    levels = c("No flood", "Flooded compound")))

# ----------------------------------------
# map
# ----------------------------------------
# create spatial points dataframe 
baseline_SPDF <- SpatialPointsDataFrame(
  coords = baseline[, c("gpslongitudedegrees", "gpslatitudedegrees")],
  data = baseline[, c("flood_compound", "dist_to_perm_water",
                      "dist_to_seasonal_water", "EPDS", "depression", "month",
                      "gpslongitudedegrees", "gpslatitudedegrees")],
  proj4string = CRS("+init=epsg:4326")
)

# Read in Sirajganj district shapefile -------------------------------------
bgd_adm4_shp <- st_read(paste0(box_shapefile_path, "bgd_shp_files/bgd_admbnda_adm4_bbs_20201113.shp"))%>%
  filter(ADM2_EN == "Sirajganj" | ADM2_EN == "Tangail")

# find center for each union

chauhali_shp <- bgd_adm4_shp %>% filter(ADM3_EN == "Chauhali")
nagarpur_shp <- bgd_adm4_shp %>% filter(ADM3_EN == "Nagarpur" & ADM4_EN %in% c("Bhara", "Salimabad"))
belkuchi_shp <- bgd_adm4_shp %>% filter(ADM3_EN == "Belkuchi" & ADM4_EN == "Bara Dhul")

comb_shp <- rbind(chauhali_shp, nagarpur_shp, belkuchi_shp)

# calculate centroids
centers <- st_centroid(comb_shp)

# add the union names
centers$union <- comb_shp$ADM4_EN

# Define color palette -------------------------------------

flood_compound_palette <- colorFactor(palette = c( "white", "red"),
                             domain = baseline$flood_compound)

dist_perm_palette <- colorNumeric(palette = "Oranges", domain = baseline$dist_to_perm_water, reverse = TRUE)

dist_seas_palette <- colorNumeric(palette = "Oranges", domain = baseline$dist_to_seasonal_water, reverse=T)

# Map of permanent water -------------------------------------
# read in dry season raster
raster_d <- raster(file.path(box_shapefile_path, "rasters/Sirajganj_Tangail_DrySeason_water.tif"))

# reduce resolution
raster_d_reduced <- aggregate(raster_d, fact = 3, fun = mean, na.rm = TRUE)

# define color palette using range() to handle NA values
color_pal <- colorNumeric("Blues", domain = range(values(raster_d_reduced), na.rm = TRUE), na.color = "transparent")

# create map
map_perm_water <- leaflet(baseline_SPDF,
                          options = leafletOptions(zoomControl = FALSE)) %>%
  
  setView(lng = 89.70, lat = 24.2, zoom = 10) %>%  # adjust center and zoom
  
  # add the dry season raster layer to represent permanent water
  addRasterImage(raster_d_reduced, 
                 colors = color_pal, 
                 opacity = 0.8) %>%
  
  # add household markers, colored by distance to permanent water
  addCircleMarkers(data = baseline_SPDF,
                   color = ~dist_perm_palette(dist_to_perm_water),
                   weight = 3.0,
                   fillOpacity = 1,
                   radius = 3) %>%
  
  # add scale bar and legend
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = FALSE)) %>%
  
  addLegend("topleft",
            pal = dist_perm_palette,
            values = ~baseline_SPDF$dist_to_perm_water,
            title = htmltools::HTML("<div style='font-size: 16px; text-align: center;'>Distance to<br>permanent water<br>(km)</div>"),
            opacity = 1) %>% 

  # add text label (a)
  addControl(
    html = "<div style='font-size: 16px; font-weight: bold;'>(a)</div>",
    position = "topright") %>% 
  
  # Custom CSS to modify scale bar marker font size
  htmlwidgets::onRender("
    function(el, x) {
      var style = document.createElement('style');
      style.innerHTML = '.leaflet-control-scale-line { font-size: 16px !important; font-weight: bold; }';
      document.head.appendChild(style);
    }
  ")

# save the map as an HTML file
saveWidget(map_perm_water, file = paste0(figure_path, "map_perm_water.html"), selfcontained = TRUE)

webshot2::webshot(paste0(figure_path, "map_perm_water.html"),
                  file = paste0(figure_path, "map_perm_water.png"),
                  cliprect = "viewport",
                  vwidth = 430,
                  vheight = 420)

# Map of seasonal water -------------------------------------

# read in the wet season raster
raster_w <- raster(file.path(box_shapefile_path, "rasters/Sirajganj_Tangail_WetSeason_water.tif"))

# reduce resolution 
raster_w_reduced <- aggregate(raster_w, fact = 3, fun = mean, na.rm = TRUE)

# define color palette using range() to handle NA values
color_pal <- colorNumeric("Blues", domain = range(values(raster_w_reduced), na.rm = TRUE), na.color = "transparent")

# create map 
map_seas_water <- leaflet(baseline_SPDF,
                          options = leafletOptions(zoomControl = FALSE)) %>%
  
  setView(lng = 89.70, lat = 24.2, zoom = 10) %>%  # adjust center and zoom
  
  # add raster layer
  addRasterImage(raster_w_reduced, 
                 colors = color_pal, 
                 opacity = 0.8) %>%
  
  # add household markers
  addCircleMarkers(data = baseline_SPDF,
                   color = ~dist_seas_palette(dist_to_seasonal_water),
                   weight = 3.0,
                   fillOpacity = 1,
                   radius = 3) %>%
  
  # add scale bar and legend
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = FALSE)) %>%
  
  addLegend(position = "topleft", 
          pal = dist_seas_palette,
          values = ~baseline_SPDF$dist_to_seasonal_water,
          title = htmltools::HTML("<div style='font-size: 16px; text-align: center;'>Distance to<br>seasonal water<br>(km)</div>"),
          opacity = 1) %>% 

  # add text label (b)
  addControl(html = "<div style='font-size: 16px; font-weight: bold;'>(b)</div>",
    position = "topright") %>% 
  
  # Custom CSS to modify scale bar marker font size
  htmlwidgets::onRender("
    function(el, x) {
      var style = document.createElement('style');
      style.innerHTML = '.leaflet-control-scale-line { font-size: 16px !important; font-weight: bold; }';
      document.head.appendChild(style);
    }
  ")

# save map as html file
saveWidget(map_seas_water, file = paste0(figure_path, "map_seas_water.html"), selfcontained = TRUE)

webshot2::webshot(paste0(figure_path, "map_seas_water.html"),
                  file = paste0(figure_path, "map_seas_water.png"),
                  cliprect = "viewport",
                  vwidth = 430,
                  vheight = 420)

# Map of compound flooding -------------------------------------

# define the color palette from raster_w_reduced
color_pal <- colorNumeric("Blues", domain = range(values(raster_w_reduced), na.rm = TRUE), na.color = "transparent")

# filter data for flooded compound
baseline_SPDF_c <- baseline_SPDF[baseline_SPDF$flood_compound == 'Flooded compound',]

# Filter shapefile for district boundaries for Sirajganj and Tangail
district_boundaries <- bgd_adm4_shp %>%
  group_by(ADM2_EN) %>%
  summarise(geometry = st_union(geometry)) %>%
  filter(ADM2_EN %in% c("Sirajganj", "Tangail"))

# Calculate centroids for labels
district_centroids <- st_centroid(district_boundaries)

# Adjust label positions
district_centroids <- district_centroids %>%
  mutate(
    adjusted_longitude = case_when(
      ADM2_EN == "Sirajganj" ~ 89.6,  # Adjust Sirajganj label position (left)
      ADM2_EN == "Tangail" ~ 89.9),     # Adjust Tangail label position (right)

    adjusted_latitude = case_when(
      ADM2_EN == "Sirajganj" ~ 24.3,  # Adjust Sirajganj label position (up)
      ADM2_EN == "Tangail" ~ 24.3))     # Adjust Tangail label position (up)

# create map
map_compound_flooding <- leaflet(baseline_SPDF,
                                 options = leafletOptions(zoomControl = FALSE)) %>%
  
  setView(lng = 89.70, lat = 24.2, zoom = 10) %>%  # adjust center and zoom
  
  # add the wet season raster to the map with the pre-defined color palette
  addRasterImage(raster_w_reduced, 
                 colors = color_pal,   
                 opacity = 0.8) %>%
  
  # show households with compound flooding
  addCircleMarkers(data = baseline_SPDF_c,
                   color = "black",
                   stroke = TRUE,
                   fillColor = ~flood_compound_palette(flood_compound),
                   weight = 3.0,
                   fillOpacity = 0.5,
                   radius = 3) %>%
  
  # Add district boundaries
  addPolygons(data = district_boundaries,
              color = "black", 
              weight = 2, 
              fillOpacity = 0, 
              label = ~ADM2_EN, 
              popup = ~paste0("District: ", ADM2_EN)) %>%
  
  # Add district labels
  addLabelOnlyMarkers(data = district_centroids,
                      lng = ~adjusted_longitude, 
                      lat = ~adjusted_latitude,
                      label = ~ADM2_EN,
                      labelOptions = labelOptions(
                        noHide = TRUE, 
                        direction = "center", 
                        textOnly = TRUE, 
                        style = list(
                          "color" = "black",
                          "font-size" = "16px",
                          "font-weight" = "bold"))) %>% 

  # add scale bar
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = FALSE)) %>%
  
  # add legend
  addLegend(position = "topleft", 
            pal = flood_compound_palette,
            values = ~baseline_SPDF_c$flood_compound,
            title = htmltools::HTML("<div style='font-size: 16px; text-align: center;'>Compound flooding</div>"),
            opacity = 1) %>% 

  # add text label (c)
  addControl(html = "<div style='font-size: 16px; font-weight: bold;'>(c)</div>",
    position = "topright") %>% 
  
  # Custom CSS to modify scale bar marker font size
  htmlwidgets::onRender("
    function(el, x) {
      var style = document.createElement('style');
      style.innerHTML = '.leaflet-control-scale-line { font-size: 16px !important; font-weight: bold; }';
      document.head.appendChild(style);
    }
  ")

# save map as html file
saveWidget(map_compound_flooding, file = paste0(figure_path, "map_compound_flooding.html"), selfcontained = TRUE)

webshot2::webshot(paste0(figure_path, "map_compound_flooding.html"),
                  file = paste0(figure_path, "map_compound_flooding.png"), 
                  cliprect = "viewport",
                  vwidth = 430,
                  vheight = 420)

# Read the PNG files as images
img_perm <- image_read(paste0(figure_path, "map_perm_water.png"))
img_seas <- image_read(paste0(figure_path, "map_seas_water.png"))
img_flood <- image_read(paste0(figure_path, "map_compound_flooding.png"))

# Add white space to each image
img_perm_padded <- image_border(img_perm, color = "white", geometry = "10x0") 
img_seas_padded <- image_border(img_seas, color = "white", geometry = "10x0") 
img_flood_padded <- image_border(img_flood, color = "white", geometry = "10x0") 

# Combine the images into a horizontal panel grid with padding
panel_grid <- image_append(c(img_perm_padded, img_seas_padded, img_flood_padded))

image_write(panel_grid, paste0(figure_path, "map_flood_panel.png"), format = "png")
