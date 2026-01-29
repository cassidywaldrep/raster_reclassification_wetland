######################## Reclassifying specific raster values ##################

# Code: This code will take a specific raster value (in this case 14 - wetland), and reassign it to be marsh or swamp based on proximity. For ones not near a marsh or swamp, it will reclassify wetland as the next closest pixel

# Date: 1/28/2026
# Author: Cassidy Waldrep

# packages 
# test test testtttt


library(sf)
library(dplyr)
library(tidyverse)
library(ggmap)
library(tmap)

# habitat layer from doi: 10.5281/zenodo.15831967

habitat_raster <- terra::rast("data/nalcms_updated.tif")

# create legend for habitat raster (based on original habitat raster)

nalc_legend <- data.frame(
  Landcover = 0:25,
  class = c("Water", 
            "Temperate or sub-polar needleleaf forest", "Sub-polar taiga needleleaf forest",
            "Tropical or sub-tropical broadleaf evergreen forest", "Tropical or sub-tropical broadleaf deciduous forest",
            "Temperate or sub-polar broadleaf deciduous forest", "Mixed Forest",
            "Tropical or sub-tropical shrubland", "Temperate or sub-polar shrubland",
            "Tropical or sub-tropical grassland", "Temperate or sub-polar grassland",
            "Sub-polar or polar shrubland-lichen-moss", "Sub-polar or polar grassland-lichen-moss",
            "Sub-polar or polar barren-lichen-moss", "Wetland", "Cropland",
            "Barren lands", "Urban", "Water", "Snow and Ice", "Bog", "Fen",
            "Peatland", "Swamp", "Marsh", "Open shallow waters"
  )
) %>%
  
  # updated landcover classes based on my data
  
  mutate(category = case_when(
    Landcover == 0 ~ "water",
    Landcover %in% 1:6 ~ "forest",
    Landcover %in% 7:8 ~ "shrubland",
    Landcover %in% 9:12 ~ "grassland",
    Landcover == 14 ~ "wetland",
    Landcover %in% c(20, 21, 22) ~ "peatland",
    Landcover == 23 ~ "swamp",
    Landcover == 24 ~ "marsh",
    Landcover == 25 ~ "open_shallow_water",
    Landcover == 15 ~ "cropland",
    Landcover %in% c(13,16) ~ "barren",
    Landcover == 17 ~ "urban",
    Landcover == 18 ~ "water",
    Landcover == 19 ~ "snow_ice",
    TRUE ~ NA_character_
  ))

# this code will work well within a loop that takes the whole habitat raster, crops it, and then transforms the wetland type. Here's an example of it with just one bird.

bird_id <- "222758143_2023"   

# read in GPS data for graphing purposes. This also helps crop the raster

gps <- read.csv("data/broodrearing_data_withdates_nofly_readyfordbbm_23Jan2026.csv") %>%
  filter(birdid_year == bird_id) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(st_crs(habitat_raster))

# cropping the large habitat raster to just be the size of the gps points.
# you could also crop it using a contour

hab_crop <- terra::crop(
  habitat_raster,
  terra::vect(st_as_sfc(st_bbox(gps)))
)

# this function takes each pixel that is wetland (14), and will create a 3x3 square. Then, for all pixels in that window that aren't wetland, the proportion is calculated. If more than 50% of the surrounding period is a forest, make the wetland class a swamp. If more than 50% of the surrounding period is a shrubland or grassland, make the wetland class a marsh 


wetland_reclass_fun <- function(x) {
  
  center <- x[ceiling(length(x)/2)]
  
  # only operate on wetlands
  if (is.na(center) || center != 14) {
    return(center)
  }
  
  nbrs <- x[-ceiling(length(x)/2)]
  
  nbrs <- nbrs[nbrs != 14]
  
  
  forest_prop <- mean(nbrs %in% 1:6, na.rm = TRUE)
  shrub_prop  <- mean(nbrs %in% 7:12, na.rm = TRUE)
  
  if (!is.na(forest_prop) && forest_prop > 0.5) {
    return(23)   # swamp
  }
  
  if (!is.na(shrub_prop) && shrub_prop > 0.5) {
    return(24)   # marsh
  }
  
  return(14)
}

hab_context <- terra::focal(
  hab_crop,
  w = matrix(1, 3, 3), # creates a 3x3 matrix
  fun = wetland_reclass_fun,
  na.policy = "omit",
  fillvalue = NA
)

# if there are still pixels that are wetland, do the following 
# this happens if the pixels are surrounded by non forest/shrub/grassland pixels

has_wetland <- any(terra::values(hab_context) == 14, na.rm = TRUE)

if (has_wetland) {
  
  habitat_no_wetland <- hab_context
  
  # assign remaining wetland pixels to be NA
  
  habitat_no_wetland[hab_context == 14] <- NA
  
  # find the closest pixel and assign it that value 
  
  d <- terra::distance(habitat_no_wetland, values = TRUE)
  
  hab_final <- hab_context
  
  hab_final[hab_context == 14] <- d[hab_context == 14]
  
} else {
  
  hab_final <- hab_context
  
}

# graphing code

tmap_mode("view")

# creating colors for the map

levels_table <- nalc_legend %>% 
  select(Landcover, category) %>% 
  distinct()

hab_final <- terra::categories(hab_final, value = levels_table)
hab_crop  <- terra::categories(hab_crop, value = levels_table)

landcover_colors <- c(
  "water"              = "#4363d8", # Blue
  "forest"             = "#228b22", # Forest Green
  "shrubland"          = "#808000", # Olive
  "grassland"          = "#ffe119", # Yellow
  "barren"             = "#a9a9a9", # Grey
  "wetland"            = "#469990", # Teal
  "cropland"           = "#f58231", # Orange
  "urban"              = "#e6194b", # Red
  "snow_ice"           = "#ffffff", # White
  "peatland"           = "#911eb4", # Purple
  "swamp"              = "#800000", # Maroon
  "marsh"              = "#3cb44b", # Lime Green
  "open_shallow_water" = "#42d4f4"  # Cyan
)

# create palette for mapping 

pal_all <- landcover_colors

tm_basemap("Esri.WorldImagery") +
  
  # graph the final layer (without wetland)
  
  tm_shape(hab_final) +
  tm_raster(
    title = "updated",
    style = "cat",
    col.scale = tm_scale_categorical(drop.levels = FALSE),
    palette = pal_all)  +
  
  # graph the original cropped layer (originally included wetland)
  
  tm_shape(hab_crop) +
  tm_raster(
    title = "original habitat",
    style = "cat",
    col.scale = tm_scale_categorical(drop.levels = FALSE),
    palette = pal_all) +
  
  # graph gps points for reference
  
  tm_shape(gps) +
  tm_dots(size = 0.3, fill = "blue") 

