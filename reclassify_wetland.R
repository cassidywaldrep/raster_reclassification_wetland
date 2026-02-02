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

#---------------------------------------------------------------------------#
###### Graphing one bird to see the map
#---------------------------------------------------------------------------#

gps <- read_csv("data/broodrearing_data_withdates_nofly_readyfordbbm_23Jan2026.csv")

contour <- st_read("results/brood_contours_26Jan26.gpkg")

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
) 

pal_all <- c(
  "#2b83ba",  # 0 Water
  "#006430",  # 1 Temperate needleleaf forest
  "#00441b",  # 2 Taiga needleleaf forest
  "#228B22",  # 3 Tropical evergreen forest
  "#66a68e",  # 4 Tropical deciduous forest
  "#41ab5d",  # 5 Temperate deciduous forest
  "#06402B",  # 6 Mixed forest
  "#b8e134",  # 7 Tropical shrubland
  "#ffdba0",  # 8 Temperate shrubland
  "#ffff99",  # 9 Tropical grassland
  "#d9ef8b",  #10 Temperate grassland
  "#9e9ac8",  #11 Polar shrub / lichen
  "#bcbddc",  #12 Polar grass / lichen
  "#dadaeb",  #13 Polar barren
  "#00254d",  #14 Wetland
  "#fdae61",  #15 Cropland
  "#bdbdbd",  #16 Barren lands
  "#d73027",  #17 Urban
  "#ffffff",  #19 Snow and Ice
  "#8c6bb1",  #20 Bog
  "#9ebcda",  #21 Fen
  "#7a0177",  #22 Peatland
  "#997950",  #23 Swamp
  "#351E10",  #24 Marsh
  "#92c5de"   #25 Open shallow waters
)


tmap_mode("view")
gps_id <- gps %>%
    filter(birdid_year == bird) %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
    st_transform(st_crs(habitat_raster))

contour_id <- contour %>%
  filter(id == bird, 
         level == "0.95") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(st_crs(habitat_raster))  
  
hab_crop <- terra::crop(
  habitat_raster,
  terra::vect(st_as_sfc(st_bbox(gps_id))))

levels(habitat_raster) <- data.frame(
  ID    = nalc_legend$Landcover,
  class = nalc_legend$class
)


tm_basemap("Esri.WorldImagery") +
  
  # graph the final layer (without wetland)
  
  tm_shape(hab_crop) +
  tm_raster(
    title = "habitat",
    style = "cat",
    col.scale = tm_scale_categorical(drop.levels = FALSE),
    palette = pal_all) +
  
  # graph 95% contour
  
  tm_shape(contour_id) + 
  tm_polygons(alpha = 0.2) +
  
  # graph gps points for reference
  
  tm_shape(gps_id) +
  tm_dots(size = 0.2, 
          fill = "blue") 



#---------------------------------------------------------------------------#
###### current methodology
#---------------------------------------------------------------------------#

layer <- terra::rast("data/map_data/commondata/raster_data/IVC_Groups_v2022_1pt0_WashPatch.tif")

prop_bird <- read_csv("data/fullrun_brooding_habitatproportions_3levels_26Jan26.csv") %>%
  filter(contour == "95", 
         !id %in% c("213720102_2022", "225744854_2025"))

prop_bird %>%
  filter(Landcover == "wetland")
  
birds <- unique(prop_bird$id)

updated_brooding_proportions <- list()

for (bird in birds) {
  
gps_id <- gps %>%
  filter(birdid_year == bird) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(st_crs(habitat_raster))

gps_buffered <- st_buffer(st_transform(gps_id, st_crs(habitat_raster)), dist = 1000)

crop_extent <- gps_buffered %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  terra::vect()

layer_crop <- terra::crop(layer, terra::vect(st_transform(st_as_sfc(st_bbox(crop_extent)), st_crs(layer))))

hab_crop <- terra::crop(habitat_raster, terra::vect(st_transform(st_as_sfc(st_bbox(crop_extent)), st_crs(habitat_raster))))

layer_project <- terra::project(layer_crop, hab_crop, method = "near")

# so, first find habitat with the raster 
contour_id <- contour %>%
  filter(id == bird, 
         level == "0.95") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(st_crs(habitat_raster))  

contour_vect <- terra::vect(contour_id)

# this is the original wetland layer...what do we have??

hab_values <- terra::extract(hab_crop, contour_vect, weights = TRUE) %>% 
  mutate(row_id = row_number())


wetland_mask <- hab_crop == 14

# 2. Apply that mask to your detailed layer
# This "burns" through the top layer to see what's underneath

detailed_wetlands <- terra::mask(layer_project, wetland_mask, maskvalues = FALSE)

terra::activeCat(detailed_wetlands) <- "ClassName"

# 3. Now extract the detailed info from that masked layer using your contour

extracted_comparison <- terra::extract(detailed_wetlands, contour_vect, weights = TRUE) %>%
  mutate(new_hab = case_when(
    grepl("swamp", ClassName, ignore.case = TRUE) ~ "24",
    grepl("marsh", ClassName, ignore.case = TRUE) ~ "23",
    grepl("forest|woodland|tree|pine", ClassName, ignore.case = TRUE) ~ "2",
    grepl("shrubland", ClassName, ignore.case = TRUE) ~ "7",
    grepl("grassland|prairie|meadow", ClassName, ignore.case = TRUE) ~ "9",
    grepl("fen|bog", ClassName, ignore.case = TRUE) ~ "22",
    grepl("agriculture", ClassName, ignore.case = TRUE) ~ "15",
    grepl("developed", ClassName, ignore.case = TRUE) ~ "17",
    grepl("rock|bluff|beach|cliff", ClassName, ignore.case = TRUE) ~ "13", # barren
    grepl("water", ClassName, ignore.case = TRUE) ~ "18", # barren
    TRUE ~ "Other" # Catches anything that doesn't match the above
  )) %>% 
  mutate(row_id = row_number()) %>%
  select(-ID, -weight)

hab_df <- as.data.frame(hab_values) 

final_combined <- hab_df %>%
  left_join(extracted_comparison, by = "row_id") %>%
  mutate(reclass = ifelse(reclass == 14 & new_hab != "Other", 
                           new_hab, 
                           as.character(reclass))) %>%
  select(reclass, weight)

# final_combined now has updated layers
updated <- final_combined %>%
  mutate(
    Landcover = case_when(
      reclass == 0 ~ "water",
      reclass %in% 1:6 ~ "forest",
      reclass %in% 7:8 ~ "shrubland",
      reclass %in% 9:12 ~ "grassland",
      reclass == 14 ~ "wetland",
      reclass %in% c(20,21,22) ~ "peatland",
      reclass == 23 ~ "swamp",
      reclass == 24 ~ "marsh",
      reclass == 25 ~ "open_shallow_water",
      reclass == 15 ~ "cropland",
      reclass %in% c(13,16) ~ "barren",
      reclass == 17 ~ "urban",
      reclass == 18 ~ "water",
      reclass == 19 ~ "snow_ice",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(Landcover) %>%
  summarise(weighted_sum = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    total_weights = sum(weighted_sum),
    percentage = (weighted_sum / total_weights) * 100) %>%
  select(Landcover, percentage) %>%
  mutate(id = bird)

wetland_row <- updated %>%
  filter(Landcover == "wetland")

has_wetland <- any(nrow(wetland_row))

# Create a copy of the cropped habitat to modify
hab_refined <- hab_crop

hab_extraction <- terra::extract(hab_crop, contour_vect, cells = TRUE, weights = TRUE) %>%
  mutate(row_id = row_number())


# Update the cells in the raster using the results from your join/mutate
# We use the cell indices extracted earlier
hab_refined[hab_extraction$cell] <- as.numeric(final_combined$reclass)

if (has_wetland) {

wetland_majority_fun <- function(x) {
  center_idx <- ceiling(length(x) / 2)
  center <- x[center_idx]
  
  # Ignore NAs or non-wetland pixels
  if (is.na(center) || center != 14) {
    return(center)
  }
  
  # Get all neighbors INCLUDING other wetlands (14s)
  # But we still remove NAs to avoid calculation errors
  nbrs <- x[!is.na(x)]
  
  # Calculate frequencies
  counts <- table(nbrs)
  
  # Find the most frequent value
  # names(counts) gives the class IDs, counts gives the frequency
  majority_class <- as.numeric(names(counts)[which.max(counts)])
  
  return(majority_class)
}

hab_context <- terra::focal(
  hab_refined,
  w = matrix(1, 5, 5), 
  fun = wetland_majority_fun
)

final_combined <- terra::extract(hab_context, contour_vect, weights = TRUE)

  # 2. Convert to the final summary table
  final_habitat_summary <- final_combined %>%
    # rename the raster column (terra often names it after the layer) to 'reclass'
    rename(reclass = 2) %>%
    mutate(
      Landcover = case_when(
        reclass == 0 ~ "water",
        reclass %in% 1:6 ~ "forest",
        reclass %in% 7:8 ~ "shrubland",
        reclass %in% 9:12 ~ "grassland",
        reclass == 14 ~ "wetland", # Should be 0% if the fill-in ran!
        reclass %in% c(20,21,22) ~ "peatland",
        reclass == 23 ~ "swamp",
        reclass == 24 ~ "marsh",
        reclass == 25 ~ "open_shallow_water",
        reclass == 15 ~ "cropland",
        reclass %in% c(13,16) ~ "barren",
        reclass == 17 ~ "urban",
        reclass == 18 ~ "water",
        reclass == 19 ~ "snow_ice",
        TRUE ~ "Other"
      )
    ) %>%
    group_by(Landcover) %>%
    summarise(weighted_sum = sum(weight, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      total_weights = sum(weighted_sum),
      percentage = (weighted_sum / total_weights) * 100,
      id = bird # Link it back to the specific bird
    ) %>%
    select(id, Landcover, percentage)

} else {
  
final_habitat_summary <- updated
  
  
}



# 
# # if it has less than 10% wetlands, make it the habitat of the closest pixel
# 
# hab_extraction <- terra::extract(hab_crop, contour_vect, cells = TRUE, weights = TRUE) %>%
#   mutate(row_id = row_number())
# 
# hab_final <- hab_crop
# 
# wetland_row <- updated %>% 
#   filter(Landcover == "wetland")
# 
# has_wetland <- any(nrow(wetland_row) > 0 && wetland_row$percentage < 10)
# 
# 
# 
# if (has_wetland) {
#   
#   # this is taking our new reclass values, and assigning them into hab_final
#   
#   hab_final[hab_extraction$cell] <- as.numeric(final_combined$reclass)
#   
#   # assign remaining wetland pixels to be NA
#   
#   hab_fill_prep <- hab_final
#   hab_fill_prep[hab_final == 14] <- NA
#   
#   # find the closest pixel and assign it that value 
#   
#   d <- terra::distance(hab_fill_prep, values = TRUE)
#   
#   hab_final_updated <- hab_final
#   
#   hab_final_updated[hab_final == 14] <- d[hab_final == 14]
#   
#   final_combined <- terra::extract(hab_final_updated, contour_vect, weights = TRUE)
#   
#   # 2. Convert to the final summary table
#   final_habitat_summary <- final_combined %>%
#     # rename the raster column (terra often names it after the layer) to 'reclass'
#     rename(reclass = 2) %>% 
#     mutate(
#       Landcover = case_when(
#         reclass == 0 ~ "water",
#         reclass %in% 1:6 ~ "forest",
#         reclass %in% 7:8 ~ "shrubland",
#         reclass %in% 9:12 ~ "grassland",
#         reclass == 14 ~ "wetland", # Should be 0% if the fill-in ran!
#         reclass %in% c(20,21,22) ~ "peatland",
#         reclass == 23 ~ "swamp",
#         reclass == 24 ~ "marsh",
#         reclass == 25 ~ "open_shallow_water",
#         reclass == 15 ~ "cropland",
#         reclass %in% c(13,16) ~ "barren",
#         reclass == 17 ~ "urban",
#         reclass == 18 ~ "water",
#         reclass == 19 ~ "snow_ice",
#         TRUE ~ "Other"
#       )
#     ) %>%
#     group_by(Landcover) %>%
#     summarise(weighted_sum = sum(weight, na.rm = TRUE), .groups = "drop") %>%
#     mutate(
#       total_weights = sum(weighted_sum),
#       percentage = (weighted_sum / total_weights) * 100,
#       id = bird # Link it back to the specific bird
#     ) %>%
#     select(id, Landcover, percentage)
#   
# } else {
#   
#   final_habitat_summary <- updated
# }
# 
# updated_brooding_proportions[[bird]] <- final_habitat_summary
# 
# print(bird)

}

updated_brooding_proportions <- dplyr::bind_rows(updated_brooding_proportions)


updated_brooding_proportions %>%
  filter(Landcover == "wetland")


#---------------------------------------------------------------------------#
###### Graphing
#---------------------------------------------------------------------------#


habitat_final_map <- hab_crop
final_combined$reclass_num <- as.numeric(final_combined$reclass)
cells_to_update <- terra::extract(hab_crop, contour_vect, cells=TRUE, weights = TRUE)$cell
habitat_final_map[cells_to_update] <- final_combined$reclass_num


library(tmap)

tmap_mode("view")

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
) 

pal_all <- c(
  "#2b83ba",  # 0 Water
  "#006430",  # 1 Temperate needleleaf forest
  "#00441b",  # 2 Taiga needleleaf forest
  "#228B22",  # 3 Tropical evergreen forest
  "#66a68e",  # 4 Tropical deciduous forest
  "#41ab5d",  # 5 Temperate deciduous forest
  "#06402B",  # 6 Mixed forest
  "#b8e134",  # 7 Tropical shrubland
  "#ffdba0",  # 8 Temperate shrubland
  "#ffff99",  # 9 Tropical grassland
  "#d9ef8b",  #10 Temperate grassland
  "#9e9ac8",  #11 Polar shrub / lichen
  "#bcbddc",  #12 Polar grass / lichen
  "#dadaeb",  #13 Polar barren
  "#00254d",  #14 Wetland
  "#fdae61",  #15 Cropland
  "#bdbdbd",  #16 Barren lands
  "#d73027",  #17 Urban
  "#ffffff",  #19 Snow and Ice
  "#8c6bb1",  #20 Bog
  "#9ebcda",  #21 Fen
  "#7a0177",  #22 Peatland
  "#997950",  #23 Swamp
  "#351E10",  #24 Marsh
  "#92c5de"   #25 Open shallow waters
)


levels_table <- data.frame(ID = 0:25, class = nalc_legend$class)
levels(hab_crop) <- levels_table
levels(hab_context) <- levels_table

tm_basemap("Esri.WorldImagery") +
  
  # graph the final layer (without wetland)
  
  tm_shape(hab_crop) +
  tm_raster(
    title = "original raster",
    style = "cat",
    col.scale = tm_scale_categorical(drop.levels = FALSE),
    palette = pal_all) +
  
  
  tm_shape(hab_context) +
  tm_raster(
    title = "updated raster",
    style = "cat",
    col.scale = tm_scale_categorical(drop.levels = FALSE),
    palette = pal_all) +
  
  # graph 95% contour
  
  tm_shape(contour_id) + 
  tm_polygons(alpha = 0.2) +
  
  # graph gps points for reference
  
  tm_shape(gps_id) +
  tm_dots(size = 0.2, 
          fill = "blue") 




######## playing around with neighbors ######

# 1. Define the swamp ID (Update this to match your actual NALC code for swamp)
swamp_val <- nalc_legend %>% 
  filter(class == "Swamp") %>% 
  pull(Landcover)

# 2. Results Container
results_list <- list()

swamp_birds <- brood_withswamp$id


for (bird in swamp_birds) {
  
  # Filter contour for the specific bird
  bird_poly <- contour %>%
    filter(id == bird, level == "0.95") %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
    st_transform(st_crs(habitat_raster)) %>%
    st_combine() %>% st_cast("POLYGON") # Ensure it's a valid polygon for masking
  
  # Crop and Mask habitat to the bird's home range
  bird_hab <- terra::crop(habitat_raster, terra::vect(bird_poly), mask = TRUE)
  
  # Create a binary mask (1 = Swamp, NA = Not Swamp)
  swamp_mask <- bird_hab == swamp_val
  swamp_mask[swamp_mask == 0] <- NA
  
  # 3. Focal Analysis: Identify cells touching swamp (3x3 window)
  # This identifies any pixel that is a neighbor to a swamp pixel
  neighbor_weight <- matrix(1, nrow = 3, ncol = 3)
  swamp_neighbors <- terra::focal(swamp_mask, w = neighbor_weight, fun = "sum", na.rm = TRUE)
  
  # Mask the original habitat raster by the neighbor areas
  # (Only keeps pixels that are adjacent to at least one swamp pixel)
  adjacent_habitat <- terra::mask(bird_hab, swamp_neighbors)
  
  # don't include swamp or wetland in the final proportion count...
  
  adjacent_habitat[adjacent_habitat == swamp_val] <- NA
  adjacent_habitat[adjacent_habitat == 14] <- NA
  
  
  # 4. Calculate Proportions
  freq_table <- terra::freq(adjacent_habitat) %>%
    as.data.frame() %>%
    mutate(
      bird_id = bird,
      proportion = count / sum(count)
    ) %>%
    left_join(nalc_legend, by = c("value" = "Landcover"))
  
  results_list[[bird]] <- freq_table
}

# Combine all results into one dataframe
swamp_prop <- bind_rows(results_list)

swamp_prop %>%
  group_by(class) %>%
  summarize(mean = mean(proportion), 
            sd = sd(proportion), 
            count = n()) %>%
  arrange(-mean)



brood_withmarsh<- read_csv("data/fullrun_brooding_habitatproportions_3levels_26Jan26.csv") %>%
  filter(contour == "95") %>%
  dplyr::select(Landcover, percentage, id) %>%
  group_by(id) %>%
  filter(Landcover == "marsh") %>%
  ungroup()

marsh_birds <- unique(brood_withmarsh$id)

# 1. Define the swamp ID (Update this to match your actual NALC code for swamp)
marsh_val <- nalc_legend %>% 
  filter(class == "Marsh") %>% 
  pull(Landcover)

# 2. Results Container
results_list <- list()

for (bird in marsh_birds) {
  
  # Filter contour for the specific bird
  bird_poly <- contour %>%
    filter(id == bird, level == "0.95") %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
    st_transform(st_crs(habitat_raster)) %>%
    st_combine() %>% st_cast("POLYGON") # Ensure it's a valid polygon for masking
  
  # Crop and Mask habitat to the bird's home range
  bird_hab <- terra::crop(habitat_raster, terra::vect(bird_poly), mask = TRUE)
  
  # Create a binary mask (1 = Swamp, NA = Not Swamp)
  marsh_mask <- bird_hab == marsh_val
  marsh_mask[marsh_mask == 0] <- NA
  
  # 3. Focal Analysis: Identify cells touching swamp (3x3 window)
  # This identifies any pixel that is a neighbor to a swamp pixel
  neighbor_weight <- matrix(1, nrow = 3, ncol = 3)
  marsh_neighbors <- terra::focal(marsh_mask, w = neighbor_weight, fun = "sum", na.rm = TRUE)
  
  # Mask the original habitat raster by the neighbor areas
  # (Only keeps pixels that are adjacent to at least one swamp pixel)
  adjacent_habitat <- terra::mask(bird_hab, marsh_neighbors)
  
  # don't include marsh or wetland in the final proportion count...
  
  adjacent_habitat[adjacent_habitat == marsh_val] <- NA
  adjacent_habitat[adjacent_habitat == 14] <- NA
  
  
  # 4. Calculate Proportions
  freq_table <- terra::freq(adjacent_habitat) %>%
    as.data.frame() %>%
    mutate(
      bird_id = bird,
      proportion = count / sum(count)
    ) %>%
    left_join(nalc_legend, by = c("value" = "Landcover"))
  
  results_list[[bird]] <- freq_table
}

# Combine all results into one dataframe
marsh_prop <- bind_rows(results_list)

marsh_prop %>%
  group_by(class) %>%
  summarize(mean = mean(proportion), 
            sd = sd(proportion), 
            count = n()) %>%
  arrange(-mean)




## wetland birds

brood_withpeatland<- read_csv("data/fullrun_brooding_habitatproportions_3levels_26Jan26.csv") %>%
  filter(contour == "95") %>%
  dplyr::select(Landcover, percentage, id) %>%
  group_by(id) %>%
  filter(Landcover == "peatland") %>%
  ungroup()

# 1. Define the  ID (Update this to match your actual NALC code for peatland)
peatland_val <- nalc_legend %>% 
  filter(Landcover %in% 20:22) %>% 
  pull(Landcover)

peatland_birds <- unique(brood_withpeatland$id)

# 2. Results Container
results_list <- list()

for (bird in peatland_birds) {
  
  # Filter contour for the specific bird
  bird_poly <- contour %>%
    filter(id == bird, level == "0.95") %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
    st_transform(st_crs(habitat_raster)) %>%
    st_combine() %>% st_cast("POLYGON") # Ensure it's a valid polygon for masking
  
  # Crop and Mask habitat to the bird's home range
  bird_hab <- terra::crop(habitat_raster, terra::vect(bird_poly), mask = TRUE)
  
  # Create a binary mask (1 = Swamp, NA = Not Swamp)
  wet_mask <- bird_hab == peatland_val
  wet_mask[wet_mask == 0] <- NA
  
  # 3. Focal Analysis: Identify cells touching swamp (3x3 window)
  # This identifies any pixel that is a neighbor to a swamp pixel
  neighbor_weight <- matrix(1, nrow = 3, ncol = 3)
  wet_neighbors <- terra::focal(wet_mask, w = neighbor_weight, fun = "sum", na.rm = TRUE)
  
  # Mask the original habitat raster by the neighbor areas
  # (Only keeps pixels that are adjacent to at least one swamp pixel)
  adjacent_habitat <- terra::mask(bird_hab, wet_neighbors)
  
  # don't include marsh or wetland in the final proportion count...
  
  adjacent_habitat[adjacent_habitat == 14] <- NA
  
  
  # 4. Calculate Proportions
  freq_table <- terra::freq(adjacent_habitat) %>%
    as.data.frame() %>%
    mutate(
      bird_id = bird,
      proportion = count / sum(count)
    ) %>%
    left_join(nalc_legend, by = c("value" = "Landcover"))
  
  results_list[[bird]] <- freq_table
}

# Combine all results into one dataframe
peatland_prop <- bind_rows(results_list)

peatland_prop %>%
  group_by(class) %>%
  summarize(mean = mean(proportion), 
            sd = sd(proportion), 
            count = n()) %>%
  arrange(-count)
