######################## Reclassifying specific raster values ##################

# Code: This code will take a specific raster value (in this case 14 - wetland), and reassign it based on two rules. First, it overlays a new habitat raster (https://www.arcgis.com/home/item.html?id=66c13612635d4ee9bd4d6500cf462e7f), extracts those and replaces the old wetland class. Then, it recalculates proportions and for birds with less than 10% wetland, it assigns them pixels based on 1) the closest pixel or 2) the pixels within a 5x5 radius (only works for some).

# Date: 1/28/2026
# Author: Cassidy Waldrep

# packages 

library(sf)
library(dplyr)
library(tidyverse)
library(ggmap)
library(tmap)

## reading in data and contours

gps <- read_csv("data/broodrearing_data_withdates_nofly_readyfordbbm_23Jan2026.csv")

contour <- st_read("results/brood_contours_26Jan26.gpkg")

# habitat layer from doi: 10.5281/zenodo.15831967

habitat_raster <- terra::rast("nalcms_updated.tif")

#---------------------------------------------------------------------------#
###### running wetland update reclassification 
#---------------------------------------------------------------------------#

# reading in new habitat layer
# this needs to be downloaded from https://www.arcgis.com/home/item.html?id=66c13612635d4ee9bd4d6500cf462e7f

layer <- terra::rast("map_data/commondata/raster_data/IVC_Groups_v2022_1pt0_WashPatch.tif")

# reading in all birds, while filtering some out I don't want. 

prop_bird <- read_csv("data/fullrun_brooding_habitatproportions_3levels_26Jan26.csv") %>%
  filter(contour == "95", 
         !id %in% c("213720102_2022", "225744854_2025"))

# creating a list of all the birds for the analysis
  
birds <- unique(prop_bird$id)

# starting a list to store data for this loop 

updated_brooding_proportions <- list()

for (bird in birds) {
  
##### STEP ONE ###########
  
  # the first step is to overlay the wetland pixels with the new layer to try and extract additional information. 
  
# first, extract gps data for the specific bird you're looking at 
  
gps_id <- gps %>%
  filter(birdid_year == bird) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(st_crs(habitat_raster))

# buffer points by 1 km for cropping of rasters

gps_buffered <- st_buffer(st_transform(gps_id, st_crs(habitat_raster)), dist = 1000)

crop_extent <- gps_buffered %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  terra::vect()

# this crops the newer habitat layer 

layer_crop <- terra::crop(layer, terra::vect(st_transform(st_as_sfc(st_bbox(crop_extent)), st_crs(layer))))

# this crops the old habitat layer

hab_crop <- terra::crop(habitat_raster, terra::vect(st_transform(st_as_sfc(st_bbox(crop_extent)), st_crs(habitat_raster))))

# makes the new habitat layer have the same projection as the old one

layer_project <- terra::project(layer_crop, hab_crop, method = "near")


# First find habitat with the original raster. To do this, overlay the contour of interest and extract the habitat values from the contour

contour_id <- contour %>%
  filter(id == bird, 
         level == "0.95") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(st_crs(habitat_raster))  

contour_vect <- terra::vect(contour_id)

hab_values <- terra::extract(hab_crop, contour_vect, weights = TRUE) %>% 
  filter(!is.nan(weight)) %>%
  mutate(row_id = row_number())

# next, for all values assigned 14 (wetland), make them stand out. 

wetland_mask <- hab_crop == 14

# Creates a new raster that has the same values of the original raster, except for the cells in the mask (meaning the wetland cells)

detailed_wetlands <- terra::mask(layer_project, wetland_mask, maskvalues = FALSE)

terra::activeCat(detailed_wetlands) <- "ClassName"

# Now extract the detailed info from that masked layer using your contour

extracted_comparison <- terra::extract(detailed_wetlands, contour_vect, weights = TRUE) %>%
  
  # this updated raster has over 300 classes. This is my best attempt as crosswalking based on the names.
  
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

# this takes our new data and combines with the older habitat data. The new habitat raster data replaces the old one if the old habitat data was the wetland category. 

final_combined <- hab_df %>%
  left_join(extracted_comparison, by = "row_id") %>%
  mutate(reclass = ifelse(reclass == 14 & new_hab != "Other", 
                           new_hab, 
                           as.character(reclass))) %>%
  select(reclass, weight)

# Using my new raster values (final_combined), I'm now able to recalculate percentages of habitat

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

##### STEP TWO ###########

# For the remaining birds, if they have less than 10% of the wetland category, assign the pixel value as the one closest to them OR based on a 5x5 area around the pixel. The first option will mean the majority of birds make it through. The second option is tricky, as not all wetland pixels will fit the assumptions. 

wetland_row <- updated %>%
  filter(Landcover == "wetland")

wetland_less_10 <- any(nrow(wetland_row %>%
  filter(percentage < 10)))

# Create a copy of the cropped habitat to modify

hab_refined <- hab_crop

# extracts again but brings in the cell numbers for merging with the updated data in final_combined 

hab_extraction <- terra::extract(hab_crop, contour_vect, cells = TRUE, weights = TRUE) %>%
  mutate(row_id = row_number()) %>%
  
  # weight is not allowed to be NA
  
  filter(weight != "NaN")


# this creates a new raster with our old and new data

hab_refined[hab_extraction$cell] <- as.numeric(final_combined$reclass)


# there are two ways to do this...


  if (wetland_less_10) {
    
    ####  first way - based on distance  #####
    
    # this is taking our new reclass values, and assigning them into hab_final

    hab_refined[hab_extraction$cell] <- as.numeric(final_combined$reclass)

    # assign remaining wetland pixels to be NA

    hab_fill_prep <- hab_refined
    hab_fill_prep[hab_refined == 14] <- NA

    # find the closest pixel and assign it that value

    d <- terra::distance(hab_fill_prep, values = TRUE)

    hab_final_updated <- hab_refined

    hab_final_updated[hab_refined == 14] <- d[hab_refined == 14]

    final_combined <- terra::extract(hab_final_updated, contour_vect, weights = TRUE)

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

    
    ####  second way - based on 5x5 window  #####
    # if this is what you want to use, uncomment, and comment the text above after "first way". You still want to leave in the if (wetland_less_10). 
    

# wetland_majority_fun <- function(x) {
#   center_idx <- ceiling(length(x) / 2)
#   center <- x[center_idx]
#   
#   # Ignore NAs or non-wetland pixels
#   if (is.na(center) || center != 14) {
#     return(center)
#   }
#   
#   # Get all neighbors INCLUDING other wetlands (14s)
#   # But we still remove NAs to avoid calculation errors
#   nbrs <- x[!is.na(x)]
#   
#   # Calculate frequencies
#   counts <- table(nbrs)
#   
#   # Find the most frequent value
#   # names(counts) gives the class IDs, counts gives the frequency
#   majority_class <- as.numeric(names(counts)[which.max(counts)])
#   
#   return(majority_class)
# }
# 
# hab_context <- terra::focal(
#   hab_refined,
#   w = matrix(1, 5, 5), 
#   fun = wetland_majority_fun
# )
# 
# final_combined <- terra::extract(hab_context, contour_vect, weights = TRUE)
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


} else {
  
  # for birds that have no wetland remaining, we take their proportions (updated) and don't change anything 
final_habitat_summary <- updated
  
}

# adding all bird data into the list 

updated_brooding_proportions[[bird]] <- final_habitat_summary

print(bird) # helps you visually see what birds are being processed

}

# this creates a dataframe of your birds and their updated proportions

updated_brooding_proportions <- dplyr::bind_rows(updated_brooding_proportions)

# check for how many birds you have remaining

updated_brooding_proportions %>%
  filter(Landcover == "wetland")

#---------------------------------------------------------------------------#
###### Graphing
#---------------------------------------------------------------------------#

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

habitat_final_map <- hab_crop
final_combined$reclass_num <- as.numeric(final_combined$reclass)
cells_to_update <- terra::extract(hab_crop, contour_vect, cells=TRUE, weights = TRUE)$cell
habitat_final_map[cells_to_update] <- final_combined$reclass_num

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
  
  
  tm_shape(habitat_final_map) +
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

