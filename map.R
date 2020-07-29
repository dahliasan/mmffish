# Plot map of marine finfish farms and seal haulout + breeding locations

library(sf)
library(tidyverse)
library(tmap)
library(tmaptools)
library("rnaturalearth")
library("rnaturalearthdata")
library(raster)

# Load shape file
dir("data")
farm <- st_read("data/LIST_MARINE_LEASES_STATEWIDE/list_marine_leases_statewide.shp")

# Load finfish farm lease number reference
finfarm_id <- read_csv("data/finfish_licences.csv")

finfarm <- farm %>% 
  filter(M_LEASE_NO %in% finfarm_id$lease_no)

# Load seal locations
breed <- st_read("data/breeding_sites.kml")
haulout <- st_read("data/haulouts.kml")


# Plot --------------------------------------------------------------------

## Convert all datasets to same crs
world <- ne_countries(scale = "large", returnclass = "sf")
finfarm <- st_transform(finfarm, crs = crs(world, asText = TRUE))
breed <- st_transform(breed, crs = crs(world, asText = TRUE))
haulout <- st_transform(haulout, crs = crs(world, asText = TRUE))


# Combine all locations to one dataframe
finfarm <- finfarm %>% mutate(loc_type = "Finfish Farm")
breed <- breed %>% mutate(loc_type = "Breeding Site")
haulout <- haulout %>% mutate(loc_type = "Haul-out")

loc <- rbind(breed, haulout) %>% 
  dplyr::select(-Description)

finfarm_tmp <- finfarm %>%
  rename(Name = M_LEASE_NO) %>%
  dplyr::select(Name, loc_type) %>% 
  st_cast("MULTIPOINT")

loc <- rbind(loc, finfarm_tmp) %>% 
  mutate(loc_type = factor(loc_type, levels = c('Finfish Farm', 'Breeding Site', 'Haul-out')))

## Set colour palette
pal1 <- get_brewer_pal("Dark2", n = 2)


## Create bounding box for tasmania
tas_bbox <- st_bbox(c(xmin = 143.4, xmax = 148.8, ymax = -39.2, ymin = -43.9), crs = st_crs(world))
## Tasmania base map
tasmap1 <- tm_shape(world, bbox = tas_bbox) +
  tm_fill() +
  tm_borders() 
  
## Add finfish farm locations
tasmap1 +
  tm_shape(finfarm) +
  tm_dots(col = 'red', size = 0.2) + 
  # Add seal haulouts
  tm_shape(haulout) + 
  tm_dots(col = 'blue', size = 0.2) 



## Inset map to SE Tas
setas_bbox <- st_bbox(c(xmin = 146.8, xmax = 148.3, ymax = -42.2, ymin = -43.9), crs = st_crs(world))

tm_shape(world, bbox = setas_bbox) +
  tm_polygons() + 
  tm_shape(finfarm) +
  # tm_polygons(col = 'red') + 
  tm_dots(col = 'red', size = 0.2) +
  # Add seal haulouts
  tm_shape(haulout) + 
  tm_dots(col = 'blue', size = 0.2)

## Create Tas map with SE Tas region box

# Create SE Tas region box
setas_region <- setas_bbox %>% st_as_sfc()

p1 <- tasmap1 +
  # Add all location types
  tm_shape(loc) +
  tm_symbols(col = "loc_type", shape = "loc_type", size = 0.2, palette = c(pal1[1], pal1[2], pal1[2]), shapes = c(21,3,21), legend.shape.show = FALSE, shapes.legend = c(21,3,21), title.col = '') + 
  ## Add SE Tas region box
  # tm_shape(setas_region) + tm_borders(lwd = 3) + 
  # Add map features
  tm_grid(lines = FALSE, labels.size = 0.8) +
  tm_compass(position = c(.03, .25), size  = 1) +
  tm_scale_bar(breaks = c(0, 50, 100), position = c(0.03, .15), text.size = 1) + 
  tm_style("col_blind", legend.text.size = 1) 

tmap_save(p1, filename = 'Finfish x Seal Map.png', units = 'in', height = 7, width = 6)
