install.packages("tidyverse")
install.packages("purrr")
install.packages("magick")
install.packages("rayshader")
install.packages("raster")
install.packages("dplyr")
install.packages("rgdal")


#given a set of .asc terrain files and some latitude/longitude coordinates,
#the following code produces 2d and 3d maps of the area with Rayshader

#I gathered .asc LiDAR terrain files from DEFRA here: 
#https://environment.data.gov.uk/DefraDataDownload/?Mode=survey
#using data from tile SZ19SE at 50cm resolution

library(tidyverse)
library(rayshader)
library(purrr)
library(rayrender)
library(tibble)
library(tidyverse)
library(rayshader)
library(raster)
library(dplyr)
library(magick)
library(rgdal)

# Set working directory.
# Load terrain files.
raster_layers <- tibble(filename = list.files(path = getwd(),"*.asc$")) %>% 
  mutate(raster =
           map(filename, .f = ~raster::raster(rgdal::readGDAL(.)))) %>% 
  pull(raster)

#Combine raster layers
raster_layers$fun <- mean
raster_mosaic <- do.call(raster::mosaic, raster_layers)

#function for transforming latitude and longitude 
lat_long_to_other = function(lat,long, epsg_code) {
  data = data.frame(long=long, lat=lat)
  coordinates(data) <- ~ long+lat
  #Input--lat/long
  proj4string(data) <- CRS("+init=epsg:4326")
  #Convert to coordinate system specified by EPSG code
  xy = data.frame(spTransform(data, CRS(paste0("+init=epsg:", epsg_code))))
  colnames(xy) = c("x","y")
  return(unlist(xy))
}

#create the boundaries for our map. Latlong data from Google Maps
# Bournemouth Pier
bottomleft = lat_long_to_other(50.713540, -1.882434, 27700)
topright = lat_long_to_other(50.724410, -1.869810, 27700)

#hh
bottomleft = lat_long_to_other(50.708359, -1.777976, 27700)
topright = lat_long_to_other(50.732398, -1.733914, 27700)

#ph
bottomleft = lat_long_to_other(50.650140, -2.089052, 27700)
bottomright = lat_long_to_other(50.743724, -1.873141, 27700)

# Create an extent object:
  e = extent(c(bottomleft[1], topright[1], bottomleft[2], topright[2]))

#convert raster layer to a matrix
hh <- crop(raster_mosaic, e) %>%
  raster_to_matrix() %>%
  reduce_matrix_size(0.25) 

# build a static plot
hh %>%
  sphere_shade(texture = "imhof1") %>%
  add_water(detect_water(hh, zscale = 1, cutoff = 0.2,
                         min_area = length(hh)/150,
                         max_height = 1.1)) %>%
  add_shadow(ray_shade(hh, zscale = 1, multicore = TRUE, 
                       sunaltitude = 10, sunangle = -110),0.3) %>%
  plot_map()


#3d map
  hh %>%
    sphere_shade(texture = "imhof1") %>%
    add_shadow(ray_shade(hh)) %>%
    add_water(detect_water(hh,zscale = 1, cutoff = 0.2,
                           min_area = length(hh)/1500,
                           max_height = 1.3)) %>%
   add_shadow(ambient_shade(hh)) %>%
   plot_3d(hh, solid = TRUE, shadow = TRUE, water = TRUE, waterdepth = 1, baseshape = "rectangle",
           soliddepth = -50)

#change the zoom and angle
render_camera(zoom = 0.5, theta = 1)

#saves rgl window output to display
render_snapshot()

#normal water level. Water depth units are 1 = 50cm. 
render_water(hh, zscale = 1, waterdepth = 1.1)

#30cm increase.
render_water(hh, zscale = 1, waterdepth = 1.7)

#1m increase.
render_water(hh, zscale = 1, waterdepth = 3.1)

#4m increase.
render_water(hh, zscale = 1, waterdepth = 9.1)


#old code
hh %>%
  sphere_shade(texture = "imhof1") %>%
  add_shadow(ray_shade(hh, zscale = 1, multicore = TRUE, sunaltitude = 10, sunangle = -110),0.3) %>% 
  add_water(detect_water(hh, zscale = 1, cutoff = 0.2,
                         min_area = length(hh)/1500,
                         max_height = 1.3)) %>%
  
  plot_3d(hh, zscale = 1, watercolor = "turqoise4", water = TRUE, waterdepth = 8, wateralpha = 0.8,
          zoom=0.5, windowsize = 1000, 
          background = "grey50", shadowcolor = "grey20", solid=TRUE, shadow = TRUE, soliddepth = -150, baseshape = "circle")



