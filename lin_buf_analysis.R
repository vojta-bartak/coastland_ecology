library(sf)
library(tmap)
library(dplyr)
library(purrr)
source("clean_polygons.R")

tmap_mode("view")

# input data
coast <- read_sf("data/coastline.shp")
lc <- read_sf("data/Landcover2019_ETRS.shp")

# study areas
area300 <- coast %>% 
  st_buffer(300) %>% 
  st_intersection(lc %>% st_union()) %>% 
  clean(thres = 1000)
area400 <- coast %>% 
  st_buffer(400) %>% 
  st_intersection(lc %>% st_union())%>% 
  clean(thres = 1000)

tm_shape(area400) + tm_borders() + tm_fill()
tm_shape(lc %>% st_union()) + tm_borders() + tm_fill()

# middle lines


