library(sf)
library(tmap)
library(dplyr)
library(purrr)
source("clean_polygons.R")

tmap_mode("view")

# input data
coast <- read_sf("data/coastline.shp")
lc <- read_sf("data/Landcover2019_ETRS.shp")
pnt <- read_sf("data/Allplotindici_3003.shp") %>%
  st_transform(3035)

# study areas
area300 <- coast %>% 
  st_buffer(300) %>% 
  st_intersection(lc %>% st_union()) %>% 
  clean(thres = 10000000)
area400 <- coast %>% 
  st_buffer(400) %>% 
  st_intersection(lc %>% st_union())%>% 
  clean(thres = 10000000)

# middle lines
middle300 <- coast %>%
  st_buffer(150) %>%
  st_union() %>%
  st_cast("MULTILINESTRING") %>%
  st_intersection(area300)
middle400 <- coast %>%
  st_buffer(200) %>%
  st_union() %>%
  st_cast("MULTILINESTRING") %>%
  st_intersection(area400)

tm_shape(area400) + tm_borders() + tm_fill(col="grey") +
  tm_shape(middle400) + tm_lines(col="red") +
  tm_shape(pnt) + tm_dots(col = "blue")

#tm_shape(lc %>% st_union()) + tm_borders() + tm_fill()

# linear buffers
pnt <- pnt %>% 
  mutate(
    buffer = pnt %>% 
      
      # snap points
      st_nearest_points(mdln) %>%
      st_endpoint() %>%
      
      # buffer
      st_buffer(100) %>%
      st_intersection(mdln) %>%
      st_buffer(500, endCapStyle = "FLAT") %>%
      st_intersection(bnd)
    
  )