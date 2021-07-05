library(sf)
library(tmap)
library(dplyr)
library(purrr)
source("clean_polygons.R")
# source("linear_buffer.R")

tmap_mode("view")

# input data
coast <- read_sf("data/coastline.shp")
lc <- read_sf("data/Landcover2019_ETRS.shp")
lcfld <- "Abbr"
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
  st_intersection(area300) %>%
  st_union()
middle400 <- coast %>%
  st_buffer(200) %>%
  st_union() %>%
  st_cast("MULTILINESTRING") %>%
  st_intersection(area400) %>%
  st_union()

tm_shape(area400) + tm_borders() + tm_fill(col="grey") +
  tm_shape(middle400) + tm_lines(col="red") +
  tm_shape(pnt) + tm_dots(col = "blue")

#tm_shape(lc %>% st_union()) + tm_borders() + tm_fill()

buf <- pnt %>% linear_buffer(middle300, 100, area300)

# linear buffers
pnt <- pnt %>% 
  mutate(
    buffer300 = linear_buffer(pnt, middle300, 100, area300),
    buffer400 = linear_buffer(pnt, middle400, 100, area400),
  )

patches <- lc %>%
  pull(lcfld) %>%
  unique() %>%
  map(function(i){
    pnt %>% 
      pull("buffer") %>%
      map(~ st_intersection(.x %>% st_sfc(crs = st_crs(lc)), 
                            lc %>% filter(get(lcfld) == i)) %>%
            st_cast("MULTIPOLYGON") %>%
            st_cast("POLYGON") %>%
            st_sf() %>%
            mutate(pnt_id = pntid)) %>%
      bind_rows() %>%
      mutate(cl_id = i)
  }) %>%
  bind_rows()
