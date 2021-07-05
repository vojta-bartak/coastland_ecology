library(sf)
library(tmap)
library(dplyr)
library(purrr)

# input data
coast <- read_sf("data/coastline.shp")
lc <- read_sf("data/Landcover2019_ETRS.shp")

# study areas and middle lines
area300 <- coast %>% st_buffer(300) %>% st_intersection(lc %>% st_union()) %>% st_union() 
area400 <- coast %>% st_buffer(400) %>% st_intersection(lc %>% st_union())

map(st_cast(st_geometry(area400[1,],), "POLYGON")[[1]], function(x) st_area(st_polygon(x)))


clean_pol <- function(pol, thres=1){
  parts <- list()
  i <- 1
  for (part in pol[[1]]) {
    if (st_area(st_polygon(list(part))) > thres){
      parts[[i]] <- part
      i <- i + 1
    }
  }
  return(st_polygon(parts))
}

clean_multipol <- function(mpol, thres=1){
  mpol %>% 
    st_cast("POLYGON") %>%
    st_geometry() %>%
    lapply(function(x){
      parts <- list()
      i <- 1
      for (part in x) {
        if (st_area(st_polygon(list(part))) > thres){
          parts[[i]] <- part
          i <- i + 1
        }
      }
      return(st_polygon(parts))
    }) %>%
    st_multipolygon()
}

clean <- function(polygons, thres=1){
  crs <- st_crs(polygons)
  polygons %>%
    st_geometry() %>%
    lapply(function(y) {
      mpol <- y %>% 
        st_cast("POLYGON") %>%
        st_geometry() %>%
        lapply(function(x){
          parts <- list()
          i <- 1
          for (part in x) {
            if (st_area(st_polygon(list(part))) > thres){
              parts[[i]] <- part
              i <- i + 1
            }
          }
          return(st_polygon(parts))
        }) %>%
        st_multipolygon()
      return(mpol)
    }) %>%
    #"[["(1) %>%
    st_as_sfc(crs = crs) %>%
    st_as_sf()
}


pols <- lapply(st_geometry(area400), function(x) st_cast(x, "POLYGON"))

tmap_mode("plot")
qtm(st_geometry(st_linestring(st_cast(st_geometry(area400[1,],), "POLYGON")[[1]][2][[1]])))

st_area(st_polygon(st_cast(st_geometry(area400[1,],), "POLYGON")[[1]][2]))

tm_shape(st_cast(area400[1,], "POLYGON") %>% mutate(pol=row_number())) +
  tm_fill(col="pol") +
  tm_borders()


tmap_mode("view")
tm_shape(coast) +
  tm_lines()

qtm(area300)
