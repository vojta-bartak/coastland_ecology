library(sf)
library(dplyr)
library(lwgeom)

## Clean polygons from tiny inner holes
##
## thres is a threshold value of area (in squared crs units) - hole with area larger than 
## the thres will be retained
clean_polygons <- function(polygons, thres=1){
  crs <- st_crs(polygons)
  polygons %>%
    st_geometry() %>%
    lapply(function(y) {
      mpol <- y %>% 
        st_cast("POLYGON") %>%
        st_geometry() %>%
        lapply(function(x){
          parts <- list()
          parts[[1]] <- x[[1]]
          i <- 2
          if (length(x) > 1){
            for (part in x[2:length(x)]) {
              if (st_area(st_polygon(list(part))) > thres){
                parts[[i]] <- part
                i <- i + 1
              }
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

## Create a geometry column of linear buffers
linear_buffer <- function(pnt, mdln, dist, bndr, width=500){
  pnt %>% 
    
    # snap points
    st_nearest_points(mdln) %>%
    st_endpoint() %>%
    
    # buffer
    st_buffer(dist) %>%
    st_intersection(mdln) %>%
    st_buffer(width, endCapStyle = "FLAT") %>%
    st_intersection(bndr %>% st_union())
}

