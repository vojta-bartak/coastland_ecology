library(sf)
library(dplyr)

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

