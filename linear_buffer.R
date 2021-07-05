library(sf)
library(tidyverse)
library(mapview)
library(lwgeom)
library(smoothr)
library(sfdct)

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

# inputs
bnd <- read_sf("CoastaLandEcol/boundary.shp")
mdln <- read_sf("CoastaLandEcol/SmoothLine2.shp")
pnt <- read_sf("CoastaLandEcol/test_point.shp")
pntid <- "FID_XYplot"
width <- 500
lc <- read_sf("CoastaLandEcol/LandCoverLazio_2008.shp")
lcfld <- "Codice"

# vizualize
bnd %>% 
  mapview() +
  mapview(mdln, color="red") +
  mapview(pnt, color="green")

# create middleline
midl <- midline(bnd)

# smooth middleline
mdln %>% smooth(method="chaikin") %>%
  mapview() +
  mapview(mdln, color="red")

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

mapview(pnt$buffer) + mapview(pnt)

bufs <- pnt %>% 
  
  # snap points
  st_nearest_points(mdln) %>%
  st_endpoint() %>%

  # buffer
  st_buffer(100) %>%
  st_intersection(mdln) %>%
  st_buffer(500, endCapStyle = "FLAT") %>%
  st_intersection(bnd)

# calculate class relative areas
patches <- lc %>%
  pull(lcfld) %>%
  unique() %>%
  map(function(i){
    pnt %>%
      st_drop_geometry() %>%
      select(all_of(pntid)) %>%
      mutate(
        "{{i}}" := bufs %>% 
          map_dbl(~ st_intersection(.x %>% st_sfc(crs=st_crs(bufs)), 
                                    lc %>% filter(get(lcfld) == i)) %>% 
                    st_area() %>%
                    sum() %>%
                    "/"(.x %>% st_area()))
      )
  }) %>%
  reduce(left_join, by = pntid)

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

# middle line
midline <- function(polygon){
  xy <- polygon %>% 
    ct_triangulate() %>% 
    st_collection_extract("POLYGON") %>% 
    st_geometry() %>%
    map(~st_coordinates(.x)) %>%
    map(function(.x){
      map(1:3, function(i) c(X=(.x[i,1]+.x[i+1,1])/2, Y=(.x[i,2]+.x[i+1,2])/2)) %>% 
        bind_rows()
    }) %>%
    bind_rows() %>%
    st_as_sf(coords=c("X.X","Y.Y"), crs=st_crs(polygon)) %>%
    filter(st_is_within_distance(., 
                                 polygon %>% st_geometry() %>% st_cast("LINESTRING"), 
                                 0.3*width) %>% lengths() %>% "=="(0)) %>%
    unique()
    st_coordinates()
  indices <- xy %>% 
    dist() %>% 
    as("matrix") %>%
    graph.adjacency(weighted=TRUE, mode="upper") %>%
    minimum.spanning.tree(weighted=TRUE) %>%
    get_diameter()
  xy[indices,] %>% 
    st_linestring() %>%
    st_sfc()
}


densifyit <- function(xy,n=5){
  ## densify a 2-col matrix
  cbind(dens(xy[,1],n=n),dens(xy[,2],n=n))
}

dens <- function(x,n=5){
  ## densify a vector
  out = rep(NA,1+(length(x)-1)*(n+1))
  ss = seq(1,length(out),by=(n+1))
  out[ss]=x
  for(s in 1:(length(x)-1)){
    out[(1+ss[s]):(ss[s+1]-1)]=seq(x[s],x[s+1],len=(n+2))[-c(1,n+2)]
  }
  out
}