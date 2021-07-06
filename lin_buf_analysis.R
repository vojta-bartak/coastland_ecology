library(sf)
library(tmap)
library(dplyr)
library(purrr)
library(nngeo)
library(tidyr)

# input data and parameters ------------------------------------------------------------------
coast <- read_sf("data/coastline.shp")
lc <- read_sf("data/Landcover2019_ETRS.shp")
lcfld <- "Abbr"
pnt <- read_sf("data/Allplotindici_3003.shp") %>%
  st_transform(3035)
pntid <- "NAME"
buffer_distance <- 100
out_file <- paste("indices", buffer_distance, sep="_")

# study areas --------------------------------------------------------------------------------
bndr300 <- coast %>% 
  st_buffer(300) %>% 
  st_intersection(lc %>% st_union()) %>% 
  st_remove_holes() %>%
  st_union()
bndr400 <- coast %>% 
  st_buffer(400) %>% 
  st_intersection(lc %>% st_union()) %>% 
  st_remove_holes() %>%
  st_union() 

# middle lines -------------------------------------------------------------------------------
middle300 <- coast %>%
  st_buffer(150) %>%
  st_union() %>%
  st_cast("MULTILINESTRING") %>%
  st_intersection(bndr300) %>%
  st_union()
middle400 <- coast %>%
  st_buffer(200) %>%
  st_union() %>%
  st_cast("MULTILINESTRING") %>%
  st_intersection(bndr400) %>%
  st_union()

# visualizing --------------------------------------------------------------------------------
tmap_mode("view")
tm_shape(bndr400) + tm_borders() + tm_fill(col="grey") +
  tm_shape(middle400) + tm_lines(col="red") +
  tm_shape(pnt) + tm_dots(col = "blue")

#tm_shape(lc %>% st_union()) + tm_borders() + tm_fill()

# function to create a geometry column of linear buffers -------------------------------------
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

# linear buffers computation -----------------------------------------------------------------
pnt <- pnt %>% 
  mutate(
    buffer300 = linear_buffer(pnt, middle300, buffer_distance, bndr300),
    buffer400 = linear_buffer(pnt, middle400, buffer_distance, bndr400),
    area300 = st_area(buffer300),
    area400 = st_area(buffer400)
  )

# landcover patches --------------------------------------------------------------------------
patches400 <- lc %>% 
  st_make_valid() %>%
  st_intersection(pnt %>% st_set_geometry("buffer400"))
patches300 <- lc %>% 
  st_make_valid() %>%
  st_intersection(pnt %>% st_set_geometry("buffer300"))
patches400$area <- st_area(patches400)
patches300$area <- st_area(patches300)

# Class - level indices ----------------------------------------------------------------------
class_ind300 <- patches300 %>%
  group_by(NAME, Abbr) %>%
  summarize(area = sum(area),
            rel_area = as.numeric(sum(area)/area300[1]),
            n_patches = length(area),
            mean_area = sum(area)/length(area)) %>%
  st_drop_geometry()
class_ind400 <- patches400 %>%
  group_by(NAME, Abbr) %>%
  summarize(area = sum(area),
            rel_area = as.numeric(sum(area)/area400[1]),
            n_patches = length(area),
            mean_area = sum(area)/length(area)) %>%
  st_drop_geometry()

# Landscape - level indices ------------------------------------------------------------------
land_ind300 <- class_ind300 %>%
  ungroup() %>%
  group_by(NAME) %>%
  summarize(n_classes = length(area),
            shannon = -sum(rel_area*log(rel_area)),
            simpson = 1 - sum(rel_area*rel_area))
land_ind400 <- class_ind400 %>%
  ungroup() %>%
  group_by(NAME) %>%
  summarize(n_classes = length(area),
            shannon = -sum(rel_area*log(rel_area)),
            simpson = 1 - sum(rel_area*rel_area))

# Adding indices as attributes of input points -----------------------------------------------
pnt <- pnt %>% 
  left_join(
    class_ind300 %>%
      select(NAME, Abbr, rel_area) %>%
      pivot_wider(values_from = rel_area, names_from = Abbr, names_prefix = "RA3_")
  ) %>% 
  left_join(
    class_ind300 %>%
      select(NAME, Abbr, mean_area) %>%
      pivot_wider(values_from = mean_area, names_from = Abbr, names_prefix = "MA3_")
  ) %>% 
  left_join(
    class_ind300 %>%
      select(NAME, Abbr, n_patches) %>%
      pivot_wider(values_from = n_patches, names_from = Abbr, names_prefix = "NP3_")
  ) %>% 
  left_join(
    class_ind400 %>%
      select(NAME, Abbr, rel_area) %>%
      pivot_wider(values_from = rel_area, names_from = Abbr, names_prefix = "RA4_")
  ) %>% 
  left_join(
    class_ind400 %>%
      select(NAME, Abbr, mean_area) %>%
      pivot_wider(values_from = mean_area, names_from = Abbr, names_prefix = "MA4_")
  ) %>% 
  left_join(
    class_ind400 %>%
      select(NAME, Abbr, n_patches) %>%
      pivot_wider(values_from = n_patches, names_from = Abbr, names_prefix = "NP4_")
  ) %>% 
  left_join(land_ind300 %>% 
              rename(
                NC300 = n_classes,
                SH300 = shannon,
                SI300 = simpson
              )) %>% 
  left_join(land_ind400 %>% 
              rename(
                NC400 = n_classes,
                SH400 = shannon,
                SI400 = simpson
              )) 

# Saving as table ----------------------------------------------------------------------------
pnt %>%
  st_drop_geometry() %>%
  write.table(paste("data/", out_file, ".csv", sep=""), row.names = F, sep=";")

# Saving as shapefile ------------------------------------------------------------------------
pnt %>%
  st_write(paste("data/", out_file, ".shp", sep=""))

