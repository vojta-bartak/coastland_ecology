tmap_options(check.and.fix = TRUE)


middle300_ksmooth2 <- smooth(middle300, method = "ksmooth", smoothness = 100)
pnt <- pnt %>% 
  mutate(
    buffer300sm = linear_buffer(pnt, middle300_ksmooth2, buffer_distance, bndr300),
  )
tm_shape(pnt$buffer300) +
  tm_fill(col="blue", alpha = 0.3) +
  tm_borders(col = "darkblue") +
  tm_shape(pnt$buffer300sm) +
  tm_fill(col="red", alpha = 0.3) +
  tm_borders(col = "darkred") +
  tm_shape(middle300) +
  tm_lines(col="blue") +
  tm_shape(middle300_ksmooth) +
  tm_lines(col = "red") +
  tm_shape(middle300_ksmooth2) +
  tm_lines(col = "green") +
  tm_shape(pnt) +
  tm_dots(col = "grey")

st_write(middle300_ksmooth2, "middle_300_sm100.shp")
st_write(pnt %>% st_set_geometry("buffer300sm"), "buffer_300_sm100.shp")
