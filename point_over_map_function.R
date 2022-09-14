#A function for joining polygon attributes to point data.
point_over_map <- function(input_df, var_lon, var_lat, map_over, full_result = TRUE) {
  points_input <- st_as_sf(input_df, coords = c(var_lon, var_lat)) %>% 
    st_set_crs(4326) %>% 
    rename_()
  if (full_result == FALSE) {
    points_input <- select(points_input, geometry)
  }
  result <- st_join(points_input, map_over) %>% 
    st_drop_geometry() %>% 
    bind_cols(input_df %>% 
                select(all_of(c(var_lon, var_lat))) %>% 
                unite(col = "point_original", sep = " "))
  return(result)
}
