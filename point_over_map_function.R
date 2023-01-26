# A function for joining polygon attributes to point data.
point_over_map <- function(input_df, var_lon, var_lat, map_over, full_result = TRUE) {
  points_input <- st_as_sf(input_df, coords = c(var_lon, var_lat)) %>% 
    st_set_crs(4326) %>% 
    mutate(id_entry = row_number())
  if (full_result == FALSE) {
    points_input <- select(points_input, c(id_entry, geometry))
  }
  result <- try(st_join(points_input, map_over))
  if (nrow(result) > nrow(points_input)) {
    result_coords <- as.data.frame(st_coordinates(result)) %>% 
      unite(col = "point_original", sep = " ")
    result_dupl <- result %>% 
      st_drop_geometry() %>% 
      select(all_of(c("id_entry", colnames(map_over)[-ncol(map_over)]))) %>% 
      bind_cols(result_coords) %>% 
      group_by(id_entry) %>% 
      mutate(occurance_count = n()) %>% 
      filter(occurance_count > 1)
    dupl_points <- unique(result_dupl$point_original)
    showNotification(
      paste0("Possible overllaping of boundaries near these points: ", paste0(dupl_points, collapse = ", ")),
      type = "warning", duration = 30
    )
    return(result_dupl)
  }
  result <- result %>% 
    st_drop_geometry() %>% 
    bind_cols(input_df %>% 
                select(all_of(c(var_lon, var_lat))) %>% 
                unite(col = "point_original", sep = " "))
  return(result)
}
