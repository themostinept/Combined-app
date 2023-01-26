# A function for parsing spatial dataframes before merging.
parsing_dfs <- function(region_shp, list_df) {
  region_name_column <<- colnames(region_shp)[grepl("name", colnames(region_shp), ignore.case = TRUE)]
  if (length(region_name_column) < 1) {
    showNotification("Can not detect region name!", 
                     type = "warning", duration = 10)
    return(NULL)
  } else {
    if (length(region_name_column) > 1) {
      showNotification("Ambiguous region name!", 
                       type = "warning", duration = 10)
      return(NULL)
    } else {
      region_name <- region_shp %>%
        st_drop_geometry() %>%
        select(all_of(region_name_column)) %>%
        pull()
    }
  }
# Check contents of xlsx files (if they have correct coordinates, id and common attributes)
# Add `region name` and `id` attribute, then make sf objects
  list_attr <<- unlist(map(list_df, ~ unique(colnames(.x))))
  list_attr <- table(list_attr)
  list_attr <- names(list_attr[list_attr >= length(list_df)])
  if (length(list_attr) > 0) {
    showNotification(paste("Common attributes:", paste(list_attr, collapse = ", ")), 
                     type = "message")
  } else {
    showNotification("No common attributes!", 
                     type = "warning", duration = 10)
    return(NULL)
  }
# Check coordinates and make sf dataframes
  for (i in seq_along(list_df)) {
    if (any(grepl("point", list_attr))) {
      point <- list_df[[i]] %>%
        select("point") %>%
        separate(
          point,
          into = c("lon", "lat"),
          sep = "[[:space:]]",
          remove = FALSE,
          convert = TRUE) %>%
        mutate(lon = as.numeric(gsub(",", ".", lon)),
               lat = as.numeric(gsub(",", ".", lat))) %>%
        mutate(corr = if_else(is.na(lon) | is.na(lat), FALSE, TRUE))
      if (any(point$corr) == FALSE) {
        bad_rows <- which(point$corr == FALSE)
        showNotification(
          print(paste(
            "Incorrect coordinates in",
            toupper(names(list_df[i])),
            "table in this rows:",
            paste(sort(bad_rows + 1), collapse = ", ")
            )
          ),
          type = "warning", duration = 15)
        return(NULL)
      } else {
        showNotification(paste("Correct coordinates in", toupper(names(list_df[i]))),
                         type = "message")
      }
    } else {
      showNotification(paste("No point column in", toupper(names(list_df[i]))),
                       type = "warning", duration = 15)
      return(NULL)
    }
    if (any(grepl("id", list_attr))) {
      if (length(unique(list_df[[i]]$id)) < nrow(list_df[[i]])) {
        showNotification(paste("Not unique ids in", toupper(names(list_df[i]))),
                         type = "warning", duration = 15)
        return(NULL)
      }
    } else {
      showNotification(paste("No id column in", toupper(names(list_df[i]))),
                       type = "warning", duration = 15)
      return(NULL)
    }
    list_df[[i]] <- bind_cols(list_df[[i]], point[, c("lon", "lat")]) %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
      st_join(region_shp)
  }
  # Update list of common attributes
  list_attr <- list_attr[-which(list_attr %in% c("point", "id", "dist"))]
  if (length(list_attr) > 0) {
    showNotification(paste("Common attributes:", paste(list_attr, collapse = ", ")),
                     type = "message")
  } else {
    showNotification("No common attributes!", type = "warning", duration = 15)
  }
  return(list(list_df, list_attr, region_name))
}
