# A function for parsing spatial dataframes before merging.
parsing_dfs <- function(region_shp, list_df) {
  progress <- Progress$new()
  on.exit(progress$close())
  result_list <- list()
  region_name_column <- colnames(region_shp)[grepl("name", colnames(region_shp), ignore.case = TRUE)]
  if (length(region_name_column) < 1) {
    msg <- "Can not detect region name!"
    return(list(status = 'err', message = msg))
  } else {
    if (length(region_name_column) > 1) {
      msg <- "Ambiguous region name!"
      return(list(status = 'err', message = msg))
    } else {
      region_name <- region_shp %>%
        st_drop_geometry() %>%
        select(all_of(region_name_column)) %>%
        pull()
    }
  }
# Check contents of xlsx files (if they have correct coordinates, id and common attributes)
# Add `region name` and `id` attribute, then make sf objects
  list_attr <- unlist(map(list_df, ~ unique(colnames(.x))))
  list_attr <- table(list_attr)
  list_attr <- names(list_attr[list_attr >= length(list_df)])
  if (length(list_attr) > 0) {
    result_list$'Common attributes:' <- paste(list_attr, collapse = ", ")
  } else {
    msg <- "No common attributes!"
    return(list(status = 'err', message = msg))
  }
# Check coordinates and make sf dataframes
  progress$set(message = enc2utf8("Читаем и проверяем файлы"), value = 0)
  inc_2 <- 0
  for (i in seq_along(list_df)) {
    inc_2 <<- inc_2 + 1
    progress$inc(1 / length(list_df), detail = paste(toupper(names(list_df[i]))))
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
        msg <- paste(
          "Incorrect coordinates in",
          toupper(names(list_df[i])),
          "table in this rows:",
          paste(sort(bad_rows + 1), collapse = ", ")
          )
        return(list(status = 'err', message = msg))
      } else {
      }
    } else {
      msg <- paste("No point column in", toupper(names(list_df[i])))
      return(list(status = 'err', message = msg))
    }
    if (any(grepl("id", list_attr))) {
      if (length(unique(list_df[[i]]$id)) < nrow(list_df[[i]])) {
        msg <- paste("Not unique ids in", toupper(names(list_df[i])))
        return(list(status = 'err', message = msg))
      }
    } else {
      msg <- paste("No id column in", toupper(names(list_df[i])))
      return(list(status = 'err', message = msg))
    }
    list_df[[i]] <- bind_cols(list_df[[i]], point[, c("lon", "lat")]) %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
      st_join(region_shp)
  }
  # Update list of common attributes
  list_attr <- list_attr[-which(list_attr %in% c("point", "id", "dist"))]
  if (length(list_attr) > 0) {
    result_list$'Common attributes:' <- paste(list_attr, collapse = ", ")
  } else {
    result_list$'Common attributes:' <- "No common attributes!"
  }
  result_list$status <- "ok"
  result_list <- c(result_list, list(list_df, list_attr, region_name))
  return(result_list)
}
