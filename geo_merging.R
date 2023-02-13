# A function for merging spatial dataframes (points).
geo_merging <- function(master_set = "", d_max = 25, check_attributes = FALSE, list_df, list_attr, region_name, region_name_column) {
  ## Determine EPSG for all dataframes
  EPSG_2_UTM <- lonlat2UTM(list_df[[1]]$point)
  ## sort list of dataframes by number of rows
  list_df_2 <- list_df
  ordered_rows <- sort(map_int(list_df, nrow), decreasing = TRUE)
  if (master_set != "") {
    m <- which(names(ordered_rows) == master_set)
    ordered_rows <- c(ordered_rows[m], ordered_rows[-m])
    rm(m)
  }
  list_df <- list_df[names(ordered_rows)]
  rm(ordered_rows)
  
  # Start matching----------------------------------------------------------------
  result <- list()
  no_match <- list(NULL)
  for (i in seq_len(length(list_df) - 1)) {
   print(i)
  ## take a dataframe to match with
    df <- list_df[[i]] %>%
      select(c("id", contains(c(list_attr, region_name_column, "dist")), "point"))
    if (i > 1 && length(no_match[[i]] > 0)) {
      df <- df %>%
        filter(id %in% no_match[[i]])
    }
    if (nrow(df) < 1) {
      next()
    }
    df$geometry <- st_transform(df$geometry, EPSG_2_UTM)
    df_region_column <- colnames(df)[grepl(region_name_column, colnames(df))][1]
    df_regions_vec <- df %>%
      st_drop_geometry() %>%
      select(contains(df_region_column)) %>%
      distinct() %>%
      pull()
    ## If there is a distance column - use it.
    ## If not - use global dist variable (also fill missing dist values)
    if (any(colnames(df) == "dist")) {
      df[which(is.na(df$dist) == TRUE), "dist"] <- d_max
    } else {
      df$dist <- d_max
    }
    result_temp <- st_drop_geometry(df)
  ## loop through other dataframes
    for (k in seq_along(list_df)[-(1:i)]) {
      print(names(list_df[k]))
      df_comp <- list_df[[k]]
      df_comp <- df_comp %>%
        select(c("id", contains(c(list_attr, region_name_column, "dist")), "point"))
      df_comp_region_column <- colnames(df_comp)[grepl(region_name_column, colnames(df_comp))][1]
      if (nrow(df_comp) < 1) {
        next()
      }
      df_comp$geometry <- st_transform(df_comp$geometry, EPSG_2_UTM)
      result_district <- data.frame()
      no_match_id <- NULL
  ## start of loop through regions------------------------------------------------
      for (j in seq_along(df_regions_vec)) {
        print(df_regions_vec[j])
        lost_id <- NULL
  ## choose a region
        df_samp <- df[df[[df_region_column]] == df_regions_vec[j], ]
        df_comp_samp <- df_comp[df_comp[[df_comp_region_column]] == df_regions_vec[j], ]
        if (nrow(df_samp) < 0 || nrow(df_comp_samp) < 0) {
          next()
        }
  ## make a buffer.
        df_samp_buff <- st_buffer(df_samp, dist = df_samp$dist)
  ## join layers
        sj <- st_join(df_samp_buff, df_comp_samp) %>%
          st_drop_geometry()
  ## save ids not in buffers
        lost_id <- unique(c(lost_id, df_comp_samp$id[-which(df_comp_samp$id %in% sj$id.y)]))
        sj <- sj %>%
          filter(is.na(id.y) == FALSE)
        if (nrow(sj) < 1) {
          no_match_id <- c(no_match_id, lost_id)
          result_district <- rbind(result_district, sj)
          next()
        }
  ## find rows that match by attributes (optional)
        if (check_attributes == TRUE && length(list_attr) > 0) {
          eq_attr <- as_tibble(map2(select(sj, contains(paste0(list_attr, ".x"))),
                                    select(sj, contains(paste0(list_attr, ".y"))),
                                    ~ .x == .y))
          eq_attr <- cbind(eq_attr, eq_num = rowSums(eq_attr, na.rm = TRUE)) %>%
            mutate(matched_attr = if_else(eq_num >= length(list_attr), TRUE, FALSE)) %>%
            pull(matched_attr)
          sj <- sj[eq_attr, ]
        }
  ## save ids with unmatched attributes
        lost_id <- unique(c(lost_id, df_comp_samp$id[-which(df_comp_samp$id %in% sj$id.y)]))
        if (nrow(sj) < 1) {
          no_match_id <- c(no_match_id, lost_id)
          result_district <- rbind(result_district, sj)
          next()
        }
  ## find multiple joined points and select closest
        sj <- sj %>%
          group_by(id.y) %>%
          mutate(dups = n()) %>%
          ungroup()
        sj_dups <- sj %>%
          filter(dups > 1)
        if (nrow(sj_dups) > 1) {
          sj_dups <- sj_dups %>%
            mutate(point_x = str_split(point.x, pattern = " "),
                   point_y = str_split(point.y, pattern = " ")) %>%
            mutate(point_x = map(point_x, as.numeric),
                   point_y = map(point_y, as.numeric)) %>%
            mutate(dist = unlist(map2(point_x, point_y, distGeo))) %>%
            group_by(id.y) %>%
            mutate(min_dist = min(dist)) %>%
            ungroup() %>%
            filter(dist == min_dist) %>%
            select(-c(point_x, point_y, dist, min_dist))
        }
        sj <- sj %>%
          filter(dups == 1) %>%
          bind_rows(sj_dups) %>%
          select(-dups)
        result_district <- rbind(result_district, sj)
        no_match_id <- c(no_match_id, lost_id)
      }
      gc()
  ## end of loop through regions--------------------------------------------------
      suppressWarnings(rm(df_samp, df_comp_samp, df_samp_buff, sj, sj_dups, eq_attr, j, lost_id))
      result_district <- result_district %>%
        select(c(id.x, id.y, point.y))
      colnames(result_district) <- c("id.x", paste(names(list_df[k]), c("id", "point"), sep = "_"))
      result_temp <- result_temp %>%
        left_join(result_district, by = c("id" = "id.x"))
      no_match[[k]] <- no_match_id
    }
    gc()
    result[[i]] <- result_temp
    suppressWarnings(rm(result_district, result_temp, k, no_match_id, df_region_column,
       df_comp_region_column, df_regions_vec, df, df_comp))
  }
  last_sheet <- list_df[[i + 1]] %>%
    st_drop_geometry() %>%
    filter(id %in% no_match[[i + 1]]) %>%
    select(c("id", contains(c(list_attr, region_name_column)), "point"))
  result[[i + 1]] <- last_sheet
  names(result) <- names(list_df)
  return(result)
}
