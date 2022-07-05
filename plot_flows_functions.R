##Function to parse debug-files
get_flows_df <- function(flows_files, region, zone_name = "Зоны транспортирования") {
  #prepare flow files
  periods <- unlist(str_extract_all(flows_files, "period-[[:digit:]].debug"))
  periods <- as.numeric(unlist(str_extract_all(periods, "[[:digit:]]")))
  flow_df_list <- list()
  infs <- data.frame()
  sources <- data.frame()
  flows <- data.frame()
  # tt_types <- data.frame()
  zones <- data.frame()
  region_column <- colnames(region)[grepl("name", colnames(region), ignore.case = TRUE)]
  for (i in seq_along(flows_files)) {
    progress$set(message = paste("Обработка файла номер", i))
    # start_time <- Sys.time()
    # print(paste0("Period ", periods[i], ": ", start_time))
    wb_names <- getSheetNames(flows_files[i])
    ##Infrastructure dataframe
    progress$set(detail = "Обработка объектов")
    infs_temp <- read.xlsx(flows_files[i], sheet = wb_names[grepl("Объекты", wb_names)])
    infs_temp <- infs_temp %>% 
      filter(node_type == "input") %>% 
      select(c(3, 7, 10:12, 20:21)) %>% 
      mutate(period = periods[i],
             point = st_as_sfc(point))
    infs_temp <- st_sf(infs_temp) %>% 
      st_set_crs(4326)
    ##Sources dataframe
    progress$set(detail = "Обработка источников")
    sources_temp <- read.xlsx(flows_files[i], sheet = "Источники")
    sources_temp <- sources_temp %>% 
      select(c(1:3, 6:7)) %>% 
      mutate(period = periods[i],
             point = st_as_sfc(point))
    sources_temp <- st_sf(sources_temp) %>% 
      st_set_crs(4326)
    ##Flows
    progress$set(detail = "Обработка потоков")
    flows_ln <- wb_names[grepl("Потоки", wb_names)]
    flows_temp <- data.frame()
    for (j in seq_along(flows_ln)) {
      flows_rank <- read.xlsx(flows_files[i], sheet = flows_ln[j])
      flows_temp <- rbind(flows_temp, flows_rank)
    }
    flows_temp <- flows_temp %>% 
      select(-c(8, 10:11)) %>% 
      group_by(end_type, start) %>% 
      mutate(dup = n(),
             row_n = row_number(),
             period = periods[i],
             type = if_else(start < 0, "inf", "source")) %>% 
      ungroup() %>% 
      mutate(start = abs(start),
             end = abs(end)) %>% 
      ungroup() %>% 
      filter(!(dup == 2 & row_n == 1)) %>% 
      select(-c(row_n, dup, end_type, start_type))
    ##Edges. Just tt_types dictionary for now
    # progress$set(detail = "Обработка ребер")
    # edges <- read.xlsx(flows_files[i], sheet = "Ребра")
    # tt_type <- edges %>% 
    #   select(treat_id, output_treat_name) %>% 
    #   distinct() %>% 
    #   filter(is.na(treat_id) == FALSE)
    ##Transport zones
    progress$set(detail = "Обработка групп зон и территорий")
    zones_temp <- read.xlsx(flows_files[i], sheet = "Зоны")
    chars <- which(sapply(zones_temp, class) == "character")
    zones_temp[ , chars] <- lapply(zones_temp[ , chars], `Encoding<-`, 'UTF-8')
    zones_temp <- zones_temp %>% 
      filter(`Название.группы.зон` == zone_name) %>% 
      select(-c(3, 5, 7)) %>% 
      mutate(period = periods[i]) %>% 
      distinct()
    dedicated_terr_temp <- read.xlsx(flows_files[i], sheet = wb_names[grepl("Выделенные", wb_names)])
    zones_temp <- left_join(dedicated_terr_temp, zones_temp,
                            by = "ID.административной.территори")
    ##Combine into bigger dataframes
    infs <- rbind(infs, infs_temp)
    sources <- rbind(sources, sources_temp)
    flows <- rbind(flows, flows_temp)
    # tt_types <- rbind(tt_types, tt_type)
    zones <- rbind(zones, zones_temp)
    ##set progress bar for shiny app
    inc <<- inc + 1
    progress$inc(1 / length(flows_files), detail = paste("Обработан файл номер", i))
    # end_time <- Sys.time()
    # print(paste0("Period ", periods[i], ": ", end_time))
    # print(end_time - start_time)
  }
  rm(flows_rank, flows_temp, flows_ln, i, j, infs_temp, sources_temp, 
     dedicated_terr_temp, zones_temp, wb_names)
  
  #merging and cleaning data
  ##add zones info into regions
  sources_terr <- st_sf(left_join(zones, distinct(select(sources, 4)),
                                  by = c("id" = "ID.выделленой.территории")))
  region <- st_join(region, distinct(select(sources_terr, c(3, 5:6, 8)),
                                     across(c(1:3)), 
                                     .keep_all = TRUE))
  region <- region %>% 
    mutate(popup = paste(paste0("<b>", region[[region_column]], "</b>"),
                         `ID.административной.территори`,
                         OKTMO,
                         `Название.зоны`, sep = "<br/>"))
  
  ##tt_types dictionary
  # tt_types <- distinct(tt_types)
  
  ##infs_colors
  infs_types <- infs %>% 
    st_drop_geometry() %>% 
    select(`Тип.объекта`) %>% 
    distinct() %>% 
    arrange(`Тип.объекта`) %>% 
    mutate(clr = palette.colors(n = length(`Тип.объекта`), palette = "Dark2"))
  
  ##flows points
  flows_src <- flows %>% 
    filter(type == "source") %>% 
    left_join(select(sources, c(id, point, `ID.выделленой.территории`)),
              by = c("start" = "id")) %>%
    left_join(distinct(select(zones, c(id,  `ID.административной.территори`, `Название.административной.территори`))),
              by = c("ID.выделленой.территории" = "id")) %>% 
    mutate(start_name = `Название.административной.территори`, .keep = "unused") %>% 
    select(-`ID.выделленой.территории`)
  
  flows_src_aggregated <- flows_src %>% 
    mutate(start = `ID.административной.территори`,
           original_id = `ID.административной.территори`) %>% 
    group_by(start, end, original_id, treatment_type, treat_id, output_treat_name, period,
             type, `ID.административной.территори`, start_name) %>% 
    summarise(flow = sum(flow),
              length = mean(length),
              volume = sum(volume)) %>% 
    ungroup() %>% 
    left_join(st_centroid(distinct(select(region, `ID.административной.территори`)),
                          of_largest_polygon = TRUE), 
              by = "ID.административной.территори") %>% 
    mutate(point = geometry, .keep = "unused")
  
  flows_inf <- flows %>% 
    filter(type == "inf") %>% 
    left_join(distinct(select(infs, c(ID, point, `Название`))),
              by = c("start" = "ID")) %>% 
    mutate(start_name = `Название`, .keep = "unused")
  
  flows <- bind_rows(flows_src, flows_inf) %>% 
    mutate(start_point = point, .keep = "unused") %>% 
    left_join(distinct(select(infs, c(ID, point, `Название`))),
              by = c("end" = "ID")) %>% 
    mutate(end_point = point, 
           end_name = `Название`, .keep = "unused") %>% 
    mutate(flow_line = map2(start_point, end_point,
                            ~ st_linestring(
                              matrix(rbind(st_coordinates(.x), st_coordinates(.y)), ncol = 2)))) %>% 
    mutate(flow_line = st_sfc(flow_line, crs = 4326)) %>% 
    select(-c(start_point, end_point))
  flows <- st_sf(flows)
  
  flows_aggregated <- bind_rows(flows_src_aggregated, flows_inf) %>% 
    mutate(start_point = point, .keep = "unused") %>% 
    left_join(distinct(select(infs, c(ID, point, `Название`))),
              by = c("end" = "ID")) %>% 
    mutate(end_point = point, 
           end_name = `Название`, .keep = "unused") %>% 
    mutate(flow_line = map2(start_point, end_point,
                            ~ st_linestring(
                              matrix(rbind(st_coordinates(.x), st_coordinates(.y)), ncol = 2)))) %>% 
    mutate(flow_line = st_sfc(flow_line, crs = 4326)) %>% 
    select(-c(start_point, end_point))
  
  flows_aggregated <- flows_aggregated[ , colnames(flows)]
  flows_aggregated <- st_sf(flows_aggregated)
  
  ##return result list
  flow_df_list <- list("infs" = infs, "sources" = sources, "flows" = flows, "flows_aggr" = flows_aggregated,
                       "zones" = zones, "infs_types" = infs_types, "region" = region, "periods" = periods)
  gc()
  return(flow_df_list)
}

##Function to recreate the set of data based on selected period and plot it on a map
get_flows_period <- function(flows_result, flow_map, set_period, flow_col, show_aggregated_flows = TRUE, leafletmap_name = "map_flows") {
  ##prepare regions subset and colors
  region_filt <- flows_result$region[which(flows_result$region$period == set_period), ]
  zones_ids <- sort(unique(region_filt$`ID.зоны`))
  region_col <- colorFactor(palette = "RdYlBu", domain = zones_ids)
  
  ##prepare flows subset and colors
  if (show_aggregated_flows == TRUE) {
    flows_filt <- flows_result$flows_aggr[which(flows_result$flows_aggr$period == set_period), ]
  } else {
    flows_filt <- flows_result$flows[which(flows_result$flows$period == set_period), ]
  }
  flows_split <- flows_filt %>%
    group_split(output_treat_name)
  fl_groups <- unlist(lapply(flows_split, function(x) unique(x$output_treat_name)))
  
  ##prepare infs popups
  infs_types <- lapply(flows_result["infs_types"], '[')[[1]]
  infs_filt <- lapply(flows_result["infs"], '[')[[1]] %>%
    select(-contains("тип.обращения")) %>%
    filter(period == set_period) %>%
    distinct() %>%
    group_by(ID) %>%
    mutate(popup = paste(paste0("<b>", `Название`, "</b>"),
                         `Тип.объекта`,
                         user_id, sep = "<br/>")) %>%
    ungroup() %>%
    left_join(infs_types, by = "Тип.объекта")
  
  ##prepare sources popups
  src_unhndl <- lapply(flows_result["flows"], '[')[[1]] %>%
    st_drop_geometry() %>%
    filter(period == set_period) %>%
    select(start) %>%
    distinct() %>%
    pull()
  sources_filt <- lapply(flows_result["sources"], '[')[[1]] %>%
    filter(period == set_period) %>%
    mutate(in_flow = if_else(id %in% src_unhndl, "removed", "not removed"))
  sources_filt_removed <- sources_filt %>%
    filter(in_flow == "removed") %>%
    group_split(`Выходной.тип.обращения`)
  sources_filt_unremoved <- sources_filt %>%
    filter(in_flow == "not removed") %>%
    group_split(`Выходной.тип.обращения`)
  
  ##Plot a map
  flow_map <- leafletProxy(map = leafletmap_name) %>% 
    clearShapes() %>%
    clearMarkers() %>% 
    removeControl(layerId = "infs_legend") %>% 
    addPolygons(data = region_filt,
                popup = region_filt$popup,
                fillColor = region_col(region_filt$ID.зоны),
                fillOpacity = 0.6,
                color = "black",
                weight = 1) %>%
    addAwesomeMarkers(data = infs_filt,
                      popup = infs_filt$popup,
                      icon = awesomeIcons(icon = "map-marker",
                                          markerColor = "lightgray",
                                          iconColor = infs_filt$clr),
                      group = "object_types") %>%
    addLegend(colors = infs_types$clr,
              labels = infs_types$Тип.объекта,
              group = "object_types",
              layerId = "infs_legend")
  
  for (i in seq_along(sources_filt_removed)) {
    sources_removed_temp <- sources_filt_removed[[i]]
    flow_map <- addCircleMarkers(map = flow_map,
                                 data = sources_removed_temp,
                                 radius = 2,
                                 weight = 1,
                                 color = "black",
                                 fillColor = flow_col(sources_removed_temp$Выходной.тип.обращения),
                                 fillOpacity = 1,
                                 group = fl_groups[fl_groups == unique(sources_removed_temp$Выходной.тип.обращения)])
  }
  
  for (i in seq_along(sources_filt_unremoved)) {
    sources_unremoved_temp <- sources_filt_unremoved[[i]]
    flow_map <- addCircleMarkers(map = flow_map,
                                 data = sources_unremoved_temp,
                                 radius = 5,
                                 weight = 1,
                                 color = "black",
                                 fillColor = "yellow",
                                 fillOpacity = 1,
                                 group = "unremoved")
  }
  
  for (i in seq_along(flows_split)) {
    fl_temp <- flows_split[[i]]
    flow_map <- addPolygons(map = flow_map,
                            data = fl_temp,
                            group = fl_groups[i],
                            weight = 3,
                            color = flow_col(fl_temp$output_treat_name))
  }
  
  flow_map <- addLayersControl(map = flow_map,
                               overlayGroups = fl_groups,
                               options = layersControlOptions(collapsed = FALSE))
  
  return(flow_map)
}

