##get all df's from debug-files
get_flows_df <- function(flows_files, region, zone_name = "Зоны транспортирования") {
  #prepare flow files
  flow_df_list <- list()
  infs <- data.frame()
  sources <- data.frame()
  flows <- data.frame()
  # tt_types <- data.frame()
  zones <- data.frame()
  for (i in seq_along(flows_files)) {
    start_time <- Sys.time()
    print(paste0("Period ", periods[i], ": ", start_time))
    wb_names <- getSheetNames(flows_files[i])
    ##Infrastructure dataframe
    infs_temp <- read.xlsx(flows_files[i], sheet = wb_names[grepl("Объекты", wb_names)])
    infs_temp <- infs_temp %>% 
      filter(node_type == "input") %>% 
      select(c(3, 7, 10, 11, 12, 19, 20)) %>% 
      mutate(period = periods[i],
             point = st_as_sfc(point))
    infs_temp <- st_sf(infs_temp)
    ##Sources dataframe
    sources_temp <- read.xlsx(flows_files[i], sheet = "Источники")
    sources_temp <- sources_temp %>% 
      select(c(1:3, 6:7)) %>% 
      mutate(period = periods[i],
             point = st_as_sfc(point))
    sources_temp <- st_sf(sources_temp)
    ##Flows
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
      mutate(start = abs(start),
             end = abs(end)) %>% 
      ungroup() %>% 
      filter(!(dup == 2 & row_n == 1)) %>% 
      select(-c(row_n, dup, end_type, start_type))
    ##Edges. Just tt_types dictionary for now
    # edges <- read.xlsx(flows_files[i], sheet = "Ребра")
    # tt_type <- edges %>% 
    #   select(treat_id, output_treat_name) %>% 
    #   distinct() %>% 
    #   filter(is.na(treat_id) == FALSE)
    ##Transport zones
    zones_temp <- read.xlsx(flows_files[i], sheet = "Зоны")
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
    end_time <- Sys.time()
    print(paste0("Period ", periods[i], ": ", end_time))
    print(end_time - start_time)
  }
  rm(flows_rank, flows_temp, flows_ln, i, j, infs_temp, sources_temp, 
     dedicated_terr_temp, zones_temp, wb_names, start_time, end_time)
  
  #merging and cleaning data
  ##add zones info into regions
  sources_terr <- st_sf(left_join(zones, distinct(select(sources, 4)),
                                  by = c("id" = "ID.выделленой.территории")))
  region <- st_join(region, distinct(select(sources_terr, c(3, 5:6, 8)),
                                     across(c(1:3)), 
                                     .keep_all = TRUE)) %>% 
    mutate(popup = paste(paste0("<b>", LOCNAME, "</b>"),
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
                       "zones" = zones, "infs_types" = infs_types, "region" = region)
  gc()
  return(flow_df_list)
}
