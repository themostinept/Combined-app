#api Yandex справочника
yandex_geosearch_bb <- function(search_req, coords, apikey) {
  coords <- unlist(strsplit(trimws(coords), split = "_"))
  #First, prepare request phrase and convert coordinates from 'lat, lon' into 'lon, lat' format
  request <- URLencode(enc2utf8(search_req))
  coord1 <- unlist(strsplit(trimws(coords[1]), split = ","))
  coord1 <- paste(coord1[2], coord1[1], sep = ",")
  coord2 <- unlist(strsplit(trimws(coords[2]), split = ","))
  coord2 <- paste(coord2[2], coord2[1], sep = ",")
  #Combine a complete url for request  
  first_part <- "https://search-maps.yandex.ru/v1/?apikey="
  url_compl <- gsub(" ", "", paste(first_part, apikey, "&text=", request, "&type=biz&lang=ru_RU&", "bbox=", coord1, "~", coord2, "&results=500", collapse = ""))
  #Obtain a request results in json format  
  full_req <- suppressWarnings(fromJSON(paste(readLines(url_compl, encoding = 'UTF-8'), collapse="")))
  #Desired results are stored in element, called 'feautures'. Here we take from there only name, address, url and coordinates of an object 
  req_data <- full_req$features
  if (length(req_data) < 1) {
    return(NULL)
  }
  prop <- req_data$properties
  geo <- req_data$geometry
  vec_geo <- unlist(geo$coordinates)
  geo_df <- data.frame(lat = vec_geo[seq(2,length(vec_geo), by = 2)], lon = vec_geo[seq(1,length(vec_geo), by = 2)])
  #Combine our vectors in a dataframe
  total <- cbind(prop$name, prop$description, geo_df)
  colnames(total) <- c("Name", "Address", "Lat", "Lon")
  return(total)
}

#api Yandex геокодера
yandex_geocode <- function(geocode, apikey, rspn, coord_left_low, coord_right_up) {
  #Combine a complete url for request
  geocode <- paste(unlist(strsplit(geocode, split = " ")), collapse = "+")
  if (rspn == TRUE) {
    coord1 <- unlist(strsplit(coord_left_low, split = ", "))
    coord1 <- paste(coord1[2], coord1[1], sep = ",")
    coord2 <- unlist(strsplit(coord_right_up, split = ", "))
    coord2 <- paste(coord2[2], coord2[1], sep = ",")
    url <- gsub(" ", "", paste('https://geocode-maps.yandex.ru/1.x?apikey=', apikey, "&geocode=", curl_escape(iconv(geocode,"UTF-8")), "&rspn=1", "&bbox=", coord1, "~", coord2, collapse = ""))
  } else {
    url <- gsub(" ", "", paste('https://geocode-maps.yandex.ru/1.x?apikey=', apikey, "&geocode=", curl_escape(iconv(geocode,"UTF-8")), collapse = ""))
  }
  result <- as_list(read_xml(url))
  result_to_parse <- result$ymaps$GeoObjectCollection$featureMember$GeoObject$metaDataProperty$GeocoderMetaData
  found_add <- unlist(lapply(list(result_to_parse$text,
                                  result[["ymaps"]][["GeoObjectCollection"]][["featureMember"]][["GeoObject"]][["Point"]][["pos"]][[1]],
                                  result_to_parse$kind,
                                  result_to_parse$precision,
                                  result_to_parse$AddressDetails$Country$CountryName,
                                  result_to_parse$AddressDetails$Country$AdministrativeArea$AdministrativeAreaName,
                                  result_to_parse$AddressDetails$Country$AdministrativeArea$Locality$LocalityName,
                                  result_to_parse$AddressDetails$Country$AdministrativeArea$Locality$Thoroughfare$ThoroughfareName,
                                  result_to_parse$AddressDetails$Country$AdministrativeArea$Locality$Thoroughfare$Premise$PremiseNumber),
                             function(x) ifelse(is.null(x) == TRUE, NA, x)))
  return(found_add)
}
