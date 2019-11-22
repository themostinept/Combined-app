yandex_geocode <- function(search_line, apikey, ...) {
  geo_find <- function(geocode, rspn = 0, coord_left_low, coord_right_up) {
    #Combine a complete url for request
    geocode <- paste(unlist(strsplit(geocode, split = " ")), collapse = "+")
    if (rspn == 1) {
      coord1 <- unlist(strsplit(coord_left_low, split = ", "))
      coord1 <- paste(coord1[2], coord1[1], sep = ",")
      coord2 <- unlist(strsplit(coord_right_up, split = ", "))
      coord2 <- paste(coord2[2], coord2[1], sep = ",")
      url <- gsub(" ", "", paste('https://geocode-maps.yandex.ru/1.x?apikey=', apikey, "&geocode=", curl_escape(iconv(geocode,"UTF-8")), "&rspn=", rspn, "&bbox=", coord1, "~", coord2, collapse = ""))
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
                                    result_to_parse$AddressDetails$Country$AdministrativeArea$Locality$Thoroughfare$Premise$PremiseNumber), function(x) ifelse(is.null(x) == T, NA, x)))
    print(found_add)
    return(found_add)
  }
  geocode_result <- lapply(search_line, function(x) tryCatch(geo_find(x),
                                                             error = function(e) 'error'))
  geocode_result <- as.data.frame(do.call(rbind, geocode_result))
  colnames(geocode_result) <- c('AddressLine', 'point',	'kind', 'precision', 'Country', 'AdministrativeAreaName',	'LocalityName',	'ThoroughfareName',	'PremiseNumber')
  geocode_result <- cbind(search_line, geocode_result)
  return(geocode_result)
}
