library(gtools)
y <- '58.622468, 31.406503'
z <- '58.461637, 31.118112'
make_grid <- function(ru, ld) {
  coord1 <- as.numeric(unlist(strsplit(trimws(ru), split = ",")))
  coord2 <- as.numeric(unlist(strsplit(trimws(ld), split = ",")))
  s1 <- seq(from = coord2[1], to = coord1[1], length.out = 6)
  s2 <- seq(from = coord2[2], to = coord1[2], length.out = 6)
  ss <- expand.grid(s1, s2)
  ss <- ss %>% 
    unite(col = coords, Var1, Var2, sep = ", ") %>% 
    mutate(n = rep(1:(length(s1)), length(s1)))
coords <- matrix(nrow = 5, ncol = 5)
for(i in 1:max(ss[, 2])) {
      if (i + 1 < max(ss[, 2]) + 1) {
        lcv <- ss[which(ss[, 2] == i), 1]
        lcv <- lcv[-length(lcv)]
        rcv <- ss[which(ss[, 2] == (i + 1)), 1]
        rcv <- rcv[-1]
        n <- i
        coords[, i] <- paste(lcv, rcv, sep = "_")
      }
  }
  return(coords)
}
as.data.frame(do.call(rbind, res))

x <- 'АЗС'
apikey <- '3d382e5a-4cbd-4f19-86e0-112a084057a9'
coords <- paste(s11, s22, sep = "_")

yandex_geosearch_bb <- function(x, coord_left_low, coord_right_up, apikey, export = F) {
  require(jsonlite)
  require(openxlsx)
  coords <- unlist(strsplit(trimws(coords), split = "_"))
  
  #First, prepare request phrase and convert coordinates from 'lat, lon' into 'lon, lat' format
  request <- URLencode(enc2utf8(x))
  coord1 <- unlist(strsplit(trimws(coord_left_low), split = ","))
  coord1 <- paste(coord1[2], coord1[1], sep = ",")
  coord2 <- unlist(strsplit(trimws(coord_right_up), split = ","))
  coord2 <- paste(coord2[2], coord2[1], sep = ",")
  
  #Combine a complete url for request  
  first_part <- "https://search-maps.yandex.ru/v1/?apikey="
  url_compl <- gsub(" ", "", paste(first_part, apikey, "&text=", request, "&type=biz&lang=ru_RU&", "bbox=", coord1, "~", coord2, "&results=500", collapse = ""))
  
  #Obtain a request results in json format  
  full_req <- suppressWarnings(fromJSON(paste(readLines(url_compl, encoding = 'UTF-8'), collapse="")))
  
  #Desired results are stored in element, called 'feautures'. Here we take from there only name, address, url and coordinates of an object 
  req_data <- full_req$features
  if (length(req_data) < 1) {
    return('Nothing to show')
  }
  prop <- req_data$properties
  geo <- req_data$geometry
  vec_geo <- unlist(geo$coordinates)
  geo_df <- data.frame(lat = vec_geo[seq(2,length(vec_geo), by = 2)], lon = vec_geo[seq(1,length(vec_geo), by = 2)])
  
  #Combine our vectors in a dataframe and save it as csv if 'export' argument was set to 'TRUE'
  total <- cbind(prop$name, prop$description, ifelse(is.null(prop$CompanyMetaData$url) == TRUE, rep(NA, length(prop$name)), prop$CompanyMetaData$url), geo_df)
  colnames(total) <- c("Name", "Address", "URL", "Lat", "Lon")
  if (export == TRUE) {
    write.xlsx(total, 'request_api_result.xlsx')
  }
  return(total)
}