# find UTM: https://gis.stackexchange.com/questions/311502/how-to-use-st-buffer
lonlat2UTM <- function(p) {
  point <- as.numeric(unlist(str_split(p, " ")))
  utm <- (floor((point[1] + 180) / 6) %% 60) + 1
  ifelse(point[2] > 0, utm + 32600, utm + 32700)
}
