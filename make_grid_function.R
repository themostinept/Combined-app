#A little tool to break a big bounding box (for yandex geosearch) into smaller rectangles.
#Coordinates should be passed in a format from yandex maps ('lat, lon').
# TEST------------------------------------------
# ru <- "55.953409, 37.872145"
# ld <- "55.570540, 37.374809"
# height <- 3
# width <- 2
# ----------------------------------------------
make_grid <- function(ru, ld, height, width) {
  if (height > 1 || width > 1) {
    coord1 <- as.numeric(unlist(strsplit(trimws(ru), split = ",")))
    coord2 <- as.numeric(unlist(strsplit(trimws(ld), split = ",")))
    s1 <- seq(from = coord2[1], to = coord1[1], length.out = height + 1)
    s2 <- seq(from = coord2[2], to = coord1[2], length.out = width + 1)
    ss <- expand.grid(s1, s2)
    ss <- ss %>% 
      unite(col = coords, Var1, Var2, sep = ", ") %>% 
      mutate(n = rep(1:(length(s1)), length(s2)))
    coords <- matrix(nrow = width, ncol = height)
    for(i in 1:max(ss[ , 2])) {
      if (i + 1 < max(ss[ , 2]) + 1) {
        lcv <- ss[which(ss[ , 2] == i), 1]
        lcv <- lcv[-length(lcv)]
        rcv <- ss[which(ss[ , 2] == (i + 1)), 1]
        rcv <- rcv[-1]
        coords[, i] <- paste(lcv, rcv, sep = "_")
      }
    }
    coords <- matrix(coords, ncol = 1)
  } else {
    coords <- paste(ld, ru, sep = "_")
  }
  return(coords)
}
