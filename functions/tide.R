numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

tide_station <- function(lon,lat) {
  #  convert <- plot_data[order(plot_data$ID,plot_data$cut),]
  #  mins <- difftime(convert$cut[2], convert$cut[1], units="mins")
  #  mins <- ifelse(as.numeric(mins) < 1, 1, as.numeric(mins))
  tdist <- read_html("https://flaterco.com/xtide/locations.html")
  tides <- tdist %>% html_table()
  tides <- tides[[1]]
  tides <- tides[tides$Type=="Ref",]
  tides$lat <- as.numeric(numextract(sapply(strsplit(tides$Latitude, " "), "[[", 1)))
  tides$latdir <- sub("^([[:alpha:]]*).*", "\\1", sapply(strsplit(tides$Latitude, " "), "[[", 2))
  tides$lon <- as.numeric(numextract(sapply(strsplit(tides$Longitude, " "), "[[", 1)))
  tides$londir <- sapply(strsplit(tides$Longitude, " "), "[[", 2)
  tides$lat[tides$latdir=="S"] <- tides$lat[tides$latdir=="S"]*-1
  tides$lon[tides$londir=="W"] <- tides$lon[tides$londir=="W"]*-1
  
  DT_sf = st_as_sf(tides, coords = c("lon", "lat"), 
                   crs = 4326, agr = "constant")
  x <- st_sfc(st_point(c(lon,lat)))
  st_crs(x) <- 4326
  y <- DT_sf[st_nearest_feature(x, DT_sf),]
  tidedata <- node[!duplicated(node$Time),]
  tidedata$DateTime <- tidedata$Time
  tidedata$Station <- y$Name
  #  tidedata$DateTime <- as.Date(tidedata$Time)
  
  localtide <- tide_height_data(tidedata[,c("Station","DateTime")])
  return(localtide)}

tide_times <- function(lon, lat) {
  local_tide <- as.data.frame(tide_station(lon, lat))
  colnames(local_tide)[colnames(local_tide)=="DateTime"] <- "time"
  local_tide <- local_tide[order(local_tide$time),]
  colnames(local_tide)[colnames(local_tide)=="TideHeight"] <- "h"
  local_tide$Station <- NULL
  predicted <- extrema(local_tide, min(local_tide$h))
  return(predicted)}