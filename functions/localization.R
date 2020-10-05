list.of.packages <- c("sp", "dplyr", "padr", "rgdal", "raster", "sf")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(sp)
library(data.table)
library(dplyr)
library(padr)
library(rgdal)
library(raster)
library(sf)

beep_prep <- function(df, tag_id) {
  ###datafile
  #NodeId, TagId must be string
  ###rawbeepfile
  pre_count = nrow(df)
  df <- df[complete.cases(df), ]
  dropped_count = pre_count - nrow(df)
  print(paste("dropped", dropped_count, "n/a records from", pre_count, "records"))
  
  #if (dropped_count > 0) {logging.error('dropped {:,} n/a records from {:,} records'.format(dropped_count, pre_count))}
  #df = df.set_index('Time') set rownames to time?
  
  FILTER_MAX_RSSI = -20
  df = df[df$TagRSSI < FILTER_MAX_RSSI,]
  #def beep_count(self):
  #  return self.df.shape[0]
  if (!is.null(tag_id)) {
    df = df[df$TagId %in% tag_id,]}
  return(df)}

node_prep <- function(df) {
  df$NodeId <- toupper(df$NodeId)
  ###nodelocationfile
  colnames(df)[colnames(df)=="lat"] <- "node_lat"
  colnames(df)[colnames(df)=="lng"] <- "node_lng"
  
  long2UTM <- function(long) {
    (floor((long + 180)/6) %% 60) + 1
  }
  
  df$zone <- long2UTM(df$node_lng)
  
  lat2UTM <- function(Lat) {
    if (Lat < -72) {Letter='C'
    } else if (Lat < -64) {Letter='D'
    } else if (Lat < -56) {Letter='E'
    } else if (Lat < -48) {Letter='F'
    } else if (Lat < -40) {Letter='G'
    } else if (Lat < -32) {Letter='H'
    } else if (Lat < -24) {Letter='J'
    } else if (Lat < -16) {Letter='K'
    } else if (Lat < -8) {Letter='L'
    } else if (Lat < 0) {Letter='M'
    } else if (Lat < 8) {Letter='N'
    } else if (Lat < 16) {Letter='P'
    } else if (Lat < 24) {Letter='Q'
    } else if (Lat < 32) {Letter='R'
    } else if (Lat < 40) {Letter='S'
    } else if (Lat < 48) {Letter='T'
    } else if (Lat < 56) {Letter='U'
    } else if (Lat < 64) {Letter='V'
    } else if (Lat < 72) {Letter='W'
    } else {Letter='X'}
    return(Letter)}
  
  df$letter <- sapply(df$node_lat, lat2UTM)
  df$x <- df$node_lng
  df$y <- df$node_lat
  
  LongLatToUTM <- function(data_frame){
    ## Args: df, data frame must have x and y columns. Should be from same UTM zone.
    ## Create a spatial dataframe
    coordinates(data_frame) <- ~x+y
    proj4string(data_frame) <- CRS("+proj=longlat +datum=WGS84")  
    
    ## Get zones for all the points in the data frame. 
    ## Stop if more than one zone is present. 
    ## You can write your own code for handling cases where your 
    ## data comes from different UTM zones.
    
    zone <- long2UTM(data_frame$x)
    if (length(unique(zone)) > 1) stop("values from different UTM zones")
    zone <- unique(zone)
    
    ## Change CRS of the spatial data frame and convert to data frame
    res <- spTransform(data_frame, CRS(paste0("+proj=utm +zone=", zone, "+datum=WGS84")))
    return(as.data.frame(res))
  }
  
  nodes <- LongLatToUTM(df)
  return(nodes)}
#if delta > 0:
#  logging.warning('dropped {:,} records after merging node locations'.format(delta))
#self.df = df

merge_df <- function(beep_df, node_df, tag_id=NULL, channel=NULL) {
  df <- beep_prep(beep_df, tag_id)
  nodes_df <- node_prep(node_df)
  beep_count <- nrow(df) 
  if (any(nodes_df$NodeId %in% df$NodeId)) {
  df <- merge(df,nodes_df, by="NodeId") } else {print("none of those nodes are in this data! node file not merged")}
  delta = beep_count - nrow(df)
  if (delta > 0) {print(paste("dropped",delta,"records after merging node locations"))}
  if (!is.null(channel)) {
    df <- df[df$RadioId %in% channel,]
  }
  return(df)}

###nodedataset
#beep_data <- beep_prep(beep_data)
#channels <- sort(unique(df$RadioId))

###NOT CONVERTED: station/gpsfile.py, nodehealthfile.py
###api

#self.MAX_NODES = max_nodes

#merged_df <- data.table(df)
#tags <- merged_df[, .N,by="TagId"]
#tags <- tags[order(-tags$N),]

#if (exists("n_tags")) {
#  tags <- tags[1:n_tags,]
  #return tag index for tag in tags
#}

get_radius_from_rssi <- function(rssi, path_loss_coefficient=5) {
  alpha = 1
  MAX_RSSI = -40
  delta_rssi = 0-(MAX_RSSI + rssi)
  exponent = delta_rssi / (10*path_loss_coefficient)
  radius = alpha * 10^exponent
  return(radius)
}

advanced_resampled_stats <- function(beeps, node, freq, tag_id=NULL, channel=NULL) {
  df <- merge_df(beeps, node, tag_id, channel)
  filtered_df <- df %>% thicken(freq, colname="freq") %>%
    group_by(TagId, RadioId, freq, NodeId) %>%
    summarise(max_rssi = max(TagRSSI), beep_count = length(TagRSSI), node_x = max(x), node_y = max(y), min_rssi = min(TagRSSI), std_rssi = sd(TagRSSI),
              node_lat = max(node_lat), node_lng = max(node_lng))
  outdf <- as.data.frame(filtered_df)
  DEFAULT_PATH_LOSS_COEFFICIENT=5
  outdf$radius <- sapply(outdf$max_rssi, get_radius_from_rssi, DEFAULT_PATH_LOSS_COEFFICIENT)
  return(outdf)}

weighted_average <- function(freq, beeps, node, MAX_NODES=0, tag_id=NULL, channel=NULL) {
  df <- merge_df(beeps, node, tag_id, channel)
  zone = df$zone[1]
  letter = df$letter[1]
  filtered_df <- advanced_resampled_stats(beeps,node,freq)
  #filtered_df <- merge(filtered_df, noderssi, by="NodeId")
  filtered_df$weight <- filtered_df$beep_count
  #filtered_df$weight <- filtered_df$beep_count/(filtered_df$max_rssi*-1)#(filtered_df$V1*-1)
  filtered_df$num_x <- filtered_df$node_x*filtered_df$weight
  filtered_df$num_y <- filtered_df$node_y*filtered_df$weight
  filtered_df <- filtered_df[order(filtered_df$TagId, filtered_df$RadioId, filtered_df$freq, -filtered_df$max_rssi),]
  filtered_df <- data.table(filtered_df)
  #WHAT NEEDS TO BE ADDED HERE: SORT BY FREQ THEN MAX RSSI AND ONLY KEEP TOP X ROWS WITHIN EACH RESAMPLE TIME PERIOD
  if (MAX_NODES > 0) {
    filtered_df <- filtered_df[, head(.SD, MAX_NODES), by=c("TagId", "RadioId", "freq")]
  }
  outdf <- filtered_df %>% group_by(TagId, RadioId, freq) %>%
    summarise(num_x = sum(num_x), num_y = sum(num_y), total=sum(weight), unique_nodes = length(unique(NodeId)), easting = mean(node_x), northing = mean(node_y)) #lat = mean(node_lat), lng = mean(node_lng), 
  outdf <- as.data.frame(outdf)
  outdf$avg_x <- outdf$num_x / outdf$total
  outdf$avg_y <- outdf$num_y / outdf$total
  
  outdf$avex <- outdf$avg_x
  outdf$avey <- outdf$avg_y
  #CALCULATE BACK TO LAT/LNG?
  outdf$zone <- zone
  outdf$letter <- letter
  outdf$date <- format(outdf$freq, "%Y-%m-%d")
  outdf$time_of_day <- format(outdf$freq, "%H:%M:%s")
  outdf$hour <- as.integer(format(outdf$freq, "%H"))
  outdf <- outdf[complete.cases(outdf),]
  coordinates(outdf) <- ~avg_x+avg_y
  crs(outdf) <- CRS(paste0("+proj=utm +zone=", zone, "+datum=WGS84")) 
  outdf <- spTransform(outdf,CRS("+proj=longlat +datum=WGS84"))
  return(outdf)}

export_locs <- function(y, beeps, node, tag_id=NULL, outpath) {
  lapply(y, function(x) {
    locations <- weighted_average(x, beeps, node, 0, tag_id)
    locations <- data.frame(x=coordinates(locations)[,1], y=coordinates(locations)[,2], locations@data)
    write.csv(locations,paste(outpath,gsub(" ", "", paste("estimates_",x,".csv",sep=""), fixed = TRUE)))})
}

node_file <- function(health) {
  if (nrow(health) < 1) stop("no node health data!")
  health$timediff <- as.integer(health$Time - health$RecordedAt)
  health <- health[health$timediff == 0,]
  health <- aggregate(health[,c("Latitude", "Longitude")],list(health$NodeId), mean, na.rm=TRUE)
  if (any(is.na(health))) {health <- health[-which(is.na(health$Latitude) | is.na(health$Latitude)),]}
  #
  colnames(health)[colnames(health)=="Latitude"] <- "lat"
  colnames(health)[colnames(health)=="Longitude"] <- "lng"
  colnames(health)[colnames(health)=="Group.1"] <- "NodeId"
return(health)}

export_node <- function(health, out_path) {
  nodehealth <- node_file(health)
  write.csv(nodehealth,file=paste0(outpath,"node_loc.csv"))
}