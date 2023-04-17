## ---------------------------
##
## Script name: localization.R
##
## Purpose of script: produce location estimates
##
## Author: Dr. Jessica Gorzo and Dr. Kristina Paxton
##
## Date Created: 2020-07-13
##
## Email: jessica.gorzo@celltracktech.com
##
## ---------------------------
##
## Notes: 
##
## ---------------------------

options(scipen = 6, digits = 9) # I prefer to view outputs in non-scientific notation

## ---------------------------


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
  df$Time <- as.POSIXct(df$Time, tz="UTC")
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

node_prep <- function(df, latlng) {
  df$NodeId <- toupper(df$NodeId)
  ###nodelocationfile
  colnames(df)[colnames(df)=="lat"] <- "node_lat"
  colnames(df)[colnames(df)=="lng"] <- "node_lng"
  
  long2UTM <- function(long) {
    (floor((long + 180)/6) %% 60) + 1
  }
  
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
  
  if(latlng) {
  
    df$zone <- long2UTM(df$node_lng)
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
      res <- spTransform(data_frame, CRS(paste0("+proj=utm +zone=", zone, " +datum=WGS84")))
      return(as.data.frame(res))
    }
  
  nodes <- LongLatToUTM(df) } else {
    nodes <- df
    nodes$y <- nodes$node_lat
    nodes$x <- nodes$node_lng}
  return(nodes)}
#if delta > 0:
#  logging.warning('dropped {:,} records after merging node locations'.format(delta))
#self.df = df

merge_df <- function(beep_df, node_df, tag_id=NULL, latlng) {
  df <- beep_prep(beep_df, tag_id)
  nodes_df <- node_prep(node_df, latlng)
  beep_count <- nrow(df) 
  if (any(nodes_df$NodeId %in% df$NodeId)) {
  df <- merge(df,nodes_df, by="NodeId") } else {print("none of those nodes are in this data! node file not merged")}
  delta = beep_count - nrow(df)
  if (delta > 0) {print(paste("dropped",delta,"records after merging node locations"))}
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

advanced_resampled_stats <- function(beeps, node, node_health=NULL, freq, tag_id=NULL, keep_cols = NULL, calibrate = NULL, latlng = TRUE) {
  df <- merge_df(beeps, node, tag_id, latlng)
  cols <- c("TagRSSI", "x", "y", "node_lat", "node_lng")
  if(!is.null(node_health)) {
    node_health$nodetime <- node_health$Time
    node_health <- node_health[order(node_health$Time, node_health$NodeId),]
    node_health$channel <- node_health$RadioId
    node_health <- data.table(node_health)
  
    beep_data <- beep_prep(beeps, tag_id)
    beep_data <- beep_data[order(beep_data$Time, beep_data$NodeId),]
    beep_data$beeptime <- beep_data$Time
    beep_data <- data.table(beep_data)
  
    setkey(beep_data, NodeId, Time)
    setkey(node_health, NodeId, Time)
  #https://www.r-bloggers.com/understanding-data-table-rolling-joins/
    nodebeep <- node_health[beep_data, roll = "nearest", mult="first"] 
    merged_df <- data.table(df)
    df <- merged_df[nodebeep, on=c("NodeId", "Time", "RadioId", "TagId")]
    cols <- c(cols, "NodeRSSI")
  }
  
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  df <- as.data.frame(df)
  min_max <- list(
    min = ~min(.x, na.rm = TRUE), 
    max = ~max(.x, na.rm = TRUE),
    length = ~length(.x),
    sd = ~sd(.x, na.rm = TRUE),
    mean = ~mean(.x, na.rm = TRUE),
    mode = ~getmode(.x)
  )
 
  if(!is.null(keep_cols)) {cols <- c(cols, keep_cols)}
  if(is.null(calibrate)) {
  filtered_df <- df %>% thicken(freq, colname="freq", by="Time") %>%
    group_by(TagId, RadioId, freq, NodeId) %>%
    summarise_at(cols, min_max)
  #filtered_df$freq <- as.POSIXct(filtered_df$freq, tz="UTC")
  } else {
    df$freq <- df[,c(calibrate)]
    cols <- c(cols, "Time")
    filtered_df <- df %>% 
      group_by(TagId, RadioId, freq, NodeId) %>%
      summarise_at(cols, min_max)
  }
  outdf <- as.data.frame(filtered_df)
  if(inherits(outdf$freq, "Date")) {outdf$freq <- as.POSIXct(paste(outdf$freq, "00:00:00"), tz="UTC")}

  DEFAULT_PATH_LOSS_COEFFICIENT=5
  outdf$radius <- sapply(outdf$TagRSSI_max, get_radius_from_rssi, DEFAULT_PATH_LOSS_COEFFICIENT)
  outdf$beep_count <- outdf$TagRSSI_length
  outdf$node_x <- outdf$x_min
  outdf$node_y <- outdf$y_min #could NULL some columns here
  #outdf$node_dff <- (max(outdf$NodeRSSI_mean) - outdf$NodeRSSI_mean)
  #outdf$node_exp <- outdf$node_dff^(2)
  return(outdf)}

weighted_average <- function(freq, beeps, node, node_health=NULL, MAX_NODES=0, tag_id=NULL, calibrate = NULL, keep_cols = NULL, latlng = TRUE, minRSSI = 0) {
  
  df <- merge_df(beeps, node, tag_id, latlng)
  
  zone <- df$zone[1]
  letter <- df$letter[1]
  filtered_df <- advanced_resampled_stats(beeps = beeps, node = node, node_health = node_health, freq = freq, tag_id = tag_id, calibrate = calibrate, keep_cols = keep_cols)
  #filtered_df <- merge(filtered_df, noderssi, by="NodeId")
  #filtered_df$weight <- filtered_df$beep_count
  filtered_df$id <- paste(filtered_df$TagId, filtered_df$freq, filtered_df$NodeId)
  filtered_df <- filtered_df[order(filtered_df$id, -filtered_df$TagRSSI_mean, -filtered_df$beep_count),]
  filtered_df <- filtered_df[!duplicated(filtered_df$id),]
  if (minRSSI < 0) {filtered_df <- filtered_df[filtered_df$TagRSSI_mean > minRSSI,]}
  filtered_df$weight <- (filtered_df$beep_count)/(filtered_df$TagRSSI_mean)
  filtered_df$num_x <- filtered_df$node_x*filtered_df$weight
  filtered_df$num_y <- filtered_df$node_y*filtered_df$weight
  filtered_df <- filtered_df[order(filtered_df$TagId, filtered_df$freq, -filtered_df$TagRSSI_mean),]
  filtered_df <- data.table(filtered_df)
  
  allnodes <- filtered_df[, length(unique(NodeId)), by=c("TagId", "freq")]
  allnodes$pid <- paste(allnodes$TagId, allnodes$freq)

  #WHAT NEEDS TO BE ADDED HERE: SORT BY FREQ THEN MAX RSSI AND ONLY KEEP TOP X ROWS WITHIN EACH RESAMPLE TIME PERIOD
  if (MAX_NODES > 0) {
    filtered_df <- filtered_df[, head(.SD, MAX_NODES), by=c("TagId", "freq")]
  }
  
  if (!is.null(calibrate)) {
    outdf <- filtered_df %>% group_by(TagId, freq) %>%
      summarise(num_x = sum(num_x, na.rm=TRUE), num_y = sum(num_y, na.rm=TRUE), total=sum(weight, na.rm=TRUE), unique_nodes = length(unique(NodeId)), easting = mean(node_x, na.rm=TRUE), northing = mean(node_y, na.rm=TRUE), Time = max(Time_max, na.rm=TRUE)) #lat = mean(node_lat), lng = mean(node_lng), 
  } else {
  outdf <- filtered_df %>% group_by(TagId, freq) %>%
    summarise(num_x = sum(num_x, na.rm=TRUE), num_y = sum(num_y, na.rm=TRUE), total=sum(weight, na.rm=TRUE), unique_nodes = length(unique(NodeId)), easting = mean(node_x, na.rm=TRUE), northing = mean(node_y, na.rm=TRUE))
  }
  #lat = mean(node_lat), lng = mean(node_lng), 
  outdf$id <- paste(outdf$TagId, outdf$freq)
  outdf$total_nodes <- allnodes$V1[match(outdf$id, allnodes$pid)]
  outdf <- as.data.frame(outdf)
  outdf$avg_x <- outdf$num_x / outdf$total
  outdf$avg_y <- outdf$num_y / outdf$total
  
  outdf$avex <- outdf$avg_x
  outdf$avey <- outdf$avg_y
  #CALCULATE BACK TO LAT/LNG?
  outdf$zone <- zone
  outdf$letter <- letter
  if (!inherits(outdf$freq, "POSIXct")) {
    outdf$group <- outdf$freq
    outdf$freq <- outdf$Time
    }
  outdf$date <- format(outdf$freq, "%Y-%m-%d")
  outdf$time_of_day <- format(outdf$freq, "%H:%M:%S")
  outdf$hour <- as.integer(format(outdf$freq, "%H"))
  outdf <- outdf[complete.cases(outdf),]
  coordinates(outdf) <- ~avg_x+avg_y
  crs(outdf) <- CRS(paste0("+proj=utm +zone=", zone, " +datum=WGS84")) 
  outdf <- spTransform(outdf,CRS("+proj=longlat +datum=WGS84"))
  return(outdf)}

export_locs <- function(y, beeps, node, tag_id=NULL, outpath) {
  lapply(y, function(x) {
    locations <- weighted_average(freq = x, beeps = beeps, node = node, MAX_NODES = 0, tag_id = tag_id)
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

nodes_spatial <- function(nodes) {
  nodes$NodeId <- toupper(nodes$NodeId)
  nodespatial <- nodes
  coordinates(nodespatial) <- ~lng+lat
  crs(nodespatial) <- CRS("+proj=longlat +datum=WGS84")  
  return(nodespatial)}

calibrate <- function(beep_data, calibration, nodes, calibrate = TRUE, freq = "3 min", max_nodes = 0) {
  beep_data <- beep_data[beep_data$TagId %in% calibration$TagId,]
  dt1 <- data.table(beep_data, start=beep_data$Time, end=beep_data$Time)
  dt2 <- data.table(calibration)
  setkey(dt2, TagId, start, end)
  indx <- foverlaps(dt1, dt2, type='within')
  beep_data <- indx[!is.na(indx$start),]
  if(isTRUE(calibrate)) {
    test <- advanced_resampled_stats(beep_data, nodes, freq = freq, keep_cols = c("TagLat", "TagLng", "pt"), calibrate = "session_id")
  } else {
    test <- advanced_resampled_stats(beep_data, nodes, freq = freq, keep_cols = c("TagLat", "TagLng", "pt"))
  }
  test$id <- paste(test$TagId, test$freq, test$NodeId)
  test <- test[order(test$id, -test$TagRSSI_mean, -test$beep_count),]
  test <- test[!duplicated(test$id),]
  test$groups <- paste(test$TagId, test$freq)
  alltags <- test[!duplicated(test$groups),]
  pts <- test[!duplicated(test$pt_min),]
  
  tag_loc <- alltags
  coordinates(tag_loc) <- ~TagLng_min+TagLat_min
  crs(tag_loc) <- CRS("+proj=longlat +datum=WGS84") 
  
  nodespatial <- nodes_spatial(nodes)
  
  dst <- raster::pointDistance(tag_loc, nodespatial, lonlat = T, allpairs = T)
  dist_df <- data.frame(dst, row.names = tag_loc$groups)
  colnames(dist_df) <- nodespatial$NodeId
  dist_df$Test.Group <- rownames(dist_df)
  
  dist.gather <- dist_df %>%
    tidyr::gather(key = "NodeId", value = "distance", -Test.Group)
  dist.gather$id <- paste(dist.gather$Test.Group, dist.gather$NodeId)
  test$distance <- dist.gather$distance[match(test$id, dist.gather$id)]
  exp.mod <- nls(TagRSSI_mean ~ SSasymp(distance, Asym, R0, lrc), data = test)
  
  a <- coef(exp.mod)[["R0"]]
  S <- exp(coef(exp.mod)[["lrc"]])
  K <- coef(exp.mod)[["Asym"]]
  
  all_data <- data.frame(TagId = test$TagId, NodeId = test$NodeId, long = test$node_lng_min, lat = test$node_lat_min, avg.RSSI = test$TagRSSI_mean, Test.Group = paste(test$TagId, test$freq))
  return(list(all_data, a, S, K))}

triangulate <- function(all_data, rssi = -100, node = 3, distance = relation) {
  test.g90.dat <- all_data[all_data$avg.RSSI > rssi,]
  sample.size <- test.g90.dat %>%
    dplyr::group_by(Test.Group) %>%
    dplyr::summarise(n.nodes = n()) %>%
    dplyr::filter(n.nodes < node)
  test.red.dat <- test.g90.dat[!test.g90.dat$Test.Group %in% sample.size$Test.Group,]
  #K = -100.8424
  #a = -63.06914
  #S = 0.009777435
  x <- test.red.dat$avg.RSSI
  test.red.dat$dist.est <- eval(parse(text=distance))
  #changed sign on S despite relationship in Paxton's original script
  estimated.location_results <- data.frame(Test.Group=character(), long.est=numeric(), lat.est=numeric(), est.error =numeric())
  tests = unique(test.red.dat$Test.Group)
  for(j in 1:length(tests)) {
    
    # Isolate the test 
    sub.test <- test.red.dat %>% dplyr::filter(Test.Group == tests[j]) 
    
    # Determine the node with the strongest avg.RSSI value to be used as starting values
    max.RSSI <- sub.test[which.max(sub.test$avg.RSSI),]
    print(j)
    # Non-linear test to optimize the location of unknown signal by looking at the radius around each Node based on RSSI values (distance) and the pairwise distance between all nodes
    nls.test <- nls(dist.est ~ geosphere::distm(data.frame(long, lat), c(lng_solution, lat_solution), fun=distHaversine), # distm - matrix of pairwise distances between lat/longs
                    data = sub.test, start=list(lng_solution=max.RSSI$long, lat_solution=max.RSSI$lat), # used long/lat of NodeId with largest RSSI identified in max.RSSI
                    control=nls.control(warnOnly = T, minFactor=1/30000)) # gives a warning, but doesn't stop the test from providing an estimate based on the last iteration before the warning
    
    # Determine error around the point location estimate
    c <- car::confidenceEllipse(nls.test, levels=0.95) 
    ellipse_line <- c[1, ] # isolating one point on the line 
    ellipse_line <- rbind(ellipse_line, coef(nls.test)) # bringing together the one isolated point on the line and the estimated point - coef(nls.test)
    est.error <-(distm(ellipse_line[1,], ellipse_line[2,]))
    
    # estimated location of the test and error
    estimated.loc <- data.frame(Test.Group = tests[j], long.est = ellipse_line[2,1], lat.est = ellipse_line[2,2], est.error = est.error)
    
    # Populate dataframe with results
    estimated.location_results <- rbind(estimated.location_results, estimated.loc)
    
  }
  
  estimated.location_results$session_id <- sapply(strsplit(as.character(estimated.location_results$Test.Group), " "), "[[", 2)
  return(estimated.location_results)}

relate <- function(a, S, K) {
  form <- paste0("abs(log((x - ",K,")/abs(",a,"))/",S,")")
  print(form)
  return(form)}


loc_prep <- function(beep_data, nodes, freq) {
  test <- advanced_resampled_stats(beep_data, nodes, freq=freq)
  #test$pt <- test$pt_min
  
  test$id <- paste(test$TagId, test$freq, test$NodeId)
  test <- test[order(test$id, -test$TagRSSI_mean, -test$beep_count),]
  test <- test[!duplicated(test$id),]
  #pt3 <- test[test$pt==10,]
  
  test$groups <- paste(test$TagId, test$freq)
  
  all_data <- data.frame(TagId = test$TagId, NodeId = test$NodeId, long = test$node_lng_min, lat = test$node_lat_min, avg.RSSI = test$TagRSSI_mean, Test.Group = paste(test$TagId, test$freq))
  return(all_data)}
