library(data.table)
#directory_name <- "../test/V1"
load_data <- function(directory_name=getwd(), starttime=NULL, endtime=NULL, tags=NULL) {
  beep_pattern <- '*-data*.*csv*'
  #fancy = '.*CTT-(?P<station_id>[a-fA-F0-9]{12})-(?P<filetype>[a-zA-Z-_]+)+' test if this in correct type of expression gets files
  gps_pattern = '*-gps*.*csv*'
  health_pattern = '*-node*.*csv*'

#"""load data files from a directory that contains all the compressed (or uncompressed) data files straight off the station"""
  beep_files <- list.files(directory_name, pattern = beep_pattern, full.names = TRUE, recursive = TRUE)
  beep_files <- beep_files[grep("^(?=.*data)(?!.*(node|log|gps))", beep_files, perl=TRUE)]
  DatePattern = '^[[[:digit:]]{4}-[[[:digit:]]{2}-[[[:digit:]]{2}[T, ][[[:digit:]]{2}:[[[:digit:]]{2}:[[[:digit:]]{2}(.[[[:digit:]]{3})?[Z]?'
  time = "UTC"

  dfs <- function(x) {lapply(x, function(i) {
    df <- tryCatch({
      if (file.size(i) > 0) {
        read.csv(i,as.is=TRUE, na.strings=c("NA", ""))
      }}, error = function(err) {
    # error handler picks up where error was generated
      print(paste("Read.table didn't work!:  ",err))
    })
    
    time_cols <- c("Time", "RecordedAt", "recorded.at", "gps.at")
    timecols <- lapply(time_cols, function(x) {
      if(x %in% colnames(df)) {
        idx <- which(colnames(df)==x)
        #pre_count = nrow(df)
        #df = df[!is.na(df[,idx]),]
        #df = df[grepl(DatePattern,df[,idx]),]
        if(any(grepl("T", df[,idx]))) {df[,idx] <- as.POSIXct(df[,idx],format="%Y-%m-%dT%H:%M:%OS",tz = "UTC")
        } else {df[,idx] <- as.POSIXct(df[,idx], tz = "UTC")}
        #post_count = nrow(df)
        #delta = pre_count - post_count
      vals <- df[,idx]} else {vals <- c()} 
    return(vals)})
    reformat <- which(!sapply(timecols, is.null))
    df[,time_cols[reformat]] <- timecols[reformat]
  return(df)})
  }

  dfs_to_merge <- dfs(beep_files)
  remove <- which(!sapply(dfs_to_merge, is.data.frame))*-1
  if (length(remove) > 0) {dfs_to_merge <- dfs_to_merge[remove]} 
  
  df_merge <- function(df_list) {
    df <- rbindlist(df_list)
    #df <- df[order(df$Time),]
  return(df)}
  
  if (length(dfs_to_merge) > 0) {
    beep_data <- df_merge(dfs_to_merge)
    beep_data$ID <- paste(beep_data$Time, beep_data$RadioId, beep_data$TagId, beep_data$NodeId)
    beep_data <- beep_data[!duplicated(beep_data$ID),]
    beep_data$ID <- NULL
    beep_data$NodeId <- toupper(beep_data$NodeId)
  }

  node_health_files <- list.files(directory_name, pattern = health_pattern, full.names = TRUE, recursive = TRUE)
  dfs_to_merge <- dfs(node_health_files)
  remove <- which(!sapply(dfs_to_merge, is.data.frame))*-1
  if (length(remove) > 0) {dfs_to_merge <- dfs_to_merge[remove]} 
#what this does differently here is also checks for and removes records with NA times or times that don't fit the format
#also converts RecordedAt column to POSIXct
  if (length(dfs_to_merge) > 0) {
    health_data <- df_merge(dfs_to_merge)
    health_data$ID <- paste(health_data$Time, health_data$RadioId, health_data$NodeId)
    health_data <- health_data[!duplicated(health_data$ID),]
    health_data$ID <- NULL
    health_data$NodeId <- toupper(health_data$NodeId)
  }
#this also converts Time to POSIXct, removes records that have NA time or don't fit the format
  gps_files <- list.files(directory_name, pattern = gps_pattern, full.names = TRUE, recursive = TRUE)
  dfs_to_merge <- dfs(gps_files)
  remove <- which(!sapply(dfs_to_merge, is.data.frame))*-1
  if (length(remove) > 0) {dfs_to_merge <- dfs_to_merge[remove]} 
  if (length(dfs_to_merge) > 0) {
    gps_data <- df_merge(dfs_to_merge)
  }
  if(!is.null(starttime)) {attr(starttime, "tzone") <- "UTC"}
  if(!is.null(endtime)) {attr(endtime, "tzone") <- "UTC"}
  
  if (exists("beep_data")) {
    if (!is.null(starttime)) {
      beep_data <- beep_data[beep_data$Time > starttime,]}
    if (!is.null(endtime)) {
      beep_data <- beep_data[beep_data$Time < endtime,]}
    if (!is.null(tags)) beep_data <- beep_data[beep_data$TagId %in% tags,]
  }
  
  if (exists("health_data")) {
    if (!is.null(starttime)) {
      health_data <- health_data[health_data$Time > starttime,]}
    if (!is.null(endtime)) {
      health_data <- health_data[health_data$Time < endtime,]}
  }
  
  if (exists("gps_data")) {
    if (!is.null(starttime)) {
      gps_data <- gps_data[gps_data$Time > starttime,]}
    if (!is.null(endtime)) {
      gps_data <- gps_data[gps_data$Time < endtime,]}
  }

return(list(beep_data, health_data, gps_data))}

load_node_data <- function(infile) {
  files <- list.files(infile, pattern = "beep*", full.names = TRUE, recursive = TRUE)
  Sam3 <- lapply(files, function(x) {
    df <- read.csv(x,as.is=TRUE, na.strings=c("NA", ""))
    return(df)})
  dfs <- Sam3[!duplicated(Sam3)]
  dfs <- Map(cbind, dfs, file = files[!duplicated(Sam3)])
  nodes <- rbindlist(dfs)
  nodes <- as.data.frame(nodes)
  nodes$NodeId <- sapply(strsplit(as.character(nodes$file), "[/]"), function(x) {x[[length(x)-1]]})
  time = "UTC"
  nodes$Time <- as.POSIXct(nodes$time,format="%Y-%m-%dT%H:%M:%SZ",tz = time)
  #nodes <- nodes[nodes$Time > as.POSIXct("2020-08-20"),]
  nodes <- nodes[order(nodes$Time, nodes$file),]
  nodes$RadioId <- NA
  nodes$TagId <- nodes$id
  nodes$TagRSSI <- nodes$rssi
  nodes$Validated <- NA
return(nodes)}

export_data <- function(outpath) {
  now <- Sys.time()
  attr(now, "tzone") <- "UTC"
if (exists("beep_data")) write.csv(beep_data, file = paste(outpath,"stationbeep_",strptime(now,format='%Y-%m-%d_%H%M%S'),".csv"), row.names = FALSE)
if (exists("health_data")) write.csv(health_data, file = paste(outpath,"stationhealth_",strptime(now,format='%Y-%m-%d_%H%M%S'),".csv"), row.names = FALSE)
if (exists("gps_data")) write.csv(gps_data, file = paste(outpath,"stationgps_",strptime(now,format='%Y-%m-%d_%H%M%S'),".csv"), row.names = FALSE)
}
