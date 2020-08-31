library(data.table)
#starttime <- as.POSIXct("1980-07-30 12:00:00", tz = "America/New_York")
#endtime <- as.POSIXct("2050-08-20 12:00:00", tz = "America/New_York")
#tags <- c()

load_data <- function(directory_name=getwd(), starttime=NULL, endtime=NULL, tags=NULL) {
 
  # '*-data*.*csv*'
  PERLexp <- '*-data*.*csv*'
  beep_pattern <-PERLexp
  #fancy = '.*CTT-(?P<station_id>[a-fA-F0-9]{12})-(?P<filetype>[a-zA-Z-_]+)+' test if this in correct type of expression gets files
  #PERL? this is from Bob's python script. maybe use e.g. grep or something like it on list of files?
  gps_pattern = '*-gps*.*csv*'
  health_pattern = '*-node*.*csv*'

#"""load data files from a directory that contains all the compressed (or uncompressed) data files straight off the station"""
  beep_files <- list.files(directory_name, pattern = beep_pattern, full.names = TRUE, recursive = TRUE)
  beep_files <- beep_files[grep("^(?=.*data)(?!.*node)", beep_files, perl=TRUE)]
#beep_files <- beep_files[!grepl("node", beep_files)]

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
    
    if("recorded.at" %in% colnames(df)) {
      if(any(grepl("T",df$gps.at))) {
      df$RecordedAt = as.POSIXct(df$recorded.at,format="%Y-%m-%dT%H:%M:%OS",tz = time) } else {
        df$RecordedAt <- as.POSIXct(df$recorded.at,tz = time)
      }
    }
    
    if("gps.at" %in% colnames(df)) {
      if(any(grepl("T",df$gps.at))) {
        df$Time <- as.POSIXct(df$gps.at,format="%Y-%m-%dT%H:%M:%OS",tz = time)} else {
          df$Time <- as.POSIXct(df$gps.at,tz = time)
        } 
    }
    
    if("Time" %in% colnames(df)) {
      pre_count = nrow(df)
      df = df[!is.na(df$Time),]
      df = df[grepl(DatePattern,df$Time),]
      if(any(grepl("T", df$Time))) {df$Time <- as.POSIXct(df$Time,format="%Y-%m-%dT%H:%M:%OS",tz = time)
      } else {df$Time <- as.POSIXct(df$Time, tz = time)}
      post_count = nrow(df)
      delta = pre_count - post_count
      }
  
    if("RecordedAt" %in% colnames(df)) {
      if(any(grepl("T", df$RecordedAt))) {df$RecordedAt <- as.POSIXct(df$RecordedAt,format="%Y-%m-%dT%H:%M:%OS",tz = time)
      } else {df$RecordedAt <- as.POSIXct(df$RecordedAt, tz = time)}
    }
  return(df)})
  }

  dfs_to_merge <- dfs(beep_files)
  if (length(dfs_to_merge) > 0) {
    beep_data <- rbindlist(dfs_to_merge)
    beep_data <- beep_data[order(beep_data$Time),]
    beep_data$ID <- paste(beep_data$Time, beep_data$RadioId, beep_data$TagId, beep_data$NodeId)
    beep_data <- beep_data[!duplicated(beep_data$ID),]
    beep_data$ID <- NULL
  }

  node_health_files <- list.files(directory_name, pattern = health_pattern, full.names = TRUE, recursive = TRUE)
  dfs_to_merge <- dfs(node_health_files)
#what this does differently here is also checks for and removes records with NA times or times that don't fit the format
#also converts RecordedAt column to POSIXct
  if (length(dfs_to_merge) > 0) {
    health_data <- rbindlist(dfs_to_merge)
    health_data <- health_data[order(health_data$Time),]
    health_data$ID <- paste(health_data$Time, health_data$RadioId, health_data$NodeId)
    health_data <- health_data[!duplicated(health_data$ID),]
    health_data$ID <- NULL
  }
#this also converts Time to POSIXct, removes records that have NA time or don't fit the format
  gps_files <- list.files(directory_name, pattern = gps_pattern, full.names = TRUE, recursive = TRUE)
  dfs_to_merge <- dfs(gps_files)
  if (length(dfs_to_merge) > 0) {
    gps_data <- rbindlist(dfs_to_merge)
    gps_data <- gps_data[order(gps_data$Time),]
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

export_data <- function(outpath) {
  now <- Sys.time()
  attr(now, "tzone") <- "UTC"
if (exists("beep_data")) write.csv(beep_data, file = paste(outpath,"stationbeep_",strptime(now,format='%Y-%m-%d_%H%M%S'),".csv"), row.names = FALSE)
if (exists("health_data")) write.csv(health_data, file = paste(outpath,"stationhealth_",strptime(now,format='%Y-%m-%d_%H%M%S'),".csv"), row.names = FALSE)
if (exists("gps_data")) write.csv(gps_data, file = paste(outpath,"stationgps_",strptime(now,format='%Y-%m-%d_%H%M%S'),".csv"), row.names = FALSE)
}
