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

  dfs <- function(x, cols=NULL) {lapply(x, function(i) {
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
        if(any(grepl("T", df[,idx]))) {df[,idx] <- as.POSIXct(df[,idx],format="%Y-%m-%dT%H:%M:%OS",tz = "UTC", optional=TRUE)
        } else {df[,idx] <- as.POSIXct(df[,idx], tz = "UTC", optional=TRUE)}
        #post_count = nrow(df)
        #delta = pre_count - post_count
        vals <- df[,idx]} else {vals <- c()} 
      return(vals)})
    reformat <- which(!sapply(timecols, is.null))
    df[,time_cols[reformat]] <- timecols[reformat]
    
    #if((!is.null(cols) & !all(cols %in% colnames(df))) | !any(time_cols %in% colnames(df))) {df <- NULL}
    return(df)})
  }
  
  df_merge <- function(files, cols=NULL) {
    df_list <- dfs(files, cols)
    remove <- which(!sapply(df_list, is.data.frame))*-1
    if (length(remove) > 0) {df_list <- df_list[remove]} 
    if (length(df_list) > 0) {
      df <- rbindlist(df_list)
      df <- as.data.frame(df)
      if("Time" %in% colnames(df)) {
        df <- df[order(df$Time),]
        if(!is.null(starttime) & inherits(starttime, "POSIXct")) {
          attr(starttime, "tzone") <- "UTC"
          df <- df[df$Time > starttime,]
        }
        if(!is.null(endtime) & inherits(endtime, "POSIXct")) {
          attr(endtime, "tzone") <- "UTC"
          df <- df[df$Time < endtime,]
        }
        } else if("recorded.at" %in% colnames(df)) {
        df <- df[order(df$recorded.at),]
        if(!is.null(starttime) & inherits(starttime, "POSIXct")) {
          attr(starttime, "tzone") <- "UTC"
          df <- df[df$recorded.at > starttime,]
        }
        if(!is.null(endtime) & inherits(endtime, "POSIXct")) {
          attr(endtime, "tzone") <- "UTC"
          df <- df[df$recorded.at < endtime,]
        }}
      
      if(!is.null(cols) & all(cols) %in% colnames(df)) {
        df$ID <- apply(df[,cols],1, paste , collapse = "-" )
        df <- df[!duplicated(df$ID),]
        df$ID <- NULL
      }
      if("NodeId" %in% colnames(df)) {df$NodeId <- toupper(df$NodeId)}} else {df <- data.frame()}
  return(df)}
  
  beep_data <- df_merge(beep_files, c("Time", "RadioId", "TagId", "NodeId"))
  #beep_data$RadioId <- as.integer(beep_data$RadioId)

#what this does differently here is also checks for and removes records with NA times or times that don't fit the format
#also converts RecordedAt column to POSIXct
  health_data <- df_merge(list.files(directory_name, pattern = health_pattern, full.names = TRUE, recursive = TRUE), c("Time", "RadioId", "NodeId"))
  #health_data$RadioId <- as.integer(health_data$RadioId)

#this also converts Time to POSIXct, removes records that have NA time or don't fit the format
  gps_data <- df_merge(list.files(directory_name, pattern = gps_pattern, full.names = TRUE, recursive = TRUE))
  #gps_data$latitude <- as.numeric(gps_data$latitude)
  #gps_data$longitude <- as.numeric(gps_data$longitude)
  #gps_data$altitude <- as.numeric(gps_data$altitude)
  #gps_data$quality <- as.integer(gps_data$quality)
  #gps_data$mean.lat <- as.numeric(gps_data$mean.lat)
  #gps_data$mean.lng <- as.numeric(gps_data$mean.lng)
  #gps_data$n.fixes <- as.integer(gps_data$n.fixes)
  
  if (!is.null(tags) & !is.null(beep_data) & any(tags %in% beep_data$TagId)) {beep_data <- beep_data[beep_data$TagId %in% tags,]}
return(list(beep_data, health_data, gps_data))}

load_node_data <- function(infile) {
  files <- list.files(infile, pattern = "beep*", full.names = TRUE, recursive = TRUE)
  Sam3 <- lapply(files, function(x) {
    df <- tryCatch({
      if (file.size(x) > 0) {
        read.csv(x,as.is=TRUE, na.strings=c("NA", ""))
      }}, error = function(err) {
        # error handler picks up where error was generated
        print(paste("Read.table didn't work!:  ",err))
      })
    #if(!all((c("time", "id", "rssi") %in% colnames(df)))) {df <- NULL}
    return(df)})
  dflist <- Sam3[!duplicated(Sam3)]
  files <- files[!duplicated(Sam3)]
  remove <- which(!sapply(dflist, is.data.frame))*-1
  if (length(remove) > 0) {
    dflist <- dflist[remove]
    files <- files[remove]} 
  dflist <- Map(cbind, dflist, file = files)
  nodes <- rbindlist(dflist)
  nodes <- as.data.frame(nodes)
  nodes$NodeId <- toupper(sapply(strsplit(as.character(nodes$file), "[/]"), function(x) {x[[length(x)-1]]}))
  time = "UTC"
  nodes$Time <- as.POSIXct(nodes$time,format="%Y-%m-%dT%H:%M:%SZ",tz = time, optional=TRUE)
  #nodes <- nodes[nodes$Time > as.POSIXct("2020-08-20"),]
  nodes <- nodes[order(nodes$Time, nodes$file),]
  nodes$RadioId <- NA
  nodes$TagId <- nodes$id
  nodes$TagRSSI <- as.integer(nodes$rssi)
  nodes$Validated <- NA
return(nodes)}

export_data <- function(infile, outpath, starttime=NULL, endtime=NULL, tags=NULL) {
  all_data <- load_data(infile, starttime, endtime, tags)
  beep_data <- all_data[[1]]
  health_data <- all_data[[2]]
  gps_data <- all_data[[3]]
  now <- Sys.time()
  attr(now, "tzone") <- "UTC"
if (exists("beep_data")) write.csv(beep_data, file = paste(outpath,"stationbeep_",strptime(now,format='%Y-%m-%d_%H%M%S'),".csv"), row.names = FALSE)
if (exists("health_data")) write.csv(health_data, file = paste(outpath,"stationhealth_",strptime(now,format='%Y-%m-%d_%H%M%S'),".csv"), row.names = FALSE)
if (exists("gps_data")) write.csv(gps_data, file = paste(outpath,"stationgps_",strptime(now,format='%Y-%m-%d_%H%M%S'),".csv"), row.names = FALSE)
}
