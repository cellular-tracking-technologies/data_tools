## ---------------------------
##
## Script name: node_health.R
##
## Purpose of script: produce diagnostics visualizations
##
## Author: Dr. Jessica Gorzo
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

## load up the packages we will need:  (un-comment as required)
list.of.packages <- c("ggplot2", "tidyr", "suncalc", "geosphere", "egg")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

require(data.table)
require(ggplot2)
require(tidyr)
require(suncalc)
require(geosphere)
require(egg)
#BELOW REQUIRED FOR TIDE CALCULATIONS
#require(rvest)
#library(stringr)
#library(sf)
#library(rtide)
#library(Tides)

## ---------------------------
## load up our functions into memory, change path as needed so the folder containing the function scripts can be found

# source("functions/tide.R") 
source("functions/data_manager.R")

## --------------------------

#operations for each unique combination of channel and node, summarized by the specified time interval 
summarize_health_data <- function(health, freq) {
  version <- health[[2]]
  health <- health[[1]]
  health$ID <- paste(health$RadioId, health$NodeId, sep="_")  
  health$time <- cut(health$Time, freq)
  health$timebin <- as.POSIXct(health$time)
  node <- setDT(health)[, .(batt = mean(Battery), RSSI = mean(NodeRSSI), .N), by = .(cut(Time, freq),ID)] #V = mean(SolarVolts), A = mean(SolarCurrent), 
  if (version > 1) {
    health[which(health$Longitude < -180 | health$Longitude > 180),]$Longitude <- NA
    health[which(health$Latitude < -90),]$Latitude <- NA
    node <- setDT(health)[, .(batt = mean(Battery), RSSI = mean(NodeRSSI), A = mean(SolarCurrent), Lat = mean(Latitude), 
                              Lon = mean(Longitude), std_lat = sd(Latitude), std_lon = sd(Longitude), .N), by = .(cut(Time, freq),ID)] #V = mean(SolarVolts), , 
    
    node$lowlat <- node$Lat - node$std_lat
    node$uplat <- node$Lat + node$std_lat
    node$lowlon <- node$Lon - node$std_lon
    node$uplon <- node$Lon + node$std_lon
    node$d <- distVincentyEllipsoid(cbind(node$lowlon, node$lowlat), cbind(node$uplon, node$uplat))
    node$d <- (node$d)/1000
  }
  node$col <- cut(node$batt, c(0,3.7, 4, Inf))
  #node$color <- "#FF0000"
  #node[node$col=="(4,Inf]",]$color <- "#00FF00"
  #node[node$col=="(3.7,4]",]$color <-  "#F5B041" 
  #filling missing time intervals with NA values for visualization
  plot_data <- as.data.frame(complete(node,cut,ID))
#putting the newly summarized time intervals back into the correct data format
  plot_data$Time <- as.POSIXct(plot_data$cut, tz="UTC")
  health <- as.data.frame(health)
  health$Time <- as.POSIXct(health$Time, tz="UTC")
#theme_set(theme_classic())
#qplot(y=wetlands$NodeRSSI, x= 1, geom = "boxplot")
#for (j in unique(wetlands$ID)) {
#  each <- wetlands[wetlands$ID==j,]
#  boxp <- ggplot(each, aes(factor(ID), NodeRSSI)) + geom_boxplot()# + facet_wrap(~ID, scale="free")
#  ggsave(boxp, file=paste("boxp_",j, ".png", sep=''), scale=2)
#}
return(list(plot_data, health))}

gps_data_summary <- function(gps, freq) {
  node <- setDT(gps)[, .(Alt = mean(altitude), good = length(which(quality > 2)), bad = length(which(quality < 3))), by = .(cut(gps.at, freq))]
return(node)}

#inspect this to find the index if you want to pull plots from the list for an ID x channel combo
node_channel_plots <- function(health, freq, ids, lat=NULL, lon=NULL) { #freq, 
  version <- health[[2]]
  plot_dataset <- summarize_health_data(health, freq)
  health <- health[[1]]
  #time <- unlist(strsplit(freq, " "))
  #a <- as.difftime(as.integer(time[1]), unit=time[2])
  minx <- min(health$Time)
  maxx <- max(health$Time)
  minday <- as.Date(minx)
  maxday <- as.Date(maxx)

  plot_data <- plot_dataset[[1]]
  health_df <- plot_dataset[[2]]
  
  if(!is.null(lat) & !is.null(lon)) {
    sun <- getSunlightTimes(date=seq.Date(minday,maxday,by=1), keep=c("dawn", "dusk"), lat=lat, lon=lon)
    pbase <- ggplot() + theme_bw() + geom_rect(data=sun, aes(xmin=dusk, xmax=dawn, ymin=-Inf, ymax=Inf),fill='light grey') #+ theme(legend.position = "none")
  } else if (version > 1) {
    lat <- mean(health_df$Latitude, na.rm=TRUE)
    lon <- mean(health_df$Longitude, na.rm=TRUE)
    sun <- getSunlightTimes(date=seq.Date(minday,maxday,by=1), keep=c("dawn", "dusk"), lat=lat, lon=lon)
    pbase <- ggplot() + theme_bw() + geom_rect(data=sun, aes(xmin=dusk, xmax=dawn, ymin=-Inf, ymax=Inf),fill='light grey') #+ theme(legend.position = "none") 
  } else {pbase <- ggplot() + theme_bw() #+ theme(legend.position = "none")
  }

  outplots <- lapply(ids, function(park) {
#ea <- plot_data[plot_data$RadioId == "2" & plot_data$NodeId == "328b99",]
#this is for scaling, in order to look for patterns in RSSI in relation to other variables
    ea <- plot_data[plot_data$ID==park,]
    ea[nrow(ea) + 1,] <- NA
    ea$RSSI[nrow(ea)] <- -95
    ea$rssi <- scale(ea$RSSI)
    threshold <- ea$rssi[nrow(ea),]
    ea <- ea[-nrow(ea),]
    ea <- ea[order(ea$cut),]
    batt <- data.frame(x1 = head(ea$Time, -1), x2 = tail(ea$Time, -1), y1 = head(ea$batt, -1), y2 = tail(ea$batt, -1),
                       col = head(ea$col, -1))
    
    #battery
    p = pbase +
      geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, colour=col), data=batt) + #size=2
      scale_color_manual(values = c("(0,3.7]" = "#FF0000","(3.7,4]" = "#F5B041","(4,Inf]" = "#00FF00")) +
      scale_x_datetime(date_labels="%b %d", limits=c(min(minx), max(maxx))) +
      scale_y_continuous(name="Batt", limits=c(2,5))

#RSSI scatter plot, A&V lines commented out    
    p1 = pbase + 
      theme(axis.text=element_text(size=10),
                          axis.title=element_text(size=30,face="bold")) +
    #scaled: geom_point(data = ea, aes(x = Time, y = rssi, group=1)) +
      geom_point(data = ea, aes(x = Time, y = RSSI, group=1)) +
    #scaled: geom_hline(yintercept = threshold) +
      geom_hline(yintercept = -95) +
    #geom_line(data = ea, aes(x = Time, y = scale(V), group=1), colour="red") +
    #geom_line(data = ea, aes(x = Time, y = scale(A), group=1), colour="orange") +
      scale_x_datetime(date_labels="%b %d", limits=c(min(minx), max(maxx)))
  #+ scale_y_continuous(name="Count", limits=c(-110,-45))
    
    health_node <- health_df[health_df$ID==park,]
    boxp2 <- pbase + #MAKE SURE THIS WORKS WITH DAYLIGHT BACKGROUND
      geom_boxplot(data=health_node, aes(timebin, NodeRSSI, group=time)) +# + facet_wrap(~ID, scale="free")
      scale_x_datetime(date_labels="%b %d", limits=c(min(minx), max(maxx)))
#number of check-ins
    p2 = pbase +
      geom_bar(data = ea, aes(x = Time, y = N, group=1), stat="identity") +
      scale_x_datetime(date_labels="%b %d", limits=c(minx, maxx)) +
      scale_y_continuous(name="Count", limits=c(0,12))

#check-ins as scaled line overlay
    p3 = pbase +
      geom_hline(yintercept = threshold) +
      geom_point(data = ea, aes(x = Time, y = rssi, group=1)) +
      geom_line(data = ea, aes(x = Time, y = scale(N), group=1), colour="purple") +
      scale_x_datetime(date_labels="%b %d", limits=c(minx, maxx)) +
      theme(axis.text=element_text(size=10),
            axis.title=element_text(size=30,face="bold"))
  
  return(list(batt = p, RSSI = p1, n = p2, rssi = p3, rssi_box = boxp2))})
  
  if (version > 1) {
    
    outplot <- lapply(ids, function(park) {
      #ea <- plot_data[plot_data$RadioId == "2" & plot_data$NodeId == "328b99",]
      #this is for scaling, in order to look for patterns in RSSI in relation to other variables
      ea <- plot_data[plot_data$ID==park,]
      ea[nrow(ea) + 1,] <- NA
      ea$RSSI[nrow(ea)] <- -95
      ea$rssi <- scale(ea$RSSI)
      threshold <- ea$rssi[nrow(ea),]
      ea <- ea[-nrow(ea),]
      
      p = pbase +
        #scaled: geom_point(data = ea, aes(x = Time, y = rssi, group=1)) +
        geom_line(data = ea, aes(x = Time, y = Lat, group=1)) +
        #scaled: geom_hline(yintercept = threshold) +
        #geom_line(data = ea, aes(x = Time, y = scale(V), group=1), colour="red") +
        #geom_line(data = ea, aes(x = Time, y = scale(A), group=1), colour="orange") +
        scale_x_datetime(date_labels="%b %d", limits=c(minx, maxx))
      #+ scale_y_continuous(name="Count", limits=c(-110,-45))
      
      p1 = pbase +
        #scaled: geom_point(data = ea, aes(x = Time, y = rssi, group=1)) +
        geom_line(data = ea, aes(x = Time, y = Lon, group=1)) +
        #scaled: geom_hline(yintercept = threshold) +
        #geom_line(data = ea, aes(x = Time, y = scale(V), group=1), colour="red") +
        #geom_line(data = ea, aes(x = Time, y = scale(A), group=1), colour="orange") +
        scale_x_datetime(date_labels="%b %d", limits=c(minx, maxx))
      
      #RSSI scatter plot, A&V lines commented out    
      p2 = pbase +
        #scaled: geom_point(data = ea, aes(x = Time, y = rssi, group=1)) +
        geom_point(data = ea, aes(x = Time, y = RSSI, group=1)) +
        #scaled: geom_hline(yintercept = threshold) +
        geom_hline(yintercept = -95) +
        #geom_line(data = ea, aes(x = Time, y = scale(V), group=1), colour="red") +
        #geom_line(data = ea, aes(x = Time, y = scale(A), group=1), colour="orange") +
        scale_x_datetime(date_labels="%b %d", limits=c(minx, maxx))
      #+ scale_y_continuous(name="Count", limits=c(-110,-45))
      
      p3 = pbase +
        #scaled: geom_point(data = ea, aes(x = Time, y = rssi, group=1)) +
        geom_bar(data = ea, aes(x = Time, y = d), stat="identity") +
        #scaled: geom_hline(yintercept = threshold) +
        #geom_line(data = ea, aes(x = Time, y = scale(V), group=1), colour="red") +
        #geom_line(data = ea, aes(x = Time, y = scale(A), group=1), colour="orange") +
        #scale_y_continuous(limits=c())
        scale_x_datetime(date_labels="%b %d", limits=c(minx, maxx))
    return(list(lat = p, lng = p1, rssi_scale = p2, d = p3))})
    
    outplots <- Map(c, outplots, outplot)
  }
  names(outplots) <- ids
  
return(outplots)}

#summary_v2 <- function(x) {
#  
#}

#ONLY FOR V2 STATIONS

node_plots <- function(health, nodes, freq, lat = NULL, lon = NULL) {
  version <- health[[2]]
  plot_dataset <- summarize_health_data(health, freq)
  health <- health[[1]]
  minx <- min(health$Time)
  maxx <- max(health$Time)
  
  minday <- as.Date(minx)
  maxday <- as.Date(maxx)
  
  if(!is.null(lat) & !is.null(lon)) {
    sun <- getSunlightTimes(date=seq.Date(minday,maxday,by=1), keep=c("dawn", "dusk"), lat=lat, lon=lon)
    pbase <- ggplot() + theme_bw() + theme(legend.position = "none") + geom_rect(data=sun, aes(xmin=dusk, xmax=dawn, ymin=-Inf, ymax=Inf),fill='light grey') 
  } else if (version > 1) {
    lat <- mean(health$Latitude, na.rm=TRUE)
    lon <- mean(health$Longitude, na.rm=TRUE)
    sun <- getSunlightTimes(date=seq.Date(minday,maxday,by=1), keep=c("dawn", "dusk"), lat=lat, lon=lon)
    pbase <- ggplot() + theme_bw() + geom_rect(data=sun, aes(xmin=dusk, xmax=dawn, ymin=-Inf, ymax=Inf),fill='light grey') 
  } else {pbase <- ggplot() + theme_bw()}
  
  plotting <- plot_dataset[[1]]
  plotting$RadioId <- sapply(strsplit(plotting$ID, "_"), "[[", 1)
  plotting$NodeId <- sapply(strsplit(plotting$ID, "_"), "[[", 2)
  
  plots <- lapply(nodes, function(park) {
  #ea <- plot_data[plot_data$RadioId == "2" & plot_data$NodeId == "496",]
  #ea <- plot_data[plot_data$NodeId == "496",]
    plotter <- plotting[plotting$NodeId==park,]
    
    p = pbase +
      theme(axis.text=element_text(size=15),
            axis.title=element_text(size=30,face="bold")) +
      #scaled: geom_point(data = ea, aes(x = Time, y = rssi, group=1)) +
      geom_point(data = plotter, aes(x = Time, y = RSSI, group=RadioId, colour=RadioId)) +
      #scaled: geom_hline(yintercept = threshold) +
      geom_hline(yintercept = -95) +
      #geom_line(data = ea, aes(x = Time, y = scale(V), group=1), colour="red") +
      #geom_line(data = ea, aes(x = Time, y = scale(A), group=1), colour="orange") +
      scale_x_datetime(date_labels="%b %d", limits=c(minx, maxx)) #date_breaks="1 day"
    
    p4 = pbase +
      theme(axis.text=element_text(size=15),
            axis.title=element_text(size=30,face="bold")) +
      geom_bar(data = plotter, stat="identity", position=position_dodge(), aes(x = Time, y = N, group=RadioId, fill=RadioId)) +
      scale_x_datetime(date_labels="%b %d", limits=c(minx, maxx)) +
      scale_y_continuous(name="Count", limits=c(0,12))
    
    plotter <- plotter[!duplicated(plotter$Time),] 
    plotter <- plotter[order(plotter$Time),]
    batt <- data.frame(x1 = head(plotter$Time, -1), x2 = tail(plotter$Time, -1), y1 = head(plotter$batt, -1), y2 = tail(plotter$batt, -1),
                       col = head(plotter$col, -1))
    
    #battery
    p3 = pbase +
      geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, colour=col), data=batt) + #size=2
      scale_color_manual(values = c("(0,3.7]" = "#FF0000","(3.7,4]" = "#F5B041","(4,Inf]" = "#00FF00")) +
      scale_x_datetime(date_labels="%b %d", limits=c(minx, maxx)) +
      scale_y_continuous(name="Batt", limits=c(2,5))
  return(list(p, p4, p3))})
  if (version > 1) {
    health$timediff <- health$Time - health$RecordedAt
    biggest <- as.integer(max(health$timediff[health$timediff < 60]))
    outplots <- lapply(nodes, function(park) {
      plot_data <- health[health$NodeId==park,]
      #these plots are to see if/where there are date mismatches for the time sent by and received from the node (i.e. GPS fix loss)
      plot_data$time <- format(plot_data$Time,'%Y-%m-%d')
      plot_data$recorded <- format(plot_data$RecordedAt,'%Y-%m-%d')
      plot_data$match <- as.integer(unname(unlist(Map(identical, plot_data$time, plot_data$recorded))))
      plot_data[plot_data$match < 1,]$match <- as.integer(plot_data[plot_data$match < 1,]$timediff < 60)
      plot_data$scaled <- plot_data$timediff
      
      #if (any(plot_data$timediff > 100000000)) {plot_data[plot_data$timediff > 100000000,]$scaled <- biggest + 2}
      plot_data <- plot_data[!duplicated(plot_data$Time),] #not done by channel
      
      ea2 <- plot_data[plot_data$match > 0,]
      p1 = pbase +
        #geom_point(data = ea2, aes(x = Time, y = scaled, group=1)) + 
        geom_point(data = plot_data, aes(x = Time, y = scaled, group=1, colour=is.na(RecordedAt))) + 
        #colour=factor(RadioId)#position = "jitter", 
        scale_x_datetime(date_labels="%b %d", limits=c(minx, maxx)) +
        #geom_hline(yintercept = biggest + 1) +
        geom_hline(yintercept = 0) +
        scale_y_continuous(name="Time Mismatch") #limits=c(-2,biggest+3)
      
      p2 = pbase +
        geom_point(data = ea2, aes(x = Time, y = timediff, colour=factor(RadioId))) + #colour=factor(RadioId)#position = "jitter", 
        scale_x_datetime(date_labels="%b %d", limits=c(minx, maxx)) + #, limits=c(min(plot_data$Time),max(plot_data$Time)
        scale_y_continuous(name="Mismatches < 60 sec") #limits=c(0,60)
      return(list(p1, p2))})
    plots <- Map(c, plots, outplots)
  }
  names(plots) <- nodes
return(plots)}

#gps <- read.csv("~/Downloads/89460800120046859680.csv")

gps_plots <- function(gps, freq) {
  gps_data <- gps_data_summary(gps, freq)
  p1 <- ggplot(data = gps_data, mapping = aes(x = cut, y = Alt, group = 1)) +
    geom_line() +
    geom_area()
  
  aql <- melt(gps_data, id.vars = c("cut", "Alt"))
  
  p2 <- ggplot(data=aql, aes(x=cut, y=value, fill=variable)) +
    geom_bar(stat="identity")
return(list(p1,p2))}

export_node_channel_plots <- function(plotlist=NULL,health,freq="1 hour",out_path=getwd(),whichplots = c(3,2,1)) {
  if (is.null(plotlist)) {
    plotdf <- summarize_health_data(health, freq)
    plotdf <- plotdf[[1]]
    filenames <- unique(plotdf$ID)
    outplot <- node_channel_plots(health, freq, filenames)} else {
      outplot <- plotlist
      filenames <- names(plotlist)}

  for (i in 1:length(filenames)) {
    file_name = paste(out_path,"node_",filenames[i],".png", sep="")
    print(file_name)
    png(file_name, width=1800, height=1000)
    myplots <- outplot[[i]][whichplots]
    formatted <- lapply(myplots[1:length(myplots)-1], function(y) y + theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), 
                                                                            axis.text=element_text(size=15),
                                                                            axis.title=element_text(size=30,face="bold"))
    )
    
    formatted2 <- lapply(myplots[length(myplots)], function(y) y + xlab("Time") + theme(axis.text=element_text(size=15),
                                                                                        axis.title=element_text(size=30,face="bold"))
    )
    
    #last <- myplots[length(myplots)] + theme(axis.text=element_text(size=15),axis.title=element_text(size=30,face="bold"))
    
    myplots <- c(formatted,  formatted2)
    #+ theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text=element_text(size=15),
    #                                               axis.title=element_text(size=30,face="bold"))
    #+ theme(axis.title=element_text(size=30,face="bold"), axis.text=element_text(size=15))

#+ theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text=element_text(size=15),
#                                               axis.title=element_text(size=30,face="bold"))
#+ theme(axis.title=element_text(size=30,face="bold"), axis.text=element_text(size=15))
    if (tides) {
      tides <- tide_times(longitude, latitude)
      ggarrange(outplot[[i]][[x]] +
                     geom_vline(xintercept = tides$HL[tides$HL$HL=="H",]$time, colour="red") +
                     geom_vline(xintercept = tides$HL[tides$HL$HL=="L",]$time, colour="blue"),
                   outplot[[i]][[y]], outplot[[i]][[z]])
    } else {ggarrange(plots=myplots, nrow=length(myplots))}
    dev.off()
  }}

#ONLY FOR V2 STATIONS
export_node_plots <- function(plotlist = NULL, health,freq,out_path=getwd(), x=2, y=3, z=1) {
  if(is.null(plotlist)) {
  health_data <- health[[1]]
  nodes <- unique(health_data$NodeId)
  outplot <- node_plots(health, nodes, freq) } else {
    outplot <- plotlist
    nodes <- name(plotlist)}
  for (i in 1:length(nodes)) {
    file_name = paste(out_path,"node_",nodes[i],".png", sep="")
    print(file_name)
    png(file_name, width=1800, height=1000)
    gA <- outplot[[i]][[x]] +
                       theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text=element_text(size=15),
                                             axis.title=element_text(size=30,face="bold"))
    gB <- outplot[[i]][[y]] +
                       theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text=element_text(size=15),
                             axis.title=element_text(size=30,face="bold"))
    gC <- outplot[[i]][[z]] + theme(axis.text=element_text(size=15))
    ggarrange(plots = list(gA,gB,gC), nrow=3)
    dev.off()}
}
