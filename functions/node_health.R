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
list.of.packages <- c("ggplot2", "tidyr", "gridExtra", "suncalc", "geosphere")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

require(data.table)
require(ggplot2)
require(tidyr)
require(gridExtra)
require(suncalc)
require(geosphere)
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
  health$ID <- paste(health$RadioId, health$NodeId, sep="_")  
  health$time <- cut(health_data$Time, freq)
  health$timebin <- as.POSIXct(health$time)
  node <- setDT(health)[, .(batt = mean(Battery), RSSI = mean(NodeRSSI), .N), by = .(cut(Time, freq),ID)] #V = mean(SolarVolts), A = mean(SolarCurrent), 
  node$col <- cut(node$batt, c(0,3.7, 4, Inf))
  #node$color <- "#FF0000"
  #node[node$col=="(4,Inf]",]$color <- "#00FF00"
  #node[node$col=="(3.7,4]",]$color <-  "#F5B041" 
  #filling missing time intervals with NA values for visualization
  plot_data <- as.data.frame(complete(node,cut,ID))
#putting the newly summarized time intervals back into the correct data format
  plot_data$Time <- as.POSIXct(plot_data$cut, tz="UTC")

#theme_set(theme_classic())
#qplot(y=wetlands$NodeRSSI, x= 1, geom = "boxplot")
#for (j in unique(wetlands$ID)) {
#  each <- wetlands[wetlands$ID==j,]
#  boxp <- ggplot(each, aes(factor(ID), NodeRSSI)) + geom_boxplot()# + facet_wrap(~ID, scale="free")
#  ggsave(boxp, file=paste("boxp_",j, ".png", sep=''), scale=2)
#}
return(list(plot_data, health))}

v2_health_data <- function(health, freq) {
  health$ID <- paste(health$RadioId, health$NodeId, sep="_")  
  health$time <- cut(health_data$Time, freq)
  health$timebin <- as.POSIXct(health$time)
  node <- setDT(health)[, .(batt = mean(Battery), RSSI = mean(NodeRSSI), A = mean(SolarCurrent), Lat = mean(Latitude), 
                            Lon = mean(Longitude), std_lat = sd(Latitude), std_lon = sd(Longitude), .N), by = .(cut(Time, freq),ID)] #V = mean(SolarVolts), , 
  node$col <- cut(node$batt, c(0,3.7, 4, Inf))
  node$lowlat <- node$Lat - node$std_lat
  node$uplat <- node$Lat + node$std_lat
  node$lowlon <- node$Lon - node$std_lon
  node$uplon <- node$Lon + node$std_lon
  node$d <- distVincentyEllipsoid(cbind(node$lowlon, node$lowlat), cbind(node$uplon, node$uplat))
  node$d <- (node$d)/1000
  #node$color <- "#FF0000"
  #node[node$col=="(4,Inf]",]$color <- "#00FF00"
  #node[node$col=="(3.7,4]",]$color <-  "#F5B041" 
  #filling missing time intervals with NA values for visualization
  plot_data <- as.data.frame(complete(node,cut,ID))
  #putting the newly summarized time intervals back into the correct data format
  plot_data$Time <- as.POSIXct(plot_data$cut, tz="UTC")
  
  health <- as.data.frame(health)
  health$Time <- as.POSIXct(health$Time, tz="UTC")
  #health$time <- as.POSIXct(health$time, tz="UTC")
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
node_channel_plots <- function(health, freq) { #freq, 
  #time <- unlist(strsplit(freq, " "))
  #a <- as.difftime(as.integer(time[1]), unit=time[2])
  plot_dataset <- summarize_health_data(health, freq)
  plot_data <- plot_dataset[[1]]
  health_df <- plot_dataset[[2]]
  filenames <- unique(plot_data$ID)
  outplots <- lapply(filenames, function(park) {
#ea <- plot_data[plot_data$RadioId == "2" & plot_data$NodeId == "328b99",]
#this is for scaling, in order to look for patterns in RSSI in relation to other variables
    ea <- plot_data[plot_data$ID==park,]
    ea[nrow(ea) + 1,] <- NA
    ea$RSSI[nrow(ea)] <- -95
    ea$rssi <- scale(ea$RSSI)
    threshold <- ea$rssi[nrow(ea),]
    ea <- ea[-nrow(ea),]
    batt <- data.frame(x1 = head(ea$Time, -1), x2 = tail(ea$Time, -1), y1 = head(ea$batt, -1), y2 = tail(ea$batt, -1),
                       col = head(ea$col, -1))
    
    health_node <- health_df[health_df$ID==park,]
#RSSI scatter plot, A&V lines commented out    
    p1 = ggplot() + theme_bw() +
      #geom_rect(data=sun, aes(xmin=dusk, xmax=dawn, ymin=-Inf, ymax=Inf),fill='light grey') +
      theme(axis.text=element_text(size=20),
                          axis.title=element_text(size=30,face="bold")) +
    #scaled: geom_point(data = ea, aes(x = Time, y = rssi, group=1)) +
      geom_point(data = ea, aes(x = Time, y = RSSI, group=1)) +
    #scaled: geom_hline(yintercept = threshold) +
      geom_hline(yintercept = -95) +
    #geom_line(data = ea, aes(x = Time, y = scale(V), group=1), colour="red") +
    #geom_line(data = ea, aes(x = Time, y = scale(A), group=1), colour="orange") +
      scale_x_datetime(date_breaks="1 day", date_labels="%b %d", limits=c(min(plot_data$Time), max(plot_data$Time)))
  #+ scale_y_continuous(name="Count", limits=c(-110,-45))

#battery
    p = ggplot() + theme_bw() +
      #geom_rect(data=sun, aes(xmin=dusk, xmax=dawn, ymin=-Inf, ymax=Inf),fill='light grey') +
        geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, colour=col), data=batt) + #size=2
        scale_color_manual(values = c("(0,3.7]" = "#FF0000","(3.7,4]" = "#F5B041","(4,Inf]" = "#00FF00")) +
        scale_x_datetime(date_breaks="1 day", date_labels="%b %d", limits=c(min(plot_data$Time), max(plot_data$Time))) +
        theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none", axis.text=element_text(size=20),
              axis.title=element_text(size=30,face="bold")) +
        scale_y_continuous(name="Batt", limits=c(2,5))
    ea <- ea[order(ea$cut),]
    boxp2 <- ggplot() + 
      #geom_rect(data=sun, aes(xmin=dusk, xmax=dawn, ymin=-Inf, ymax=Inf),fill='light grey') +
      geom_boxplot(data=health_node, aes(timebin, NodeRSSI, group=time))# + facet_wrap(~ID, scale="free")
#number of check-ins
    p2 = ggplot(data = ea, aes(x = Time, y = N, group=1)) + theme_bw() +
      #geom_rect(data=sun, aes(xmin=dusk, xmax=dawn, ymin=-Inf, ymax=Inf),fill='light grey') +
      geom_bar(stat="identity") +
      scale_x_datetime(date_breaks="1 day", date_labels="%b %d", limits=c(min(health_data$Time), max(health_data$Time))) +
      theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text=element_text(size=20),
            axis.title=element_text(size=30,face="bold")) +
      scale_y_continuous(name="Count", limits=c(0,12))

#check-ins as scaled line overlay
    p3 = ggplot() + theme_bw() +
      #geom_rect(data=sun, aes(xmin=dusk, xmax=dawn, ymin=-Inf, ymax=Inf),fill='light grey') + 
      geom_hline(yintercept = threshold) +
      geom_point(data = ea, aes(x = Time, y = rssi, group=1)) +
      geom_line(data = ea, aes(x = Time, y = scale(N), group=1), colour="purple") +
      scale_x_datetime(date_breaks="1 day", date_labels="%b %d", limits=c(min(health_data$Time), max(health_data$Time))) +
      theme(axis.text=element_text(size=20),
            axis.title=element_text(size=30,face="bold"))
  
  return(list(p1,p,p2,p3))})
return(outplots)}

#summary_v2 <- function(x) {
#  
#}

v2_plots <- function(health, freq) {
  summarized <- v2_health_data(health, freq)
  plot_data <- summarized[[1]]
  health_df <- summarized[[2]]
  
  lat <- mean(health_df$Latitude, na.rm=TRUE)
  lon <- mean(health_df$Longitude, na.rm=TRUE)
  minday <- as.Date(min(health_df$Time))
  maxday <- as.Date(max(health_df$Time))
  sun <- getSunlightTimes(date=seq.Date(minday,maxday,by=1), keep=c("dawn", "dusk"), lat=lat, lon=lon)
  
  filenames <- unique(plot_data$ID)
  outplots <- lapply(filenames, function(park) {
    #ea <- plot_data[plot_data$RadioId == "2" & plot_data$NodeId == "328b99",]
    #this is for scaling, in order to look for patterns in RSSI in relation to other variables
    ea <- plot_data[plot_data$ID==park,]
    ea[nrow(ea) + 1,] <- NA
    ea$RSSI[nrow(ea)] <- -95
    ea$rssi <- scale(ea$RSSI)
    threshold <- ea$rssi[nrow(ea),]
    ea <- ea[-nrow(ea),]
    
    #RSSI scatter plot, A&V lines commented out    
    p1 = ggplot() + theme_bw() +
      geom_rect(data=sun, aes(xmin=dusk, xmax=dawn, ymin=-Inf, ymax=Inf),fill='light grey') +
      #scaled: geom_point(data = ea, aes(x = Time, y = rssi, group=1)) +
      geom_point(data = ea, aes(x = Time, y = RSSI, group=1)) +
      #scaled: geom_hline(yintercept = threshold) +
      geom_hline(yintercept = -95) +
      #geom_line(data = ea, aes(x = Time, y = scale(V), group=1), colour="red") +
      #geom_line(data = ea, aes(x = Time, y = scale(A), group=1), colour="orange") +
      scale_x_datetime(date_breaks="1 day", date_labels="%b %d", limits=c(min(plot_data$Time), max(plot_data$Time)))
    #+ scale_y_continuous(name="Count", limits=c(-110,-45))
    
    p = ggplot() + theme_bw() +
      geom_rect(data=sun, aes(xmin=dusk, xmax=dawn, ymin=-Inf, ymax=Inf),fill='light grey') +
      #scaled: geom_point(data = ea, aes(x = Time, y = rssi, group=1)) +
      geom_line(data = ea, aes(x = Time, y = Lat, group=1)) +
      #scaled: geom_hline(yintercept = threshold) +
      #geom_line(data = ea, aes(x = Time, y = scale(V), group=1), colour="red") +
      #geom_line(data = ea, aes(x = Time, y = scale(A), group=1), colour="orange") +
      scale_x_datetime(date_breaks="1 day", date_labels="%b %d", limits=c(min(health_df$Time), max(health_df$Time)))
    #+ scale_y_continuous(name="Count", limits=c(-110,-45))
    
    p2 = ggplot() + theme_bw() +
      geom_rect(data=sun, aes(xmin=dusk, xmax=dawn, ymin=-Inf, ymax=Inf),fill='light grey') +
      #scaled: geom_point(data = ea, aes(x = Time, y = rssi, group=1)) +
      geom_line(data = ea, aes(x = Time, y = Lon, group=1)) +
      #scaled: geom_hline(yintercept = threshold) +
      #geom_line(data = ea, aes(x = Time, y = scale(V), group=1), colour="red") +
      #geom_line(data = ea, aes(x = Time, y = scale(A), group=1), colour="orange") +
      scale_x_datetime(date_breaks="1 day", date_labels="%b %d", limits=c(min(health_df$Time), max(health_df$Time)))
    
    p3 = ggplot() + theme_bw() +
      geom_rect(data=sun, aes(xmin=dusk, xmax=dawn, ymin=-Inf, ymax=Inf),fill='light grey') +
      #scaled: geom_point(data = ea, aes(x = Time, y = rssi, group=1)) +
      geom_bar(data = ea, aes(x = Time, y = d), stat="identity") +
      #scaled: geom_hline(yintercept = threshold) +
      #geom_line(data = ea, aes(x = Time, y = scale(V), group=1), colour="red") +
      #geom_line(data = ea, aes(x = Time, y = scale(A), group=1), colour="orange") +
      #scale_y_continuous(limits=c())
      scale_x_datetime(date_breaks="1 day", date_labels="%b %d", limits=c(min(health_df$Time), max(health_df$Time)))
    
  return(list(p1, boxp, p, p2, p3))})
return(outplots)}

#ONLY FOR V2 STATIONS
node_plots <- function(health, nodes, freq) {
  lat <- mean(health$Latitude, na.rm=TRUE)
  lon <- mean(health$Longitude, na.rm=TRUE)
  minday <- as.Date(min(health$Time))
  maxday <- as.Date(max(health$Time))
  sun <- getSunlightTimes(date=seq.Date(minday,maxday,by=1), keep=c("dawn", "dusk"), lat=lat, lon=lon)

  #health$altitude <- sun$altitude[match(health$Time, sun$date)]
  #health$light <- "day"
  #health[health$altitude < 0,]$light <- "night"
  
  health$timediff <- health$Time - health$RecordedAt
  biggest <- as.integer(max(health$timediff[health$timediff < 60]))
  
  plot_dataset <- summarize_health_data(health, freq)
  plotting <- plot_dataset[[1]]
  plotting$RadioId <- sapply(strsplit(plotting$ID, "_"), "[[", 1)
  plotting$NodeId <- sapply(strsplit(plotting$ID, "_"), "[[", 2)
  
  plots <- lapply(nodes, function(park) {
    print(park)
  #ea <- plot_data[plot_data$RadioId == "2" & plot_data$NodeId == "496",]
  #ea <- plot_data[plot_data$NodeId == "496",]
    plot_data <- health[health$NodeId==park,]
    plotter <- plotting[plotting$NodeId==park,]
    
    p = ggplot() + theme_bw() +
      #geom_rect(data=sun, aes(xmin=dusk, xmax=dawn, ymin=-Inf, ymax=Inf),fill='light grey') +
      theme(axis.text=element_text(size=20),
            axis.title=element_text(size=30,face="bold")) +
      #scaled: geom_point(data = ea, aes(x = Time, y = rssi, group=1)) +
      geom_point(data = plotter, aes(x = Time, y = RSSI, group=RadioId, colour=RadioId)) +
      #scaled: geom_hline(yintercept = threshold) +
      geom_hline(yintercept = -95) +
      #geom_line(data = ea, aes(x = Time, y = scale(V), group=1), colour="red") +
      #geom_line(data = ea, aes(x = Time, y = scale(A), group=1), colour="orange") +
      scale_x_datetime(date_breaks="1 day", date_labels="%b %d", limits=c(min(health$Time), max(health$Time)))
    
    p4 = ggplot(data = plotter, aes(x = Time, y = N, group=RadioId, fill=RadioId)) + theme_bw() +
      #geom_rect(data=sun, aes(xmin=dusk, xmax=dawn, ymin=-Inf, ymax=Inf),fill='light grey') +
      geom_bar(stat="identity", position=position_dodge()) +
      scale_x_datetime(date_breaks="1 day", date_labels="%b %d", limits=c(min(health$Time), max(health$Time))) +
      theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text=element_text(size=20),
            axis.title=element_text(size=30,face="bold")) +
      scale_y_continuous(name="Count", limits=c(0,12))
    
    plotter <- plotter[!duplicated(plotter$Time),] 
    plotter <- plotter[order(plotter$Time),]
    batt <- data.frame(x1 = head(plotter$Time, -1), x2 = tail(plotter$Time, -1), y1 = head(plotter$batt, -1), y2 = tail(plotter$batt, -1),
                       col = head(plotter$col, -1))
    
    p5 = ggplot(data = plotter, aes(x = Time, y = N, group=NodeId, fill=NodeId)) + theme_bw() +
      #geom_rect(data=sun, aes(xmin=dusk, xmax=dawn, ymin=-Inf, ymax=Inf),fill='light grey') +
      geom_bar(stat="identity") +
      scale_x_datetime(date_breaks="1 day", date_labels="%b %d", limits=c(min(health$Time), max(health$Time))) +
      theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text=element_text(size=20),
            axis.title=element_text(size=30,face="bold")) +
      scale_y_continuous(name="Count", limits=c(0,12))

    #battery
    p3 = ggplot() + theme_bw() +
      #geom_rect(data=sun, aes(xmin=dusk, xmax=dawn, ymin=-Inf, ymax=Inf),fill='light grey') +
      geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, colour=col), data=batt) + #size=2
      scale_color_manual(values = c("(0,3.7]" = "#FF0000","(3.7,4]" = "#F5B041","(4,Inf]" = "#00FF00")) +
      scale_x_datetime(date_breaks="1 day", date_labels="%b %d", limits=c(min(health$Time), max(health$Time))) +
      theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text=element_text(size=20), #legend.position = "none", 
            axis.title=element_text(size=30,face="bold")) +
      scale_y_continuous(name="Batt", limits=c(2,5))
    
  #these plots are to see if/where there are date mismatches for the time sent by and received from the node (i.e. GPS fix loss)
    plot_data$time <- format(plot_data$Time,'%Y-%m-%d')
    plot_data$recorded <- format(plot_data$RecordedAt,'%Y-%m-%d')
    plot_data$match <- as.integer(unname(unlist(Map(identical, plot_data$time, plot_data$recorded))))
    plot_data[plot_data$match < 1,]$match <- as.integer(plot_data[plot_data$match < 1,]$timediff < 60)
    plot_data$scaled <- plot_data$timediff
    
    #if (any(plot_data$timediff > 100000000)) {plot_data[plot_data$timediff > 100000000,]$scaled <- biggest + 2}
    plot_data <- plot_data[!duplicated(plot_data$Time),] #not done by channel
    
    ea2 <- plot_data[plot_data$match > 0,]
    p1 = ggplot() + theme_bw() +
      theme(axis.text=element_text(size=20),axis.title=element_text(size=30)) +
      geom_rect(data=sun, aes(xmin=dusk, xmax=dawn, ymin=Inf, ymax=biggest-Inf),fill='light grey') +
      #geom_point(data = ea2, aes(x = Time, y = scaled, group=1)) + 
      geom_point(data = plot_data, aes(x = Time, y = scaled, group=1, colour=is.na(RecordedAt))) + 
      #colour=factor(RadioId)#position = "jitter", 
      scale_x_datetime(date_breaks="1 week", date_labels="%b %d", limits=c(min(health$Time),max(health$Time))) +
      #geom_hline(yintercept = biggest + 1) +
      geom_hline(yintercept = 0) +
      scale_y_continuous(name="Match?") #limits=c(-2,biggest+3)
    
    p2 = ggplot() +
      geom_point(data = ea2, aes(x = Time, y = timediff, group=1)) + #colour=factor(RadioId)#position = "jitter", 
      scale_x_datetime(date_breaks="1 week", date_labels="%b %d") + #, limits=c(min(plot_data$Time),max(plot_data$Time)
      scale_y_continuous(name="Mismatch") #limits=c(0,60)
    
  return(list(p1, p2, p, p4, p3, p5))})
return(plots)}

#gps <- read.csv("~/Downloads/89460800120046859680.csv")

gps_plots <- function(gps) {
  p1 <- ggplot(data = gps, mapping = aes(x = cut, y = Alt, group = 1)) +
    geom_line() +
    geom_area()
  
  aql <- melt(gps, id.vars = c("cut", "Alt"))
  
  p2 <- ggplot(data=aql, aes(x=cut, y=value, fill=variable)) +
    geom_bar(stat="identity")
return(p1)}

export_node_channel_plots <- function(health_data,freq="1 hour",out_path=getwd(),x=3,y=2,z=1) {
  outplot <- node_channel_plots(health_data, freq)
  plotdf <- summarize_health_data(health_data, freq)
  plotdf <- plotdf[[1]]
  filenames <- unique(plotdf$ID)

  for (i in 1:length(filenames)) {
    file_name = paste(out_path,"node_",filenames[i],".png", sep="")
    png(file_name, width=1800, height=1000)
    
    gA <- ggplotGrob(outplot[[i]][[x]])
    gB <- ggplotGrob(outplot[[i]][[y]])
    gC <- ggplotGrob(outplot[[i]][[z]])
    
    if (tides) {
      tides <- tide_times(longitude, latitude)
      grid.arrange(outplot[[i]][[x]] +
                     geom_vline(xintercept = tides$HL[tides$HL$HL=="H",]$time, colour="red") +
                     geom_vline(xintercept = tides$HL[tides$HL$HL=="L",]$time, colour="blue"),
                   outplot[[i]][[y]], outplot[[i]][[z]], nrow = 3)
    } else {
      grid::grid.newpage()
      grid::grid.draw(rbind(gA, gB, gC))}
    dev.off()
  }}

#ONLY FOR V2 STATIONS
export_node_plots <- function(health_data,out_path=getwd(), x=6, y=5, z=3) {
  nodes <- unique(health_data$NodeId)
  outplot <- node_plots(health_data, nodes)
  for (i in 1:length(nodes)) {
    file_name = paste(out_path,"node_",nodes[i],".png", sep="")
    png(file_name, width=1800, height=1000)
      
    gA <- ggplotGrob(outplot[[i]][[x]])
    gB <- ggplotGrob(outplot[[i]][[y]])
    gC <- ggplotGrob(outplot[[i]][[z]])
      
    grid::grid.newpage()
    grid::grid.draw(rbind(gA, gB, gC))
    dev.off()}
}
