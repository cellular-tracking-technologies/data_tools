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
list.of.packages <- c("ggplot2", "tidyr", "gridExtra", "suncalc")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

require(data.table)
require(ggplot2)
require(tidyr)
require(gridExtra)
require(suncalc)
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
  node <- setDT(health)[, .(batt = mean(Battery), RSSI = mean(NodeRSSI), .N), by = .(cut(Time, freq),ID)] #V = mean(SolarVolts), A = mean(SolarCurrent), 
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
return(plot_data)}

#inspect this to find the index if you want to pull plots from the list for an ID x channel combo
node_channel_plots <- function(plot_data,filenames) {
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
    p1 = ggplot() +
    #scaled: geom_point(data = ea, aes(x = Time, y = rssi, group=1)) +
      geom_point(data = ea, aes(x = Time, y = RSSI, group=1)) +
    #scaled: geom_hline(yintercept = threshold) +
      geom_hline(yintercept = -95) +
    #geom_line(data = ea, aes(x = Time, y = scale(V), group=1), colour="red") +
    #geom_line(data = ea, aes(x = Time, y = scale(A), group=1), colour="orange") +
      scale_x_datetime(date_breaks="1 day", date_labels="%b %d", limits=c(min(health_data$Time), max(health_data$Time)))
  #+ scale_y_continuous(name="Count", limits=c(-110,-45))

#battery
    batt <- data.frame(x1 = head(ea$Time, -1), x2 = tail(ea$Time, -1) , 
                       y1 = head(ea$batt, -1), y2 = tail(ea$batt, -1))
    batt$col <- cut(batt$y1, c(0,3.7, 4, Inf))
    p = ggplot(data = batt, aes(x = x1, y = y1, xend = x2, yend = y2, group=1, colour=col)) + 
    geom_segment(size=2) +
    scale_x_datetime(date_breaks="1 day", date_labels="%b %d", limits=c(min(health_data$Time), max(health_data$Time))) +
    theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    scale_y_continuous(name="Batt", limits=c(2,5))

#number of check-ins
    p2 = ggplot(data = ea, aes(x = Time, y = N, group=1)) + 
    geom_bar(stat="identity") +
    scale_x_datetime(date_breaks="1 day", date_labels="%b %d", limits=c(min(health_data$Time), max(health_data$Time))) +
    theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    scale_y_continuous(name="Count", limits=c(0,12))

#check-ins as scaled line overlay
    p3 = ggplot() + geom_hline(yintercept = threshold) +
    geom_point(data = ea, aes(x = Time, y = rssi, group=1)) +
    geom_line(data = ea, aes(x = Time, y = scale(N), group=1), colour="purple") +
    scale_x_datetime(date_breaks="1 day", date_labels="%b %d", limits=c(min(health_data$Time), max(health_data$Time)))
  
  return(list(p1,p,p2,p3))})
return(outplots)}

#summary_v2 <- function(x) {
#  
#}

#ONLY FOR V2 STATIONS
node_plots <- function(health, nodes) {
  lat <- health$Latitude[1]
  lon <- health$Longitude[1]
  minday <- as.Date(min(health$Time))
  maxday <- as.Date(max(health$Time))
  sun <- getSunlightTimes(date=seq.Date(minday,maxday,by=1), keep=c("dawn", "dusk"), lat=lat, lon=lon)

  #health$altitude <- sun$altitude[match(health$Time, sun$date)]
  #health$light <- "day"
  #health[health$altitude < 0,]$light <- "night"
  health$timediff <- health$Time - health$RecordedAt
  biggest <- as.integer(max(health$timediff[health$timediff < 100000000]))
  plots <- lapply(nodes, function(park) {
    print(park)
  #ea <- plot_data[plot_data$RadioId == "2" & plot_data$NodeId == "496",]
  #ea <- plot_data[plot_data$NodeId == "496",]
    plot_data <- health[health$NodeId==park,]
  
  #these plots are to see if/where there are date mismatches for the time sent by and received from the node (i.e. GPS fix loss)
    plot_data$time <- format(plot_data$Time,'%Y-%m-%d')
    plot_data$recorded <- format(plot_data$RecordedAt,'%Y-%m-%d')
    plot_data$match <- as.integer(unname(unlist(Map(identical, plot_data$time, plot_data$recorded))))
    plot_data[plot_data$match < 1,]$match <- as.integer(plot_data[plot_data$match < 1,]$timediff < 100000000)
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
    
    #p2 = ggplot() +
    #  geom_point(data = ea2, aes(x = Time, y = timediff, group=1)) + #colour=factor(RadioId)#position = "jitter", 
    #  scale_x_datetime(date_breaks="1 week", date_labels="%b %d") + #, limits=c(min(plot_data$Time),max(plot_data$Time)
    #  scale_y_continuous(name="Mismatch") #limits=c(0,60)
    
  return(list(p1))})
return(plots)}

export_node_channel_plots <- function(health_data,freq="1 hour",out_path=getwd(),x=3,y=2,z=1) {
  plotdata <- summarize_health_data(health_data, freq)
  filenames <- unique(plotdata$ID)
  outplot <- node_channel_plots(plotdata, filenames)

  for (i in 1:length(filenames)) {
    file_name = paste(out_path,"node_",filenames[i],".png", sep="")
    png(file_name, width=1800, height=1200)
    if (tides) {
      tides <- tide_times(longitude, latitude)
      grid.arrange(outplot[[i]][[x]] +
                     geom_vline(xintercept = tides$HL[tides$HL$HL=="H",]$time, colour="red") +
                     geom_vline(xintercept = tides$HL[tides$HL$HL=="L",]$time, colour="blue"),
                   outplot[[i]][[y]], outplot[[i]][[z]], nrow = 3)
    } else {grid.arrange(outplot[[i]][[x]], outplot[[i]][[y]], outplot[[i]][[z]], nrow = 3)}
    dev.off()
  }}

#ONLY FOR V2 STATIONS
export_node_plots <- function(health_data,out_path=getwd()) {
  nodes <- unique(health_data$NodeId)
  outplots <- node_plots(health_data, nodes)
  for (i in 1:length(nodes)) {
    file_name = paste(out_path,"nodes_",nodes[i],".png", sep="")
    png(file_name, width=1800, height=1200)
    #grid.arrange(outplots[[i]][[2]], outplots[[i]][[1]], nrow = 2)
    grid.arrange(outplots[[i]][[1]], nrow = 1)
    dev.off()
    #ggsave(file_name, outplots[[i]][1], width=6, height = 6, device="png", path=out_path)
  }}