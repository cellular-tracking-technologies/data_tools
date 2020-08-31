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

require(data.table)
require(ggplot2)
require(tidyr)
require(gridExtra)
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

## ---------------------------

format_data <- function(health) {
  health$ID <- paste(health$RadioId, health$NodeId, sep="_") 
return(health)}

#operations for each unique combination of channel and node, summarized by the specified time interval 
summarize_health_data <- function(health, freq) {
  health$ID <- paste(health$RadioId, health$NodeId, sep="_")  
  node <- setDT(health)[, .(batt = mean(Battery), RSSI = mean(NodeRSSI), V = mean(SolarVolts), A = mean(SolarCurrent), .N), by = .(cut(Time, freq),ID)]
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
    p = ggplot(data = ea, aes(x = Time, y = batt, group=1)) + 
    geom_line() +
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

#ONLY FOR V2 STATIONS
node_plots <- function(health, nodes) {
  plots <- lapply(nodes, function(park) {
  #ea <- plot_data[plot_data$RadioId == "2" & plot_data$NodeId == "496",]
  #ea <- plot_data[plot_data$NodeId == "496",]
    plot_data <- health[health$NodeId==park,]
  
  #these plots are to see if/where there are date mismatches for the time sent by and received from the node
    plot_data$time <- format(plot_data$Time,'%Y-%m-%d')
    plot_data$recorded <- format(plot_data$RecordedAt,'%Y-%m-%d')
    plot_data$match <- as.integer(unname(unlist(Map(identical, plot_data$time, plot_data$recorded))))
    ea <- plot_data[!duplicated(plot_data$Time),] #not done by channel
    p1 = ggplot() +
      geom_point(data = ea, aes(x = Time, y = match, group=1, colour=is.na(RecordedAt))) + #colour=factor(RadioId)#position = "jitter", 
      scale_x_datetime(date_breaks="1 week", date_labels="%b %d", limits=c(min(plot_data$Time),max(plot_data$Time))) +
      scale_y_continuous(name="Match?", limits=c(0,1))
  return(p1)})
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
  nodes <- unique(health_data$NodeId)
  for (i in 1:length(nodes)) {
    file_name = paste("nodes_",nodes[i],".png", sep="")
    #  png(file_name, width=1800, height=1200)
    ggsave(file_name, outplots[[i]], width=6, height = 6, device="png", path=out_path)
  }}