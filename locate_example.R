rm(list=ls())
library(raster)
library(sp)
library(rgdal)
library(sf)
source("functions/data_manager.R")
source("functions/localization.R")

###EDIT THESE VALUES
infile <- "../owl-dataset"
nodes <- read.csv("~/Downloads/Nodes08262020.csv", as.is=TRUE, na.strings=c("NA", "")) #uppercase node letters
tags <- read.csv("~/Downloads/Tags.csv", as.is=TRUE, na.strings=c("NA", "")) #uppercase node letters

all_data <- load_data(infile)
beep_data <- all_data[[1]]

filename <- "output.csv"
###UNCOMMENT THESE AND FILL WITH YOUR DESIRED VALUES IF YOU WANT YOUR OUTPUT AS ONLY A SUBSET OF THE DATA
#channel <- a vector of RadioId value(s)
#tag_id <- a vector of TagId value(s)
#n_tags <- how many tags go into the "top tags"
#freq <- The interval of the added datetime variable. Any character string that would be accepted by seq.Date or seq.POSIXt

#EXAMPLE POSSIBLE VALUES
tag_id <- c("07072A2A","52784C2D") #tags$TagId
#
#channel <- c(2)
freq <- "5 min"

max_nodes <- 0 #how many nodes should be used in the localization calculation?
df <- merge_df(beep_data, nodes)

resampled <- advanced_resampled_stats(beep_data, nodes, freq)
locations <- weighted_average(beep_data,nodes,freq)

nodes_spatial <- nodes
coordinates(nodes_spatial) <- 3:2
crs(nodes_spatial) <- CRS("+proj=longlat +datum=WGS84") 

boulder_df <- locations[,c("TagId","avg_x","avg_y")]
coordinates(boulder_df) <- 2:3
utm <- CRS(paste0("+proj=utm +zone=", locations$zone[1], "+datum=WGS84"))
crs(boulder_df) <- utm
boulder_df_geog <- spTransform(boulder_df, proj4string(nodes_spatial))

locations <- st_as_sf(boulder_df_geog)
nodes_plot <- st_as_sf(nodes_spatial)

ggplot() + 
  #  ggmap(ph_basemap) +
  geom_sf(data = locations, aes(colour=TagId), inherit.aes = FALSE) + 
  geom_sf(data = nodes_plot) +
  geom_text(data = nodes, aes(x=lng, y=lat, label = NodeId), size = 5)
