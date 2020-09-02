rm(list=ls())
source("/functions/data_manager.R")
source("/functions/localization.R")

###EDIT THESE VALUES
infile <- "../owl-dataset"
nodes <- read.csv("~/Downloads/Node.csv", as.is=TRUE, na.strings=c("NA", "")) #uppercase node letters
tags <- read.csv("~/Downloads/Tags.csv", as.is=TRUE, na.strings=c("NA", "")) #uppercase node letters

###these are some fixes related to this specific file, disregard generally
nodes[14,"lng"] <- nodes[14,"lng"]*-1
nodes[11,"NodeId"] <- "328E34"
####

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