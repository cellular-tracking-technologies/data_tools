## ---------------------------
##
## Script name: example.R
##
## Purpose of script: This is an example R script for working with your CTT data! click the data_tools.RProj file in this folder, and your working directory will be set
## to that folder. produce diagnostics visualizations
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
## load up the packages we will need:  (un-comment as required)
## load up our functions into memory
source("functions/data_manager.R")
source("functions/node_health.R")

#####Values for the user to change##################################################
#This points to a directory that ONLY has your downloaded data from the sensor station.
#It can contain any/all of your downloaded data files, just don't manipulate/add your own unrelated/altered files.
#Unzip any zipped directories therein, but compressed csv files (csv.gz) don't need to be unzipped

infile <- "~/Documents/data/radio_projects/node_health_test"

#This is where you want your output to go
outpath <- "~/Documents/plots/indigo"

freq <- "1 hour" #interval to summarize node health indicators of interest

## if you want tide-based viz on your graphs, un-comment the tide.R function line in the header of node_health.R & all needed packages
#change tides to TRUE here
##you can find your closest location here: https://tides.mobilegeographics.com/ and click to display the tide chart
tides = FALSE

#Optional parameters: start time of your data of interest, end time.
#You can set these in whatever time zone you want & it will translate
#https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
#you can also subset by a vector of TagId(s)

#EXAMPLE POSSIBLE VALUES
#tags <- c("61526633")
#start_time = as.POSIXct("2021-05-16 01:00:00", tz = "America/New_York")
#end_time = as.POSIXct("2020-06-03 23:00:00", tz = "America/New_York")

all_data <- load_data(infile) #start_time, end_time, tags
#set arguments if you choose to subset by date or tags
####################################################################################

beep_data <- all_data[[1]]
beep_data <- all_data[[1]][[1]]
subset_data <- beep_data[beep_data$Time > as.POSIXct("2021-05-15") & beep_data$Time < as.POSIXct("2021-06-04"),]
my_tag <- beep_data[beep_data$TagId == "your tag ID",]
my_tags <- beep_data[beep_data$TagId %in% c("your tag IDs"),]
#beep_data <- beep_data[complete.cases(beep_data), ]

health_data <- all_data[[2]]
#health_data now has a data frame of all of your node health files. 
#subset_data <- health_data[[1]][health_data[[1]]$Time > as.POSIXct("2021-08-12") & health_data$Time < as.POSIXct("2021-08-20"),]

gps_data <- all_data[[3]]

#put your beep files straight off the node each into a folder corresponding to the node ID
#put all of these node ID folders into a folder, which is where "indir" should be pointed
#indir <- "../data/from_node"
#my_node_data <- load_node_data(indir)
#my_node_data <- my_node_data[my_node_data$Time > as.POSIXct("2020-08-18 16:41:00", tz="UTC") & my_node_data$Time < as.POSIXct("2020-09-09 17:59:00", tz="UTC"),]

#tags <- read.csv("../data/Deployed Tags.csv", as.is=TRUE, na.strings=c("NA", ""), header=TRUE, skipNul = TRUE, colClasses=c("TagId"="character"))
#beep_data <- beep_data[beep_data$TagId %in% tags$TagId,]
#my_node_data <- my_node_data[my_node_data$TagId %in% my_node_data$TagId,]

#UNCOMMENT AND RUN THE export_data() FUNCTION below IF YOU WANT OUTPUT CSV FILES
export_data(infile, outpath, starttime=NULL, endtime=NULL, tags=NULL)

#Alternatively, if you have a file already created that you'd like to work
#with in the same format, you can always read.csv() into a data frame of the same name here

#this creates a unique ID for each combo of radio + node, summarizes node health variables for the input time interval and each unique combo of node x radio, and then...
#...expands the data frame to NA fill for missing time x ID combos based on your time interval chosen
plotting_data <- summarize_health_data(health_data, freq)
summarized <- plotting_data[[1]]
ids <- unique(summarized$ID)

#this creates a nested list of diagnostic plots for each combo of node and radio ID. You can index the list by the vector of node x ID combos passed to it
radionode_plots <- node_channel_plots(health_data, freq, ids)
#for instance radionode_plots[[1]] corresponds to the plots for ids[1]

#wanna make your plots look different? here's an example of how you can work with the list structure to make your own ggplot mods!
formatted <- lapply(radionode_plots, function(x) lapply(x, function(y) y + theme(axis.text=element_text(size=10),axis.title=element_text(size=30,face="bold"))))

#change 1 plot in the list (e.g. all of the batt plots)
batt_mod <- lapply(radionode_plots, function(x) {
  x[[1]] <- x[[1]] + theme(axis.text=element_text(size=10),axis.title=element_text(size=30,face="bold"))
  return(x)})

#PLOT INDICES
#1. RSSI scatter plot
#2. Battery
#3. number of check-ins
#4. check-ins as scaled line overlay of scaled RSSI plot

## if you want to write out plot images...
#call the function "export_node_channel_plots(health,outpath,x,y,z)" replacing x, y, z with the integer index of the plot desired for each of the 3 panels
#the resulting plots will be in "outpath" named "node_<RadioId>_<NodeId>.png"
export_node_channel_plots(health=health_data,freq=freq,out_path=outpath,whichplots = c(4,2,1))

###FOR V2 STATIONS ONLY
health_df <- health_data[[1]]
health_df <- health_df[health_df$Time > as.POSIXct("2021-08-12") & health_df$Time < as.POSIXct("2021-08-20"),]
nodes <- unique(health_df$NodeId)
#produces a list of plots per node showing if/when time stamp on sending vs. receiving mismatches occur, and if there are NA values
#you can index the list by the vector of nodes passed to it
mynodes <- node_plots(health_data,nodes,freq)
#mynodes[1]$`3274F4`[[1]]
#90649225 is min time diff to get to 2017
#for instance mynodes[[1]] corresponds to the plots for nodes[1]

#PLOT INDICES
#1. time mismatches (i.e. indicates when a GPS fix was likely lost)
#2. smaller time delays

#call the export_node_plots() function to output the plots looking for time stamp mismatches
#the resulting plots will be in "outpath" named "nodes_<node>.png"
export_node_plots(health=health_data,freq=freq,out_path=outpath, x=5,y=2,z=1) #start = as.POSIXct("2021-08-12"))
