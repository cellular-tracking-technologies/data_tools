source("functions/data_manager.R")
source("functions/gps_health.R")
library(ggplot2)
library(ggpubr)
sdf <- load_gps("~/Documents/data/ExploreUnits")
serials <- unique(sdf$serial)
dsf <- gps_channel_plots(sdf,"1 hour",serials)
export_gps_channel_plots(dsf,"../plots/test/", whichplots = c(1,2,3,4))

