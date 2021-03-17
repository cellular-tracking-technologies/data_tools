require(suncalc)

gps_channel_plots <- function(health, freq, ids, lat=NULL, lon=NULL) { #freq, 
  version <- 2
  #time <- unlist(strsplit(freq, " "))
  #a <- as.difftime(as.integer(time[1]), unit=time[2])

  health_df <- health
  health_df$col <- cut(health_df$data_voltage, c(0,3.7, 4, Inf))
  
  #if(!is.null(lat) & !is.null(lon)) {
  #  sun <- getSunlightTimes(date=seq.Date(minday,maxday,by=1), keep=c("dawn", "dusk"), lat=lat, lon=lon)
  #  pbase <- ggplot() + theme_bw() + geom_rect(data=sun, aes(xmin=dusk, xmax=dawn, ymin=-Inf, ymax=Inf),fill='light grey') #+ theme(legend.position = "none")
  #} else if (version > 1) {
  #  lat <- mean(health_df$lat, na.rm=TRUE)
  #  lon <- mean(health_df$lon, na.rm=TRUE)
  #  sun <- getSunlightTimes(date=seq.Date(minday,maxday,by=1), keep=c("dawn", "dusk"), lat=lat, lon=lon)
  #  pbase <- ggplot() + theme_bw() + geom_rect(data=sun, aes(xmin=dusk, xmax=dawn, ymin=-Inf, ymax=Inf),fill='light grey') #+ theme(legend.position = "none") 
  #} else {
  pbase <- ggplot() + theme_bw() #+ theme(legend.position = "none")
  #}
  
  outplots <- lapply(ids, function(park) {
    #ea <- plot_data[plot_data$RadioId == "2" & plot_data$NodeId == "328b99",]
    #this is for scaling, in order to look for patterns in RSSI in relation to other variables
    
    ea <- health_df[health_df$serial==park,]

    ea <- ea[order(ea$GPS_YYYY.MM.DD_HH.MM.SS),]
    batt <- data.frame(x1 = head(ea$GPS_YYYY.MM.DD_HH.MM.SS, -1), x2 = tail(ea$GPS_YYYY.MM.DD_HH.MM.SS, -1), y1 = head(ea$data_voltage, -1), y2 = tail(ea$data_voltage, -1),
                       col = head(ea$col, -1))
    minx <- min(ea$GPS_YYYY.MM.DD_HH.MM.SS)
    maxx <- max(ea$GPS_YYYY.MM.DD_HH.MM.SS)
    minday <- as.Date(minx)
    maxday <- as.Date(maxx)
    #battery
    p = pbase +
      geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, colour=col), data=batt) + #size=2
      scale_color_manual(values = c("(0,3.7]" = "#FF0000","(3.7,4]" = "#F5B041","(4,Inf]" = "#00FF00")) +
      scale_x_datetime(date_labels="%b %d", limits=c(min(minx), max(maxx))) +
      scale_y_continuous(name="Batt", limits=c(2,5)) + theme(legend.position = "none")
    
    #RSSI scatter plot, A&V lines commented out    
    p1 = pbase + 
      theme(axis.text=element_text(size=10),
            axis.title=element_text(size=30,face="bold")) +
      #scaled: geom_point(data = ea, aes(x = Time, y = rssi, group=1)) +
      #geom_point(data = ea, aes(x = Time, y = RSSI, group=1)) +
      #scaled: geom_hline(yintercept = threshold) +
      #geom_hline(yintercept = -95) +
      geom_line(data = ea, aes(x = GPS_YYYY.MM.DD_HH.MM.SS, y = solar_current, group=1), colour="blue") +
      scale_x_datetime(date_labels="%b %d", limits=c(min(minx), max(maxx)))
    #+ scale_y_continuous(name="Count", limits=c(-110,-45))
    
    p2 = pbase + 
      theme(axis.text=element_text(size=10),
            axis.title=element_text(size=30,face="bold")) +
      #scaled: geom_point(data = ea, aes(x = Time, y = rssi, group=1)) +
      #geom_point(data = ea, aes(x = Time, y = RSSI, group=1)) +
      #scaled: geom_hline(yintercept = threshold) +
      #geom_hline(yintercept = -95) +
      geom_line(data = ea, aes(x = GPS_YYYY.MM.DD_HH.MM.SS, y = solar_charge, group=1), colour="orange") +
      scale_x_datetime(date_labels="%b %d", limits=c(min(minx), max(maxx)))
    
    p3 = pbase + 
      theme(axis.text=element_text(size=10),
            axis.title=element_text(size=30,face="bold")) +
      #scaled: geom_point(data = ea, aes(x = Time, y = rssi, group=1)) +
      #geom_point(data = ea, aes(x = Time, y = RSSI, group=1)) +
      #scaled: geom_hline(yintercept = threshold) +
      #geom_hline(yintercept = -95) +
      geom_line(data = ea, aes(x = GPS_YYYY.MM.DD_HH.MM.SS, y = temperature, group=1), colour="red") +
      scale_x_datetime(date_labels="%b %d", limits=c(min(minx), max(maxx)))
    
    p4 = pbase + 
      theme(axis.text=element_text(size=10),
            axis.title=element_text(size=30,face="bold")) +
      #scaled: geom_point(data = ea, aes(x = Time, y = rssi, group=1)) +
      #geom_point(data = ea, aes(x = Time, y = RSSI, group=1)) +
      #scaled: geom_hline(yintercept = threshold) +
      #geom_hline(yintercept = -95) +
      geom_line(data = ea, aes(x = GPS_YYYY.MM.DD_HH.MM.SS, y = activity, group=1), colour="purple") +
      scale_x_datetime(date_labels="%b %d", limits=c(min(minx), max(maxx)))
    
    #check-ins as scaled line overlay
    
    return(list(batt = p, current = p1, solarv = p2, temp = p3, act = p4))})
names(outplots) <- ids
return(outplots)}

export_gps_channel_plots <- function(plotlist=NULL,out_path=getwd(),whichplots = c(3,2,1)) {
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
    #png(file_name, width=1800, height=1000)
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
    ggarrange(plotlist=myplots, nrow=length(myplots)) %>% ggexport(filename = file_name, width=1800, height=1000)
    #dev.off()
  }
}