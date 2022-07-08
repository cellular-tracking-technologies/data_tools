#contour shapefile conversion ####
#A function to convert a shapefile of contour lines into points for ggmap mapping. The resulting database can be used by geom_path to add contourlines to any ggmap figure.
#
#Returns database of contour lines for specified area.
#
#Arguments
#contour_shp = shapefile of contours, obtainable from https://osdatahub.os.uk/downloads/open/Terrain50

contour_to_tibble <- function(contour_shp) {
  
  #Create new contours of sp class
  new_contour <- as(contour_shp, Class = "Spatial")
  
  
  #Tidy sp data into a dataframe
  new_contour <-broom::tidy(new_contour)
  
  
  #Add new column named id to original contours dataframe
  contour_shp <- dplyr::mutate(contour_shp, id = seq(from = 1, to = nrow(contour_shp), by = 1))
  
  
  #make this new column into a character vairbiable
  contour_shp$id <- as.character(contour_shp$id)
  
  
  #join data from sp and tibble contours data
  new_contour <- left_join(new_contour, contour_shp, by="id")
  
  
  #remove geometry column
  new_contour <- dplyr::select(new_contour, -geometry)
  
  return(new_contour)
}



#GPX file to tibble conversion ####
#A function to convert a GPX file to tibble format.GPX file must contain geometry column with corresponding latitude and longitude point.
#
#Returns database of waypoint locations in London timezone.
#
#Arguments
#route_gpx = A gpx file of transect

route_to_tibble <- function(route_gpx) {
  
  route.dataframe <- as.data.frame(route_gpx)
  
  #Separate geometry column into Latitude and Longitude
  route.dataframe <- separate(separate(separate(route.dataframe,
                                                col = geometry,
                                                into = c("Longitude", "Latitude"),
                                                sep = "\\,"),
                                       col = Longitude,
                                       into = c("c", "Longitude"),
                                       sep = "\\("),
                              col = Latitude,
                              into = c("Latitude"),
                              sep = "\\)")
  
  #Change Lat and Long from factor to numeric values
  route.dataframe <- mutate(route.dataframe,
                            Longitude = as.numeric(Longitude),
                            Latitude = as.numeric(Latitude))
  
  
  #Subset to meaningful data
  route.dataframe <- dplyr::select(route.dataframe, track_seg_point_id, ele, time, Longitude, Latitude)
  
  #Edit name of Long and Lat variable to correspond to route
  route.dataframe <- dplyr::rename(route.dataframe,
                                   R.Long = Longitude,
                                   R.Lat = Latitude)
  
  #Set timezone to match CTT datatime
  attr(route.dataframe$time, "tzone") <- "Europe/London"
  
  return(route.dataframe)
}



#Detection Waypoint Join Function ####
#A function to join CTT detection data to a corresponding waypoint by the time the record was taken. Both datasets must be in a tibble format and must include a variable "time" in POSIXct YYYY-MM-DD HH-MM-SS format.
#
#Returns database of positive detections and their corresponding waypoint position.
#
#Arguments
#detection_data = CTT detection database, must include time column
#route_data = transect route points, must include a latitude, longitude and time column

detection_route_join <- function(detection_data, route_data) {
  
  #arranged data by time
  new_detection_data <- arrange(detection_data, time)
  new_route_data <- arrange(route_data, time)
  
  #change to datatable
  detection_data_dt <- data.table(new_detection_data, key = names(new_detection_data))
  route_data_dt <- data.table(new_route_data, key = names(new_route_data))
  
  #set column to match by (time)
  setkey(detection_data_dt, time)
  setkey(route_data_dt, time)
  
  #join datatables by nearest time
  all_trial_data <- route_data_dt[detection_data_dt, roll = "nearest"]
  
  #change to dataframe
  all_trial_data <- as.data.frame(all_trial_data)
  
  return(all_trial_data)
}
