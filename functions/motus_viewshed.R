motus_viewshed <- function(coords = c(38.897659, -77.036564), ht = 0, zoom = 10) {
  if (!requireNamespace("elevatr", quietly = TRUE)) install.packages("elevatr", quiet = TRUE)
  if (!requireNamespace("raster", quietly = TRUE)) install.packages("raster", quiet = TRUE)
  if (!requireNamespace("mapview", quietly = TRUE)) install.packages("mapview", quiet = TRUE)
  
  # Set up mapview display options
  mt <- c("CartoDB.DarkMatter", "Esri.WorldImagery", "Esri.WorldTopoMap", "OpenStreetMap")
  mapview::mapviewOptions(basemaps = mt, na.color = "#FFFFFF00")
  
  # Calculate range rings
  lon <- coords[2]; lat <- coords[1]
  # coords <- cbind(lon, lat)
  prj <- sprintf("+proj=laea +x_0=0 +y_0=0 +lon_0=%f +lat_0=%f", lon, lat)
  
  pt_ll <- sf::st_sfc(sf::st_point(rev(coords)), crs = 4326)
  pt <- sf::st_transform(pt_ll, prj)
  poly20 <- sf::st_buffer(pt, 20000)
  poly15 <- sf::st_buffer(pt, 15000)
  poly10 <- sf::st_buffer(pt, 10000)
  poly5 <- sf::st_buffer(pt, 5000)
  divisions <- st_wedges(0, 0, 20000, 8)
  divisions <- sf::st_sf(sf::st_set_crs(divisions, prj))
  
  # Get DEM
  suppressMessages(
    elev <- elevatr::get_elev_raster(poly20, z = zoom, clip = "locations", verbose = FALSE)
  )
  
  # Calculate viewshed
  pt_elev <- raster::extract(elev, sf::as_Spatial(pt)) + ht
  
  if (all(pt_elev > max(raster::values(elev), na.rm = TRUE))) {
    message("No elevation deficits found in viewshed. Displaying only 5, 10, and 15 km range rings.")
    suppressWarnings(
      m <- mapview::mapview(pt, layer.name = "Proposed station", 
                            alpha.regions = 0, cex = 4, color = "orange",
                            legend = FALSE, label = NULL) +
        mapview::mapview(poly15, layer.name = "15 km range", 
                         alpha.regions = 0, color = "white",
                         lwd = 1.25, legend = FALSE, label = NULL) +
        mapview::mapview(poly10, layer.name = "10 km range", 
                         alpha.regions = 0, color = "white",
                         lwd = 1.25, legend = FALSE, label = NULL) +
        mapview::mapview(poly5, layer.name = "5 km range", 
                         alpha.regions = 0, color = "white",
                         lwd = 1.25, legend = FALSE, label = NULL))
  } else {
    out <- elev - pt_elev
    out[out < 0] <- NA
    suppressWarnings(
      m <- mapview::mapview(out, layer.name = "Elev. deficit (m)",
                            col.regions = grDevices::hcl.colors(12, palette = "viridis")) +
        mapview::mapview(pt, layer.name = "Proposed station", 
                         alpha.regions = 0, cex = 4, color = "orange",
                         lwd = 1.25, legend = FALSE, label = NULL) +
        mapview::mapview(divisions, layer.name = "Direction guide",
                         alpha.regions = 0, color = "white",
                         lwd = 1.25, legend = FALSE, label = NULL) +
        mapview::mapview(poly20, layer.name = "20 km range", 
                         alpha.regions = 0, color = "white",
                         lwd = 1.25, legend = FALSE, label = NULL) +
        mapview::mapview(poly15, layer.name = "15 km range", 
                         alpha.regions = 0, color = "white",
                         lwd = 1.25, legend = FALSE, label = NULL) +
        mapview::mapview(poly10, layer.name = "10 km range", 
                         alpha.regions = 0, color = "white",
                         lwd = 1.25, legend = FALSE, label = NULL) +
        mapview::mapview(poly5, layer.name = "5 km range", 
                         alpha.regions = 0, color = "white",
                         lwd = 1.25, legend = FALSE, label = NULL))
  }
  
  
  # m <- leaflet::setView(m@map, lon, lat, zoom = 11)
  return(out)
}

st_wedge <- function(x,y,r,start,width,n=50){
  theta = seq(start, start+width, length=n)
  xarc = x + r*sin(theta)
  yarc = y + r*cos(theta)
  xc = c(x, xarc, x)
  yc = c(y, yarc, y)
  sf::st_polygon(list(cbind(xc,yc)))   
}

st_wedges <- function(x, y, r, nsegs){
  width = (2*pi)/nsegs
  starts = seq(-0.5, by = 1, length.out = nsegs + 1)*width
  polys = lapply(starts, function(s){st_wedge(x,y,r,s,width)})
  mpoly = sf::st_cast(do.call(sf::st_sfc, polys), "MULTIPOLYGON")
  mpoly
}
