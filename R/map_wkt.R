#' Map WKT geometry (points and polygons)
#'
#' @description `map_wkt()` takes a well-known text (WKT) geometry column and maps points and polygons onto a gray leaflet map. All NA geometry is dropped before mapping.
#'
#' @details Define your dataframe, the column that contains WKT, and an option to map specific geometry types.
#'
#' @param df - The name of the data frame that contains WKT geometry.
#' @param wellknowntext - The name of the specific column within the data frame that contains the WKT geometry. This parameter is currently not fully implemented and defaults to the Darwin Core 'footprintsWKT'.
#' @param type -  Pick one from "points", "polygons", or "all" to map specific geometry types.
#'
#' @return The function returns a dynamic, zoomable leaflet map with the specific geometry plotted.
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' \dontrun{
#' #map species observations
#' map_wkt(my_NPS_species_obs)
#' 
#' #map species observations - points only
#' map_wkt(my_NPS_species_obs, wellknowntext = "footprintWKT", type = "points")
#' }
map_wkt <- function(df, wellknowntext = "footprintWKT", type = "all", remove.duplicates = FALSE) {
  #filter to just wellknowntext column:
  wkt_grepl <- paste0('\\b', wellknowntext, '\\b')
  df <- df[grepl(wkt_grepl, colnames(df))]
  
  #omit NAs - not important for plotting anyway:
  df <- na.omit(df)
  
  #convert to geographic object:
  df <- sf::st_as_sf(df, wkt = wellknowntext)
  
  #new column in data frame for the geometry type
  df$geometry_types <- sf::st_geometry_type(df)
  
  #use the geometry_type column to filter only for POINT or POLYGON
  df_pts <- df[df$geometry_types == "POINT",]
  df_polys <- df[df$geometry_types == "POLYGON",]
  
  #only map what is requested
  if(type == "points") {
    map <- leaflet::leaflet(df, 
              options = leafletOptions(preferCanvas = TRUE)) %>%
      #addTiles(group = "OSM (default)"); prevent unwanted map updates:
              leaflet::addProviderTiles(providers$Esri.WorldGrayCanvas, 
                                        options = providerTileOptions(
                                          updateWhenZooming = FALSE,
                                          updateWhenIdle = TRUE)) %>%
              leaflet::addCircles(data = df_pts, color = "blue",) #odd stray ,
  } else if(type == "polygons") {
    map <- leaflet::leaflet(df, 
              options = leafletOptions(preferCanvas = TRUE)) %>%
      #addTiles(group = "OSM (default)"); prevent unwanted map updates:
              leaflet::addProviderTiles(providers$Esri.WorldGrayCanvas, 
                                        options = providerTileOptions(
                                          updateWhenZooming = FALSE,
                                          updateWhenIdle = TRUE)) %>%
              leaflet::addPolygons(data = df_polys, color = "red",) #odd stray ,
  } else if(type == "all") {
    map <- leaflet::leaflet(df, 
              options = leafletOptions(preferCanvas = TRUE)) %>%
      #addTiles(group = "OSM (default)") %>%; prevent unwatned map updates:
              leaflet::addProviderTiles(providers$Esri.WorldGrayCanvas,
                                        options = providerTileOptions(
                                          updateWhenZooming = FALSE,
                                          updateWhenIdle = TRUE)) %>%
              leaflet::addCircles(data = df_pts, color = "blue",) %>%
              leaflet::addPolygons(data = df_polys, color = "red",)  #odd stray ,
  }
  return(map)
}