# Script for testing coordinates
# Load libraries
library(leaflet) # For map creation
library(ggmap) # For geocoding/geolocating
library(tmaptools) # For geocoding with open street maps
library(geojsonio) # For manipulating map data
library(rgdal) # For map data

# Set base directory
rootdir<-"C:\\Users\\Tiamat\\Dropbox\\GIT\\"
basedir<-paste0(rootdir,"Travel_map\\")
setwd(basedir)

test.df<-data.frame(matrix(ncol=3,nrow=0))
colnames(test.df)<-c("name","Lon","Lat")


loc<-"Uspantán, Guatemala"

test.df["name",1]<-loc
#coords<-geocode(enc2utf8(test.df$name)) # Get spatial coordinates from Google # NO LONGER USED
coords<-geocode_OSM(enc2utf8(test.df$name)) # Get spatial coordinates from Open Street Maps
test.df$Lon<-coords$coords[1]
test.df$Lat<-coords$coords[2]
test.df

# Now create a base map.
map <- leaflet() %>%
  
  # Add basemap from a free set given by providers
  # https://leaflet-extras.github.io/leaflet-providers/preview/index.html
  # https://github.com/leaflet-extras/leaflet-providers
  # addProviderTiles(providers$Esri.NatGeoWorldMap) # I like this but it's kind of busy
  addProviderTiles(providers$Esri.WorldPhysical) %>%
  
  # focus map in a certain area / zoom level when it opens
  # Santa Cruz, CA
  # setView(lng = -122.026389, lat = 36.971944, zoom = 3)
  # setView(lng = 0, lat = 0, zoom = 3)
  setView(lng = -39.37, lat = 23.3, zoom = 2) # I like the centering here


# Load in polygons for US states
# http://eric.clst.org/tech/usgeojson/
# https://github.com/kate-harrison/west/blob/master/west/data/Region/UnitedStates/Boundaries/cb_2013_us_state_20m/cb_2013_us_state_20m.shp
states <- geojson_read("data/gz_2010_us_040_00_500k.json", what = "sp")

# Load in polygons for countries
# http://thematicmapping.org/downloads/world_borders.php
countries <- readOGR("./Data/TM_WORLD_BORDERS_SIMPL-0.3/TM_WORLD_BORDERS_SIMPL-0.3.shp")

# Plot!

# Start with US states 
# https://rstudio.github.io/leaflet/choropleths.html
# https://rstudio.github.io/leaflet/shapes.html
map <- map %>% addPolygons(data=states,
                           weight = 1,
                           smoothFactor = 0.5,
                           opacity = 1.0,
                           color = "white"
) %>%
  
  # Add in polygons for countries
  addPolygons(data=countries,
              weight = 1,
              smoothFactor = 0.5,
              opacity = 0.6,
              color = "black"
  ) %>%

  addMarkers(data=test.df, ~Lon, ~Lat,
             label=~name,
             labelOptions = labelOptions(textsize = "15px")
  )

# Check the maps
map