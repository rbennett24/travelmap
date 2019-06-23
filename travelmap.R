# https://hansenjohnson.org/post/interactive-maps-in-r/
# https://rstudio.github.io/leaflet/
# https://bhaskarvk.github.io/user2017.geodataviz/notebooks/03-Interactive-Maps.nb.html

# https://rstudio.github.io/leaflet/map_widget.html
# https://hansenjohnson.org/post/bathymetric-maps-in-r/
# https://stackoverflow.com/questions/32504880/street-address-to-geolocation-lat-long
# https://www.jessesadler.com/post/geocoding-with-r/


# Load libraries
library(leaflet) # For map creation
library(ggmap) # For geocoding/geolocating
library(tmaptools) # For geocoding with Open Street Maps
library(geojsonio) # For manipulating map data
library(rgdal) # For map data
library(htmlwidgets) # To save map as an html widget
# library(mapview) # For saving static map images

# Set base directory
username<-"Tiamat"
rootdir<-paste0("C:\\Users\\",username,"\\Dropbox\\GIT\\")
basedir<-paste0(rootdir,"Travel_map\\")
setwd(basedir)

# Set directory where you eventually want to save the html file
htmldir<-paste0(rootdir,"professional-webpage\\")

# Create a base dataframe that you'll use for storing US locations
df.base<-data.frame(matrix(ncol=4,nrow=0))
colnames(df.base)<-c("City","State","Lon","Lat")

# Create a list of cities, and get coordinates
born<-c("Santa Rosa","CA")
born.df<-df.base
born.df[1,]<-born
born.df$Merged<-paste(born.df[,"City"],born.df[,"State"],sep=", ")
born.coords<-geocode_OSM(enc2utf8(born.df$Merged)) # Get spatial coordinates from Open Street Maps. Encode to UTF8 for special characters.
born.df$Lon<-born.coords$coords[1]
born.df$Lat<-born.coords$coords[2]

# Save coordinates for reference
# write.csv(born.df,"Data/Born.csv",row.names=F)

# In general, it's probably easier to automate this by keeping a .csv file with information about locations, then loading it.
born.df<-read.csv("Data/Born.csv",header=T)
lived.df<-read.csv("Data/Lived.csv",header=T)
visited.us.df<-read.csv("Data/Visited_US.csv",header=T)
visited.internat.df<-read.csv("Data/Visited_international.csv",header=T)

currentlyLive<-subset(lived.df,City=="Santa Cruz")

# Make an icon for where you currently live
customIcons <- iconList(
  house = makeIcon("home-solid.svg", 18, 18),
  hospital = makeIcon("hospital-solid.svg", 18, 18)
)

# Write a function to process location lists.
getSpatialData<-function(df,
                         locationval="City",
                         sortval="State",
                         checkfile=T,
                         filetocheck="./Data/Lived.csv"){
  
  if (checkfile==F){
  
      df$Merged<-paste(df[,locationval],df[,sortval],sep=", ") # Combine city + state/country for geolocating
      
      df<-df[order(df[,sortval],df[,locationval]),] # If you're compulsive and want to sort your dataframe...
      
      # It would be better to use mutate_geocode() for data frames, but there's some dumb problem I don't want to deal with.
      # https://github.com/dkahle/ggmap/issues/106
      coords<-geocode_OSM(enc2utf8(as.character(df$Merged)))
      df$Lon<-coords$coords[1]
      df$Lat<-coords$coords[2]
      rm(coords)
      return(df)

  }else{
      
      infile<-read.csv(filetocheck,header=T)
      tofind<-subset(infile,is.na(infile$Lat)) # If lat isn't yet specified
      
      if (nrow(tofind)==0){
        # Do nothing
        return(infile)
      }else{
        
        # Already processed rows
        found<-subset(infile,!(is.na(infile$Lat)))
        
        # Combine city + state/country for geolocating
        tofind$Merged<-paste(tofind[,locationval],tofind[,sortval],sep=", ")

        coords<-geocode_OSM(enc2utf8(as.character(tofind$Merged)))
        tofind$Lon<-coords$lon
        tofind$Lat<-coords$lat
        rm(coords)
        
        df<-rbind(found,tofind)
        # If you're compulsive and want to sort your dataframe...
        df<-df[order(df[,sortval],df[,locationval]),]
        
        # This creates compound labels for locations that you had to enter lat/lon for manually.
        df$Merged<-paste(df[,locationval],df[,sortval],sep=", ")
        return(df)
      }
    }
  }


lived.df<-getSpatialData(lived.df)
write.csv(lived.df,"./Data/Lived.csv",row.names = F)

visited.us.df<-getSpatialData(visited.us.df,filetocheck="./Data/Visited_US.csv")
write.csv(visited.us.df,"./Data/Visited_US.csv",row.names = F)

visited.internat.df<-getSpatialData(visited.internat.df,sortval="Country",filetocheck="./Data/Visited_international.csv")
write.csv(visited.internat.df,"./Data/Visited_international.csv",row.names = F)

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
  setView(lng = -10, lat = 23.3, zoom = 2) # I like the centering here

# Check map
# map


# Load in polygons for US states
# http://eric.clst.org/tech/usgeojson/
# https://github.com/kate-harrison/west/blob/master/west/data/Region/UnitedStates/Boundaries/cb_2013_us_state_20m/cb_2013_us_state_20m.shp
states <- geojson_read("data/gz_2010_us_040_00_500k.json", what = "sp")

# Load in polygons for countries
# http://www.naturalearthdata.com/downloads/110m-cultural-vectors/110m-admin-0-countries/
# http://thematicmapping.org/downloads/world_borders.php
countries <- readOGR("./Data/ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp")
# summary(countries)

##################
# Here, you want to make a list of states you have/haven't lived in or visited, so that you can color them differently with the polygon layers.
# https://stackoverflow.com/questions/5411979/state-name-to-abbreviation-in-r

# This stores saved states, and converts them from abbreviations to full names!
states.visited<-state.name[match(visited.us.df$State,state.abb)]
states.visited.poly<-subset(states,NAME%in%states.visited)

# Do the same thing for countries
countries.visited.poly<-subset(countries,NAME%in%visited.internat.df$Country)



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
  
        # Color in states that you've visited
        addPolygons(data=states.visited.poly,
                    weight = 1,
                    smoothFactor = 0.5,
                    opacity = 1.0,
                    color = "white",
                    fillOpacity = 0.25,
                    fillColor="blue"
                    ) %>%
  
        # Add in polygons for countries, except the US, because
        # the state shapefiles may not align with the country
        # shapefile
        addPolygons(data=subset(countries,NAME!="United States"),
                    weight = 1,
                    smoothFactor = 0.5,
                    opacity = 0.6,
                    color = "black"
        ) %>%
        
        # Color in countries that you've visited
        addPolygons(data=countries.visited.poly,
                    weight = 1,
                    smoothFactor = 0.5,
                    opacity = 1.0,
                    color = "black",
                    fillOpacity = 0.25,
                    fillColor="yellow"
        ) %>%
        
  # Add points for places you've visited in the US (states only)
  addCircleMarkers(data = visited.us.df, ~Lon, ~Lat,
                   weight = 0.5,
                   col = 'black', 
                   fillColor = "darkred",
                   radius = 9, 
                   fillOpacity = 0.6,
                   stroke = T,
                   label = ~Merged, 
                   group = 'Visited (US)',
                   labelOptions = labelOptions(textsize = "15px")) %>%
  
        # Add points for places you've visited internationally
        addCircleMarkers(data = visited.internat.df, ~Lon, ~Lat,
                         weight = 0.5,
                         col = 'black', 
                         fillColor = "darkmagenta",
                         radius = 9, 
                         fillOpacity = 0.6, 
                         stroke = T,
                         label = ~Merged, 
                         group = 'Visited (international)',
                         labelOptions = labelOptions(textsize = "15px")) %>% 
        
        # Add points for places you've lived
        addCircleMarkers(data = lived.df, ~Lon, ~Lat,
                         weight = 0.5,
                         col = 'black', 
                         fillColor = 'darkslategrey',
                         radius = 9, 
                         fillOpacity = 0.6, 
                         stroke = T, 
                         label = ~Merged, 
                         group = 'Lived in',
                         labelOptions = labelOptions(textsize = "15px")) %>% 
  
        # Add in current home
        # TO DO: change the marker icon
        #
        # https://rstudio.github.io/leaflet/markers.html
        # https://fontawesome.com/icons/home?style=solid
        addMarkers(data=currentlyLive, ~Lon, ~Lat,
                  label=paste("Current home:",currentlyLive$Merged),
                  icon = ~customIcons["house"],
                  labelOptions = labelOptions(textsize = "15px")
                  ) %>%
              
        # Add in birthplace
        addMarkers(data=born.df, ~Lon, ~Lat,
                   label=paste("Birthplace:",born.df$Merged),
                   icon = ~customIcons["hospital"],
                   labelOptions = labelOptions(textsize = "15px")
        ) %>%
  
        # Legend for controlling plotting layers
        addLayersControl(overlayGroups = c('Lived in',
                                           'Visited (US)',
                                           "Visited (international)"),
                      options = layersControlOptions(collapsed = FALSE),
                         position = 'topright')

# list groups to hide on startup
# hideGroup(c('Visited (US)'))

        
# Check the maps
map


# Save your work
save.image("mapcode.Rdata")


# Save a stand-alone, interactive map as an html file
saveWidget(widget = map, file = 'travelmap.html', selfcontained = T)
file.copy('travelmap.html',paste0(htmldir,'travelmap.html'),overwrite=T)


# Save a snapshot as a png file
# mapshot(map, file = 'travelmap.png')

#################################################