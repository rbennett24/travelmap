# Load libraries
library(tidyverse)
library(leaflet) # For map creation
library(tmaptools) # For geocoding with Open Street Maps
library(sf) # For working with shape files
library(htmlwidgets) # To save map as an html widget
library(readr) # For reading CSVs; probably not necessary


# For high-resolution outlines of countries and states
# Needs to be installed in atypical ways; pick one of the following
# library(remotes)
# remotes::install_github("ropensci/rnaturalearthhires")
# 
# install.packages("rnaturalearthhires", repos = "https://ropensci.r-universe.dev", type = "source")
# install.packages("C:/Users/Ryan Bennett 2/Downloads/rnaturalearthhires_0.2.1.tar.gz", repos = NULL, type="source")
library(rnaturalearth)


# Set base directory
# username<-"rbennett"
# rootdir<-paste0("D:\\",username,"\\Dropbox\\GIT\\")
username<-"Tiamat"
rootdir<-paste0("C:\\Users\\",username,"\\Dropbox\\GIT\\")
basedir<-paste0(rootdir,"Travel_map\\")
setwd(basedir)

# Set directory where you eventually want to save the html file
htmldir<-paste0(rootdir,"professional-webpage\\")


# Save the local encoding of files on your windows machine so you can
# use it to force UTF-8 encoding when reading CSV files.
# Probably not needed/overkill, but you've run into encoding 
# problems before with accented characters so we do this.
winencoding <- "ISO-8859-1"



# Read in list of locations you've lived in, and that you've visited in the US and internationally.
# These CSV files should all have the column structure {"City","State","Lon","Lat","Merged"} for US locations, 
# e.g. {Huntsville,AL,-86.5861037,34.7303688,"Huntsville, AL"},
# and {"City","Country","Lon","Lat","Merged"} for international locations,
# e.g. {Melbourne,Australia,144.9631608,-37.8142176,"Melbourne, Australia"}
# The 'Merged' column gets used for geocoding spatial coordinates of locations
# that you've newly entered into your Excel-format CSV spreadsheets as City, State/Country alone.

# Notice that we are explicitly demanding a specific encoding here, rather than letting readr::read_csv guess the local machine encoding, just in case.
lived.df<-read_csv("Data/Lived.csv",locale=locale(encoding=winencoding))
visited.us.df<-read_csv("Data/Visited_US.csv",locale=locale(encoding=winencoding))
visited.internat.df<-read_csv("Data/Visited_international.csv",locale=locale(encoding=winencoding))



# # Make sure that every column of these dataframes is treated as UTF-8, if needed.
# forceUTF8 <- function(df){
#   df %>% mutate(across(everything(),~ iconv(.x,from=winencoding,to="UTF-8")))
# }
# lived.df <- forceUTF8(lived.df)
# visited.us.df <- forceUTF8(visited.us.df)
# visited.internat.df <- forceUTF8(visited.internat.df)



# Write a function to get spatial coordinates for any locations in these spreadsheets which currently lack them
getSpatialData<-function(df,
                         locationval="City",
                         sortval="State"){

      tofind<-subset(df,is.na(df$Lat)) # If lat isn't yet specified

      if (nrow(tofind)==0){
        # Do nothing
        return(df)
      }else{

        # Already processed rows
        found<-subset(df,!(is.na(df$Lat)))

        # Combine city + state/country for geolocating
        # We use unite() rather than paste() because
        # paste() now has some weird behaviors that break this, maybe
        # because of City/State/Country entries with a space in them.
        # tofind$Merged<-paste(tofind[,locationval],tofind[,sortval],sep=", ")
        tofind <- tofind %>% unite(Merged,c(locationval,sortval),remove=F,sep=", ") %>%
                             relocate(Merged,.after=Lat)
        

        # Get spatial coordinates for any locations missing those coordinates.
        coords<-geocode_OSM(tofind$Merged,as.data.frame=T)

        tofind$Lon<-coords[,"lon"]
        tofind$Lat<-coords[,"lat"]
        rm(coords)

        df.out<-rbind(found,tofind)
        # If you're compulsive and want to sort your dataframe.
        # https://stackoverflow.com/questions/27034655/how-to-use-dplyrarrangedesc-when-using-a-string-as-column-name
        df.out <- df.out %>% arrange(!!sym(sortval),!!sym(locationval))
        
        return(df.out)
  }
}

# Update location lists
lived.df <- getSpatialData(lived.df)
visited.us.df<-getSpatialData(visited.us.df)
visited.internat.df<-getSpatialData(visited.internat.df,sortval="Country")



#############
# Save updated CSVs, using the same encoding that you loaded them with. 
write.csv(lived.df,"./Data/Lived.csv",row.names = F, fileEncoding = winencoding)
write.csv(visited.us.df,"./Data/Visited_US.csv",row.names = F, fileEncoding = winencoding)
write.csv(visited.internat.df,"./Data/Visited_international.csv",row.names = F, fileEncoding = winencoding)


# Create base map of world.
map <- leaflet() %>%
  
  # Add basemap from a free set given by providers
  # https://leaflet-extras.github.io/leaflet-providers/preview/index.html
  # https://github.com/leaflet-extras/leaflet-providers
  # addProviderTiles(providers$Esri.NatGeoWorldMap) # I like this but it's kind of busy
  addProviderTiles(providers$Esri.WorldPhysical
                   # options = providerTileOptions(maxZoom = 19)
                   ) %>%
  
  # focus map in a certain area / zoom level when it opens
  # Santa Cruz, CA
  # setView(lng = -122.026389, lat = 36.971944, zoom = 3)
  # setView(lng = 0, lat = 0, zoom = 3)
  setView(lng = -10, lat = 23.3, zoom = 2) # I like the centering here


# Get US states and add them to the map
states <- ne_states(country="united states of america",
                    returnclass = "sf")

map <- map %>% addPolygons(data=states,
                           weight = 1,
                           smoothFactor = 0.5,
                           opacity = 1.0,
                           color = "white"
)


# Color in states that you've visited
# First, take abbreviations of states (as in your CSV file) and convert to full names.
states.visited<-state.name[match(visited.us.df$State,state.abb)]
states.visited.poly<-subset(states,name_en %in% states.visited)

map <- map %>% addPolygons(data=states.visited.poly,
            weight = 1,
            smoothFactor = 0.5,
            opacity = 1.0,
            color = "white",
            fillOpacity = 0.25,
            fillColor="blue"
            )


# Get national boundaries and add them to the map.
# We do this after adding US states (in white) so that US national boundary (in black) is visible too.
countries <- ne_countries(scale = "medium", # Use "large" for more precision larger filesize
                          type = 'map_units',
                          returnclass = "sf")

map <- map %>% addPolygons(data=countries,
                           weight = 1,
                           smoothFactor = 0.5,
                           opacity = 0.6,
                           color = "black"
)



# Color in countries that you've visited
countries.visited.poly<-subset(countries,#name %in% visited.internat.df$Country|
                                         #sovereignt %in% visited.internat.df$Country|
                                         # admin %in% visited.internat.df$Country |
                                         subunit %in% visited.internat.df$Country
                                         )

if (length(unique(countries.visited.poly$subunit)) < length(unique(visited.internat.df$Country))){

  print("WARNING: some of the countries that you've visited are not being located for the purpose of coloring those countries in on the map. These are:")
  
  print(subset(unique(visited.internat.df$Country),
        !(unique(countries.visited.poly$subunit) %in% unique(visited.internat.df$Country))))

}

map <- map %>% 
        addPolygons(data=countries.visited.poly,
                    weight = 1,
                    smoothFactor = 0.5,
                    opacity = 1.0,
                    color = "black",
                    fillOpacity = 0.25,
                    fillColor="yellow"
        )

# Add points for places you've visited in the US (states only)
map <- map %>% addCircleMarkers(data = visited.us.df, ~Lon, ~Lat,
                 weight = 0.5,
                 col = 'black',
                 fillColor = "darkred",
                 radius = 9,
                 fillOpacity = 0.6,
                 stroke = T,
                 label = ~Merged,
                 group = 'Visited (US)',
                 # clusterOptions = markerClusterOptions(), # Collapse points that are close together
                 labelOptions = labelOptions(textsize = "15px"))

# Add points for places you've visited internationally
map <- map %>% addCircleMarkers(data = visited.internat.df, ~Lon, ~Lat,
                 weight = 0.5,
                 col = 'black',
                 fillColor = "darkmagenta",
                 radius = 9,
                 fillOpacity = 0.6,
                 stroke = T,
                 label = ~Merged,
                 group = 'Visited (international)',
                 # clusterOptions = markerClusterOptions(), # Collapse points that are close together
                 labelOptions = labelOptions(textsize = "15px"))


# Make an icon for where you currently live, and where you were born,
# and add them to the map.
customIcons <- iconList(
  house = makeIcon("home-solid.png", 18, 18),
  hospital = makeIcon("hospital-solid.png", 18, 18)
)

currentlyLive<-subset(lived.df,City=="Santa Cruz")

map <- map %>% addMarkers(data=currentlyLive, ~Lon, ~Lat,
                          label=paste("Current home:",currentlyLive$Merged),
                          icon = ~customIcons["house"],
                          labelOptions = labelOptions(textsize = "15px")
)


# Add your birthplace.
born.df<-data.frame(matrix(ncol=4,nrow=0))
colnames(born.df)<-c("City","State","Lon","Lat")
born<-c("Santa Rosa","CA",NA,NA)
born.df[1,]<-born
born.df <- getSpatialData(born.df)
born.df

map <- map %>%addMarkers(data=born.df, ~Lon, ~Lat,
                         label=paste("Birthplace:",born.df$Merged),
                         icon = ~customIcons["hospital"],
                         labelOptions = labelOptions(textsize = "15px")
)


# Add points for places you've lived
map <- map %>% addCircleMarkers(data = lived.df, ~Lon, ~Lat,
                                weight = 0.5,
                                col = 'black',
                                fillColor = 'darkslategrey',
                                radius = 9,
                                fillOpacity = 0.6,
                                stroke = T,
                                label = ~Merged,
                                group = 'Lived in',
                                # clusterOptions = markerClusterOptions(), # Collapse points that are close together
                                labelOptions = labelOptions(textsize = "15px"))



# Legend for controlling plotting layers
map <- map %>% addLayersControl(overlayGroups = c('Lived in',
                                   'Visited (US)',
                                   "Visited (international)"),
              options = layersControlOptions(collapsed = FALSE),
                 position = 'topright')


# Check map
map

# Save your work
save.image("mapcode.Rdata")

# Save a stand-alone, interactive map as an html file
saveWidget(widget = map, file = 'travelmap.html', selfcontained = T)
file.copy('travelmap.html',paste0(htmldir,'travelmap.html'),overwrite=T)