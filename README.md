# travelmap
R code and data files for an interactive html map showing places I've lived and visited. The main file is travelmap.R, everything else is just input/supporting files.

Code is heavily borrowed from https://hansenjohnson.org/post/interactive-maps-in-r/ and other sources cited in travelmap.R. The code is probably a lot uglier than it could be.

Note that the input is a set of .csv files specifying "City" and "Country" or "State" in separate columns. The scripts should fill in everything else.

Lat/Lon are retrieved using [OpenStreetMap](https://www.openstreetmap.org/)
