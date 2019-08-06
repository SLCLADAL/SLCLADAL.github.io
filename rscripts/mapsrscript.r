
# "Geo-Spatial Data Visualizaion in R"
# "UQ SLC Digital Team"
#
# clean current workspace
rm(list=ls(all=T))
# set options
options(stringsAsFactors = F)         # no automatic data transformation
options("scipen" = 100, "digits" = 4) # supress math annotation
# install libraries
install.packages(c("RgoogleMaps", "ggmap", "mapproj", "sf",
                   "dplyr", "OpenStreetMap", "devtools"))
# install package from github
devtools::install_github("dkahle/ggmap", ref = "tidyup")
# load library
library(OpenStreetMap)
# extract map
australia <- openmap(c(-8,110),
	c(-45,160),
	type = "nps",
	minNumTiles=6)
# plot map
plot(australia)
# extract map
queensland1 <- openmap(c(-8,135),
	c(-30,160),
	type = "osm",
	minNumTiles=4)
queensland2 <- openmap(c(-8,135),
	c(-30,160),
	type = "esri-topo",
	minNumTiles=4)
queensland3 <- openmap(c(-8,135),
	c(-30,160),
	type = "osm-public-transport",
	minNumTiles=4)
# plot maps
par(mfrow = c(1, 3)) # display plots in 1 row/3 columns
plot(queensland1); plot(queensland2); plot(queensland3); par(mfrow = c(1, 1)) # restore original settings
# load library
library(rworldmap)
# get map
worldmap <- getMap(resolution = "coarse")
# plot worldmap
plot(worldmap, xlim = c(-180, 180), ylim = c(-90, 90), 
     asp = 1, wrap=c(-180,180))
# load libraries
library(maptools)
library(maps)
# plot maps
par(mfrow = c(1, 2)) # display plots in 1 row/3 columns
# show map with Latitude 200 as center
map('world', xlim = c(100, 300))
# add axes
map.axes()
# show filled map with Latitude 200 as center
ww2 <- map('world', wrap=c(0,360), plot=FALSE, fill=TRUE)
map(ww2, xlim = c(100, 300), fill=TRUE)
par(mfrow = c(1, 1)) # restore original settings
# load data
airports <- read.delim("D:\\Uni\\UQ\\LADAL\\SLCLADAL.github.io\\data/airports.txt", sep = "\t", header = T)
# inspect data
head(airports)
# plot data on world map
plot(worldmap, xlim = c(-80, 160), ylim = c(-50, 100), 
     asp = 1, bg = "lightblue", col = "black", fill = T)
# add points
points(airports$Longitude, airports$Latitude, col = "red", cex = .01)
# load library
library(rworldmap)
# get map
newmap <- getMap(resolution = "low")
# plot map
plot(newmap, xlim = c(-20, 59), ylim = c(35, 71), 
     asp = 1, fill = T, border = "darkgray", 
     col = "wheat2", bg = "gray95")
# add points
points(airports$Longitude, airports$Latitude, col = "red", cex = .5, pch = 20)
# plot data on world map
plot(worldmap, xlim = c(120, 140), ylim = c(-45, -10), 
     asp = 1, bg = "lightblue", border = "lightgrey", col = "wheat3", 
     fill = T, wrap=c(-180,180))
points(airports$Longitude, airports$Latitude, 
       col = "darkblue", cex = .5, pch = 20)
# read in routes data
routes <- read.delim("D:\\Uni\\UQ\\LADAL\\SLCLADAL.github.io\\data/routes.txt", sep = "\t", header=T)
# inspect first 6 lines of routes data
head(routes)
# load library
library(plyr)
# extract number of arrivals
arrivals <- ddply(routes, .(destinationAirportID), "nrow")
names(arrivals)[2] <- "flights"
# inspect data
head(arrivals)
# create arrival  table
airportA <- merge(airports, arrivals, by.x = "ID", by.y = "destinationAirportID")
# inspect arrival table
head(airportA)
# get map
australia <- getMap(resolution = "low")
# plot data on world map
plot(australia, xlim = c(110, 160), ylim = c(-45, -10), 
     asp = 1, bg = "lightblue", border = "darkgrey", 
     col = "wheat3", fill = T)
# add points
points(airportA$Longitude, airportA$Latitude,
       # define colors as transparent
       col = rgb(red = 0, green = 0, blue = 1, alpha = 0.3),
       # define size as number of flights div. by 50
       cex = airportA$flights/50, pch = 20)
# load ggplot2 library
library(ggplot2)
# create a layer of borders
map <- borders("world", colour="gray70", fill="wheat2") 
AustralianAirports <- ggplot() +   
  map +
  geom_point(aes(x=airportA$Longitude, y= airportA$Latitude), 
             color="blue", alpha = .2, size=airportA$flights/20) +
  scale_x_continuous(name="Longitude", limits=c(110, 160)) +
  scale_y_continuous(name="Latitude", limits=c(-45, -10)) +
  theme(panel.background = element_rect(fill = "lightblue",
                                colour = "lightblue"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
AustralianAirports
# load libraries
library(maps)
library(RColorBrewer)
# load data
worldpopulation <- read.delim("D:\\Uni\\UQ\\LADAL\\SLCLADAL.github.io\\data/worldpopulation.txt", sep = "\t", header = T)
# inspect data
head(worldpopulation)
# create a color palette
palette = colorRampPalette(brewer.pal(n=9, name='Oranges'))(nrow(worldpopulation))
# sort the colors in the same order as the countries' populations
palette = palette[rank(-worldpopulation$Population)]
# draw a map of the world
map(database='world', fill = T, border = "darkgray", 
    mar=c(0,0,0,0), col = palette, bg = "gray95", 
    ylim = c(-60, 85))
# clean current workspace
rm(list=ls(all=T))
# set options
options(stringsAsFactors = F)         # no automatic data transformation
options("scipen" = 100, "digits" = 4) # supress math annotation
# load libraries
library(ggmap)
library(maptools)
library(maps)
library(geonames)
# create vector with places I have visited
visited <- data.frame(matrix(c(
  "Australia", "AUS", "Brisbane", -27.455, 153.0351,
  "Australia", "AUS", "Adelaide",	-34.935,	138.6,
  "Australia", "AUS", "Canberra",	-35.283,	149.129,
  "Australia", "AUS", "Sydney",	-33.92,	151.1852,
  "Australia", "AUS", "Melbourne",	-37.82,	144.975,
  "Japan", "JPN", "Tokyo",	35.685,	139.7514,
  "Finland", "FIN", "Helsinki",	60.1756,	24.9341,
  "Finland", "FIN", "Tampere",	61.5,	23.75,
  "Czech Republic", "CZE", "Prague",	50.0833,	14.466,
  "Poland", "POL", "Poznan",	52.4058,	16.8999,
  "Austria", "AUT", "Vienna",	48.2,	16.3666,
  "Austria", "AUT", "Innsbruck",	47.2804,	11.41,
  "Austria", "AUT", "Salzburg",	47.79941, 13.04399,
  "United States", "USA", "New York",	40.6943,	-73.9249,
  "United States", "USA", "Kansas City",	39.1239,	-94.5541,
  "United States", "USA", "St. Louis",	38.627003, -90.199402,
  "United States", "USA", "Chicago",	41.881832, -87.623177,
  "United Kingdom", "GBR", "London",	51.500,	-0.1167,
  "United Kingdom", "GBR", "Cardiff",	51.481583, -3.179090,
  "Ireland", "IRL", "Galway",	53.2724,	-9.0488,
  "Ireland", "IRL", "Dublin",	53.3331,	-6.2489,
  "Ireland", "IRL", "Limerick",	52.668018, -8.630498,
  "Ireland", "IRL", "Athlone",	53.423933, -7.94069,
  "Ireland", "IRL", "Bray",	53.20278, -6.09833,
  "United Kingdom", "GBR", "Belfast",	54.607868, -5.926437,
  "United Kingdom", "GBR", "Derry",	54.9981, -7.30934,
  "Germany", "DEU", "Hamburg",	53.55,	10.00,
  "Germany", "DEU", "Kassel", 51.31667, 9.5,
  "Germany", "DEU", "Berlin",	52.5218,	13.4015,
  "Germany", "DEU", "Cologne",	50.93,	6.95,
  "Germany", "DEU", "Munich",	48.1299,	11.575,
  "Germany", "DEU", "Mainz",	49.98419, 8.2791,
  "Germany", "DEU", "Frankfurt",	50.11552, 8.68417,
  "Germany", "DEU", "Hannover",	52.37052, 9.73322,
  "Germany", "DEU", "Bremen",	53.07516, 8.80777,
  "Germany", "DEU", "Bonn",	50.73438, 7.09549,
  "Germany", "DEU", "Greifswald",	54.09311, 13.38786,
  "Denmark", "DNK", "Copenhagen", 55.67594, 12.56553,
  "France", "FRA", "Paris",	48.8667,	2.3333,
  "Netherlands", "NDL", "Amsterdam",	52.35,	4.9166,
  "Netherlands", "NDL", "Arnhem",	51.98, 5.91111,
  "Belgium", "BEL", "Antwerpen",	51.2204,	4.415,
  "Belgium", "BEL", "Leuven",	50.87959, 4.70093,
  "Spain", "ESP", "Madrid",	40.4,	-3.6834,
  "Spain", "ESP", "Gibraltar",	35.946339, -5.655601,
  "Spain", "ESP", "Mallorca",	39.56939, 2.65024,
  "Spain", "ESP", "Fuerteventura",	28.413460, -14.008890, 
  "Croatia", "HRV", "Zagreb",	45.8,	16,
  "Croatia", "HRV", "Novi Vinodolski", 45.12806, 14.78889,
  "Croatia", "HRV", "Djakovo", 45.309997, 18.409782,
  "Croatia", "HRV", "Rieka", 45.34306, 14.40917,
  "Slovenia", "SVN", "Maribor", 46.55472, 15.64667,
  "Norway", "NOR", "Bergen",	60.391,	5.3245,
  "Italy", "ITA", "Rome",	41.896,	12.4833,
  "Italy", "ITA", "Venice",	45.43713, 12.33265,
  "Italy", "ITA", "Trieste",	45.64325, 13.7903,
  "Hungary", "HUN", "Budapest",	47.5,	19.0833,
  "Romania", "ROU", "Bucarest",	44.439663, 26.096306,
  "Bulgaria", "BGR", "Sofia",	42.698334, 23.319941,
  "Bulgaria", "BGR", "Varna",	43.204666, 27.910543,
  "Greece", "GRC", "Athens",	37.9833,	23.7333,
  "Turkey", "TUR", "Istanbul",	41.105,	29.01,
  "Switzerland", "CHE", "Zurich",	47.38,	8.55,
  "Switzerland", "CHE", "Basel",	47.5804,	7.59,
  "United Arab Emirates", "ARE", "Dubai",	25.23,	55.28,
  "United Arab Emirates", "ARE", "Abu Dhabi",	24.4667,	54.3666,
  "United Kingdom", "GBR", "Edinburgh", 55.953251, -3.188267), 
  ncol = 5, byrow = T))
colnames(visited) <- c("Country", "ISO3", "City", "Latitude", "Longitude")
visited$Longitude  <- as.numeric(visited$Longitude)
visited$Latitude <- as.numeric(visited$Latitude)
# order table
visited <- visited[order(visited$Country, visited$City),]
# write to disc
write.table(visited, "D:\\Uni\\UQ\\LADAL\\SLCLADAL.github.io\\data/visited.txt", sep = "\t", col.names=T, row.names = F)
# inspect data
head(visited)
# load data
visited <- read.delim("D:\\Uni\\UQ\\LADAL\\SLCLADAL.github.io\\data/visited.txt", sep = "\t", header = T)
# inspect data
head(visited)
# determine how often a country was visited
ncountry <- as.data.frame(table(visited$ISO3))
colnames(ncountry)[1] <- "ISO3" 
# add frequency to visited
visited <- merge(visited, ncountry, by = "ISO3")
# inspect data
head(visited)
# load world data for plotting 
data(wrld_simpl)
# def. color (bias for contrast)
pal <- colorRampPalette(brewer.pal(6, 'Reds'), 
                        bias = 10)(length(visited$Freq))
pal <- pal[with(visited, findInterval(Freq, sort(unique(Freq))))]
# define color for countries not in visited
countrycolor <- rep(grey(0.90), length(wrld_simpl@data$NAME))
# define colors for countries in visited
countrycolor[match(visited$Country, wrld_simpl@data$NAME)] <- pal
# plot map
plot(wrld_simpl, ylim=c(-40, 85), xlim = c(-180, 180),
     mar=c(0,0,0,0), bg="white", border = "gray70", 
     col=countrycolor)
# add points
points(visited$Longitude, visited$Latitude, col="black", pch=20, cex = .5)
# load libraries
library(RColorBrewer)
library(maptools)
library(ggplot2)
# load data
data(wrld_simpl)
# use the contry name instead of 3-letter ISO as id
wrld_simpl@data$id <- wrld_simpl@data$NAME
wrld <- fortify(wrld_simpl, region="id")
# remove Antarctica from map
wrld <- subset(wrld, id != "Antarctica") 
# start plotting
ggmapplot <- ggplot() + 
  geom_map(data=wrld, map=wrld, aes(map_id=id, x=long, y=lat), 
           fill="white", color="gray30", size=0.25) + 
  geom_map(data=visited, map=wrld, 
           aes(map_id=Country, fill=as.factor(Freq)),
           color="gray30", size=0.25) +
  geom_point(data = visited,
             aes(x = Longitude, y = Latitude), 
             col = "red", size = .75) +
  scale_fill_manual(
    values=colorRampPalette(brewer.pal(6, 'Greens'), 
                        bias = 5)(length(visited$Freq)), 
                               name="No. cities visited") + 
  coord_map("gilbert") +  # spherical
# coord_map() +           # for normal Mercator projection
  labs(x="", y="") + 
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
                   panel.border = element_blank(),
                   panel.background = 
          element_rect(fill = "transparent", colour = NA),
                   panel.grid = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank(),
                   legend.position = "right")
ggmapplot
#map <- get_map(location = "Europe")
# plot map and layers
mapPoints <- ggmap() + map
  geom_point(aes(x = lon, y = lat, 
                 size = sqrt(flights)), 
             data = airportD, alpha = .5)
mapPointsLegend <- mapPoints +
  scale_area(breaks = sqrt(c(1, 5, 10, 50, 100, 500)), 
             labels = c(1, 5, 10, 50, 100, 500), 
             name = "departing routes")
mapPointsLegend
# create the data set containing both departures and arrivals
airportD$type <- "departures"
airportA$type <- "arrivals"
airportDA <- rbind(airportD, airportA)
# map the data
# map + data points
mapPointsDA <- ggmap(map) +
  geom_point(aes(x = lon, y = lat, size = sqrt(flights)), data = airportDA, alpha = .5)
# adjust the legend
mapPointsLegendDA <- mapPointsDA +
  scale_area(breaks = sqrt(c(1, 5, 10, 50, 100, 500)), labels = c(1, 5, 10, 50, 100, 500), name = "routes")
# panels according to type (departure/arrival)
mapPointsFacetsDA <- mapPointsLegendDA +
  facet_grid(. ~ type)
# plot the map
mapPointsFacetsDA
# load library
library(googleVis)
# create motion chart object
Geo=gvisGeoMap(Population, locationvar="Country", numvar="Population", options = list(height=350, daatMode='regions'))
# display motion chart
plot(Geo)
# load libraries
library(sf)
library(RgoogleMaps)
# retrieve map
map <- get_map(location = 'Australia', zoom = 4)
ggmap(map)
library(ggmap)
library(mapproj)
map <- get_map(location = 'Europe', zoom = 4)
ggmap(map)
