#' ---
#' title: "Grid for mink habitat suitability model"
#' author: "Eleonora Milea"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#'

#' ---
#' 
#' In this script, I create a 1 km grid and extract from each grid cell the covariates 
#' that were used in the GLM for the American mink. The grid will be then used 
#' to develop the habitat suitability model
#'
#' ---
#'

# Loading libraries and objects
library(sp)
library(mapview)
library(rgdal)
library(raster)
library(ncdf4)
library(sf)
library(rgeos)
boundary <-  readOGR("layers_27700/boundary_nohebrids.shp",
                     GDAL1_integer64_policy = T) # loading boundary of study area


# Creating grid points
grid <- makegrid(boundary, cellsize = 1000) 
grid <- SpatialPoints(grid, proj4string = boundary@proj4string)
mapview(grid)
grid <- grid[boundary,]
grid <- SpatialPointsDataFrame(coords = grid@coords,
                               proj4string = grid@proj4string,
                               data = data.frame(id = 1:length(grid)))
# mapview(grid)


#' ---
#' 
#' **National parks/reserves**
#' 
# Importing reserves boundaries
reserves <- readOGR("layers_27700/reserves.shp",
                    GDAL1_integer64_policy = T)
reserves@proj4string


# Distance calculation between points and nearest reserve
dist_reserves <- gDistance(reserves, grid, byid = T)
grid@data$dist_reserves <- apply(dist_reserves, MARGIN = 1, FUN = min) # using minimum dist for each ID, margin 1 indicates to apply function over rows
head(grid@data)
grid@data$dist_reserves <- round(grid@data$dist_reserves, 0) # rounding up and plaing into main object
head(grid@data$dist_reserves)


# Importing national parks
nat_parks <- readOGR("layers_27700/national_parks.shp",
                     GDAL1_integer64_policy = T)


# Distance calculation between points and nearest park
dist_parks <- gDistance(nat_parks, grid, byid = T)
grid@data$dist_parks <- apply(dist_parks, MARGIN = 1, FUN = min) 
head(grid@data)
grid@data$dist_parks <- round(grid@data$dist_parks, 0)
head(grid@data$dist_parks)


# Importing ramsars
ramsars <- readOGR("layers_27700/ramsar.shp",
                   GDAL1_integer64_policy = T)


# Distance calculation between points and nearest ramsar
dist_ramsars <- gDistance(ramsars, grid, byid = T)
grid@data$dist_ramsars <- apply(dist_ramsars, MARGIN = 1, FUN = min) 
head(grid@data)
grid@data$dist_ramsars <- round(grid@data$dist_ramsars, 0)
head(grid@data$dist_ramsars)


# Cleaning the environment
rm(ramsars)
rm(reserves)
rm(nat_parks)
rm(dist_reserves)
rm(dist_parks)
rm(dist_ramsars)
gc()


#' ---
#' 
#' **Slope, aspect, elevation**
#'  
# Importing elevation
elev <- raster("layers_27700/dem_50m.tif")


# Extracting elevation at obs points
elev_grid <- raster::extract(elev, grid)
grid@data$elevation <- elev_grid
head(grid$elevation)


# Calculating slope from elevaion
slope <- terrain(elev, opt = "slope",
                 neighbors = 8, units = "degrees")


# Extracting slope at obs points
slope_grid <- raster::extract(slope, grid)
grid@data$slope <- slope_grid
head(grid$slope)


# Calculating aspect from elevation
aspect <- terrain(elev, opt = "aspect",
                  neighbors = 8, units = "degrees")


# Extracting aspect at obs points
aspect_grid <- raster::extract(aspect, grid)
grid@data$aspect <- aspect_grid
head(grid$aspect)


# Cleaning the environment
rm(elev)
rm(elev_grid)
rm(aspect)
rm(aspect_grid)
rm(slope)
rm(slope_grid)
gc()


#' ---
#' 
#' **Roads**
#' 
# Importing file
roads <- readOGR("layers_27700/roads.shp",
                 GDAL1_integer64_policy = T)
crs(roads) <- boundary@proj4string # set CRS
unique(roads@data$fclass)


# Subsetting roads
paths <- subset(roads, fclass == "path" | fclass == "footway")
unique(paths@data$fclass)       

special_roads <- subset(roads, fclass == "living_street" | fclass == "track" | 
                          fclass == "pedestrian" | fclass == "track_grade1" | 
                          fclass == "track_grade2" | fclass == "track_grade3" |
                          fclass == "track_grade4" | fclass == "track_grade5" |
                          fclass == "pedestrian")  
unique(special_roads@data$fclass)             

roads <- subset(roads, fclass == "residential" | fclass == "motorway" 
                | fclass == "primary" | fclass == "secondary"
                | fclass == "tertiary" | fclass == "trunk" 
                | fclass == "unclassified")
unique(roads@data$fclass)


# Distance calculation between points and nearest path
dist_paths <- gDistance(paths, grid, byid = T) 

# put dist roads records at id positions in mink df
grid@data$dist_paths <- apply(dist_paths, MARGIN = 1, FUN = min) 
head(grid$dist_paths)


# Cleaning the environment
rm(paths)
rm(dist_paths)


# Distance calculation between points and nearest path
dist_specialroads <- gDistance(special_roads, grid, byid = T) 

# put dist roads records at id positions in mink df
grid@data$dist_specialroads <- apply(dist_specialroads, MARGIN = 1, FUN = min) 
head(grid$dist_specialroads)


# Cleaning environment
rm(special_roads)
rm(dist_specialroads)


# Distance calculation between points and nearest path
dist_roads <- gDistance(roads, grid, byid = T) 

# put dist roads records at id positions in mink df
grid@data$dist_roads <- apply(dist_roads, MARGIN = 1, FUN = min) 
head(grid$dist_roads)


# Cleaning environment
rm(roads)
rm(dist_roads)
gc()


#' ---
#' 
#' **Waterways**
#'  
# Importing waterways
water <- readOGR("layers_27700/waterways.shp",
                 GDAL1_integer64_policy = T)
unique(water@data$layer)


# distance calculation between points and nearest waterway
dist_water <- gDistance(water, grid, byid = T)


# Placing them in grid data
grid@data$dist_water <- apply(dist_water, MARGIN = 1, FUN = min)
head(grid@data)


# Cleaning environment
rm(water)
rm(dist_water)
gc()



#' Now we create several objects to create a grid for land cover data available
#' These will be used to calculate grid for the land cover raster files we have. 
#' It was not done before because previous variables did not have any years associated
#' with them, since they are all variables that are not expected to change over time.
#' These grids objects will be used to create 5 different mink habitat suitability 
#' models. Hence one model for 2007, 2015, 2017, 2018 ^ 2019. 
#' Each raster of mink hsm will be used to extract levels of mink habitat suitability
#' at pres and abs points of water voles. So we will group water voles observations 
#' by year intervals (e.g. observations from 2000 to 2010) to extract these values 
#' using the hsm closest in time. For instance, water vole obs from 2000 until 2010
#' will be grouped together and levels of mink habitat suitability will be extracted
#' using the habitat suitability from 2007. This was needed as if we were to use
#' only grid predictions from the most recent years, we would have had to use 
#' the hsm based on 2019 predictions only to extract values for water voles values 
#' from previous years. This would have assumed that mink habitat suitability had
#' not changed over the years.

grid07 <- grid 
grid15 <- grid
grid17 <- grid
grid18 <- grid
grid19 <- grid


#' ---
#' 
#' **Land cover: proportions with 100 m buffer**
#' 
# Importing land covers
land_07 <- raster("layers_27700/cover/landcover_2007.tif")
land_15 <- raster("layers_27700/cover/landcover_2015.tif")
land_17 <- raster("layers_27700/cover/landcover_2017.tif")
land_18 <- raster("layers_27700/cover/landcover_2018.tif")
land_19 <- raster("layers_27700/cover/landcover_2019.tif")


#'##### **Creating buffers for each time period**
#' 
# Creating a buffer 
gridbuffer_100 <- gBuffer(grid, width = 100, byid = T) 


#'##### **Intersection of land cover**
#' Now we intersect each buffered point with land cover type for each time period
#' using a for loop to iterate through each observation point. 
#'
# 2000 - 2010
Sys.time()
int.list07 <- list() # creating an empty list to store data
for (i in 1:length(gridbuffer_100)){
  int.list07[[i]] <- raster::intersect(land_07,  gridbuffer_100[i,]) 
  print(i)
}
# beep(2)
Sys.time()


# 2011 - 2016
int.list15 <- list() # creating an empty list to store data
for (i in 1:length(gridbuffer_100)){
  int.list15[[i]] <- raster::intersect(land_15, gridbuffer_100[i,])
  print(i)
}
# beep(2)
Sys.time()


# 2017
int.list17 <- list() # creating an empty list to store data
for (i in 1:length(gridbuffer_100)){
  int.list17[[i]] <- raster::intersect(land_17, gridbuffer_100[i,])
  print(i)
}
# beep(2)
Sys.time()


# 2018
int.list18 <- list() # creating an empty list to store data
for (i in 1:length(gridbuffer_100)){
  int.list18[[i]] <- raster::intersect(land_18, gridbuffer_100[i,])
  print(i)
}
Sys.time()


# Intersecting land cover type at buffered points
int.list19 <- list()
for (i in 1:length(gridbuffer_100)){
  int.list19[[i]] <- raster::intersect(land_19, gridbuffer_100[i,])
}


#' ---
#' 
#'##### **Counting pixels** 
#'
#' Now we count the number of pixels for each land cover type at each buffered point
#' and store them into a new data frame. We create a df for each time period where
#' the number of rows in the df is determined by the number of observations present
#' in each buffer object, while the number of rows is determined by the number
#' of land cover categories from each raster file. This information can be found
#' in the supplementary documents when downloading the land cover raster files. 

# Creating df
# 2000 - 2010
landcover_07100 <- data.frame(matrix(NA, nrow = length(gridbuffer_100),
                                     ncol = 23))

# Labelling columns
names(landcover_07100) <- c( "1","2","3","4","5","6", "7", "8", "9", "10",
                             "11", "12", "13", "14", "15", "16", "17", "18",
                             "19", "20", "21", "22", "23")
head(landcover_07100)


# 2011 - 2016
landcover_15100 <- data.frame(matrix(NA, nrow = length(gridbuffer_100),
                                     ncol = 21))

# Labelling columns
names(landcover_15100) <- c( "1","2","3","4","5","6", "7", "8", "9", "10",
                             "11", "12", "13", "14", "15", "16", "17", "18",
                             "19", "20", "21")


# 2017
landcover_17100 <- data.frame(matrix(NA, nrow = length(gridbuffer_100),
                                     ncol = 21))

# Labelling columns
names(landcover_17100) <- c( "1","2","3","4","5","6", "7", "8", "9", "10",
                             "11", "12", "13", "14", "15", "16", "17", "18",
                             "19", "20", "21")


# 2018
landcover_18100 <- data.frame(matrix(NA, nrow = length(gridbuffer_100),
                                     ncol = 21))

# Labelling columns
names(landcover_18100) <- c( "1","2","3","4","5","6", "7", "8", "9", "10",
                             "11", "12", "13", "14", "15", "16", "17", "18",
                             "19", "20", "21")


# 2019
landcover_19100 <- data.frame(matrix(NA, nrow = length(gridbuffer_100),
                                  ncol = 21))

names(landcover_19100) <- c( "1","2","3","4","5","6", "7", "8", "9", "10",
                          "11", "12", "13", "14", "15", "16", "17", "18",
                          "19", "20", "21")


#' Now we can count the number of pixels at each intersection
#'  
# 2000 - 2010
for (i in 1:length(gridbuffer_100)){
  for ( j in 1:length(names(landcover_07100))) {
    landcover_07100[i,j] <- table(
      values(int.list07[[i]]))[as.character(names(landcover_07100)[j])]
  }}

# 2011 - 2016
for (i in 1:length(gridbuffer_100)){
  for ( j in 1:length(names(landcover_15100))) {
    landcover_15100[i,j] <- table(
      values(int.list15[[i]]))[as.character(names(landcover_15100)[j])]
  }}

# 2017
for (i in 1:length(gridbuffer_100)){
  for ( j in 1:length(names(landcover_17100))) {
    landcover_17100[i,j] <- table(
      values(int.list17[[i]]))[as.character(names(landcover_17100)[j])]
  }}

# 2018
for (i in 1:length(gridbuffer_100)){
  for ( j in 1:length(names(landcover_18100))) {
    landcover_18100[i,j] <- table(
      values(int.list18[[i]]))[as.character(names(landcover_18100)[j])]
  }}


# 2019
for (i in 1:length(gridbuffer_100)){
  for ( j in 1:length(names(landcover_19100))) {
    landcover_19100[i,j] <- table(
      values(int.list19[[i]]))[as.character(names(landcover_19100)[j])]
  }}


#' Now we can change the columns names using the names of land cover types that
#' can be found in supplementary documents from each raster file. The order needs 
#' to be the same as the one provided in the suppl. files.
#' 
# 2000 - 2010
names(landcover_07100) <- c ("mixed_woodland",
                             "coniferous_woodland",
                             "arable_horticulture",
                             "improved_grassland",
                             "rough_grassland",
                             "neutral_grassland",
                             "calcareous_grassland",
                             "acid_grassland",
                             "fen_marsh_swamp",
                             "heather",
                             "heather_grassland",
                             "bog",
                             "montane_habitats",
                             "inland_rock",
                             "saltwater",
                             "freshwater",
                             "supralittoral_rock",
                             "supralittoral_sediment",
                             "littoral_rock",
                             "littoral_sediment",
                             "saltmarsh",
                             "urban",
                             "suburban")

# 2011 - 2016
names(landcover_15100) <- c ("mixed_woodland",
                             "coniferous_woodland",
                             "arable_horticulture",
                             "improved_grassland",
                             "neutral_grassland",
                             "calcareous_grassland",
                             "acid_grassland",
                             "fen_marsh_swamp",
                             "heather",
                             "heather_grassland",
                             "bog",
                             "inland_rock",
                             "saltwater",
                             "freshwater",
                             "supralittoral_rock",
                             "supralittoral_sediment",
                             "littoral_rock",
                             "littoral_sediment",
                             "saltmarsh",
                             "urban",
                             "suburban")

# 2017
names(landcover_17100) <- c ("mixed_woodland",
                             "coniferous_woodland",
                             "arable_horticulture",
                             "improved_grassland",
                             "neutral_grassland",
                             "calcareous_grassland",
                             "acid_grassland",
                             "fen_marsh_swamp",
                             "heather",
                             "heather_grassland",
                             "bog",
                             "inland_rock",
                             "saltwater",
                             "freshwater",
                             "supralittoral_rock",
                             "supralittoral_sediment",
                             "littoral_rock",
                             "littoral_sediment",
                             "saltmarsh",
                             "urban",
                             "suburban")

# 2018
names(landcover_18100) <- c ("mixed_woodland",
                             "coniferous_woodland",
                             "arable_horticulture",
                             "improved_grassland",
                             "neutral_grassland",
                             "calcareous_grassland",
                             "acid_grassland",
                             "fen_marsh_swamp",
                             "heather",
                             "heather_grassland",
                             "bog",
                             "inland_rock",
                             "saltwater",
                             "freshwater",
                             "supralittoral_rock",
                             "supralittoral_sediment",
                             "littoral_rock",
                             "littoral_sediment",
                             "saltmarsh",
                             "urban",
                             "suburban")


names(landcover_19100) <- c("mixed_woodland",
                         "coniferous_woodland",
                         "arable_horticulture",
                         "improved_grassland",
                         "neutral_grassland",
                         "calcareous_grassland",
                         "acid_grassland",
                         "fen_marsh_swamp",
                         "heather",
                         "heather_grassland",
                         "bog",
                         "inland_rock",
                         "saltwater",
                         "freshwater",
                         "supralittoral_rock",
                         "supralittoral_sediment",
                         "littoral_rock",
                         "littoral_sediment",
                         "saltmarsh",
                         "urban",
                         "suburban")


#'---
#'
#' **Land cover: proportions with 500m buffer**
#' 
#'##### **Creating buffers for each time period**
#' 
# Creating a buffer 
gridbuffer_500 <- gBuffer(grid, width = 500, byid = T) 


#'##### **Intersection of land cover**
#' Now we intersect each buffered point with land cover type for each time period
#' using a for loop to iterate through each observation point. 
#'
# 2000 - 2010
Sys.time()
int.list07 <- list() # creating an empty list to store data
for (i in 1:length(gridbuffer_500)){
  int.list07[[i]] <- raster::intersect(land_07,  gridbuffer_500[i,]) 
  print(i)
}
# beep(2)
Sys.time()


# 2011 - 2016
int.list15 <- list() # creating an empty list to store data
for (i in 1:length(gridbuffer_500)){
  int.list15[[i]] <- raster::intersect(land_15, gridbuffer_500[i,])
  print(i)
}
# beep(2)
Sys.time()


# 2017
int.list17 <- list() # creating an empty list to store data
for (i in 1:length(gridbuffer_500)){
  int.list17[[i]] <- raster::intersect(land_17, gridbuffer_500[i,])
  print(i)
}
# beep(2)
Sys.time()


# 2018
int.list18 <- list() # creating an empty list to store data
for (i in 1:length(gridbuffer_500)){
  int.list18[[i]] <- raster::intersect(land_18, gridbuffer_500[i,])
  print(i)
}
Sys.time()


# Intersecting land cover type at buffered points
int.list19 <- list()
for (i in 1:length(gridbuffer_500)){
  int.list19[[i]] <- raster::intersect(land_19, gridbuffer_500[i,])
}


#' ---
#' 
#'##### **Counting pixels** 
#'
#' Now we count the number of pixels for each land cover type at each buffered point
#' and store them into a new data frame. We create a df for each time period where
#' the number of rows in the df is determined by the number of observations present
#' in each buffer object, while the number of rows is determined by the number
#' of land cover categories from each raster file. This information can be found
#' in the supplementary documents when downloading the land cover raster files. 

# Creating df
# 2000 - 2010
landcover_07500 <- data.frame(matrix(NA, nrow = length(gridbuffer_500),
                                     ncol = 23))

# Labelling columns
names(landcover_07500) <- c( "1","2","3","4","5","6", "7", "8", "9", "10",
                             "11", "12", "13", "14", "15", "16", "17", "18",
                             "19", "20", "21", "22", "23")
head(landcover_07500)


# 2011 - 2016
landcover_15500 <- data.frame(matrix(NA, nrow = length(gridbuffer_500),
                                     ncol = 21))

# Labelling columns
names(landcover_15500) <- c( "1","2","3","4","5","6", "7", "8", "9", "10",
                             "11", "12", "13", "14", "15", "16", "17", "18",
                             "19", "20", "21")


# 2017
landcover_17500 <- data.frame(matrix(NA, nrow = length(gridbuffer_500),
                                     ncol = 21))

# Labelling columns
names(landcover_17500) <- c( "1","2","3","4","5","6", "7", "8", "9", "10",
                             "11", "12", "13", "14", "15", "16", "17", "18",
                             "19", "20", "21")


# 2018
landcover_18500 <- data.frame(matrix(NA, nrow = length(gridbuffer_500),
                                     ncol = 21))

# Labelling columns
names(landcover_18500) <- c( "1","2","3","4","5","6", "7", "8", "9", "10",
                             "11", "12", "13", "14", "15", "16", "17", "18",
                             "19", "20", "21")


# 2019
landcover_19500 <- data.frame(matrix(NA, nrow = length(gridbuffer_500),
                                     ncol = 21))

names(landcover_19500) <- c( "1","2","3","4","5","6", "7", "8", "9", "10",
                             "11", "12", "13", "14", "15", "16", "17", "18",
                             "19", "20", "21")


#' Now we can count the number of pixels at each intersection
#'  
# 2000 - 2010
for (i in 1:length(gridbuffer_500)){
  for ( j in 1:length(names(landcover_07500))) {
    landcover_07500[i,j] <- table(
      values(int.list07[[i]]))[as.character(names(landcover_07500)[j])]
  }}

# 2011 - 2016
for (i in 1:length(gridbuffer_500)){
  for ( j in 1:length(names(landcover_15500))) {
    landcover_15500[i,j] <- table(
      values(int.list15[[i]]))[as.character(names(landcover_15500)[j])]
  }}

# 2017
for (i in 1:length(gridbuffer_500)){
  for ( j in 1:length(names(landcover_17500))) {
    landcover_17500[i,j] <- table(
      values(int.list17[[i]]))[as.character(names(landcover_17500)[j])]
  }}

# 2018
for (i in 1:length(gridbuffer_500)){
  for ( j in 1:length(names(landcover_18500))) {
    landcover_18500[i,j] <- table(
      values(int.list18[[i]]))[as.character(names(landcover_18500)[j])]
  }}


# 2019
for (i in 1:length(gridbuffer_500)){
  for ( j in 1:length(names(landcover_19500))) {
    landcover_19500[i,j] <- table(
      values(int.list19[[i]]))[as.character(names(landcover_19500)[j])]
  }}


#' Now we can change the columns names using the names of land cover types that
#' can be found in supplementary documents from each raster file. The order needs 
#' to be the same as the one provided in the suppl. files.
#' 
# 2000 - 2010
names(landcover_07500) <- c ("mixed_woodland",
                             "coniferous_woodland",
                             "arable_horticulture",
                             "improved_grassland",
                             "rough_grassland",
                             "neutral_grassland",
                             "calcareous_grassland",
                             "acid_grassland",
                             "fen_marsh_swamp",
                             "heather",
                             "heather_grassland",
                             "bog",
                             "montane_habitats",
                             "inland_rock",
                             "saltwater",
                             "freshwater",
                             "supralittoral_rock",
                             "supralittoral_sediment",
                             "littoral_rock",
                             "littoral_sediment",
                             "saltmarsh",
                             "urban",
                             "suburban")

# 2011 - 2016
names(landcover_15500) <- c ("mixed_woodland",
                             "coniferous_woodland",
                             "arable_horticulture",
                             "improved_grassland",
                             "neutral_grassland",
                             "calcareous_grassland",
                             "acid_grassland",
                             "fen_marsh_swamp",
                             "heather",
                             "heather_grassland",
                             "bog",
                             "inland_rock",
                             "saltwater",
                             "freshwater",
                             "supralittoral_rock",
                             "supralittoral_sediment",
                             "littoral_rock",
                             "littoral_sediment",
                             "saltmarsh",
                             "urban",
                             "suburban")

# 2017
names(landcover_17500) <- c ("mixed_woodland",
                             "coniferous_woodland",
                             "arable_horticulture",
                             "improved_grassland",
                             "neutral_grassland",
                             "calcareous_grassland",
                             "acid_grassland",
                             "fen_marsh_swamp",
                             "heather",
                             "heather_grassland",
                             "bog",
                             "inland_rock",
                             "saltwater",
                             "freshwater",
                             "supralittoral_rock",
                             "supralittoral_sediment",
                             "littoral_rock",
                             "littoral_sediment",
                             "saltmarsh",
                             "urban",
                             "suburban")

# 2018
names(landcover_18500) <- c ("mixed_woodland",
                             "coniferous_woodland",
                             "arable_horticulture",
                             "improved_grassland",
                             "neutral_grassland",
                             "calcareous_grassland",
                             "acid_grassland",
                             "fen_marsh_swamp",
                             "heather",
                             "heather_grassland",
                             "bog",
                             "inland_rock",
                             "saltwater",
                             "freshwater",
                             "supralittoral_rock",
                             "supralittoral_sediment",
                             "littoral_rock",
                             "littoral_sediment",
                             "saltmarsh",
                             "urban",
                             "suburban")


names(landcover_19500) <- c("mixed_woodland",
                            "coniferous_woodland",
                            "arable_horticulture",
                            "improved_grassland",
                            "neutral_grassland",
                            "calcareous_grassland",
                            "acid_grassland",
                            "fen_marsh_swamp",
                            "heather",
                            "heather_grassland",
                            "bog",
                            "inland_rock",
                            "saltwater",
                            "freshwater",
                            "supralittoral_rock",
                            "supralittoral_sediment",
                            "littoral_rock",
                            "littoral_sediment",
                            "saltmarsh",
                            "urban",
                            "suburban")


#' ---
#' 
#'##### **Calculating proportions** 
#'
#' Now we need to calculate the proportion of each land cover type in all buffers.

# Replacing NAs with 0
landcover_07500[is.na(landcover_07500)] <- 0
landcover_15500[is.na(landcover_15500)] <- 0
landcover_17500[is.na(landcover_17500)] <- 0
landcover_18500[is.na(landcover_18500)] <- 0
landcover_19500[is.na(landcover_19500)] <- 0


# Creating a list with all landcovers
landcover <- list(landcover_07500, landcover_15500,
                  landcover_17500, landcover_18500, landcover_19500)


names(landcover) <- c("07", "15", "17", "18", "19") # Changing labels
summary(landcover)


# Calculating sum of all pixels for each row (which corresponds to each grid observation)
row_sum <- lapply(landcover, rowSums)


# Creating an empty list to store proportions
props <- list()


# Using a for loop to iterate through each element of the landcover list
for(i in 1:length(landcover)){
  
  # Calculating proportion
  props[[i]] <- landcover[[i]]/row_sum[[i]] # calculating proportions for each observation of every element of the list
  props[[i]] <- round(props[[i]], 3) # rounding up
}
head(props[[1]])


# Replacing NaNs with 0
props <- rapply(props, f = function(x) ifelse(is.nan(x), 0, x),
                how ="replace" ) 
summary(props[[1]]) # check if it worked


# Creating data frame to store proportions from each time period separately 
propcover_07 <- as.data.frame(props[[1]])
dim(propcover_07)
head(propcover_07)
propcover_15 <- as.data.frame(props[[2]])
propcover_17 <- as.data.frame(props[[3]])
propcover_18 <- as.data.frame(props[[4]])
propcover_19 <- as.data.frame(props[[5]])

# 2000 - 2010
names(propcover_07)
grid07@data$woodland500 <- rowSums(propcover_07[, c(1,2),drop = F]) 
grid07@data$arable_horticulture500 <- propcover_07[,3]
grid07@data$grassland500 <- rowSums(propcover_07[, c(4:8), drop = F]) 
grid07@data$wetland500 <- rowSums(propcover_07[, c(9, 12), drop = F]) 
grid07@data$heather500 <- rowSums(propcover_07[, c(10, 11), drop = F]) 
grid07@data$montane_inlandrock500 <- rowSums(propcover_07[, c(13:14), drop = F])  
grid07@data$freshwater500 <- propcover_07[,16]
grid07@data$coastal_habitats500 <- rowSums(propcover_07[, c(15,17:21), drop = F]) 
grid07@data$urban500 <- rowSums(propcover_07[, c(22, 23), drop = F]) 
head(grid07@data)


# 2011-2016
names(propcover_15)
grid15@data$woodland500 <- rowSums(propcover_15[, c(1,2), drop = F]) 
grid15@data$arable_horticulture500 <- propcover_15[,3]
grid15@data$grassland500 <- rowSums(propcover_15[, c(4:7), drop = F])  
grid15@data$wetland500 <- rowSums(propcover_15[, c(8, 11), drop = F]) 
grid15@data$heather500 <- rowSums(propcover_15[, c(9, 10), drop = F]) 
grid15@data$montane_inlandrock500 <- propcover_15[, 12]
grid15@data$freshwater500 <- propcover_15[,14]
grid15@data$coastal_habitats500 <- rowSums(propcover_15[, c(13, 15:19), drop = F]) 
grid15@data$urban500 <- rowSums(propcover_15[, c(20, 21), drop = F])
head(grid15@data)


# 17
names(propcover_17)
grid17@data$woodland500 <- rowSums(propcover_17[, c(1,2), drop = F]) 
grid17@data$arable_horticulture500 <- propcover_17[,3]
grid17@data$grassland500 <- rowSums(propcover_17[, c(4:7), drop = F])  
grid17@data$wetland500 <- rowSums(propcover_17[, c(8, 11), drop = F])
grid17@data$heather500 <- rowSums(propcover_17[, c(9, 10), drop = F]) 
grid17@data$montane_inlandrock500 <- propcover_17[, 12]
grid17@data$freshwater500 <- propcover_17[,14]
grid17@data$coastal_habitats500 <- rowSums(propcover_17[, c(13, 15:19), drop = F]) 
grid17@data$urban500 <- rowSums(propcover_17[, c(20, 21), drop = F]) 
head(grid17@data)


# 18
names(propcover_18)
grid18@data$woodland500 <- rowSums(propcover_18[, c(1,2), drop = F]) 
grid18@data$arable_horticulture500 <- propcover_18[,3]
grid18@data$grassland500 <- rowSums(propcover_18[, c(4:7), drop = F]) 
grid18@data$wetland500 <- rowSums(propcover_18[, c(8, 11), drop = F]) 
grid18@data$heather500 <- rowSums(propcover_18[, c(9, 10), drop = F]) 
grid18@data$montane_inlandrock500 <- propcover_18[, 12]
grid18@data$freshwater500 <- propcover_18[,14]
grid18@data$coastal_habitats500 <- rowSums(propcover_18[, c(13, 15:19), drop = F]) 
grid18@data$urban500 <- rowSums(propcover_18[, c(20, 21), drop = F]) 
head(grid18@data)



# Adding proportions to grid sp object for each land cover category
# 19
names(props)
grid19@data$woodland500 <- rowSums(props[, c(1,2), drop = F]) 
grid19@data$arable_horticulture500 <- props[,3]
grid19@data$grassland500 <- rowSums(props[, c(4:7), drop = F]) 
grid19@data$wetland500 <- rowSums(props[, c(8, 11), drop = F]) 
grid19@data$heather500 <- rowSums(props[, c(9, 10), drop = F]) 
grid19@data$montane_inlandrock500 <- props[, 12]
grid19@data$freshwater500 <- props[,14]
grid19@data$coastal_habitats500 <- rowSums(props[, c(13, 15:19), drop = F]) 
grid19@data$urban500 <- rowSums(props[, c(20, 21), drop = F]) 
head(grid19@data)

# Cleaning the environment
rm(list = setdiff(ls(), c("grid", "grid07", "grid15", "grid17", "grid18", "grid19",
                          "land_07", "land_15", "land_17", "land_18", "land_19")))
gc()



#' ---
#' 
#' **Land cover: distances**
#' 
# Importing land cover distance files
#### **Calculating distance from land cover**
# 2007
dist_07 <- list.files(path = "layers_27700/coverdist_07",
                      all.files = T, pattern = ".tif$") # import folder from 2007


coverdist_07 <- lapply(file.path("layers_27700/coverdist_07", dist_07),
                       raster) # convert into raster
names(coverdist_07) <- dist_07
summary(coverdist_07)


# 2015
dist_15 <- list.files(path = "layers_27700/coverdist_15",
                      all.files = T, pattern = ".tif$") # import folder from 2015


coverdist_15 <- lapply(file.path("layers_27700/coverdist_15", dist_15),
                       raster) # convert into raster
names(coverdist_15) <- dist_15
summary(coverdist_15)


# 2017
dist_17 <- list.files(path = "layers_27700/coverdist_17",
                      all.files = T, pattern = ".tif$") # import folder from 2017


coverdist_17 <- lapply(file.path("layers_27700/coverdist_17", dist_17),
                       raster) # convert into raster
names(coverdist_17) <- dist_17
summary(coverdist_17)


# 2018
dist_18 <- list.files(path = "layers_27700/coverdist_18",
                      all.files = T, pattern = ".tif$") # import folder from 2018


coverdist_18 <- lapply(file.path("layers_27700/coverdist_18", dist_18),
                       raster) # convert into raster
names(coverdist_18) <- dist_18
summary(coverdist_18)


# 2019
dist_19 <- list.files(path = "layers_27700/coverdist_19",
                      all.files = T, pattern = ".tif$") # import folder from 2018

coverdist_19 <- lapply(file.path("layers_27700/coverdist_19", dist_19),
                       raster) # convert into raster
names(coverdist_18) <- dist_19
summary(coverdist_18)

# Put everything together into a list
coverdist_all <- list(coverdist_07, coverdist_15,
                      coverdist_17, coverdist_18, coverdist_19) 


names(coverdist_all) <- c("07", "15", "17", "18", "19")


# Calculating distance for each year range 
for(i in 1:length(coverdist_all)){
  for(j in 1:length(coverdist_all[[i]])){
    coverdist_all[[i]][[j]] <- raster::extract(coverdist_all[[i]][[j]], grid[i])
  }}


# Creating data frame for each time period 
coverdist_07 <- as.data.frame(coverdist_all[[1]])
dim(coverdist_07)
str(coverdist_07)
coverdist_15 <- as.data.frame(coverdist_all[[2]])
coverdist_17 <- as.data.frame(coverdist_all[[3]])
coverdist_18 <- as.data.frame(coverdist_all[[4]])
coverdist_19 <- as.data.frame(coverdist_all[[5]])


#' Now, as we did for land cover proportions, we place distance measurements at 
#' id locations that corresponds to the correct time period. Distances for similar
#' land cover categories (e.g. broadleaved and conifer woodlands) have been merged
#' together, keeping between the two values the minimum distance.
#' 
#' 
names(coverdist_07) # see names to make it easier to index categories
grid07@data$dist_woodland <- apply(X = coverdist_07[, c(5, 13)], # select columns
                                   MARGIN = 1,  # apply this function over rows
                                   FUN = function(x) min(x, na.rm = TRUE)) # minimum function with NA removed; mixed woods,conifer

grid07@data$dist_arable <- coverdist_07[, 2]

grid07@data$dist_grassland <- apply(X = coverdist_07[, c(1,4,9,15,17)],
                                    MARGIN = 1,  
                                    FUN = function(x) min(x, na.rm = TRUE)) # improved, rough, neutral, calcareous, acid 

grid07@data$dist_wetland <- apply(X = coverdist_07[, c(3,6)],
                                  MARGIN = 1,  
                                  FUN = function(x) min(x, na.rm = TRUE)) # fen/marsh/swamp & bog 

grid07@data$dist_heather <- apply(X = coverdist_07[, c(7:8)],
                                  MARGIN = 1,  
                                  FUN = function(x) min(x, na.rm = TRUE))  # heather and heather grassland

grid07@data$dist_montane_inlandrock <- apply(X = coverdist_07[, c(10, 14)],
                                             MARGIN = 1,  
                                             FUN = function(x) min(x, na.rm = TRUE)) #inland rock, montane

grid07@data$dist_freshwater <- coverdist_07[, 16] # rivers

grid07@data$dist_coastalhabitats <- apply(X = coverdist_07[, c(11,12,18,19,21,22)],
                                          MARGIN = 1,  
                                          FUN = function(x) min(x, na.rm = TRUE))  #supralittoral/littoral rock/sediment, saltmarsh, saltwater 

grid07@data$dist_urban <- apply(X = coverdist_07[, c(20,23)],
                                MARGIN = 1,  
                                FUN = function(x) min(x, na.rm = TRUE)) # urban suburban


# 15
names(coverdist_15) # see names to make it easier to index categories
grid15@data$dist_woodland <- apply(X = coverdist_15[, c(5, 13)], 
                                   MARGIN = 1,  
                                   FUN = function(x) min(x, na.rm = TRUE)) 

grid15@data$dist_arable <- coverdist_15[, 2]

grid15@data$dist_grassland <- apply(X = coverdist_15[, c(1,4,9,14)],
                                    MARGIN = 1,  
                                    FUN = function(x) min(x, na.rm = TRUE)) 
grid15@data$dist_wetland <- apply(X = coverdist_15[, c(3,6)],
                                  MARGIN = 1,  
                                  FUN = function(x) min(x, na.rm = TRUE)) 
grid15@data$dist_heather <- apply(X = coverdist_15[, c(7:8)],
                                  MARGIN = 1,  
                                  FUN = function(x) min(x, na.rm = TRUE)) 

grid15@data$dist_montane_inlandrock <- coverdist_15[, 10] # inland

grid15@data$dist_freshwater <- coverdist_15[, 15] # rivers

grid15@data$dist_coastalhabitats <- apply(X = coverdist_15[, c(11,12,16,17,19,20)],
                                          MARGIN = 1,  
                                          FUN = function(x) min(x, na.rm = TRUE)) 

grid15@data$dist_urban <- apply(X = coverdist_15[, c(18,21)],
                                MARGIN = 1,  
                                FUN = function(x) min(x, na.rm = TRUE))


# 2017
names(coverdist_17) # see names to make it easier to index categories
grid17@data$dist_woodland <- apply(X = coverdist_17[, c(5, 13)], 
                                   MARGIN = 1,  
                                   FUN = function(x) min(x, na.rm = TRUE)) 

grid17@data$dist_arable <- coverdist_17[, 2]

grid17@data$dist_grassland <- apply(X = coverdist_17[, c(1,4,9,14)],
                                    MARGIN = 1,  
                                    FUN = function(x) min(x, na.rm = TRUE)) 

grid17@data$dist_wetland <- apply(X = coverdist_17[, c(3,6)],
                                  MARGIN = 1,  
                                  FUN = function(x) min(x, na.rm = TRUE)) 
grid17@data$dist_heather <- apply(X = coverdist_17[, c(7:8)],
                                  MARGIN = 1,  
                                  FUN = function(x) min(x, na.rm = TRUE))  
grid17@data$dist_montane_inlandrock <- coverdist_17[, 10] # inland

grid17@data$dist_freshwater <- coverdist_17[, 15] # rivers

grid17@data$dist_coastalhabitats <- apply(X = coverdist_17[, c(11,12,16,17,19,20)],
                                          MARGIN = 1,  
                                          FUN = function(x) min(x, na.rm = TRUE))

grid17@data$dist_urban <- apply(X = coverdist_17[, c(18,21)],
                                MARGIN = 1,  
                                FUN = function(x) min(x, na.rm = TRUE)) 

# 2018
names(coverdist_18) # see names to make it easier to index categories
grid18@data$dist_woodland <- apply(X = coverdist_18[, c(5, 13)], 
                                   MARGIN = 1,  
                                   FUN = function(x) min(x, na.rm = TRUE)) 

grid18@data$dist_arable <- coverdist_18[, 2]

grid18@data$dist_grassland <- apply(X = coverdist_18[, c(1,4,9,14)],
                                    MARGIN = 1,  
                                    FUN = function(x) min(x, na.rm = TRUE)) 

grid18@data$dist_wetland <- apply(X = coverdist_18[, c(3,6)],
                                  MARGIN = 1,  
                                  FUN = function(x) min(x, na.rm = TRUE)) 

grid18@data$dist_heather <- apply(X = coverdist_18[, c(7:8)],
                                  MARGIN = 1,  
                                  FUN = function(x) min(x, na.rm = TRUE)) 

grid18@data$dist_montane_inlandrock <- coverdist_18[, 10] # inland

grid18@data$dist_freshwater <- coverdist_18[, 15] # rivers

grid18@data$dist_coastalhabitats <- apply(X = coverdist_18[, c(11,12,16,17,19,20)],
                                          MARGIN = 1,  
                                          FUN = function(x) min(x, na.rm = TRUE))  

grid18@data$dist_urban <- apply(X = coverdist_18[, c(18,21)],
                                MARGIN = 1,  
                                FUN = function(x) min(x, na.rm = TRUE)) 


# 19
names(coverdist_19)  # see names to make it easier to index categories
grid19@data$dist_woodland <- apply(X = coverdist_19[, c(5, 13)],
                                 MARGIN = 1,
                                 FUN = function(x) min(x, na.rm = TRUE)) 

grid19@data$dist_arable <- coverdist_19[, 2]

grid19@data$dist_grassland <- apply(X = coverdist_19[, c(1,4,9,14)],
                                  MARGIN = 1,
                                  FUN = function(x) min(x, na.rm = TRUE))  

grid19@data$dist_wetland <- apply(X = coverdist_19[, c(3,6)],
                                MARGIN = 1,
                                FUN = function(x) min(x, na.rm = TRUE)) 

grid19@data$dist_heather <- apply(X = coverdist_19[, c(7:8)],
                                MARGIN = 1,
                                FUN = function(x) min(x, na.rm = TRUE))   

grid19@data$dist_montane_inlandrock <- coverdist_19[, 10]

grid19@data$dist_freshwater <- coverdist_19[, 15]  

grid19@data$dist_coastalhabitats <- apply(X = coverdist_19[, c(11,12,16,17,19,20)],
                                        MARGIN = 1,
                                        FUN = function(x) min(x, na.rm = TRUE)) 
grid19@data$dist_urban <- apply(X = coverdist_19[, c(18,21)],
                              MARGIN = 1,
                              FUN = function(x) min(x, na.rm = TRUE))  

head(grid19@data)


# Cleaning the environment
rm(list = setdiff(ls(), c("grid07", "grid15", "grid17", "grid18", "grid19")))



#' ---
#' 
#' ##### **Rainfall**
#' 
#' Similar to land cover, rainfall and temperature have years associated with them.
#' Hence, the associated year will be used for each grid object. 
#' 
# Importing rainfall files
rainf_files <- list.files(path = "layers_27700/rainfall_grid",
                          all.files = T, pattern = "nc$") # containing records from 2007, 2015, 2017/2019

# Rasterise files
rainfall <- lapply(file.path("layers_27700/rainfall_grid", rainf_files), raster)
mapview(rainfall[[1]])
rainfall[[1]]@crs


# Setting up crs
for(i in 1:length(rainfall)){
  crs(rainfall[[i]]) <- crs(grid)
}
rainfall[[1]]@crs

names(rainfall) <- c("07", "15", "17", "18", "19") # changing labels

# Creating empty list to store rainf records
grid_rainf <- list() 


# Iterating through all rasters
for(i in 1:length(rainfall)){
  
  # Extracting rainfall at observation point
  grid_rainf[[i]] <- raster::extract(rainfall[[i]], grid[i])
}
str(grid_rainf)
names(grid_rainf) <- c("07", "15", "17", "18", "19") # Changing labels


# Placing them into grid objec
grid07@data$rainfall <- grid_rainf[[1]]
grid15@data$rainfall <- grid_rainf[[2]]
grid17@data$rainfall <- grid_rainf[[3]]
grid18@data$rainfall <- grid_rainf[[4]]
grid19@data$rainfall <- grid_rainf[[5]]


#'---
#'
#'#### **Temperature**
#'

# Importing temperature files
temp_files <- list.files(path = "layers_27700/temperature_grid",
                         all.files = T, pattern = "nc$") # containing records from 2007, 2015, 2017/2019

# Rasterise files
temperature <- lapply(file.path("layers_27700/temperature_grid", temp_files), raster)
mapview(temperature[[1]])
temperature[[1]]@crs


# Setting up crs
for(i in 1:length(temperature)){
  crs(temperature[[i]]) <- crs(grid)
}
temperature[[1]]@crs
names(temperature) <- c("07", "15", "17", "18", "19") # changing labels


# Creating empty list to store temp records
grid_temp <- list() 


# Iterating through all rasters
for(i in 1:length(temperature)){
  
  # Extracting temperature at observation point
  grid_temp[[i]] <- raster::extract(temperature[[i]], grid[i])
}
str(grid_temp)
names(grid_temp) <- c("07", "15", "17", "18", "19") # Changing labels


# Placing them into grid objects
grid07@data$temperature <- grid_temp[[1]]
grid15@data$temperature <- grid_temp[[2]]
grid17@data$temperature <- grid_temp[[3]]
grid18@data$temperature <- grid_temp[[4]]
grid19@data$temperature <- grid_temp[[5]]



# Cleaning environmenet
rm(list = setdiff(ls(), c("grid", "grid07", "grid15", "grid17", "grid18", "grid19")))
gc()


# Save workspace
# save.image("mink/grid/minkgrids_workspace.Rdata")