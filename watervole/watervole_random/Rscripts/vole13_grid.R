#' ---
#' title: "grid_vole for water vole habitat suitability model"
#' author: "Eleonora Milea"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#'

#' ---
#'In this script, I create a 1 km grid and extract from each grid cell the covariates 
#' that were used in the GLM for water voles. The grid will be then used 
#' to develop the habitat suitability model. Since we are interested in developing
#' a habitat suitability model for water voles today, the most recent data available
#' for the predictors will be used (e.g. land cover from 2019)
#'
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
                      GDAL1_integer64_policy = T) # loading boundary
  
  
# Creating grid_vole points
grid_vole <- makegrid_vole(boundary, cellsize = 1000) 
grid_vole <- SpatialPoints(grid_vole, proj4string = boundary@proj4string)
mapview(grid_vole)
grid_vole <- grid_vole[boundary,]
grid_vole <- SpatialPointsDataFrame(coords = grid_vole@coords,
                               proj4string = grid_vole@proj4string,
                               data = data.frame(id = 1:length(grid_vole)))
mapview(grid_vole)


#' ---
#' 
#' **Land cover: proportions**
#' 
# Importing land cover
land_19 <- raster("layers_27700/cover/landcover_2019.tif")

# Creating buffer of 100 meters around points
grid_volebuffer100 <- gBuffer(grid_vole, width = 1000, byid = T)


# Intersecting land cover type at buffered points
int.list19 <- list()
for (i in 1:length(grid_volebuffer100)){
  int.list19[[i]] <- raster::intersect(land, grid_volebuffer100[i,])
  print(i)
}

# Creating a data frame to store number of pixels for each land cover type
landcover100 <- data.frame(matrix(NA, nrow = length(grid_volebuffer100),
                                  ncol = 21))
names(landcover100) <- c( "1","2","3","4","5","6", "7", "8", "9", "10",
                          "11", "12", "13", "14", "15", "16", "17", "18",
                          "19", "20", "21")

save.image("C:/Users/elemi.LAPTOP-VIAG61N1/OneDrive - University of Glasgow/dissertation_watervole/watervole/grid/landcover_workspace1000.RData")


# Counting pixel for each land cover type at buffer
for (i in 1:length(grid_volebuffer100)){
  for ( j in 1:length(names(landcover100))) {
    landcover100[i,j] <- table(
      values(int.list19[[i]]))[as.character(names(landcover100)[j])]
    print(i)
  }}


rm(land_19)
rm(int.list19)
gc()

save.image("C:/Users/elemi.LAPTOP-VIAG61N1/OneDrive - University of Glasgow/dissertation_watervole/watervole/grid/landcover_workspace1000pixel.RData")

# Changing labels of columns with category names
names(landcover100) <- c("mixed_woodland",
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

# Replacing NAs with 0
landcover100[is.na(landcover100)] <- 0


# Calculating sum of all pixels for each row 
row_sum <- rowSums(landcover100)


# Calculating proportions using row_sums
props <- landcover100/row_sum
props <- round(props, 3) # rounding up
head(props)


# Replacing NaNs with 0
props <- rapply(props, f = function(x) ifelse(is.nan(x), 0, x),
                how ="replace" ) 
sum(is.na(props[[1]])) # check if it worked
sum(is.na(props[[3]]))

# Adding variables to main grid_vole sp object
grid_vole@data$mixed_woodland1000 <- 0
grid_vole@data$conifer_woodland1000 <- 0
grid_vole@data$arable_horticulture1000 <- 0
grid_vole@data$neutral_grassland1000 <- 0
grid_vole@data$calc_grassland1000 <- 0
grid_vole@data$improv_grassland1000 <- 0
grid_vole@data$acid_grassland1000 <- 0
grid_vole@data$fen_marsh_swamp1000 <- 0
grid_vole@data$heather1000 <- 0
grid_vole@data$heather_grassland1000 <- 0
grid_vole@data$bog1000 <- 0
grid_vole@data$montane_inlandrock1000 <- 0
grid_vole@data$suburban1000 <- 0
grid_vole@data$urban1000 <- 0
grid_vole@data$freshwater1000 <- 0
grid_vole@data$coastal_habitats1000 <- 0


names(props)
grid_vole@data$mixed_woodland1000 <- props[, 1] # broadleaf,conifer; rowSums sum up the two proportions 
grid_vole@data$conifer_woodland1000 <- props[, 2]
grid_vole@data$arable_horticulture1000 <- props[, 3]
grid_vole@data$improv_grassland1000 <- props[, 4] # improved, rough, neutral, calcareous, acid (should I include heather grassland?)
grid_vole@data$neutral_grassland1000 <- props[, 5]
grid_vole@data$calc_grassland1000 <- props[, 6]
grid_vole@data$acid_grassland1000 <- props[, 7]
grid_vole@data$fen_marsh_swamp1000 <- props[, 8]
grid_vole@data$heather1000 <- props[, 9] # heather and heather grassland
grid_vole@data$heather1000 <- props[, 10]
grid_vole@data$bog1000 <- props[, 11]
grid_vole@data$montane_inlandrock1000 <- props[, 12] # montane habitats and inland rock 
grid_vole@data$freshwater1000 <- props[,14]
grid_vole@data$coastal_habitats1000 <- rowSums(props[, c(13,15:19), drop = F]) #supralittoral/littoral rock/sediment, saltmarsh, sea
grid_vole@data$urban1000 <- props[,20] # urban suburban
grid_vole@data$suburban1000 <- props[,21]
head(grid_vole@data)


# Cleaning the environment
rm(list = setdiff(ls(), "grid_vole"))
gc()


#' ---
#' 
#' **Land cover: distances**
#' 
# Calculating distance of grid_vole points from each land cover type 
distance_cover <- list.files(path = "layers_27700/coverdist_19",
                             all.files = T, pattern = ".tif$") # import folder from 2019

coverdist <- lapply(file.path("layers_27700/coverdist_19", distance_cover),
                    raster) # convert into raster
names(coverdist) <- distance_cover
summary(coverdist)


# Calculate distance at grid_vole points 
for(i in 1:length(coverdist)){
  coverdist[[i]] <- raster::extract(coverdist[[i]], grid_vole)
  print(i)
}

save.image("C:/Users/elemi.LAPTOP-VIAG61N1/OneDrive - University of Glasgow/dissertation_watervole/watervole/grid/landcoverdist_workspace1000.RData")

# Convert into df 
coverdist <- data.frame(coverdist)


# Placing distance record in the grid_vole sp object
names(coverdist)  # see names to make it easier to index categories

# Adding variables to vole df
grid_vole@data$dist_mixedwoodland <- NA
grid_vole@data$dist_coniferwoodland <- NA
grid_vole@data$dist_fen_marsh_swamp <- NA
grid_vole@data$dist_bog <- NA
grid_vole@data$dist_urban <- NA
grid_vole@data$dist_suburban <- NA
grid_vole@data$dist_freshwater <- NA
grid_vole@data$dist_acidgrassland <- NA
grid_vole@data$dist_neutralgrassland <- NA
grid_vole@data$dist_improvgrassland <- NA
grid_vole@data$dist_calcgrassland <- NA
grid_vole@data$dist_arable <- NA
grid_vole@data$dist_montane_inlandrock <- NA
grid_vole@data$dist_heather <- NA
grid_vole@data$dist_heathergrassland <- NA
grid_vole@data$dist_coastalhabitats <- NA


grid_vole@data$dist_acidgrassland <- coverdist[, 1]
grid_vole@data$dist_arable <- coverdist[, 2]
grid_vole@data$dist_bog <- coverdist[, 3]
grid_vole@data$dist_calcgrassland <- coverdist[, 4]
grid_vole@data$dist_coniferwoodland <- coverdist[, 5]
grid_vole@data$dist_fen_marsh_swamp <- coverdist[, 6]
grid_vole@data$dist_heather <- coverdist[, 7]
grid_vole@data$dist_heathergrassland <- coverdist[, 8]
grid_vole@data$dist_improvgrassland <- coverdist[, 9]
grid_vole@data$dist_montane_inlandrock <- coverdist[, 10]
grid_vole@data$dist_coastalhabitats <- apply(X = coverdist[, c(11,12,16,17,19,20)],
                                             MARGIN = 1,  
                                             FUN = function(x) min(x, na.rm = TRUE))  #supralittoral/littoral rock/sediment, saltmarsh, saltwater 
grid_vole@data$dist_mixedwoodland <- coverdist[, 13]
grid_vole@data$dist_neutralgrassland <- coverdist[, 14]
grid_vole@data$dist_freshwater <- coverdist[, 15] 
grid_vole@data$dist_suburban <- coverdist[, 18]
grid_vole@data$dist_urban <- coverdist[, 21]
head(grid_vole@data)


# Cleaning the environment
rm(list = setdiff(ls(), "grid_vole"))
gc()


#' ---
#' 
#' **National parks/reserves**
#' 
# Importing reserves boundaries
reserves <- readOGR("layers_27700/reserves.shp",
                    GDAL1_integer64_policy = T)
reserves@proj4string


# Distance calculation between points and nearest reserve
dist_reserves <- gDistance(reserves, grid_vole, byid = T)
grid_vole@data$dist_reserves <- apply(dist_reserves, MARGIN = 1, FUN = min) 
head(grid_vole@data)
grid_vole@data$dist_reserves <- round(grid_vole@data$dist_reserves, 0)
head(grid_vole@data$dist_reserves)


# Importing national parks
nat_parks <- readOGR("layers_27700/national_parks.shp",
                     GDAL1_integer64_policy = T)


# Distance calculation between points and nearest park
dist_parks <- gDistance(nat_parks, grid_vole, byid = T)
grid_vole@data$dist_parks <- apply(dist_parks, MARGIN = 1, FUN = min) 
head(grid_vole@data)
grid_vole@data$dist_parks <- round(grid_vole@data$dist_parks, 0)
head(grid_vole@data$dist_parks)


# Importing ramsars
ramsars <- readOGR("layers_27700/ramsar.shp",
                   GDAL1_integer64_policy = T)


# Distance calculation between points and nearest ramsar
dist_ramsars <- gDistance(ramsars, grid_vole, byid = T)
grid_vole@data$dist_ramsars <- apply(dist_ramsars, MARGIN = 1, FUN = min) 
head(grid_vole@data)
grid_vole@data$dist_ramsars <- round(grid_vole@data$dist_ramsars, 0)
head(grid_vole@data$dist_ramsars)


# Cleaning the environment
rm(list = setdiff(ls(), "grid_vole"))
gc()


#' ---
#' 
#' **Rainfall & Temperature**
#' 
# Import raster
rainfall <- raster("layers_27700/rainfall/rainfall_2020.nc")


# Extracting rainfall at observation point
rainfall <- raster::extract(rainfall, grid_vole)
grid_vole@data$rainfall <- rainfall
head(grid_vole@data)


# Import raster
temperature <- raster("layers_27700/temperature/temp_2020.nc")


# Extracting rainfall at observation point
temperature <- raster::extract(temperature, grid_vole)
grid_vole@data$temperature <- temperature


# Cleaning environment
rm(list = setdiff(ls(), "grid_vole"))
gc()


#' ---
#' 
#' **Slope & elevation**
#'  
# Importing elevation
elev <- raster("layers_27700/dem_50m.tif")


# Extracting elevation at obs points
elev_grid_vole <- raster::extract(elev, grid_vole)
grid_vole@data$elevation <- elev_grid_vole
head(grid_vole$elevation)


# Calculating slope 
slope <- terrain(elev, opt = "slope",
                 neighbors = 8, units = "degrees")


# Extracting slope at obs points
slope_grid_vole <- raster::extract(slope, grid_vole)
grid_vole@data$slope <- slope_grid_vole
head(grid_vole$slope)


# Cleaning the environment
rm(list = setdiff(ls(), "grid_vole"))
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

special_roads <-  subset(roads, fclass == "living_street" | fclass == "track" | 
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
dist_paths <- gDistance(paths, grid_vole, byid = T) 

# put dist roads records at id positions in vole df
grid_vole@data$dist_paths <- apply(dist_paths, MARGIN = 1, FUN = min) 
head(grid_vole$dist_paths)


# Cleaning the environment
rm(paths)
rm(dist_paths)


# Distance calculation between points and nearest path
dist_specialroads <- gDistance(special_roads, grid_vole, byid = T) 

# put dist roads records at id positions in vole df
grid_vole@data$dist_specialroads <- apply(dist_specialroads, MARGIN = 1, FUN = min) 
head(grid_vole$dist_specialroads)


# Cleaning environment
rm(special_roads)
rm(dist_specialroads)


# Distance calculation between points and nearest path
dist_roads <- gDistance(roads, grid_vole, byid = T) 

# put dist roads records at id positions in vole df
grid_vole@data$dist_roads <- apply(dist_roads, MARGIN = 1, FUN = min) 
head(grid_vole$dist_roads)


# Cleaning environment
rm(list = setdiff(ls(), "grid_vole"))
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
dist_water <- gDistance(water, grid_vole, byid = T)


# Placing them in grid_vole data
grid_vole@data$dist_water <- apply(dist_water, MARGIN = 1, FUN = min) # take the matrix and for each row take the minimum distance (2 would have been for each column)
head(grid_vole@data)


# Cleaning environment
rm(list = setdiff(ls(), "grid_vole"))
gc()


#'---
#'
#' **Mink suitability**
#' 
# Importing raster file of mink habitat suitability
mink_suitability19 <- raster("mink/hsm_rasters/mink_suitability19.grd")


# Extract levels of hs of mink at grid_vole points
grid_vole$mink_suitability <- raster::extract(mink_suitability19, grid_vole)
summary(grid_vole$mink_suitability)
head(grid_vole$mink_suitability)


# Cleaning environment
rm(list = setdiff(ls(), "grid_vole"))
gc()



# Save new vole sp object
save(grid_vole, file = file.path("watervole/grid",
                            "grid_vole.RData"))
