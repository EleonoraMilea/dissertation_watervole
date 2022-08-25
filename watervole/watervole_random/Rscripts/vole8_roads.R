#' ---
#' title: "Distance from roads"
#' author: "Eleonora Milea"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#'

#' ---
#' 
#' In this script we will extract distance measurements of roads from observation
#' points. 
#' 
#' Roads shape file can be downloaded from OpenStreetMap and cropped using 
#' Here it was done in QGIS, saved as a shape file and imported in R. 
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
load("watervole/watervole_random/Robjects/vole_elevaspectslope.RData")
boundary <- readOGR("layers_27700/boundary_nohebrids.shp",
                    GDAL1_integer64_policy = T)
boundary@proj4string


# Importing shapefile
roads <- readOGR("layers_27700/roads.shp",
                 GDAL1_integer64_policy = T)
roads@proj4string
crs(roads) <- boundary@proj4string # setting CRS
roads@proj4string
unique(roads@data$fclass) # checking categories


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


# Split vole df by year
vole_year <- split(vole_random, vole_random$Year)


#'---
#'
#'##### **Distance from paths**
#'
# Creating empty list for storing id values
id_year <- list()


# Creating column in main vole sp object to store observations
vole_random@data$dist_paths <- NA


Sys.time()
# Iterating through all observations grouped by year
for (i in 1:length(vole_year)) {
  
  # Distance calculation between points and nearest path
  dist_paths <- gDistance(paths, vole_year[[i]], byid = T) 
  
  # take "id" from each year
  id_year <- vole_year[[i]]["id"] 
  
  # unlist it to make it a vector
  id_year <- unlist(id_year@data[[1]]) #[[1]] is to access the actual id values
  print(head(id_year))
  
  # put dist from paths records at id positions in vole df
  vole_random@data$dist_paths[id_year] <- apply(dist_paths, MARGIN = 1, FUN = min) 
  
  # Clear storage
  rm(dist_paths)
  gc()
}
#' Every time the loop runs the distance file is deleted and the storage is cleared
#' otherwise the code stops working due to memory problems. 
#' 
summary(vole_random@data$dist_paths)
vole_random@data$dist_paths <- round(vole_random@data$dist_paths, 3)
head(vole_random@data$dist_paths)
rm(paths)
gc()


#'---
#'
#'##### **Calculating distance from special roads**
#'
# Creating empty list for id records
id_year <- list()


# Creating column in vole df to store observations
vole_random@data$dist_specialroads <- NA

Sys.time()
# Iterating through all vole observtions grouped by year
for (i in 1:length(vole_year)) {
  
  # Distance calculation between points and nearest road
  dist_specialroads <- gDistance(special_roads, vole_year[[i]], byid = T) 
  
  # take "id" from each year
  id_year <- vole_year[[i]]["id"] 
  
  # unlist it to make it a vector
  id_year <- unlist(id_year@data[[1]]) #[[1]] is to access the actual id values
  print(head(id_year))
  
  # put dist from special roads records at id positions in vole df
  vole_random@data$dist_specialroads[id_year] <- apply(dist_specialroads, MARGIN = 1, FUN = min)
  
  
  # Clear storage
  rm(dist_specialroads)
  gc()
}
summary(vole_random@data$dist_specialroads)
vole_random@data$dist_specialroads <- round(vole_random@data$dist_specialroads, 3)
head(vole_random@data$dist_specialroads)
rm(special_roads)
gc()


#'---
#'
#'##### **Calculating distance from roads**
#'
# Creating empty list
id_year <- list()


# Creating column in vole df to store observations
vole_random@data$dist_roads <- NA

Sys.time()
# Iterating through the vole observations divided by year
for (i in 1:length(vole_year)) {
  
  # Distance calculation between points and nearest road
  dist_roads <- gDistance(roads, vole_year[[i]], byid = T) 
  
  # take "id" from each year
  id_year <- vole_year[[i]]["id"] 
  
  # unlist it to make it a vector
  id_year <- unlist(id_year@data[[1]]) #[[1]] is to access the actual id values
  print(head(id_year))
  
  # put dist roads records at id positions in vole df
  vole_random@data$dist_roads[id_year] <- apply(dist_roads, MARGIN = 1, FUN = min) 
  
  # Clear storage
  rm(dist_roads)
  gc()
}
summary(vole_random@data$dist_roads)
vole_random@data$dist_roads <- round(vole_random@data$dist_roads, 3)
head(vole_random@data$dist_roads)


# Cleaning the environment
rm(list = setdiff(ls(), "vole_random"))
gc()


# Save new vole sp object
save(vole_random, file = file.path("watervole/watervole_random/Robjects",
                            "vole8_roads.RData"))
