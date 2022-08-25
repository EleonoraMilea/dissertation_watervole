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
#' Scotland boundary shapefile. The latter was done in QGIS hence it is not present
#' in this script
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
library(beepr)
load("mink/Robjects/mink7_elev&slope.RData")


# Importing shapefile
roads <- readOGR("layers_27700/roads.shp",
                 GDAL1_integer64_policy = T)
roads@proj4string
crs(roads) <- mink@proj4string # setting CRS
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


# Split mink df by year
mink_year <- split(mink, mink$Year)


#'---
#'
#'##### **Distance from paths**
#'
# Creating empty list for storing id values
id_year <- list()


# Creating column in main mink sp object to store observations
mink@data$dist_paths <- NA

# Iterating through all observations grouped by year
Sys.time()
for (i in 1:length(mink_year)) {
  
  # Distance calculation between points and nearest path
  dist_paths <- gDistance(paths, mink_year[[i]], byid = T) 
  
  # take "id" from each year
  id_year <- mink_year[[i]]["id"] 
  
  # unlist it to make it a vector
  id_year <- unlist(id_year@data[[1]]) #[[1]] is to access the actual id values
  print(head(id_year))
  
  # put dist from paths records at id positions in mink df
  mink@data$dist_paths[id_year] <- apply(dist_paths, MARGIN = 1, FUN = min) 
  
  # Clear storage
  rm(dist_paths)
  gc()
}
Sys.time()
#' Every time the loop runs the distance file is deleted and the storage is cleared
#' otherwise the code stops working due to memory problems. 
#' 
summary(mink@data$dist_paths)
mink@data$dist_paths <- round(mink@data$dist_paths, 0)
head(mink@data$dist_paths)
rm(paths)
gc()



#'---
#'
#'##### **Calculating distance from special roads**
#'
# Creating empty list for id records
id_year <- list()


# Creating column in mink df to store observations
mink@data$dist_specialroads <- NA

Sys.time()
# Iterating through all mink observtions grouped by year
for (i in 1:length(mink_year)) {
  
  # Distance calculation between points and nearest road
  dist_specialroads <- gDistance(special_roads, mink_year[[i]], byid = T) 
  
  # take "id" from each year
  id_year <- mink_year[[i]]["id"] 
  
  # unlist it to make it a vector
  id_year <- unlist(id_year@data[[1]]) #[[1]] is to access the actual id values
  print(head(id_year))
  
  # put dist from special roads records at id positions in mink df
  mink@data$dist_specialroads[id_year] <- apply(dist_specialroads, MARGIN = 1, FUN = min)
  
  
  # Clear storage
  rm(dist_specialroads)
  gc()
}
Sys.time()
summary(mink@data$dist_specialroads)
mink@data$dist_specialroads <- round(mink@data$dist_specialroads, 0)
head(mink@data$dist_specialroads)
rm(special_roads)
gc()


#'---
#'
#'##### **Calculating distance from roads**
#'
# Creating empty list
id_year <- list()


# Creating column in mink df to store observations
mink@data$dist_roads <- NA

Sys.time()
# Iterating through the mink observations divided by year
for (i in 1:length(mink_year)) {
  
  # Distance calculation between points and nearest road
  dist_roads <- gDistance(roads, mink_year[[i]], byid = T) 
  
  # take "id" from each year
  id_year <- mink_year[[i]]["id"] 
  
  # unlist it to make it a vector
  id_year <- unlist(id_year@data[[1]]) #[[1]] is to access the actual id values
  print(head(id_year))
  
  # put dist roads records at id positions in mink df
  mink@data$dist_roads[id_year] <- apply(dist_roads, MARGIN = 1, FUN = min) 
  
  # Clear storage
  rm(dist_roads)
  gc()
}
summary(mink@data$dist_roads)
mink@data$dist_roads <- round(mink@data$dist_roads, 0)
head(mink@data$dist_roads)


# Cleaning the environment
rm(list = setdiff(ls(), "mink"))
gc()


# Save new mink sp object
# save(mink, file = file.path("mink/Robjects",
#                             "mink8_roads.RData"))
# 
