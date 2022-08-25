#' ---
#' title: "Distance from waterways"
#' author: "Eleonora Milea"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#'

#' ---
#' 
#' In this script we will extract the distance from waterways to each observation
#' point. The shapefile of waterways can be downloaded from OpenStreetMap.
#-
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
load("watervole/watervole_random/Robjects/vole8_roads.RData")


# Importing waterways
water <- readOGR("layers_27700/waterways.shp",
                 GDAL1_integer64_policy = T)
water@proj4string
unique(water@data$layer)


# Subsetting waterways
rivers <- subset(water, layer == "rivers")
unique(rivers$layer)
misc_waterways <- subset(water, layer == "canals" | layer == "misc_water")
unique(misc_waterways$layer)
rm(water)
gc()


# Split vole df by year
vole_year <- split(vole_random, vole_random$Year)


# Creating empty dataframe to store all distances
dist_waterALL <- data.frame(id = 1:length(vole_random))
dist_waterALL$dist_rivers <- NA
dist_waterALL$dist_miscwaterways <- NA


#' This df will be then used to extract for each row the minimum distance, so
#' that at the end we will only have one variable for all types of waterways in
#' the main vole sp object.
#' 

#'---
#'
#'##### **Calculating distance from rivers**
#'
# Creating empty list to store id values
id_year <- list()

Sys.time()
# For loop to iterate through all observations divided by year
for (i in 1:length(vole_year)) {
  
  # Distance calculation between points and nearest river
  dist_rivers <- gDistance(rivers, vole_year[[i]], byid = T) 
  
  # take "id" from each year
  id_year <- vole_year[[i]]["id"] 
  
  # unlist it to make it a vector
  id_year <- unlist(id_year@data[[1]]) #[[1]] is to access the actual id values
  print(head(id_year))
  
  # put dist from rivers records at id positions in vole df
  dist_waterALL$dist_rivers[id_year] <- apply(dist_rivers, MARGIN = 1, FUN = min) 
  
  # Clear environment
  rm(dist_rivers)
  gc()
}
summary(dist_waterALL$dist_rivers)
rm(rivers)
gc()


#'##### **Calculating distance from misc_waterways**
#'
# Creating empty list to store id values
id_year <- list()

Sys.time()
# For loop to iterate through all observations divided by year
for (i in 1:length(vole_year)) {
  
  # Distance calculation between points and nearest misc waterway
  dist_miscwaterways <- gDistance(misc_waterways, vole_year[[i]], byid = T) 
  
  # take "id" from each year
  id_year <- vole_year[[i]]["id"] 
  
  # unlist it to make it a vector
  id_year <- unlist(id_year@data[[1]]) #[[1]] is to access the actual id values
  print(head(id_year))
  
  # put dist records at id positions in vole df
  dist_waterALL$dist_miscwaterways[id_year] <- apply(dist_miscwaterways, MARGIN = 1, FUN = min) 
  
  # Clear storage
  rm(dist_miscwaterways)
  gc()
}
summary(dist_waterALL$dist_miscwaterways)
rm(misc_waterways)
gc()


# Place minimum dist records in main vole sp object
vole_random@data$dist_water <- apply(X = dist_waterALL[, 2:3], # select columns
                                     MARGIN = 1,  # apply this function over rows (which corresponds to id)
                                     FUN = function(x) min(x,  na.rm = TRUE)) # minimum distance
beep()
summary(vole_random@data$dist_water)
head(vole_random@data$dist_water)


# Cleaning environment
rm(list = setdiff(ls(), "vole_random"))
gc()


# Saving new vole sp object
save(vole_random, file = file.path("watervole/watervole_random/Robjects",
                            "vole9_waterways.RData"))