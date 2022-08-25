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
load("mink/Robjects/mink8_roads.RData")


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


# Split mink df by year
mink_year <- split(mink, mink$Year)


# Creating empty dataframe to store all distances
dist_waterALL <- data.frame(id = 1:length(mink))
dist_waterALL$dist_rivers <- NA
dist_waterALL$dist_miscwaterways <- NA


#' This df will be then used to extract for each row the minimum distance, so
#' that at the end we will only have one variable for all types of waterways in
#' the main mink sp object.
#' 

#'---
#'
#'##### **Calculating distance from rivers**
#'
# Creating empty list to store id values
id_year <- list()

Sys.time()
# For loop to iterate through all observations divided by year
for (i in 1:length(mink_year)) {
  
  # Distance calculation between points and nearest river
  dist_rivers <- gDistance(rivers, mink_year[[i]], byid = T) 
  
  # take "id" from each year
  id_year <- mink_year[[i]]["id"] 
  
  # unlist it to make it a vector
  id_year <- unlist(id_year@data[[1]]) #[[1]] is to access the actual id values
  print(head(id_year))
  
  # put dist from rivers records at id positions in mink df
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
for (i in 1:length(mink_year)) {
  
  # Distance calculation between points and nearest misc waterway
  dist_miscwaterways <- gDistance(misc_waterways, mink_year[[i]], byid = T) 
  
  # take "id" from each year
  id_year <- mink_year[[i]]["id"] 
  
  # unlist it to make it a vector
  id_year <- unlist(id_year@data[[1]]) #[[1]] is to access the actual id values
  print(head(id_year))
  
  # put dist records at id positions in mink df
  dist_waterALL$dist_miscwaterways[id_year] <- apply(dist_miscwaterways, MARGIN = 1, FUN = min) 
  
  # Clear storage
  rm(dist_miscwaterways)
  gc()
}
summary(dist_waterALL$dist_miscwaterways)
rm(misc_waterways)
gc()



# Place minimum dist records in main mink sp object
mink@data$dist_water <- apply(X = dist_waterALL[, 2:3], # select columns
                              MARGIN = 1,  # apply this function over rows (which corresponds to id)
                              FUN = function(x) min(x,  na.rm = TRUE)) # minimum distance
summary(mink@data$dist_water)
mink@data$dist_water <- round(mink@data$dist_water, 0)
head(mink@data$dist_water)


# Cleaning environment
rm(list = setdiff(ls(), "mink"))
gc()


# Saving new mink sp object
# save(mink, file = file.path("mink/Robjects",
#                             "mink9_waterways.RData"))