#' ---
#' title: "Elevation, slope and aspect"
#' author: "Eleonora Milea"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#'

#' ---
#' 
#' In this script we will extract elevation records at each observation records. 
#' Slope is then calculated using the elevation raster and used 
#' to extract these variables at the observation points. 
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
load("mink/Robjects/mink6_rain&temp.RData")


# Importing elevation
elev <- raster("layers_27700/dem_50m.tif")
elev@crs


# Extracting elevation at obs points and placing it in main mink sp object
mink@data$elevation  <- raster::extract(elev, mink)
head(mink$elevation)
summary(mink$elevation)


# Now we calculate slope using the elevation raster
slope <- raster::terrain(elev, opt = "slope",
                         neighbors = 8, unit = "degrees")
slope@crs
slope@crs <- elev@crs
slope@crs


# Extracting slope at obs points and placing it in main mink sp object
mink@data$slope <- raster::extract(slope, mink)
mink@data$slope <- round(mink@data$slope, 3)
summary(mink$slope)



# Cleaning the environment
rm(list = setdiff(ls(), "mink"))
gc()


# Saving file
# save(mink, file = file.path("mink/Robjects",
#                             "mink7_elev&slope.RData"))
