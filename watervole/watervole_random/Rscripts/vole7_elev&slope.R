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
load("watervole/watervole_random/Robjects/vole6_rain&temp.RData")


# Importing elevation
elev <- raster("layers_27700/dem_50m.tif")
elev@crs
# mapview(elev)


# Extracting elevation at obs points and placing it in main vole sp object
vole_random@data$elevation  <- raster::extract(elev, vole_random)
head(vole_random$elevation)
summary(vole_random$elevation)


# Now we calculate slope using the elevation raster
slope <- terrain(elev, opt = "slope",
                 neighbors = 8, unit = "degrees")
slope@crs
slope@crs <- elev@crs
slope@crs


# Extracting slope at obs points and placing it in main vole_random sp object
vole_random@data$slope <- raster::extract(slope, vole_random)
vole_random@data$slope <- round(vole_random@data$slope, 3)
head(vole_random$slope)
summary(vole_random$slope)


# Cleaning the environment
rm(list = setdiff(ls(), "vole_random"))
gc()


# Saving file
save(vole_random, file = file.path("watervole/watervole_random/Robjects",
                            "vole7_elev&slope.RData"))
