#' ---
#' title: "Data preparation"
#' author: "Eleonora Milea"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#'

#' In this script I will import and clean the water vole data by only keeping 
#' data from 2000 onwards.

# Loading libraries
library(sp)
library(mapview)
library(rgdal)
library(raster)
library(ncdf4)
library(sf)


# Importing vole data
vole_sp <- readOGR("layers_27700/watervole.shp",
                   GDAL1_integer64_policy = T)
str(vole_sp@data) 
head(vole_sp@data)
vole_sp@bbox
vole_sp@proj4string
length(vole_sp@data$Occurrence)


# Removing duplicates
vole_sp <- remove.duplicates(vole_sp)
length(vole_sp@data$Occurrence)


# Taking out NAs from year 
vole_sp <- subset(vole_sp, Year != "NA")
range(vole_sp@data$Year)
length(vole_sp@data$Occurrence)


# Considering only data from 1999
vole_sp <- subset(vole_sp, Year > "1999")
range(vole_sp@data$Year)
length(vole_sp@data$Occurrence)


# Keeping only landcover that we need
vole_sp@data <- vole_sp@data[, c("Occurrence", "Year")]
names(vole_sp@data)
length(vole_sp@data$Year)


# Recoding present/absent into 1/0
vole_sp@data$Occurrence[vole_sp@data$Occurrence == "absent"] <- 0
vole_sp@data$Occurrence[vole_sp@data$Occurrence == "present"] <- 1
vole_sp@data$Occurrence <- as.integer(vole_sp@data$Occurrence)
unique(vole_sp@data$Occurrence)
length(vole_sp@data$Occurrence[vole_sp@data$Occurrence == 0])
length(vole_sp@data$Occurrence[vole_sp@data$Occurrence == 1])


# Save file
# save(vole_sp, file = file.path("watervole/watervole_random/Robjects",
#                                "vole1_NOpseudoabs.RData"))

