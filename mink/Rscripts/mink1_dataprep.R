#' ---
#' title: "Data preparation"
#' author: "Eleonora Milea"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#'

#' In this script I will import and clean the mink data by only considering data
#' from 2000 onwards. 
#' 

# Loading libraries
library(sp)
library(mapview)
library(rgdal)
library(raster)
library(ncdf4)
library(sf)
boundary <- readOGR("layers_27700/boundary_nohebrids.shp",
                    GDAL1_integer64_policy = T) # importing study area
plot(boundary)
boundary@proj4string


# Importing mink data
mink_sp <- readOGR("layers_27700/mink.shp",
                   GDAL1_integer64_policy = T)
str(mink_sp@data) 
head(mink_sp@data)
mink_sp@bbox
mink_sp@proj4string
length(mink_sp@data$Occurrence)


# Removing duplicates
mink_sp <- remove.duplicates(mink_sp)
length(mink_sp@data$Occurrence)


# Taking out NAs from year 
mink_sp <- subset(mink_sp, Year != "NA")
range(mink_sp@data$Year)
length(mink_sp@data$Occurrence)


# Considering only data from 1999
mink_sp <- subset(mink_sp, Year > "1999")
range(mink_sp@data$Year)
length(mink_sp@data$Occurrence)


# Keeping only variables that we need
mink_sp@data <- mink_sp@data[, c("Occurrence", "Year")]
names(mink_sp@data)
length(mink_sp@data$Year)


# Recoding present/absent into 1/0
mink_sp@data$Occurrence[mink_sp@data$Occurrence == "absent"] <- 0
mink_sp@data$Occurrence[mink_sp@data$Occurrence == "present"] <- 1
mink_sp@data$Occurrence <- as.integer(mink_sp@data$Occurrence)
unique(mink_sp@data$Occurrence)
length(mink_sp@data$Occurrence[mink_sp@data$Occurrence == 0])
length(mink_sp@data$Occurrence[mink_sp@data$Occurrence == 1])


# Keeping only presentce records in the sp object (only 1 absence record)
mink_sp <- subset(mink_sp, Occurrence == 1)
length(mink_sp$Occurrence)


# Excluding points outside boundary area
mink_sp <- crop(mink_sp, boundary)
mapview(mink_sp)


# Cleaning environment
rm(boundary)
gc()


# Save file
# save(mink_sp, file = file.path("mink/Robjects",
#                                "mink1_NOpseudoabs.RData"))
