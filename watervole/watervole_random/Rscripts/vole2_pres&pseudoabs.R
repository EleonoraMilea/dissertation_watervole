#' ---
#' title: "Pseudo-absence points"
#' author: "Eleonora Milea"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#'

#' In this script we generate random points to create pseudo-absences records 
#' of vole and then we bind this to the main vole sp object. The final presence:absence
#' ratio will be 1:5. However, our data also include true absences, hence to 
#' have a final 1:5 (presence:pseudo-absence/true-absence ratio we need to subtract
#' to the generated pseudo-absence the number of true absence so that when we 
#' bind the pseudo absence with the main vole sp object we will have a 1:5 ratio


# Loading libraries and objects
library(sp)
library(mapview)
library(rgdal)
library(raster)
library(ncdf4)
library(sf)
library(rgeos)
library(lme4)
library(pROC)
load("watervole/watervole_random/Robjects/vole1_NOpseudoabs.RData")
boundary <- readOGR("layers_27700/boundary_nohebrids.shp",
                    GDAL1_integer64_policy = T)
plot(boundary)
boundary@proj4string


# Creating pseudo-absence points
vole_absence <- spsample(boundary,
                         length(vole_sp@data$Occurrence[vole_sp@data$Occurrence == 1])*5,
                         type = "random")


# Converting it into df
vole_absence <- SpatialPointsDataFrame(coords = vole_absence@coords,
                                       data = data.frame(
                                         Occurrence = rep(0, length(vole_absence)),
                                         Year = rep(vole_sp$Year[vole_sp$Occurrence == 1], 5)), # this ensures to create a similar sampling effort across years becaus eit repeats the year for each obs 5 times
                                       proj4string = vole_sp@proj4string)
head(vole_absence@data)


# Binding main vole sp object with pseudo-absence sp object
vole_random <- rbind(vole_sp, vole_absence)
length(vole)
  
  
# Plotting
mapview(vole_random, axes = T,
        xlab = "Longitude",
        ylab = "Latitude",
        zcol = "Occurrence")


# Creating id column 
vole_random$id <- seq(1, nrow(vole_random))
summary(vole_random)



# Cleaning environment
rm(list = setdiff(ls(), "vole_random"))
gc()


# Saving file
# save(vole_random, file = file.path("watervole/watervole_random/Robjects",
#                             "vole2_pres&pseudoabs.RData"))