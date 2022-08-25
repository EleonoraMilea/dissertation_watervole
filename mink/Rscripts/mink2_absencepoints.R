#' ---
#' title: "Pseudo-absence points"
#' author: "Eleonora Milea"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#'

#' In this script we generate points to create pseudo-absences records 
#' of mink. This pseudo-absence will be generated randomly around the presence 
#' points (5 km buffer). The final presence:pseudoabsence ratio will be 1:5. 
#' The boundary area will be used to ensure that pres points along the coast 
#' won't create random points in the sea. 
#' 


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
load("mink/Robjects/mink1_NOpseudoabs.RData")
# mapview(mink_sp)
boundary <- readOGR("layers_27700/boundary_nohebrids.shp",
                    GDAL1_integer64_policy = T)
boundary@proj4string



# Creating pseudo-absences 
abs_list <- list()
n_random <- 5

Sys.time()
for (i in 1:length(mink_sp)){
  #Creating random points around each buffered presence point 
  abs_list[[i]] <- spsample(x = intersect(boundary, # intersect to ensure that random points are created within the boundary
                                          gBuffer(mink_sp[i,], width = 5000)), # buffer of 5 km
                            n = n_random, # n of points generated per each presence 
                            type = "random",
                            iter = 10)
  
  # Transforming each point into a sp dataframe
  abs_list[[i]] <- SpatialPointsDataFrame(coords =  abs_list[[i]]@coords,
                                          data = data.frame(
                                            Occurrence = rep(0, n_random),
                                            Year = rep(mink_sp[i,]$Year, n_random)), # this ensures that points around each presence point are from the same year
                                          proj4string = mink_sp@proj4string)
  print(i)
}
Sys.time()



# Binding elements of list together 
mink_absence <- do.call("rbind", abs_list)
mink_absence@proj4string
summary(mink_absence)
mapview(mink_absence)


# Binding presence and absence sp objects
mink <- rbind(mink_sp, mink_absence)
length(mink$Occurrence)


# Plotting
mapview(mink, axes = T,
        xlab = "Longitude",
        ylab = "Latitude",
        zcol = "Occurrence")


# Creating id column 
mink$id <- seq(1, nrow(mink))
summary(mink)


# Cleaning environment
rm(list = setdiff(ls(), "mink"))
gc()


# Saving file
save(mink, file = file.path("mink/Robjects",
                            "mink2_pres&pseudoabs.RData"))
