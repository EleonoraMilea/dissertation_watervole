#' ---
#' title: "Predictions"
#' author: "Eleonora Milea"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#'
#' 
#' In this script we will make predictions using the different grid objects 
#' created in the "mink11_grid" R script. Predictions are made for 5 different 
#' time intervals (2007, 2015, 2017, 2018, 2019, 2020)
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
# load("mink/modelworkspace.RData")
# load("mink/Robjects/mink_grid.RData")


#'
#'---
#'
#' #### **Grid 2007**
#' 
# Deleting NAs (points in the sea)
grid2_07 <- grid07
grid2_07 <- subset(grid2_07, temperature != "NA")
sum(is.na(grid2_07@data)) # check if it worked
summary(grid2_07@data)
grid2_07 <- subset(grid2_07, dist_roads != "NA")
grid2_07 <- subset(grid2_07, urban > -1)
sum(is.na(grid2_07@data)) # check if it worked
summary(grid2_07)


# Predicting values using best model 
grid2_07$prediction_mink <- predict(model_best, grid2_07@data, type = "response")
summary(grid2_07$prediction_mink)
sum(is.na(grid2_07$prediction_mink))


# Transform into raster
predmink_raster07 <- rasterFromXYZ(xyz = data.frame(x = grid2_07@coords[, 1],
                                                    y = grid2_07@coords[, 2],
                                                    z = grid2_07$prediction_mink),
                                   res = c(1000, 1000), crs = crs(grid2_07))
# mapview(predmink_raster07) + # mapview(mink2, zcol = "Occurrence")


# save it as raster
writeRaster(predmink_raster07,
            filename = file.path("mink/hsm_rasters500", "mink_suitability07"),
            overwrite = T)



#'
#'---
#'
#' #### **Grid 2015**
#' 
# Deleting NAs (points in the sea)
grid2_15 <- grid15
grid2_15 <- subset(grid2_15, temperature != "NA")
sum(is.na(grid2_15@data)) # check if it worked
summary(grid2_15@data)
grid2_15 <- subset(grid2_15, dist_roads != "NA")
grid2_15 <- subset(grid2_15, urban > -1)
sum(is.na(grid2_15@data)) # check if it worked
summary(grid2_15)


# Predicting values using best model 
grid2_15$prediction_mink <- predict(model_best, grid2_15@data, type = "response")
summary(grid2_15$prediction_mink)
sum(is.na(grid2_15$prediction_mink))


# Transform into raster
predmink_raster15 <- rasterFromXYZ(xyz = data.frame(x = grid2_15@coords[, 1],
                                                    y = grid2_15@coords[, 2],
                                                    z = grid2_15$prediction_mink),
                                   res = c(1000, 1000), crs = crs(grid2_15))
# mapview(predmink_raster15) + # mapview(mink2, zcol = "Occurrence") 


# save it as raster
writeRaster(predmink_raster15,
            filename = file.path("mink/hsm_rasters500", "mink_suitability15"),
            overwrite = T)


#'
#'---
#'
#' #### **Grid 2017**
#' 
# Deleting NAs (points in the sea)
grid2_17 <- grid17
grid2_17 <- subset(grid2_17, temperature != "NA")
sum(is.na(grid2_17@data)) # check if it worked
summary(grid2_17@data)
grid2_17 <- subset(grid2_17, dist_roads != "NA")
grid2_17 <- subset(grid2_17, urban > -1)
sum(is.na(grid2_17@data)) # check if it worked
summary(grid2_17)


# Predicting values using best model 
grid2_17$prediction_mink <- predict(model_best, grid2_17@data, type = "response")
summary(grid2_17$prediction_mink)
sum(is.na(grid2_17$prediction_mink))


# Transform into raster
predmink_raster17 <- rasterFromXYZ(xyz = data.frame(x = grid2_17@coords[, 1],
                                                    y = grid2_17@coords[, 2],
                                                    z = grid2_17$prediction_mink),
                                   res = c(1000, 1000), crs = crs(grid2_17))
# mapview(predmink_raster17) + # mapview(mink2, zcol = "Occurrence") 


# save it as raster
writeRaster(predmink_raster17,
            filename = file.path("mink/hsm_rasters500", "mink_suitability17"),
            overwrite = T)


#'
#'---
#'
#' #### **Grid 2018**
#' 
# Deleting NAs (points in the sea)
grid2_18 <- grid18
grid2_18 <- subset(grid2_18, temperature != "NA")
sum(is.na(grid2_18@data)) # check if it worked
summary(grid2_18@data)
grid2_18 <- subset(grid2_18, dist_roads != "NA")
grid2_18 <- subset(grid2_18, urban > -1)
sum(is.na(grid2_18@data)) # check if it worked
summary(grid2_18)


# Predicting values using best model 
grid2_18$prediction_mink <- predict(model_best, grid2_18@data, type = "response")
summary(grid2_18$prediction_mink)
sum(is.na(grid2_18$prediction_mink))


# Transform into raster
predmink_raster18 <- rasterFromXYZ(xyz = data.frame(x = grid2_18@coords[, 1],
                                                    y = grid2_18@coords[, 2],
                                                    z = grid2_18$prediction_mink),
                                   res = c(1000, 1000), crs = crs(grid2_18))
# mapview(predmink_raster18) + # mapview(mink2, zcol = "Occurrence") 


# save it as raster
writeRaster(predmink_raster18,
            filename = file.path("mink/hsm_rasters500", "mink_suitability18"),
            overwrite = T)


#'
#'---
#'
#' #### **Grid 2019**
#' 
# Deleting NAs (points in the sea)
grid2_19 <- grid19
grid2_19 <- subset(grid2_19, temperature != "NA")
sum(is.na(grid2_19@data)) # check if it worked
summary(grid2_19@data)
grid2_19 <- subset(grid2_19, dist_roads != "NA")
grid2_19 <- subset(grid2_19, urban > -1)
sum(is.na(grid2_19@data)) # check if it worked
summary(grid2_19)


# Predicting values using best model 
grid2_19$prediction_mink <- predict(model_best, grid2_19@data, type = "response")
summary(grid2_19$prediction_mink)
sum(is.na(grid2_19$prediction_mink))


# Transform into raster
predmink_raster19 <- rasterFromXYZ(xyz = data.frame(x = grid2_19@coords[, 1],
                                                    y = grid2_19@coords[, 2],
                                                    z = grid2_19$prediction_mink),
                                   res = c(1000, 1000), crs = crs(grid2_19))
# mapview(predmink_raster19) + # mapview(mink2, zcol = "Occurrence") 


# save it as raster
writeRaster(predmink_raster19,
            filename = file.path("mink/hsm_rasters500", "mink_suitability19"),
            overwrite = T)

