#' ---
#' title: "Predictions"
#' author: "Eleonora Milea"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#'
#' 
#' **Predictions**
#' 

# Loading libraries and objects
library(sp)
library(mapview)
library(rgdal)
library(raster)
library(ncdf4)
library(sf)
library(rgeos)
library(pROC)


# Deleting NAs (points in the sea)
grid_vole2 <- grid_vole
sum(is.na(grid_vole2@data))
grid_vole2 <- subset(grid_vole2, temperature != "NA")
grid_vole2 <- subset(grid_vole2, slope != "NA")
sum(is.na(grid_vole2@data)) # check if it worked
summary(grid_vole2)
grid_vole2 <- subset(grid_vole2, urban > -1)
sum(is.na(grid_vole2@data)) # check if it worked
summary(grid_vole2)
grid_vole2 <- subset(grid_vole2, dist_roads != "NA")
sum(is.na(grid_vole2@data)) # check if it worked
summary(grid_vole2)


# Predicting values using best model 
grid_vole2$prediction_vole <- predict(model_best, grid_vole2@data,
                                      type = "response")
summary(grid_vole2$prediction_vole)
sum(is.na(grid_vole2$prediction_vole))


# Transform into raster
volesuit_random <- rasterFromXYZ(xyz = data.frame(x = grid_vole2@coords[, 1],
                                                   y = grid_vole2@coords[, 2],
                                                   z = grid_vole2$prediction_vole),
                                  res = c(1000, 1000), crs = crs(grid_vole2))
mapview(volesuit_random) +
  mapview(vole_random2, zcol = "Occurrence")


# save it as raster
writeRaster(volesuit_random,
            filename = file.path("watervole/watervole_random/HSM",
                                 "vole_suitabilitydistpatch"),
            overwrite = T)