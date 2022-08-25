#' ---
#' title: "Distance from national parks, reserves and ramsars"
#' author: "Eleonora Milea"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#'

#' ---
#' 
#' In this script we will extract the distance of observation points from each 
#' national parks, reserves and ramsars.
#' 
#' Shape files of these objects can be downloaded from the following website:
#' - Ramsars: https://data.gov.uk/dataset/083883b6-988f-4b3a-b957-51351371b26d/wetland-of-international-importance-scotland
#' 
#' - Reserves: https://scottishwildlifetrust.org.uk/our-work/our-evidence-base/our-data/
#' 
#' - Cairngorms National Park: https://data.gov.uk/dataset/8a00dbd7-e8f2-40e0-bcba-da2067d1e386/cairngorms-national-park-designated-boundary
#' 
#' - Loch Lomond National Park: https://data.gov.uk/dataset/6f63d73d-c45d-4947-8ad0-2d6f52b200ff/loch-lomond-and-the-trossachs-national-park-designated-boundary
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
load("watervole/watervole_random/Robjects/vole4_landcoverdist.RData")


# Let's first start by calculating distance of points from reserves
# Importing reserves boundaries
reserves <- readOGR("layers_27700/reserves.shp",
                    GDAL1_integer64_policy = T)
# reserves <- readOGR("path for reserve shapefile",
#                     GDAL1_integer64_policy = T)
mapview(reserves)
reserves@proj4string


# Distance calculation between points and nearest reserve
dist_reserves <- gDistance(reserves, vole_random, byid = T)


# Transfer minimum distance to vole df
vole_random@data$dist_reserves <- apply(dist_reserves, MARGIN = 1, FUN = min) 
head(vole_random@data)
vole_random@data$dist_reserves <- round(vole_random@data$dist_reserves, 3)
head(vole_random@data$dist_reserves)


# Now, we do the same with national parks
# Importing national parks
nat_parks <- readOGR("layers_27700/national_parks.shp",
                     GDAL1_integer64_policy = T)
# nat_parks <- readOGR("path for national parks shapefile",
#                     GDAL1_integer64_policy = T)
nat_parks@proj4string
mapview(nat_parks)


# Distance calculation between points and nearest park
dist_parks <- gDistance(nat_parks, vole_random, byid = T)


# Transfer minimum distance to vole_random df
vole_random@data$dist_parks <- apply(dist_parks, MARGIN = 1, FUN = min) 
head(vole_random@data)
vole_random@data$dist_parks <- round(vole_random@data$dist_parks, 0)
head(vole_random@data$dist_parks)


# Finally, we calculate distance from ramsars
# Importing ramsars
ramsars <- readOGR("layers_27700/ramsar.shp",
                   GDAL1_integer64_policy = T)
# ramsars <- readOGR("path for ramsars shapefile",
#                    GDAL1_integer64_policy = T)
# mapview(ramsars) 
ramsars@proj4string


# Distance calculation between points and nearest ramsar
dist_ramsars <- gDistance(ramsars, vole_random, byid = T)


# Transfer minimum distance to vole_random df
vole_random@data$dist_ramsars <- apply(dist_ramsars, MARGIN = 1, FUN = min) 
head(vole_random@data)
vole_random@data$dist_ramsars <- round(vole_random@data$dist_ramsars, 3)
head(vole_random@data$dist_ramsars)
beep(3)


# Cleaning the environment 
rm(list = setdiff(ls(), "vole_random"))
gc()


# Saving new vole sp object with new distances 
save(vole_random, file = file.path("watervole/watervole_random/Robjects",
                            "vole5_parks_reserves_ramsars.RData"))
