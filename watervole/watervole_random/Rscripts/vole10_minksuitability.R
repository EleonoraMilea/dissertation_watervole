#' ---
#' title: "Mink Suitability"
#' author: "Eleonora Milea"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#'

#' ---
#' 
#' In this script we will use the mink habitat suitability models created to 
#' extract the level of habitat suitability of minks at the water vole 
#' observations records. Each raster of mink hsm is used to extract levels of mink habitat suitability
#' at pres and abs points of water voles. So we will group water voles observations 
#' by year intervals (e.g. observations from 2000 to 2010) to extract these values 
#' using the hsm closest in time. For instance, water vole obs from 2000 until 2010
#' will be grouped together and levels of mink habitat suitability will be extracted
#' using the habitat suitability from 2007. This was needed as if we were to use
#' only grid predictions from the most recent years, we would have had to use 
#' the hsm based on 2019 predictions only to extract values for water voles values 
#' from previous years. This would have assumed that mink habitat suitability had
#' not changed over the years.
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
load("watervole/vole_random/Robjects/vole8_waterways.RData")


# Importing mink hsm files
hsm_files <- list.files(path = "mink/hsm_rasters",
                          all.files = T, pattern = "grd$") 

# Rasterise files
hsm_rasters <- lapply(file.path("mink/hsm_rasters", hsm_files), raster) 
names(hsm_rasters) <-  c("07", "15", "17", "18", "19")
summary(hsm_rasters)
crs(hsm_rasters[[1]])

for(i in 1:length(hsm_rasters)){
  crs(hsm_rasters[[i]]) <- crs(vole_random)
}
crs(hsm_rasters[[1]])


# Dividing vole observations by year
vole_year <- list()
vole_year[["07"]] <- subset(vole_random,
                            vole_random$Year >= 2000 & vole_random$Year <= 2010)
vole_year[["15"]] <- subset(vole_random,
                            vole_random$Year >= 2011 & vole_random$Year <= 2016)
vole_year[["17"]] <- subset(vole_random, vole_random$Year == 2017)
vole_year[["18"]] <- subset(vole_random, vole_random$Year == 2018)
vole_year[["19"]] <- subset(vole_random, vole_random$Year >= 2019)
summary(vole_year)


# Creating an empty list to store records
mink_suitability <- list()


for(i in 1:length(vole_year)){
  mink_suitability[[i]] <- raster::extract(hsm_rasters[[i]], vole_year[[i]])
}
summary(mink_suitability)
str(mink_suitability)
names(mink_suitability) <- names(vole_year)


# Transferring obs to vole df
vole_random$mink_suitability <- NA # empty column to store temp records


# Placing rainfall records to correct location in df corresponding to each different year
for(i in 1:length(vole_year)){
  
  # take id from each year
  id <- vole_year[[i]]["id"]
  
  # unlist it to make it a vector
  id <- unlist(id@data[[1]]) #[[1]] is to access the actual id values
  
  # put temp records at id positions in vole df
  vole_random$mink_suitability[id] <- mink_suitability[[i]] 
}
head(vole_random$mink_suitability)


# Cleaning environment
rm(list = setdiff(ls(), "vole_random"))
gc()


# Save new vole sp object
save(vole_random, file = file.path("watervole/watervole_random/Robjects",
                            "vole10_minksuitability.RData"))

