#' ---
#' title: "Temperature & Rainfall"
#' author: "Eleonora Milea"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#'

#' ---
#' 
#' In this script we will extract temperature and rainfall records at the
#' observation points. 
#' 
#' Temperature and rainfall records from 2000 to 2020 were taken from:
#' https://catalogue.ceda.ac.uk/uuid/786b3ce6be54468496a3e11ce2f2669c
#' 
#' mink observations will be grouped by year to extract the rainfall and
#' temperature record from that year. Since data of rainfall and temperature for 
#' 2021 and 2022 are missing, mink observations from 2020 to 2022 will be grouped 
#' together, and variables will be assumed to remain unchanged.
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
load("mink/Robjects/mink5_parks_reserves_ramsars.RData")


#' ---
#' ##### **Rainfall**
#' 
# Importing rainfall files
rainf_files <- list.files(path = "layers_27700/rainfall",
                          all.files = T, pattern = "nc$") # accessing files

# Rasterise files
rainfall <- lapply(file.path("layers_27700/rainfall", rainf_files), raster) 
mapview(rainfall[[1]])
rainfall[[1]]@crs


# Setting up crs
for(i in 1:length(rainfall)){
  crs(rainfall[[i]]) <- crs(mink)
}
rainfall[[1]]@crs


# Subsetting mink observations per year
mink_year <- split(mink, mink$Year)
mink_year[["2020"]] <- c(mink_year[["2020"]]
                         + mink_year[["2021"]]
                         + mink_year[["2022"]]) # combining 2020-2021-2022 records
mink_year["2020"] <- unlist(mink_year[["2020"]])
mink_year[["2021"]] <- NULL
mink_year[["2022"]] <- NULL


# Creating empty list to store rainf records
mink_rainf <- list() 


# Iterating through all rasters
for(i in 1:length(rainfall)){
  
  # Extracting rainfall at observation point
  mink_rainf[[i]] <- raster::extract(rainfall[[i]], mink_year[[i]])
}
str(mink_rainf)
names(mink_rainf) <- seq(2000, 2020) # Changing labels


# Creating empty column to store rainf records in the main mink sp object
mink$rainfall <- NA


# Placing rainfall records to correct location in df corresponding to each different year
for(i in 1:length(mink_year)){
  
  # take "id" from each year
  id <- mink_year[[i]]["id"] 
  
  # unlist it to make it a vector
  id <- unlist(id@data[[1]]) #[[1]] is to access the actual id values
  
  # put rainf records at id positions in mink df
  mink$rainfall[id] <- mink_rainf[[i]] 
} 
#' NOTES: every time the loop runs, the id value is overwritten, selecting those from each
#' different year range. E.g. The last id vector found in the environment represent
#' the id values from 2020 (all the others are not saved because overwritten 
#' every time the loop runs)
head(mink$rainfall)


# Cleaning environment
rm(mink_rainf)
rm(rainf_files)
rm(rainfall)
rm(id)
gc()


#'---
#'
#'#### **Temperature**
#'

# Importing temperature
temp_files <- list.files(path = "layers_27700/temperature",
                         all.files = T, pattern = "nc$") 

# Rasterise files
temperature <- lapply(file.path("layers_27700/temperature", temp_files), raster) 
temperature[[1]]@crs


# Setting up crs
for(i in 1:length(temperature)){
  crs(temperature[[i]]) <- crs(mink)
}
temperature[[1]]@crs


# Creating eempty list to store values
mink_temp <- list()


for(i in 1:length(temperature)){
  mink_temp[[i]] <- raster::extract(temperature[[i]], mink_year[[i]])
}
str(mink_temp)
names(mink_temp) <- seq(2000, 2020)


# Transferring obs to mink df
mink$temperature <- NA # empty column to store temp records


# Placing rainfall records to correct location in df corresponding to each different year
for(i in 1:length(mink_year)){
  
  # take id from each year
  id <- mink_year[[i]]["id"]
  
  # unlist it to make it a vector
  id <- unlist(id@data[[1]]) #[[1]] is to access the actual id values
  
  # put temp records at id positions in mink df
  mink$temperature[id] <- mink_temp[[i]] 
}
head(mink$temperature)


# Cleaning environment
rm(list = setdiff(ls(), "mink"))
gc()


# Saving new mink sp object with new variables
# save(mink, file = file.path("mink/Robjects",
#                             "mink6_rain&temp.RData"))
