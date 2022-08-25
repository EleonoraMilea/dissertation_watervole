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
#' Vole observations will be grouped by year to extract the rainfall and
#' temperature record from that year. Since data of rainfall and temperature for 
#' 2021 and 2022 are missing, vole observations from 2020 to 2022 will be grouped 
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
load("watervole/watervole_random/Robjects/vole5_parks_reserves_ramsars.RData")


#' ---
#' ##### **Rainfall**
#' 
# Importing rainfall files
rainf_files <- list.files(path = "layers_27700/rainfall",
                          all.files = T, pattern = "nc$") 

# Rasterise files
rainfall <- lapply(file.path("layers_27700/rainfall", rainf_files), raster) 


# Subsetting vole observations per year
vole_year <- split(vole_random, vole_random$Year)
vole_year[["2020"]] <- c(vole_year[["2020"]]
                         + vole_year[["2021"]]
                         + vole_year[["2022"]]) # combining 2020-2021-2022 records
vole_year["2020"] <- unlist(vole_year[["2020"]])
vole_year[["2021"]] <- NULL
vole_year[["2022"]] <- NULL


# Creating empty list to store rainf records
vole_rainf <- list() 


# Iterating through all rasters
for(i in 1:length(rainfall)){
  
  # Extracting rainfall at observation point
  vole_rainf[[i]] <- raster::extract(rainfall[[i]], vole_year[[i]])
}
str(vole_rainf)
names(vole_rainf) <- seq(2000, 2020) # Changing labels


# Creating empty column to store rainf records in the main vole sp object
vole_random$rainfall <- NA


# Placing rainfall records to correct location in df corresponding to each different year
for(i in 1:length(vole_year)){
  
  # take "id" from each year
  id <- vole_year[[i]]["id"] 
  
  # unlist it to make it a vector
  id <- unlist(id@data[[1]]) #[[1]] is to access the actual id values
  
  # put rainf records at id positions in vole df
  vole_random$rainfall[id] <- vole_rainf[[i]] 
} 
#' NOTES: every time the loop runs, the id value is overwritten, selecting those from each
#' different year range. E.g. The last id vector found in the environment represent
#' the id values from 2020 (all the others are not saved because overwritten 
#' every time the loop runs)
head(vole_random$rainfall)
summary(vole_random$rainfall)


# Cleaning environment
rm(vole_rainf)
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


# Creating eempty list to store values
vole_temp <- list()


for(i in 1:length(temperature)){
  vole_temp[[i]] <- raster::extract(temperature[[i]], vole_year[[i]])
}
str(vole_temp)
names(vole_temp) <- seq(2000, 2020)


# Transferring obs to vole df
vole_random$temperature <- NA # empty column to store temp records


# Placing rainfall records to correct location in df corresponding to each different year
for(i in 1:length(vole_year)){
  
  # take id from each year
  id <- vole_year[[i]]["id"]
  
  # unlist it to make it a vector
  id <- unlist(id@data[[1]]) #[[1]] is to access the actual id values
  
  # put temp records at id positions in vole df
  vole_random$temperature[id] <- vole_temp[[i]] 
}
head(vole_random$temperature)
summary(vole_random$temperature)

# Cleaning environment
rm(list = setdiff(ls(), "vole_random"))
gc()


# Saving new vole sp object with new variables
save(vole_random, file = file.path("watervole/watervole_random/Robjects",
                            "vole6_rain&temp.RData"))
