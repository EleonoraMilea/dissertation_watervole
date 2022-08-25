#' ---
#' title: "Land cover: Distance from points"
#' author: "Eleonora Milea"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#'

#' ---
#' 
#' In this script we will extract the distance of observation points from each 
#' land cover type. The raster distance files calculated in the 
#' "landcover_distance.R" R script will be used. 
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
# load("watervole/watervole_random/Robjects/vole3_landcoverprop.Rdata")

# Importing land cover distance files
# 2007
dist_07 <- list.files(path = "layers_27700/coverdist_07",
                      all.files = T, pattern = ".tif$") # import folder from 2007


coverdist_07 <- lapply(file.path("layers_27700/coverdist_07", dist_07),
                       raster) # convert into raster
names(coverdist_07) <- dist_07
summary(coverdist_07)


# 2015
dist_15 <- list.files(path = "layers_27700/coverdist_15",
                      all.files = T, pattern = ".tif$") # import folder from 2015


coverdist_15 <- lapply(file.path("layers_27700/coverdist_15", dist_15),
                       raster) # convert into raster
names(coverdist_15) <- dist_15
summary(coverdist_15)


# 2017
dist_17 <- list.files(path = "layers_27700/coverdist_17",
                      all.files = T, pattern = ".tif$") # import folder from 2017


coverdist_17 <- lapply(file.path("layers_27700/coverdist_17", dist_17),
                       raster) # convert into raster
names(coverdist_17) <- dist_17
summary(coverdist_17)


# 2018
dist_18 <- list.files(path = "layers_27700/coverdist_18",
                      all.files = T, pattern = ".tif$") # import folder from 2018


coverdist_18 <- lapply(file.path("layers_27700/coverdist_18", dist_18),
                       raster) # convert into raster
names(coverdist_18) <- dist_18
summary(coverdist_18)


# 2019
dist_19 <- list.files(path = "layers_27700/coverdist_19",
                      all.files = T, pattern = ".tif$") # import folder from 2019


coverdist_19 <- lapply(file.path("layers_27700/coverdist_19", dist_19),
                       raster) # convert into raster
names(coverdist_19) <- dist_19
summary(coverdist_19)


# Put everything together into a list
coverdist_all <- list(coverdist_07, coverdist_15, coverdist_17,
                      coverdist_18, coverdist_19) 


names(coverdist_all) <- c("07", "15", "17", "18", "19")


# Subsetting vole data by year
vole_year <- list("07" = subset(vole_random, Year >= 2000 & Year <= 2010),
                  "15" = subset(vole_random, Year >= 2011 & Year <= 2016),
                  "17" = subset(vole_random, Year == 2017),
                  "18" = subset(vole_random, Year == 2018),
                  "19" = subset(vole_random, Year >= 2019))


# Calculating distance for each year range 
for(i in 1:length(coverdist_all)){
  for(j in 1:length(coverdist_all[[i]])){
    coverdist_all[[i]][[j]] <- raster::extract(coverdist_all[[i]][[j]], vole_year[[i]])
  }}


# Creating data frame for each time period 
coverdist_07 <- as.data.frame(coverdist_all[[1]])
dim(coverdist_07)
str(coverdist_07)
coverdist_15 <- as.data.frame(coverdist_all[[2]])
coverdist_17 <- as.data.frame(coverdist_all[[3]])
coverdist_18 <- as.data.frame(coverdist_all[[4]])
coverdist_19 <- as.data.frame(coverdist_all[[5]])


# Adding variables to vole df
vole_random@data$dist_mixedwoodland <- NA
vole_random@data$dist_coniferwoodland <- NA
vole_random@data$dist_fen_marsh_swamp <- NA
vole_random@data$dist_bog <- NA
vole_random@data$dist_urban <- NA
vole_random@data$dist_suburban <- NA
vole_random@data$dist_freshwater <- NA
vole_random@data$dist_acidgrassland <- NA
vole_random@data$dist_neutralgrassland <- NA
vole_random@data$dist_improvgrassland <- NA
vole_random@data$dist_calcgrassland <- NA
vole_random@data$dist_arable <- NA
vole_random@data$dist_montane_inlandrock <- NA
vole_random@data$dist_heather <- NA
vole_random@data$dist_heathergrassland <- NA
vole_random@data$dist_coastalhabitats <- NA


# Taking id for observations from each time period 
id_07 <- vole_year[[1]]@data$id
id_15 <- vole_year[[2]]@data$id
id_17 <- vole_year[[3]]@data$id
id_18 <- vole_year[[4]]@data$id
id_19 <- vole_year[[5]]@data$id


#' Now, as we did for land cover proportions, we place distance measurements at 
#' id locations that corresponds to the correct time period. 
#'

# 2005-2012
names(coverdist_07) # see names to make it easier to index categories
vole_random@data$dist_acidgrassland[id_07] <- coverdist_07[, 1]
vole_random@data$dist_arable[id_07] <- coverdist_07[, 2]
vole_random@data$dist_bog[id_07] <- coverdist_07[, 3]
vole_random@data$dist_calcgrassland[id_07] <- coverdist_07[, 4]
vole_random@data$dist_coniferwoodland[id_07] <- coverdist_07[, 5]
vole_random@data$dist_fen_marsh_swamp[id_07] <- coverdist_07[, 6]
vole_random@data$dist_heather[id_07] <- coverdist_07[, 7]
vole_random@data$dist_heathergrassland[id_07] <- coverdist_07[, 8]
vole_random@data$dist_improvgrassland[id_07] <- coverdist_07[, 9]
vole_random@data$dist_montane_inlandrock[id_07] <- apply(X = coverdist_07[, c(10, 14)],
                                                         MARGIN = 1,  
                                                         FUN = function(x) min(x, na.rm = TRUE)) 
vole_random@data$dist_coastalhabitats[id_07] <- apply(X = coverdist_07[, c(11,12,18,19,21,22)],
                                                      MARGIN = 1,  
                                                      FUN = function(x) min(x, na.rm = TRUE))   
vole_random@data$dist_mixedwoodland[id_07] <- coverdist_07[, 13]
vole_random@data$dist_neutralgrassland[id_07] <- apply(X = coverdist_07[, c(15, 17)],
                                                       MARGIN = 1,  
                                                       FUN = function(x) min(x, na.rm = TRUE))
vole_random@data$dist_freshwater[id_07] <- coverdist_07[, 16] 

vole_random@data$dist_suburban[id_07] <- coverdist_07[, 20]
vole_random@data$dist_urban[id_07] <- coverdist_07[, 23]


# 2013-2016
names(coverdist_15) # see names to make it easier to index categories
vole_random@data$dist_acidgrassland[id_15] <- coverdist_15[, 1]
vole_random@data$dist_arable[id_15] <- coverdist_15[, 2]
vole_random@data$dist_bog[id_15] <- coverdist_15[, 3]
vole_random@data$dist_calcgrassland[id_15] <- coverdist_15[, 4]
vole_random@data$dist_coniferwoodland[id_15] <- coverdist_15[, 5]
vole_random@data$dist_fen_marsh_swamp[id_15] <- coverdist_15[, 6]
vole_random@data$dist_heather[id_15] <- coverdist_15[, 7]
vole_random@data$dist_heathergrassland[id_15] <- coverdist_15[, 8]
vole_random@data$dist_improvgrassland[id_15] <- coverdist_15[, 9]
vole_random@data$dist_montane_inlandrock[id_15] <- coverdist_15[, 10]
vole_random@data$dist_coastalhabitats[id_15] <- apply(X = coverdist_15[, c(11,12,16,17,19,20)],
                                                      MARGIN = 1,  
                                                      FUN = function(x) min(x, na.rm = TRUE))   
vole_random@data$dist_mixedwoodland[id_15] <- coverdist_15[, 13]
vole_random@data$dist_neutralgrassland[id_15] <- coverdist_15[, 14]
vole_random@data$dist_freshwater[id_15] <- coverdist_15[, 15] 
vole_random@data$dist_suburban[id_15] <- coverdist_15[, 18]
vole_random@data$dist_urban[id_15] <- coverdist_15[, 21]


# 2017
names(coverdist_17) # see names to make it easier to index categories
vole_random@data$dist_acidgrassland[id_17] <- coverdist_17[, 1]
vole_random@data$dist_arable[id_17] <- coverdist_17[, 2]
vole_random@data$dist_bog[id_17] <- coverdist_17[, 3]
vole_random@data$dist_calcgrassland[id_17] <- coverdist_17[, 4]
vole_random@data$dist_coniferwoodland[id_17] <- coverdist_17[, 5]
vole_random@data$dist_fen_marsh_swamp[id_17] <- coverdist_17[, 6]
vole_random@data$dist_heather[id_17] <- coverdist_17[, 7]
vole_random@data$dist_heathergrassland[id_17] <- coverdist_17[, 8]
vole_random@data$dist_improvgrassland[id_17] <- coverdist_17[, 9]
vole_random@data$dist_montane_inlandrock[id_17] <- coverdist_17[, 10]
vole_random@data$dist_coastalhabitats[id_17] <- apply(X = coverdist_17[, c(11,12,16,17,19,20)],
                                                      MARGIN = 1,  
                                                      FUN = function(x) min(x, na.rm = TRUE))   
vole_random@data$dist_mixedwoodland[id_17] <- coverdist_17[, 13]
vole_random@data$dist_neutralgrassland[id_17] <- coverdist_17[, 14]
vole_random@data$dist_freshwater[id_17] <- coverdist_17[, 15] 
vole_random@data$dist_suburban[id_17] <- coverdist_17[, 18]
vole_random@data$dist_urban[id_17] <- coverdist_17[, 21]

# 2018
names(coverdist_18) # see names to make it easier to index categories
vole_random@data$dist_acidgrassland[id_18] <- coverdist_18[, 1]
vole_random@data$dist_arable[id_18] <- coverdist_18[, 2]
vole_random@data$dist_bog[id_18] <- coverdist_18[, 3]
vole_random@data$dist_calcgrassland[id_18] <- coverdist_18[, 4]
vole_random@data$dist_coniferwoodland[id_18] <- coverdist_18[, 5]
vole_random@data$dist_fen_marsh_swamp[id_18] <- coverdist_18[, 6]
vole_random@data$dist_heather[id_18] <- coverdist_18[, 7]
vole_random@data$dist_heathergrassland[id_18] <- coverdist_18[, 8]
vole_random@data$dist_improvgrassland[id_18] <- coverdist_18[, 9]
vole_random@data$dist_montane_inlandrock[id_18] <- coverdist_18[, 10]
vole_random@data$dist_coastalhabitats[id_18] <- apply(X = coverdist_18[, c(11,12,16,17,19,20)],
                                                      MARGIN = 1,  
                                                      FUN = function(x) min(x, na.rm = TRUE))   
vole_random@data$dist_mixedwoodland[id_18] <- coverdist_18[, 13]
vole_random@data$dist_neutralgrassland[id_18] <- coverdist_18[, 14]
vole_random@data$dist_freshwater[id_18] <- coverdist_18[, 15] 
vole_random@data$dist_suburban[id_18] <- coverdist_18[, 18]
vole_random@data$dist_urban[id_18] <- coverdist_18[, 21]


# 2019
names(coverdist_19) # see names to make it easier to index categories
vole_random@data$dist_acidgrassland[id_19] <- coverdist_19[, 1]
vole_random@data$dist_arable[id_19] <- coverdist_19[, 2]
vole_random@data$dist_bog[id_19] <- coverdist_19[, 3]
vole_random@data$dist_calcgrassland[id_19] <- coverdist_19[, 4]
vole_random@data$dist_coniferwoodland[id_19] <- coverdist_19[, 5]
vole_random@data$dist_fen_marsh_swamp[id_19] <- coverdist_19[, 6]
vole_random@data$dist_heather[id_19] <- coverdist_19[, 7]
vole_random@data$dist_heathergrassland[id_19] <- coverdist_19[, 8]
vole_random@data$dist_improvgrassland[id_19] <- coverdist_19[, 9]
vole_random@data$dist_montane_inlandrock[id_19] <- coverdist_19[, 10]
vole_random@data$dist_coastalhabitats[id_19] <- apply(X = coverdist_19[, c(11,12,16,17,19,20)],
                                                      MARGIN = 1,  
                                                      FUN = function(x) min(x, na.rm = TRUE))   
vole_random@data$dist_mixedwoodland[id_19] <- coverdist_19[, 13]
vole_random@data$dist_neutralgrassland[id_19] <- coverdist_19[, 14]
vole_random@data$dist_freshwater[id_19] <- coverdist_19[, 15] 
vole_random@data$dist_suburban[id_19] <- coverdist_19[, 18]
vole_random@data$dist_urban[id_19] <- coverdist_19[, 21]

head(vole_random@data)
summary(vole_random@data)


# Cleaning the environment
rm(list = setdiff(ls(), "vole_random"))
gc()


# Save file
save(vole_random, file = file.path("watervole/watervole_random/Robjects",
                            "vole4_landcoverdist.RData"))