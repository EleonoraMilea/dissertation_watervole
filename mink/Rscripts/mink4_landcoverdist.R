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
load("mink/Robjects/mink3_landcoverprop.Rdata")

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


names(coverdist_all) <- c("07", "15", "17", "18", "19") # change labels


# Subsetting mink data by year
mink_year <- list("07" = subset(mink, Year >= 2000 & Year <= 2010),
                  "15" = subset(mink, Year >= 2011 & Year <= 2016),
                  "17" = subset(mink, Year == 2017),
                  "18" = subset(mink, Year == 2018),
                  "19" = subset(mink, Year >= 2019))


# Calculating distance for each year range 
for(i in 1:length(coverdist_all)){
  for(j in 1:length(coverdist_all[[i]])){
    coverdist_all[[i]][[j]] <- raster::extract(coverdist_all[[i]][[j]], mink_year[[i]])
  }}


# Creating data frame for each time period 
coverdist_07 <- as.data.frame(coverdist_all[[1]])
dim(coverdist_07)
str(coverdist_07)
coverdist_15 <- as.data.frame(coverdist_all[[2]])
coverdist_17 <- as.data.frame(coverdist_all[[3]])
coverdist_18 <- as.data.frame(coverdist_all[[4]])
coverdist_19 <- as.data.frame(coverdist_all[[5]])



# Adding variables to mink df
mink@data$dist_woodland <- NA
mink@data$dist_wetland <- NA
mink@data$dist_urban <- NA
mink@data$dist_freshwater <- NA
mink@data$dist_grassland <- NA
mink@data$dist_arable <- NA
mink@data$dist_montane_inlandrock <- NA
mink@data$dist_heather <- NA
mink@data$dist_coastalhabitats <- NA


# Taking id for observations from each time period 
id_07 <- mink_year[[1]]@data$id
id_15 <- mink_year[[2]]@data$id
id_17 <- mink_year[[3]]@data$id
id_18 <- mink_year[[4]]@data$id
id_19 <- mink_year[[5]]@data$id


#' Now, as we did for land cover proportions, we place distance measurements at 
#' id locations that corresponds to the correct time period. Distances for similar
#' land cover categories (e.g. broadleaved and conifer woodlands) have been merged
#' together, keeping between the two values the minimum distance.
#' 


# 2007
names(coverdist_07) # see names to make it easier to index categories
mink@data$dist_woodland[id_07] <- apply(X = coverdist_07[, c(5, 13)], # select columns
                                        MARGIN = 1,  # apply this function over rows
                                        FUN = function(x) min(x, na.rm = TRUE)) # minimum function with NA removed; mixed woods,conifer

mink@data$dist_arable[id_07] <- coverdist_07[, 2]

mink@data$dist_grassland[id_07] <- apply(X = coverdist_07[, c(1,4,9,15,17)],
                                         MARGIN = 1,  
                                         FUN = function(x) min(x, na.rm = TRUE)) # improved, rough, neutral, calcareous, acid 

mink@data$dist_wetland[id_07] <- apply(X = coverdist_07[, c(3,6)],
                                       MARGIN = 1,  
                                       FUN = function(x) min(x, na.rm = TRUE)) # fen/marsh/swamp & bog 
mink@data$dist_heather[id_07] <- apply(X = coverdist_07[, c(7:8)],
                                       MARGIN = 1,  
                                       FUN = function(x) min(x, na.rm = TRUE))  # heather and heather grassland

mink@data$dist_montane_inlandrock[id_07] <- apply(X = coverdist_07[, c(10, 14)],
                                                  MARGIN = 1,  
                                                  FUN = function(x) min(x, na.rm = TRUE)) #inland rock, montane

mink@data$dist_freshwater[id_07] <- coverdist_07[, 16] # rivers

mink@data$dist_coastalhabitats[id_07] <- apply(X = coverdist_07[, c(11,12,18,19,21,22)],
                                               MARGIN = 1,  
                                               FUN = function(x) min(x, na.rm = TRUE))  #supralittoral/littoral rock/sediment, saltmarsh, saltwater 

mink@data$dist_urban[id_07] <- apply(X = coverdist_07[, c(20,23)],
                                     MARGIN = 1,  
                                     FUN = function(x) min(x, na.rm = TRUE)) # urban suburban


# 2015
names(coverdist_15) # see names to make it easier to index categories
mink@data$dist_woodland[id_15] <- apply(X = coverdist_15[, c(5, 13)], 
                                        MARGIN = 1,  
                                        FUN = function(x) min(x, na.rm = TRUE)) 

mink@data$dist_arable[id_15] <- coverdist_15[, 2]

mink@data$dist_grassland[id_15] <- apply(X = coverdist_15[, c(1,4,9,14)],
                                         MARGIN = 1,  
                                         FUN = function(x) min(x, na.rm = TRUE)) 

mink@data$dist_wetland[id_15] <- apply(X = coverdist_15[, c(3,6)],
                                       MARGIN = 1,  
                                       FUN = function(x) min(x, na.rm = TRUE)) 

mink@data$dist_heather[id_15] <- apply(X = coverdist_15[, c(7:8)],
                                       MARGIN = 1,  
                                       FUN = function(x) min(x, na.rm = TRUE))  

mink@data$dist_montane_inlandrock[id_15] <- coverdist_15[, 10] 

mink@data$dist_freshwater[id_15] <- coverdist_15[, 15]

mink@data$dist_coastalhabitats[id_15] <- apply(X = coverdist_15[, c(11,12,16,17,19,20)],
                                               MARGIN = 1,  
                                               FUN = function(x) min(x, na.rm = TRUE))   

mink@data$dist_urban[id_15] <- apply(X = coverdist_15[, c(18,21)],
                                     MARGIN = 1,  
                                     FUN = function(x) min(x, na.rm = TRUE)) 


# 2017
names(coverdist_17) # see names to make it easier to index categories
mink@data$dist_woodland[id_17] <- apply(X = coverdist_17[, c(5, 13)], 
                                        MARGIN = 1,  
                                        FUN = function(x) min(x, na.rm = TRUE)) 

mink@data$dist_arable[id_17] <- coverdist_17[, 2]

mink@data$dist_grassland[id_17] <- apply(X = coverdist_17[, c(1,4,9,14)],
                                         MARGIN = 1,  
                                         FUN = function(x) min(x, na.rm = TRUE)) 

mink@data$dist_wetland[id_17] <- apply(X = coverdist_17[, c(3,6)],
                                       MARGIN = 1,  
                                       FUN = function(x) min(x, na.rm = TRUE)) 

mink@data$dist_heather[id_17] <- apply(X = coverdist_17[, c(7:8)],
                                       MARGIN = 1,  
                                       FUN = function(x) min(x, na.rm = TRUE))  

mink@data$dist_montane_inlandrock[id_17] <- coverdist_17[, 10] # inland

mink@data$dist_freshwater[id_17] <- coverdist_17[, 15] # rivers

mink@data$dist_coastalhabitats[id_17] <- apply(X = coverdist_17[, c(11,12,16,17,19,20)],
                                               MARGIN = 1,  
                                               FUN = function(x) min(x, na.rm = TRUE))  

mink@data$dist_urban[id_17] <- apply(X = coverdist_17[, c(18,21)],
                                     MARGIN = 1,  
                                     FUN = function(x) min(x, na.rm = TRUE)) 

# 2018
names(coverdist_18) # see names to make it easier to index categories
mink@data$dist_woodland[id_18] <- apply(X = coverdist_18[, c(5, 13)], 
                                        MARGIN = 1,  
                                        FUN = function(x) min(x, na.rm = TRUE)) 

mink@data$dist_arable[id_18] <- coverdist_18[, 2]

mink@data$dist_grassland[id_18] <- apply(X = coverdist_18[, c(1,4,9,14)],
                                         MARGIN = 1,  
                                         FUN = function(x) min(x, na.rm = TRUE)) 

mink@data$dist_wetland[id_18] <- apply(X = coverdist_18[, c(3,6)],
                                       MARGIN = 1,  
                                       FUN = function(x) min(x, na.rm = TRUE)) 

mink@data$dist_heather[id_18] <- apply(X = coverdist_18[, c(7:8)],
                                       MARGIN = 1,  
                                       FUN = function(x) min(x, na.rm = TRUE))  

mink@data$dist_montane_inlandrock[id_18] <- coverdist_18[, 10]

mink@data$dist_freshwater[id_18] <- coverdist_18[, 15] 

mink@data$dist_coastalhabitats[id_18] <- apply(X = coverdist_18[, c(11,12,16,17,19,20)],
                                               MARGIN = 1,  
                                               FUN = function(x) min(x, na.rm = TRUE))  

mink@data$dist_urban[id_18] <- apply(X = coverdist_18[, c(18,21)],
                                     MARGIN = 1,  
                                     FUN = function(x) min(x, na.rm = TRUE)) 


# 2019
names(coverdist_19) # see names to make it easier to index categories
mink@data$dist_woodland[id_19] <- apply(X = coverdist_19[, c(5, 13)],
                                        MARGIN = 1,
                                        FUN = function(x) min(x, na.rm = TRUE)) 

mink@data$dist_arable[id_19] <- coverdist_19[, 2]

mink@data$dist_grassland[id_19] <- apply(X = coverdist_19[, c(1,4,9,14)],
                                         MARGIN = 1,
                                         FUN = function(x) min(x, na.rm = TRUE)) 

mink@data$dist_wetland[id_19] <- apply(X = coverdist_19[, c(3,6)],
                                       MARGIN = 1,
                                       FUN = function(x) min(x, na.rm = TRUE)) 

mink@data$dist_heather[id_19] <- apply(X = coverdist_19[, c(7:8)],
                                       MARGIN = 1,
                                       FUN = function(x) min(x, na.rm = TRUE))  

mink@data$dist_montane_inlandrock[id_19] <- coverdist_19[, 10] # inland

mink@data$dist_freshwater[id_19] <- coverdist_19[, 15] # rivers

mink@data$dist_coastalhabitats[id_19] <- apply(X = coverdist_19[, c(11,12,16,17,19,20)],
                                               MARGIN = 1,
                                               FUN = function(x) min(x, na.rm = TRUE))  

mink@data$dist_urban[id_19] <- apply(X = coverdist_19[, c(18,21)],
                                     MARGIN = 1,
                                     FUN = function(x) min(x, na.rm = TRUE)) 
head(mink@data)
summary(mink@data)


# Cleaning the environment
rm(list = setdiff(ls(), "mink"))
gc()


# Save file
# save(mink, file = file.path("mink/Robjects",
#                             "mink4_landcoverdist.RData"))