#' ---
#' title: "Land cover: Proportions at buffered points"
#' author: "Eleonora Milea"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#'

#' ---
#' 
#' In this script we will calculate the proportion of land cover at each
#' observation point. Land cover raster files were downloaded from:
#' https://digimap.edina.ac.uk/environment.
#' The 25 m resolution files were downloaded for different years: 2000, 2007,
#' 2015, 2017, 2018, 2019 and 2020.
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
library(lme4)
library(pROC)
load("mink/Robjects/mink2_pres&pseudoabs.RData")
land_07 <- raster("layers_27700/cover/landcover_2007.tif")
land_15 <- raster("layers_27700/cover/landcover_2015.tif")
land_17 <- raster("layers_27700/cover/landcover_2017.tif")
land_18 <- raster("layers_27700/cover/landcover_2018.tif")
land_19 <- raster("layers_27700/cover/landcover_2019.tif")
land_20 <- raster("layers_27700/cover/landcover_2020.tif")


#' Since we do not have land cover files for each year,
#' we we will assume that land cover has not changed between the time periods for
#' which we have data. Consequently, mink observations is grouped into the same
#' time intervals, so that we can calculate the land cover type at the observation
#' point that is most likely to occur during that time period. 
#' The different time period are as follow: 
#' 
#' - 2000/2010
#' 
#' - 2011/2016
#' 
#' - 2017
#' 
#' - 2018
#' 
#' - 2019/2022
#' 
#' 
#' Proportions of each land cover type is calculated for buffers of a 500 meters
#' around each observation point. 
#' 


#' ---
#' 
#'##### **Creating buffers for each time period**
#' 
# 2000 - 2008
minkbuffer_07500 <- gBuffer(subset(mink, Year >= 2000 & Year <= 2010),
                            width = 500, byid = T) 

# 2009 - 2016
minkbuffer_15500 <- gBuffer(subset(mink, Year >= 2011 & Year <= 2016),
                            width = 500, byid = T) 

# 2017
minkbuffer_17500 <- gBuffer(subset(mink, Year == 2017),
                            width = 500, byid = T) 

# 2018
minkbuffer_18500 <- gBuffer(subset(mink, Year == 2018),
                            width = 500, byid = T) 

# 2019
minkbuffer_19500 <- gBuffer(subset(mink, Year >= 2019),
                            width = 500, byid = T) 



#'##### **Intersection of land cover**
#' Now we intersect each buffered point with land cover type for each time period
#' using a for loop to iterate through each observation point. 
#'
# 2000 - 2010
Sys.time()
int.list07 <- list() # creating an empty list to store data
for (i in 1:length(minkbuffer_07500)){
  int.list07[[i]] <- raster::intersect(land_07,  minkbuffer_07500[i,]) 
  print(i)
}
Sys.time()


# 2011 - 2016
int.list15 <- list() # creating an empty list to store data
for (i in 1:length(minkbuffer_15500)){
  int.list15[[i]] <- raster::intersect(land_15, minkbuffer_15500[i,])
  print(i)
}
Sys.time()


# 2017
int.list17 <- list() # creating an empty list to store data
for (i in 1:length(minkbuffer_17500)){
  int.list17[[i]] <- raster::intersect(land_17, minkbuffer_17500[i,])
  print(i)
}
beep(2)
Sys.time()


# 2018
int.list18 <- list() # creating an empty list to store data
for (i in 1:length(minkbuffer_18500)){
  int.list18[[i]] <- raster::intersect(land_18, minkbuffer_18500[i,])
  print(i)
}
Sys.time()


# 2019
int.list19 <- list() # creating an empty list to store data
for ( i in 1:length(minkbuffer_19500)){
  int.list19[[i]] <- raster::intersect(land_19, minkbuffer_19500[i,])
  print(i)
}
Sys.time()


#' ---
#' 
#'##### **Counting pixels** 
#'
#' Now we count the number of pixels for each land cover type at each buffered point
#' and store them into a new data frame. We create a df for each time period where
#' the number of rows in the df is determined by the number of observations present
#' in each buffer object, while the number of rows is determined by the number
#' of land cover categories from each raster file. This information can be found
#' in the supplementary documents when downloading the land cover raster files. 

# Creating df
# 2000 - 2010
landcover_07500 <- data.frame(matrix(NA, nrow = length(minkbuffer_07500),
                                     ncol = 23))

# Labelling columns
names(landcover_07500) <- c( "1","2","3","4","5","6", "7", "8", "9", "10",
                             "11", "12", "13", "14", "15", "16", "17", "18",
                             "19", "20", "21", "22", "23")
head(landcover_07500)


# 2011 - 2016
landcover_15500 <- data.frame(matrix(NA, nrow = length(minkbuffer_15500),
                                     ncol = 21))

# Labelling columns
names(landcover_15500) <- c( "1","2","3","4","5","6", "7", "8", "9", "10",
                             "11", "12", "13", "14", "15", "16", "17", "18",
                             "19", "20", "21")


# 2017
landcover_17500 <- data.frame(matrix(NA, nrow = length(minkbuffer_17500),
                                     ncol = 21))

# Labelling columns
names(landcover_17500) <- c( "1","2","3","4","5","6", "7", "8", "9", "10",
                             "11", "12", "13", "14", "15", "16", "17", "18",
                             "19", "20", "21")


# 2018
landcover_18500 <- data.frame(matrix(NA, nrow = length(minkbuffer_18500),
                                     ncol = 21))

# Labelling columns
names(landcover_18500) <- c( "1","2","3","4","5","6", "7", "8", "9", "10",
                             "11", "12", "13", "14", "15", "16", "17", "18",
                             "19", "20", "21")


# 2019
landcover_19500 <- data.frame(matrix(NA, nrow = length(minkbuffer_19500),
                                     ncol = 21))

# Labelling columns
names(landcover_19500) <- c( "1","2","3","4","5","6", "7", "8", "9", "10",
                             "11", "12", "13", "14", "15", "16", "17", "18",
                             "19", "20", "21")


#' Now we can count the number of pixels at each intersection
#'  
# 2000 - 2010
for (i in 1:length(minkbuffer_07500)){
  for ( j in 1:length(names(landcover_07500))) {
    landcover_07500[i,j] <- table(
      values(int.list07[[i]]))[as.character(names(landcover_07500)[j])]
  }}

# 2011 - 2016
for (i in 1:length(minkbuffer_15500)){
  for ( j in 1:length(names(landcover_15500))) {
    landcover_15500[i,j] <- table(
      values(int.list15[[i]]))[as.character(names(landcover_15500)[j])]
  }}

# 2017
for (i in 1:length(minkbuffer_17500)){
  for ( j in 1:length(names(landcover_17500))) {
    landcover_17500[i,j] <- table(
      values(int.list17[[i]]))[as.character(names(landcover_17500)[j])]
  }}

# 2018
for (i in 1:length(minkbuffer_18500)){
  for ( j in 1:length(names(landcover_18500))) {
    landcover_18500[i,j] <- table(
      values(int.list18[[i]]))[as.character(names(landcover_18500)[j])]
  }}

# 2019
for (i in 1:length(minkbuffer_19500)){
  for ( j in 1:length(names(landcover_19500))) {
    landcover_19500[i,j] <- table(
      values(int.list19[[i]]))[as.character(names(landcover_19500)[j])]
  }}


#' Now we can change the columns names using the names of land cover types that
#' can be found in supplementary documents from each raster file. The order needs 
#' to be the same as the one provided in the suppl. files.
#' 
# 2000 - 2010
names(landcover_07500) <- c ("mixed_woodland",
                             "coniferous_woodland",
                             "arable_horticulture",
                             "improved_grassland",
                             "rough_grassland",
                             "neutral_grassland",
                             "calcareous_grassland",
                             "acid_grassland",
                             "fen_marsh_swamp",
                             "heather",
                             "heather_grassland",
                             "bog",
                             "montane_habitats",
                             "inland_rock",
                             "saltwater",
                             "freshwater",
                             "supralittoral_rock",
                             "supralittoral_sediment",
                             "littoral_rock",
                             "littoral_sediment",
                             "saltmarsh",
                             "urban",
                             "suburban")

# 2011 - 2016
names(landcover_15500) <- c ("mixed_woodland",
                             "coniferous_woodland",
                             "arable_horticulture",
                             "improved_grassland",
                             "neutral_grassland",
                             "calcareous_grassland",
                             "acid_grassland",
                             "fen_marsh_swamp",
                             "heather",
                             "heather_grassland",
                             "bog",
                             "inland_rock",
                             "saltwater",
                             "freshwater",
                             "supralittoral_rock",
                             "supralittoral_sediment",
                             "littoral_rock",
                             "littoral_sediment",
                             "saltmarsh",
                             "urban",
                             "suburban")

# 2017
names(landcover_17500) <- c ("mixed_woodland",
                             "coniferous_woodland",
                             "arable_horticulture",
                             "improved_grassland",
                             "neutral_grassland",
                             "calcareous_grassland",
                             "acid_grassland",
                             "fen_marsh_swamp",
                             "heather",
                             "heather_grassland",
                             "bog",
                             "inland_rock",
                             "saltwater",
                             "freshwater",
                             "supralittoral_rock",
                             "supralittoral_sediment",
                             "littoral_rock",
                             "littoral_sediment",
                             "saltmarsh",
                             "urban",
                             "suburban")

# 2018
names(landcover_18500) <- c ("mixed_woodland",
                             "coniferous_woodland",
                             "arable_horticulture",
                             "improved_grassland",
                             "neutral_grassland",
                             "calcareous_grassland",
                             "acid_grassland",
                             "fen_marsh_swamp",
                             "heather",
                             "heather_grassland",
                             "bog",
                             "inland_rock",
                             "saltwater",
                             "freshwater",
                             "supralittoral_rock",
                             "supralittoral_sediment",
                             "littoral_rock",
                             "littoral_sediment",
                             "saltmarsh",
                             "urban",
                             "suburban")

# 2019
names(landcover_19500) <- c ("mixed_woodland",
                             "coniferous_woodland",
                             "arable_horticulture",
                             "improved_grassland",
                             "neutral_grassland",
                             "calcareous_grassland",
                             "acid_grassland",
                             "fen_marsh_swamp",
                             "heather",
                             "heather_grassland",
                             "bog",
                             "inland_rock",
                             "saltwater",
                             "freshwater",
                             "supralittoral_rock",
                             "supralittoral_sediment",
                             "littoral_rock",
                             "littoral_sediment",
                             "saltmarsh",
                             "urban",
                             "suburban")


#' ---
#' 
#'##### **Calculating proportions** 
#'
#' Now we need to calculate the proportion of each land cover type in all buffers.

# Replacing NAs with 0
landcover_07500[is.na(landcover_07500)] <- 0
landcover_15500[is.na(landcover_15500)] <- 0
landcover_17500[is.na(landcover_17500)] <- 0
landcover_18500[is.na(landcover_18500)] <- 0
landcover_19500[is.na(landcover_19500)] <- 0


# Creating a list with all landcovers
landcover <- list(landcover_07500, landcover_15500,
                  landcover_17500, landcover_18500, landcover_19500)
names(landcover) <- c("07", "15", "17", "18", "19") # Changing labels
summary(landcover)


# Calculating sum of all pixels for each row (which corresponds to each mink observation)
row_sum <- lapply(landcover, rowSums)


# Creating an empty list to store proportions
props <- list()


# Using a for loop to iterate through each element of the landcover list
for(i in 1:length(landcover)){
  
  # Calculating proportion
  props[[i]] <- landcover[[i]]/row_sum[[i]] # calculating proportions for each observation of every element of the list
  props[[i]] <- round(props[[i]], 3) # rounding up
}
head(props[[1]])


# Replacing NaNs with 0
props <- rapply(props, f = function(x) ifelse(is.nan(x), 0, x),
                how ="replace" ) 
summary(props[[1]]) # check if it worked


# Creating data frame to store proportions from each time period separately 
propcover_07 <- as.data.frame(props[[1]])
dim(propcover_07)
head(propcover_07)
propcover_15 <- as.data.frame(props[[2]])
propcover_17 <- as.data.frame(props[[3]])
propcover_18 <- as.data.frame(props[[4]])
propcover_19 <- as.data.frame(props[[5]])

# Taking id for observations from each time period 
id_07 <- minkbuffer_07500$id
id_15 <- minkbuffer_15500$id
id_17 <- minkbuffer_17500$id
id_18 <- minkbuffer_18500$id
id_19 <- minkbuffer_19500$id


# Adding variables to main mink sp object
mink@data$woodland500 <- 0
mink@data$wetland500 <- 0
mink@data$urban500 <- 0
mink@data$freshwater500 <- 0
mink@data$grassland500 <- 0
mink@data$arable_horticulture500 <- 0
mink@data$montane_inlandrock500 <- 0
mink@data$heather500 <- 0
mink@data$coastal_habitats500 <- 0


#' Now we place landcover proportions to the main mink sp object at the id locations
#' that corresponds to the correct time period. Similar land cover categories 
#' have been grouped together (e.g. different horticulture and woodland categories)
#' 
# 2000 - 2010
names(propcover_07)
mink@data$woodland500[id_07] <- rowSums(propcover_07[, c(1,2),drop = F]) # broadleaf and conifer; rowSums sum up the two proportions 
mink@data$arable_horticulture500[id_07] <- propcover_07[,3]
mink@data$grassland500[id_07] <- rowSums(propcover_07[, c(4:8), drop = F]) # improved, rough, neutral, calcareous, acid 
mink@data$wetland500[id_07] <- rowSums(propcover_07[, c(9, 12), drop = F]) # fen,marsh,swamp,bog
mink@data$heather500[id_07] <- rowSums(propcover_07[, c(10, 11), drop = F]) # heather and heather grassland
mink@data$montane_inlandrock500[id_07] <- rowSums(propcover_07[, c(13:14), drop = F]) # montane habitats and inland rock 
mink@data$freshwater500[id_07] <- propcover_07[,16]
mink@data$coastal_habitats500[id_07] <- rowSums(propcover_07[, c(15,17:21), drop = F]) #supralittoral/littoral rock/sediment, saltmarsh, sea
mink@data$urban500[id_07] <- rowSums(propcover_07[, c(22, 23), drop = F]) # urban suburban
head(mink@data)


# 2011-2016
names(propcover_15)
mink@data$woodland500[id_15] <- rowSums(propcover_15[, c(1,2), drop = F]) 
mink@data$arable_horticulture500[id_15] <- propcover_15[,3]
mink@data$grassland500[id_15] <- rowSums(propcover_15[, c(4:7), drop = F]) 
mink@data$wetland500[id_15] <- rowSums(propcover_15[, c(8, 11), drop = F]) 
mink@data$heather500[id_15] <- rowSums(propcover_15[, c(9, 10), drop = F]) 
mink@data$montane_inlandrock500[id_15] <- propcover_15[, 12]
mink@data$freshwater500[id_15] <- propcover_15[,14]
mink@data$coastal_habitats500[id_15] <- rowSums(propcover_15[, c(13, 15:19), drop = F]) 
mink@data$urban500[id_15] <- rowSums(propcover_15[, c(20, 21), drop = F]) 
head(mink@data)


# 17
names(propcover_17)
mink@data$woodland500[id_17] <- rowSums(propcover_17[, c(1,2), drop = F]) 
mink@data$arable_horticulture500[id_17] <- propcover_17[,3]
mink@data$grassland500[id_17] <- rowSums(propcover_17[, c(4:7), drop = F]) 
mink@data$wetland500[id_17] <- rowSums(propcover_17[, c(8, 11), drop = F]) 
mink@data$heather500[id_17] <- rowSums(propcover_17[, c(9, 10), drop = F]) 
mink@data$montane_inlandrock500[id_17] <- propcover_17[, 12]
mink@data$freshwater500[id_17] <- propcover_17[,14]
mink@data$coastal_habitats500[id_17] <- rowSums(propcover_17[, c(13, 15:19), drop = F]) 
mink@data$urban500[id_17] <- rowSums(propcover_17[, c(20, 21), drop = F]) 
head(mink@data)


# 18
names(propcover_18)
mink@data$woodland500[id_18] <- rowSums(propcover_18[, c(1,2), drop = F]) 
mink@data$arable_horticulture500[id_18] <- propcover_18[,3]
mink@data$grassland500[id_18] <- rowSums(propcover_18[, c(4:7), drop = F])
mink@data$wetland500[id_18] <- rowSums(propcover_18[, c(8, 11), drop = F]) 
mink@data$heather500[id_18] <- rowSums(propcover_18[, c(9, 10), drop = F]) 
mink@data$montane_inlandrock500[id_18] <- propcover_18[, 12]
mink@data$freshwater500[id_18] <- propcover_18[,14]
mink@data$coastal_habitats500[id_18] <- rowSums(propcover_18[, c(13, 15:19), drop = F]) 
mink@data$urban500[id_18] <- rowSums(propcover_18[, c(20, 21), drop = F]) 
head(mink@data)


# 19
names(propcover_19)
mink@data$woodland500[id_19] <- rowSums(propcover_19[, c(1,2), drop = F]) 
mink@data$arable_horticulture500[id_19] <- propcover_19[,3]
mink@data$grassland500[id_19] <- rowSums(propcover_19[, c(4:7), drop = F]) 
mink@data$wetland500[id_19] <- rowSums(propcover_19[, c(8, 11), drop = F]) 
mink@data$heather500[id_19] <- rowSums(propcover_19[, c(9, 10), drop = F]) 
mink@data$montane_inlandrock500[id_19] <- propcover_19[, 12]
mink@data$freshwater500[id_19] <- propcover_19[,14]
mink@data$coastal_habitats500[id_19] <- rowSums(propcover_19[, c(13, 15:19), drop = F]) 
mink@data$urban500[id_19] <- rowSums(propcover_19[, c(20, 21), drop = F]) 
head(mink@data)



# Cleaning the environment
rm(list = setdiff(ls(), "mink"))
gc()


# save file
# save(mink, file = file.path("mink/Robjects",
#                             "mink3_landcoverprop.RData"))