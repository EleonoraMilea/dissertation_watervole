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
#' Since we do not have land cover files for each year,
#' we we will assume that land cover has not changed between the time periods for
#' which we have data. Consequently, vole observations is grouped into the same
#' time intervals, so that we can calculate the land cover type at the observation
#' point that is most likely to occur during that time period. 
#' The different time period are as follow: 
#'
#' - 2005/2012
#' 
#' - 2013/2016
#' 
#' - 2017
#' 
#' - 2018
#' 
#' - 2019/2022
#' 
#' Proportions of each land cover type is calculated for buffers of a 100 meters
#' around each observation point. 
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
land_07 <- raster("layers_27700/cover/landcover_2007.tif")
land_15 <- raster("layers_27700/cover/landcover_2015.tif")
land_17 <- raster("layers_27700/cover/landcover_2017.tif")
land_18 <- raster("layers_27700/cover/landcover_2018.tif") 
land_19 <- raster("layers_27700/cover/landcover_2019.tif")


#' ---
#' 
#'##### **Creating buffers for each time period**
#' 
# 2000 - 2010
volebuffer_071000 <- gBuffer(subset(vole_random, Year >= 2000 & Year <= 2010),
                            width = 1000, byid = T) 

# 2011 - 2016
volebuffer_151000 <- gBuffer(subset(vole_random, Year >= 2011 & Year <= 2016),
                            width = 1000, byid = T) 

# 2017
volebuffer_171000 <- gBuffer(subset(vole_random, Year == 2017),
                            width = 1000, byid = T) 

# 2018
volebuffer_181000 <- gBuffer(subset(vole_random, Year == 2018),
                            width = 1000, byid = T) 

# 2019
volebuffer_191000 <- gBuffer(subset(vole_random, Year >= 2019),
                            width = 1000, byid = T) 



#'##### **Intersection of land cover**
#' Now we intersect each buffered point with land cover type for each time period
#' using a for loop to iterate through each observation point. 
#'
# 2000 - 2010
Sys.time()
int.list07 <- list() # creating an empty list to store data
for (i in 1:length(volebuffer_071000)){
  int.list07[[i]] <- raster::intersect(land_07,  volebuffer_071000[i,]) 
  print(i)
}
Sys.time()



# 2011 - 2016
int.list15 <- list() # creating an empty list to store data
for (i in 1:length(volebuffer_151000)){
  int.list15[[i]] <- raster::intersect(land_15, volebuffer_151000[i,])
  print(i)
}
Sys.time()


# 2017
int.list17 <- list() # creating an empty list to store data
for (i in 1:length(volebuffer_171000)){
  int.list17[[i]] <- raster::intersect(land_17, volebuffer_171000[i,])
  print(i)
}


# 2018
int.list18 <- list() # creating an empty list to store data
for (i in 1:length(volebuffer_181000)){
  int.list18[[i]] <- raster::intersect(land_18, volebuffer_181000[i,])
  print(i)
}
Sys.time()


# 2019
int.list19 <- list() # creating an empty list to store data
for ( i in 1:length(volebuffer_191000)){
  int.list19[[i]] <- raster::intersect(land_19, volebuffer_191000[i,])
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
landcover_071000 <- data.frame(matrix(NA, nrow = length(volebuffer_071000),
                                     ncol = 23))

# Labelling columns
names(landcover_071000) <- c( "1","2","3","4","5","6", "7", "8", "9", "10",
                             "11", "12", "13", "14", "15", "16", "17", "18",
                             "19", "20", "21", "22", "23")
head(landcover_071000)


# 2011 - 2016
landcover_151000 <- data.frame(matrix(NA, nrow = length(volebuffer_151000),
                                     ncol = 21))

# Labelling columns
names(landcover_151000) <- c( "1","2","3","4","5","6", "7", "8", "9", "10",
                             "11", "12", "13", "14", "15", "16", "17", "18",
                             "19", "20", "21")


# 2017
landcover_171000 <- data.frame(matrix(NA, nrow = length(volebuffer_171000),
                                     ncol = 21))

# Labelling columns
names(landcover_171000) <- c( "1","2","3","4","5","6", "7", "8", "9", "10",
                             "11", "12", "13", "14", "15", "16", "17", "18",
                             "19", "20", "21")


# 2018
landcover_181000 <- data.frame(matrix(NA, nrow = length(volebuffer_181000),
                                     ncol = 21))

# Labelling columns
names(landcover_181000) <- c( "1","2","3","4","5","6", "7", "8", "9", "10",
                             "11", "12", "13", "14", "15", "16", "17", "18",
                             "19", "20", "21")


# 2019
landcover_191000 <- data.frame(matrix(NA, nrow = length(volebuffer_191000),
                                     ncol = 21))

# Labelling columns
names(landcover_191000) <- c( "1","2","3","4","5","6", "7", "8", "9", "10",
                             "11", "12", "13", "14", "15", "16", "17", "18",
                             "19", "20", "21")


#' Now we can count the number of pixels at each intersection
#'  
# 2000 - 2010
for (i in 1:length(volebuffer_071000)){
  for ( j in 1:length(names(landcover_071000))) {
    landcover_071000[i,j] <- table(
      values(int.list07[[i]]))[as.character(names(landcover_071000)[j])]
  }}

# 2011 - 2016
for (i in 1:length(volebuffer_151000)){
  for ( j in 1:length(names(landcover_151000))) {
    landcover_151000[i,j] <- table(
      values(int.list15[[i]]))[as.character(names(landcover_151000)[j])]
  }}

# 2017
for (i in 1:length(volebuffer_171000)){
  for ( j in 1:length(names(landcover_171000))) {
    landcover_171000[i,j] <- table(
      values(int.list17[[i]]))[as.character(names(landcover_171000)[j])]
  }}

# 2018
for (i in 1:length(volebuffer_181000)){
  for ( j in 1:length(names(landcover_181000))) {
    landcover_181000[i,j] <- table(
      values(int.list18[[i]]))[as.character(names(landcover_181000)[j])]
  }}

# 2019
for (i in 1:length(volebuffer_191000)){
  for ( j in 1:length(names(landcover_191000))) {
    landcover_191000[i,j] <- table(
      values(int.list19[[i]]))[as.character(names(landcover_191000)[j])]
  }}

#' Now we can change the columns names using the names of land cover types that
#' can be found in supplementary documents from each raster file. The order needs 
#' to be the same as the one provided in the suppl. files.
#' 
# 2000 - 2010
names(landcover_071000) <- c("mixed_woodland",
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
names(landcover_151000) <- c ("mixed_woodland",
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
names(landcover_171000) <- c ("mixed_woodland",
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
names(landcover_181000) <- c ("mixed_woodland",
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
names(landcover_191000) <- c ("mixed_woodland",
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
landcover_071000[is.na(landcover_071000)] <- 0
landcover_151000[is.na(landcover_151000)] <- 0
landcover_171000[is.na(landcover_171000)] <- 0
landcover_181000[is.na(landcover_181000)] <- 0
landcover_191000[is.na(landcover_191000)] <- 0
head(landcover_071000)


# Creating a list with all landcovers
landcover <- list(landcover_071000, landcover_151000, landcover_171000,
                  landcover_181000, landcover_191000)
names(landcover) <- c("07", "15", "17", "18", "19") # Changing labels
summary(landcover)


# Calculating sum of all pixels for each row (which corresponds to each vole observation)
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
sum(is.na(props[[1]]))
summary(props[[1]])


# Replacing NaNs with 0
props <- rapply(props, f = function(x) ifelse(is.nan(x), 0, x),
                how ="replace") 
summary(props[[1]]) # check if it worked


# Creating data frame to store proportions from each time period separately 
propcover_07 <- data.frame(props[[1]])
head(propcover_07)
propcover_15 <- data.frame(props[[2]])
propcover_17 <- data.frame(props[[3]])
propcover_18 <- data.frame(props[[4]])
propcover_19 <- data.frame(props[[5]])


# Taking id for observations from each time period 
id_07 <- volebuffer_071000$id
id_15 <- volebuffer_151000$id
id_17 <- volebuffer_171000$id
id_18 <- volebuffer_181000$id
id_19 <- volebuffer_191000$id


# Adding variables to main vole_random sp object
vole_random@data$mixed_woodland1000 <- 0
vole_random@data$conifer_woodland1000 <- 0
vole_random@data$arable_horticulture1000 <- 0
vole_random@data$neutral_grassland1000 <- 0
vole_random@data$calc_grassland1000 <- 0
vole_random@data$improv_grassland1000 <- 0
vole_random@data$acid_grassland1000 <- 0
vole_random@data$fen_marsh_swamp1000 <- 0
vole_random@data$heather1000 <- 0
vole_random@data$heather_grassland1000 <- 0
vole_random@data$bog1000 <- 0
vole_random@data$montane_inlandrock1000 <- 0
vole_random@data$suburban1000 <- 0
vole_random@data$urban1000 <- 0
vole_random@data$freshwater1000 <- 0
vole_random@data$coastal_habitats1000 <- 0


#' Now we place landcover proportions to the main vole sp object at the id locations
#' that corresponds to the correct time period. 
#' 
# 2000 - 2010
names(propcover_07)
vole_random@data$mixed_woodland1000[id_07] <- propcover_07[, 1] 
vole_random@data$conifer_woodland1000[id_07] <- propcover_07[, 2]
vole_random@data$arable_horticulture1000[id_07] <- propcover_07[, 3]
vole_random@data$improv_grassland1000[id_07] <- propcover_07[, 4] 
vole_random@data$neutral_grassland1000[id_07] <- rowSums(propcover_07[, c(5, 6), drop = F])
vole_random@data$calc_grassland1000[id_07] <- propcover_07[, 7]
vole_random@data$acid_grassland1000[id_07] <- propcover_07[, 8]
vole_random@data$fen_marsh_swamp1000[id_07] <- propcover_07[, 9]
vole_random@data$heather1000[id_07] <- propcover_07[, 10] 
vole_random@data$heather1000[id_07] <- propcover_07[, 11]
vole_random@data$bog1000[id_07] <- propcover_07[, 12]
vole_random@data$montane_inlandrock1000[id_07] <- rowSums(propcover_07[, c(13:14), drop = F]) 
vole_random@data$freshwater1000[id_07] <- propcover_07[,16]
vole_random@data$coastal_habitats1000[id_07] <- rowSums(propcover_07[, c(15,17:21), drop = F]) 
vole_random@data$urban1000[id_07] <- propcover_07[,22] 
vole_random@data$suburban1000[id_07] <- propcover_07[,23]
head(vole_random@data)


# 2011-2016
names(propcover_15)
vole_random@data$mixed_woodland1000[id_15] <- propcover_15[, 1] 
vole_random@data$conifer_woodland1000[id_15] <- propcover_15[, 2]
vole_random@data$arable_horticulture1000[id_15] <- propcover_15[, 3]
vole_random@data$improv_grassland1000[id_15] <- propcover_15[, 4] 
vole_random@data$neutral_grassland1000[id_15] <- propcover_15[, 5]
vole_random@data$calc_grassland1000[id_15] <- propcover_15[, 6]
vole_random@data$acid_grassland1000[id_15] <- propcover_15[, 7]
vole_random@data$fen_marsh_swamp1000[id_15] <- propcover_15[, 8]
vole_random@data$heather1000[id_15] <- propcover_15[, 9] 
vole_random@data$heather1000[id_15] <- propcover_15[, 10]
vole_random@data$bog1000[id_15] <- propcover_15[, 11]
vole_random@data$montane_inlandrock1000[id_15] <- propcover_15[, 12] 
vole_random@data$freshwater1000[id_15] <- propcover_15[,14]
vole_random@data$coastal_habitats1000[id_15] <- rowSums(propcover_15[, c(13,15:19), drop = F]) 
vole_random@data$urban1000[id_15] <- propcover_15[,20] 
vole_random@data$suburban1000[id_15] <- propcover_15[,21]
head(vole_random@data)


# 17
names(propcover_17)
vole_random@data$mixed_woodland1000[id_17] <- propcover_17[, 1] 
vole_random@data$conifer_woodland1000[id_17] <- propcover_17[, 2]
vole_random@data$arable_horticulture1000[id_17] <- propcover_17[, 3]
vole_random@data$improv_grassland1000[id_17] <- propcover_17[, 4] 
vole_random@data$neutral_grassland1000[id_17] <- propcover_17[, 5]
vole_random@data$calc_grassland1000[id_17] <- propcover_17[, 6]
vole_random@data$acid_grassland1000[id_17] <- propcover_17[, 7]
vole_random@data$fen_marsh_swamp1000[id_17] <- propcover_17[, 8]
vole_random@data$heather1000[id_17] <- propcover_17[, 9] 
vole_random@data$heather1000[id_17] <- propcover_17[, 10]
vole_random@data$bog1000[id_17] <- propcover_17[, 11]
vole_random@data$montane_inlandrock1000[id_17] <- propcover_17[, 12] 
vole_random@data$freshwater1000[id_17] <- propcover_17[,14]
vole_random@data$coastal_habitats1000[id_17] <- rowSums(propcover_17[, c(13,15:19), drop = F]) 
vole_random@data$urban1000[id_17] <- propcover_17[,20] 
vole_random@data$suburban1000[id_17] <- propcover_17[,21]
head(vole_random@data)


# 18
names(propcover_18)
vole_random@data$mixed_woodland1000[id_18] <- propcover_18[, 1] 
vole_random@data$conifer_woodland1000[id_18] <- propcover_18[, 2]
vole_random@data$arable_horticulture1000[id_18] <- propcover_18[, 3]
vole_random@data$improv_grassland1000[id_18] <- propcover_18[, 4] 
vole_random@data$neutral_grassland1000[id_18] <- propcover_18[, 5]
vole_random@data$calc_grassland1000[id_18] <- propcover_18[, 6]
vole_random@data$acid_grassland1000[id_18] <- propcover_18[, 7]
vole_random@data$fen_marsh_swamp1000[id_18] <- propcover_18[, 8]
vole_random@data$heather1000[id_18] <- propcover_18[, 9] 
vole_random@data$heather1000[id_18] <- propcover_18[, 10]
vole_random@data$bog1000[id_18] <- propcover_18[, 11]
vole_random@data$montane_inlandrock1000[id_18] <- propcover_18[, 12] 
vole_random@data$freshwater1000[id_18] <- propcover_18[,14]
vole_random@data$coastal_habitats1000[id_18] <- rowSums(propcover_18[, c(13,15:19), drop = F]) 
vole_random@data$urban1000[id_18] <- propcover_18[,20] 
vole_random@data$suburban1000[id_18] <- propcover_18[,21]
head(vole_random@data)


# 19
names(propcover_19)
vole_random@data$mixed_woodland1000[id_19] <- propcover_19[, 1] 
vole_random@data$conifer_woodland1000[id_19] <- propcover_19[, 2]
vole_random@data$arable_horticulture1000[id_19] <- propcover_19[, 3]
vole_random@data$improv_grassland1000[id_19] <- propcover_19[, 4] 
vole_random@data$neutral_grassland1000[id_19] <- propcover_19[, 5]
vole_random@data$calc_grassland1000[id_19] <- propcover_19[, 6]
vole_random@data$acid_grassland1000[id_19] <- propcover_19[, 7]
vole_random@data$fen_marsh_swamp1000[id_19] <- propcover_19[, 8]
vole_random@data$heather1000[id_19] <- propcover_19[, 9] 
vole_random@data$heather1000[id_19] <- propcover_19[, 10]
vole_random@data$bog1000[id_19] <- propcover_19[, 11]
vole_random@data$montane_inlandrock1000[id_19] <- propcover_19[, 12] 
vole_random@data$freshwater1000[id_19] <- propcover_19[,14]
vole_random@data$coastal_habitats1000[id_19] <- rowSums(propcover_19[, c(13,15:19), drop = F]) 
vole_random@data$urban1000[id_19] <- propcover_19[,20] 
vole_random@data$suburban1000[id_19] <- propcover_19[,21]
head(vole_random@data)
head(vole_random@data)


# Checking for NAs
sum(is.na(vole_random@data)) 
summary(vole_random@data)
# these are points in the sea. we will get rid of this at the very end before
# modelling


# Cleaning the environment
rm(list = setdiff(ls(), "vole_random"))
gc()



# save file
save(vole_random, file = file.path("watervole/watervole_random/Robjects",
                                   "vole3_landcoverprop.RData"))
