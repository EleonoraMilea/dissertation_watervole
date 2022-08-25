#' ---
#' title: "Land cover: Creating distance rasters"
#' author: "Eleonora Milea"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#'

#' ---
#' 
#' In this script we will calculate the distance of each land cover category
#' using raster::distance. This function calculates the distance, for all cells
#' that are NA, to the nearest cell that is not NA. The cell that is not NA will
#' be the targeted land type category.
#' Land cover raster files were downloaded from:
#' https://digimap.edina.ac.uk/environment
#' The 25 m resolution files were downloaded for different years: 2000, 2007,
#' 2015, 2017, 2018, 2019
#' These distances will be then used to extract the distance of each land cover
#' type from vole/mink observations 
#' 
#' ---
#'

library(raster)
library(beepr)

#'##### **Land cover 2000**
#'

land_00 <- raster("layers_27700/cover/landcover_2000.tif")
# land_00 <- raster("path for land cover from 2000")
unique(values(land_00))
raster_mixwood00 <- land_00 # copying raster file
values(raster_mixwood00)[values(raster_mixwood00)!= 11] <- NA # 11 is the code of mixwoods; transform everything that is not 11 into NA
values(raster_mixwood00)[!is.na(values(raster_mixwood00))] <- 0 # transform everything that is not na into 0 (hence "11")
dist_mixwood00 <- distance(raster_mixwood00) # raster of distances between pixels that don't have mixwoods and mixwoods
beep(2)
gc()


raster_conifer00 <- land_00
values(raster_conifer00)[values(raster_conifer00)!= 21] <- NA # 21 is the code of conifers
values(raster_conifer00)[!is.na(values(raster_conifer00))] <- 0
dist_conifer00 <- distance(raster_conifer00) # raster of distances between pixels that don't have conifers and conifers
gc()


raster_cereals00 <- land_00
values(raster_cereals00)[values(raster_cereals00)!= 41] <- NA # 41 is the code of cereals
values(raster_cereals00)[!is.na(values(raster_cereals00))] <- 0
dist_cereals00 <- distance(raster_cereals00) # raster of distances between pixels that don't have cerealss and cerealss
gc()


raster_horticulture00 <- land_00
values(raster_horticulture00)[values(raster_horticulture00)!= 42] <- NA # 42 is the code of horticultures
values(raster_horticulture00)[!is.na(values(raster_horticulture00))] <- 0
dist_horticulture00 <- distance(raster_horticulture00) # raster of distances between pixels that don't have horticultures and horticultures
gc()


raster_nonrotational00 <- land_00
values(raster_nonrotational00)[values(raster_nonrotational00)!= 43] <- NA # 43 is the code of arable nonrotationals
values(raster_nonrotational00)[!is.na(values(raster_nonrotational00))] <- 0
dist_nonrotational00 <- distance(raster_nonrotational00) # raster of distances between pixels that don't have nonrotationals and nonrotationals
gc()


raster_improvgrass00 <- land_00
values(raster_improvgrass00)[values(raster_improvgrass00)!= 51] <- NA # 51 is the code of improvgrasss
values(raster_improvgrass00)[!is.na(values(raster_improvgrass00))] <- 0
dist_improvgrass00 <- distance(raster_improvgrass00) # raster of distances between pixels that don't have improvgrasss and improvgrasss
gc()


raster_setasidegrass00 <- land_00
values(raster_setasidegrass00)[values(raster_setasidegrass00)!= 52] <- NA # 52 is the code of setasidegrasss
values(raster_setasidegrass00)[!is.na(values(raster_setasidegrass00))] <- 0
dist_setasidegrass00 <- distance(raster_setasidegrass00) # raster of distances between pixels that don't have setasidegrasss and setasidegrasss
gc()


raster_neutralgrass00 <- land_00
values(raster_neutralgrass00)[values(raster_neutralgrass00)!= 61] <- NA # 61 is the code of neutralgrasss
values(raster_neutralgrass00)[!is.na(values(raster_neutralgrass00))] <- 0
dist_neutralgrass00 <- distance(raster_neutralgrass00) # raster of distances between pixels that don't have neutralgrasss and neutralgrasss
gc()


raster_calcgrass00 <- land_00
values(raster_calcgrass00)[values(raster_calcgrass00)!= 71] <- NA # 71 is the code of calcgrasss
values(raster_calcgrass00)[!is.na(values(raster_calcgrass00))] <- 0
dist_calcgrass00 <- distance(raster_calcgrass00) # raster of distances between pixels that don't have calcgrasss and calcgrasss
gc()


raster_acidgrass00 <- land_00
values(raster_acidgrass00)[values(raster_acidgrass00)!= 81] <- NA # 81 is the code of acidgrasss
values(raster_acidgrass00)[!is.na(values(raster_acidgrass00))] <- 0
dist_acidgrass00 <- distance(raster_acidgrass00) # raster of distances between pixels that don't have acidgrasss and acidgrasss
gc()


raster_bracken00 <- land_00
values(raster_bracken00)[values(raster_bracken00)!= 91] <- NA # 91 is the code of brackens
values(raster_bracken00)[!is.na(values(raster_bracken00))] <- 0
dist_bracken00 <- distance(raster_bracken00) # raster of distances between pixels that don't have brackens and brackens
gc()


raster_densedwarf00 <- land_00
values(raster_densedwarf00)[values(raster_densedwarf00)!= 101] <- NA # 101 is the code of densedwarfs
values(raster_densedwarf00)[!is.na(values(raster_densedwarf00))] <- 0
dist_densedwarf00 <- distance(raster_densedwarf00) # raster of distances between pixels that don't have densedwarfs and densedwarfs
gc()


raster_opendwarf00 <- land_00
values(raster_opendwarf00)[values(raster_opendwarf00)!= 102] <- NA # 102 is the code of opendwarfs
values(raster_opendwarf00)[!is.na(values(raster_opendwarf00))] <- 0
dist_opendwarf00 <- distance(raster_opendwarf00) # raster of distances between pixels that don't have opendwarfs and opendwarfs
gc()


raster_fen00 <- land_00
values(raster_fen00)[values(raster_fen00)!= 111] <- NA # 111 is the code of fens
values(raster_fen00)[!is.na(values(raster_fen00))] <- 0
dist_fen00 <- distance(raster_fen00) # raster of distances between pixels that don't have fens and fens
gc()


raster_bog00 <- land_00
values(raster_bog00)[values(raster_bog00)!= 121] <- NA # 121 is the code of bogs
values(raster_bog00)[!is.na(values(raster_bog00))] <- 0
dist_bog00 <- distance(raster_bog00) # raster of distances between pixels that don't have bogs and bogs
gc()


raster_inlandwater00 <- land_00
values(raster_inlandwater00)[values(raster_inlandwater00)!= 131] <- NA # 131 is the code of inlandwaters
values(raster_inlandwater00)[!is.na(values(raster_inlandwater00))] <- 0
dist_inlandwater00 <- distance(raster_inlandwater00) # raster of distances between pixels that don't have inlandwaters and inlandwaters
gc()


raster_montane00 <- land_00
values(raster_montane00)[values(raster_montane00)!= 151] <- NA # 151 is the code of montanes
values(raster_montane00)[!is.na(values(raster_montane00))] <- 0
dist_montane00 <- distance(raster_montane00) # raster of distances between pixels that don't have montanes and montanes
gc()


raster_inlandrock00 <- land_00
values(raster_inlandrock00)[values(raster_inlandrock00)!= 161] <- NA # 161 is the code of inlandrocks
values(raster_inlandrock00)[!is.na(values(raster_inlandrock00))] <- 0
dist_inlandrock00 <- distance(raster_inlandrock00) # raster of distances between pixels that don't have inlandrocks and inlandrocks
gc()


raster_suburban00 <- land_00
values(raster_suburban00)[values(raster_suburban00)!= 171] <- NA # 171 is the code of suburbans
values(raster_suburban00)[!is.na(values(raster_suburban00))] <- 0
dist_suburban00 <- distance(raster_suburban00) # raster of distances between pixels that don't have suburbans and suburbans
gc()


raster_urban00 <- land_00
values(raster_urban00)[values(raster_urban00)!= 172] <- NA # 172 is the code of urbans
values(raster_urban00)[!is.na(values(raster_urban00))] <- 0
dist_urban00 <- distance(raster_urban00) # raster of distances between pixels that don't have urbans and urbans
gc()


raster_supralitrock00 <- land_00
values(raster_supralitrock00)[values(raster_supralitrock00)!= 181] <- NA # 181 is the code of supralitrocks
values(raster_supralitrock00)[!is.na(values(raster_supralitrock00))] <- 0
dist_supralitrock00 <- distance(raster_supralitrock00) # raster of distances between pixels that don't have supralitrocks and supralitrocks
gc()


raster_supralitsediment00 <- land_00
values(raster_supralitsediment00)[values(raster_supralitsediment00)!= 191] <- NA # 191 is the code of supralitsediments
values(raster_supralitsediment00)[!is.na(values(raster_supralitsediment00))] <- 0
dist_supralitsediment00 <- distance(raster_supralitsediment00) # raster of distances between pixels that don't have supralitsediments and supralitsediments
gc()


raster_litrock00 <- land_00
values(raster_litrock00)[values(raster_litrock00)!= 201] <- NA # 201 is the code of litrocks
values(raster_litrock00)[!is.na(values(raster_litrock00))] <- 0
dist_litrock00 <- distance(raster_litrock00) # raster of distances between pixels that don't have litrocks and litrocks
gc()


raster_litsediment00 <- land_00
values(raster_litsediment00)[values(raster_litsediment00)!= 211] <- NA # 211 is the code of litsediments
values(raster_litsediment00)[!is.na(values(raster_litsediment00))] <- 0
dist_litsediment00 <- distance(raster_litsediment00) # raster of distances between pixels that don't have litsediments and litsediments
gc()


raster_saltmarsh00 <- land_00
values(raster_saltmarsh00)[values(raster_saltmarsh00)!= 212] <- NA # 212 is the code of saltmarshs
values(raster_saltmarsh00)[!is.na(values(raster_saltmarsh00))] <- 0
dist_saltmarsh00 <- distance(raster_saltmarsh00) # raster of distances between pixels that don't have saltmarshs and saltmarshs
gc()


raster_estuarysea00 <- land_00
values(raster_estuarysea00)[values(raster_estuarysea00)!= 221] <- NA # 221 is the code of estuaryseas
values(raster_estuarysea00)[!is.na(values(raster_estuarysea00))] <- 0
dist_estuarysea00 <- distance(raster_estuarysea00) # raster of distances between pixels that don't have estuaryseas and estuaryseas
gc()


#'#### **Land cover 2007**
#'

# land_07 <- raster("layers_27700/cover/landcover_2007.tif")
# land_07 <- raster("path for land cover from 2007")
# unique(land_07)
raster_mixwood07 <- land_07
values(raster_mixwood07)[values(raster_mixwood07)!= 1] <- NA # 1 is the code of mixwoods
values(raster_mixwood07)[!is.na(values(raster_mixwood07))] <- 0
dist_mixwood07 <- distance(raster_mixwood07) # raster of distances between pixels that don't have mixwoods and mixwoods
gc()


raster_conifer07 <- land_07
values(raster_conifer07)[values(raster_conifer07)!= 2] <- NA # 2 is the code of conifers
values(raster_conifer07)[!is.na(values(raster_conifer07))] <- 0
dist_conifer07 <- distance(raster_conifer07) # raster of distances between pixels that don't have conifers and conifers
gc()


raster_arable07 <- land_07
values(raster_arable07)[values(raster_arable07)!= 3] <- NA # 3 is the code of arables
values(raster_arable07)[!is.na(values(raster_arable07))] <- 0
dist_arable07 <- distance(raster_arable07) # raster of distances between pixels that don't have arables and arables
gc()


raster_improvgrass07 <- land_07
values(raster_improvgrass07)[values(raster_improvgrass07)!= 4] <- NA # 4 is the code of improvgrasss
values(raster_improvgrass07)[!is.na(values(raster_improvgrass07))] <- 0
dist_improvgrass07 <- distance(raster_improvgrass07) # raster of distances between pixels that don't have improvgrasss and improvgrasss
gc()


raster_roughgrass07 <- land_07
values(raster_roughgrass07)[values(raster_roughgrass07)!= 5] <- NA # 5 is the code of roughgrasss
values(raster_roughgrass07)[!is.na(values(raster_roughgrass07))] <- 0
dist_roughgrass07 <- distance(raster_roughgrass07) # raster of distances between pixels that don't have roughgrasss and roughgrasss
gc()


raster_neutralgrass07 <- land_07
values(raster_neutralgrass07)[values(raster_neutralgrass07)!= 6] <- NA # 6 is the code of neutralgrasss
values(raster_neutralgrass07)[!is.na(values(raster_neutralgrass07))] <- 0
dist_neutralgrass07 <- distance(raster_neutralgrass07) # raster of distances between pixels that don't have neutralgrasss and neutralgrasss
gc()


raster_calcgrass07 <- land_07
values(raster_calcgrass07)[values(raster_calcgrass07)!= 7] <- NA # 7 is the code of calcgrasss
values(raster_calcgrass07)[!is.na(values(raster_calcgrass07))] <- 0
dist_calcgrass07 <- distance(raster_calcgrass07) # raster of distances between pixels that don't have calcgrasss and calcgrasss
gc()


raster_acidgrass07 <- land_07
values(raster_acidgrass07)[values(raster_acidgrass07)!= 8] <- NA # 8 is the code of acidgrasss
values(raster_acidgrass07)[!is.na(values(raster_acidgrass07))] <- 0
dist_acidgrass07 <- distance(raster_acidgrass07) # raster of distances between pixels that don't have acidgrasss and acidgrasss
gc()


raster_fen07 <- land_07
values(raster_fen07)[values(raster_fen07)!= 9] <- NA # 9 is the code of fens
values(raster_fen07)[!is.na(values(raster_fen07))] <- 0
dist_fen07 <- distance(raster_fen07) # raster of distances between pixels that don't have fens and fens
gc()


raster_heather07 <- land_07
values(raster_heather07)[values(raster_heather07)!= 10] <- NA # 10 is the code of heathers
values(raster_heather07)[!is.na(values(raster_heather07))] <- 0
dist_heather07 <- distance(raster_heather07) # raster of distances between pixels that don't have heathers and heathers
gc()


raster_heathergrass07 <- land_07
values(raster_heathergrass07)[values(raster_heathergrass07)!= 11] <- NA # 11 is the code of heathergrass
values(raster_heathergrass07)[!is.na(values(raster_heathergrass07))] <- 0
dist_heathergrass07 <- distance(raster_heathergrass07) # raster of distances between pixels that don't have heathers and heathers
gc()


raster_bog07 <- land_07
values(raster_bog07)[values(raster_bog07)!= 12] <- NA # 12 is the code of bogs
values(raster_bog07)[!is.na(values(raster_bog07))] <- 0
dist_bog07 <- distance(raster_bog07) # raster of distances between pixels that don't have bogs and bogs
gc()


raster_montane07 <- land_07
values(raster_montane07)[values(raster_montane07)!= 13] <- NA # 13 is the code of montanes
values(raster_montane07)[!is.na(values(raster_montane07))] <- 0
dist_montane07 <- distance(raster_montane07) # raster of distances between pixels that don't have montanes and montanes
gc()


raster_inlandrock07 <- land_07
values(raster_inlandrock07)[values(raster_inlandrock07)!= 14] <- NA # 14 is the code of inlandrocks
values(raster_inlandrock07)[!is.na(values(raster_inlandrock07))] <- 0
dist_inlandrock07 <- distance(raster_inlandrock07) # raster of distances between pixels that don't have inlandrocks and inlandrocks
gc()


raster_saltwater07 <- land_07
values(raster_saltwater07)[values(raster_saltwater07)!= 15] <- NA # 15 is the code of saltwaters
values(raster_saltwater07)[!is.na(values(raster_saltwater07))] <- 0
dist_saltwater07 <- distance(raster_saltwater07) # raster of distances between pixels that don't have saltwaters and saltwaters
gc()


raster_rivers07 <- land_07
values(raster_rivers07)[values(raster_rivers07)!= 16] <- NA # 16 is the code of rivers
values(raster_rivers07)[!is.na(values(raster_rivers07))] <- 0
dist_rivers07 <- distance(raster_rivers07) # raster of distances between pixels that don't have riverss and riverss
gc()


raster_supralitrock07 <- land_07
values(raster_supralitrock07)[values(raster_supralitrock07)!= 17] <- NA # 17 is the code of supralitrocks
values(raster_supralitrock07)[!is.na(values(raster_supralitrock07))] <- 0
dist_supralitrock07 <- distance(raster_supralitrock07) # raster of distances between pixels that don't have supralitrocks and supralitrocks
gc()


raster_supralitsediment07 <- land_07
values(raster_supralitsediment07)[values(raster_supralitsediment07)!= 18] <- NA # 18 is the code of supralitsediments
values(raster_supralitsediment07)[!is.na(values(raster_supralitsediment07))] <- 0
dist_supralitsediment07 <- distance(raster_supralitsediment07) # raster of distances between pixels that don't have supralitsediments and supralitsediments
gc()


raster_litrock07 <- land_07
values(raster_litrock07)[values(raster_litrock07)!= 19] <- NA # 19 is the code of litrocks
values(raster_litrock07)[!is.na(values(raster_litrock07))] <- 0
dist_litrock07 <- distance(raster_litrock07) # raster of distances between pixels that don't have litrocks and litrocks
gc()


raster_litsediment07 <- land_07
values(raster_litsediment07)[values(raster_litsediment07)!= 20] <- NA # 20 is the code of litsediments
values(raster_litsediment07)[!is.na(values(raster_litsediment07))] <- 0
dist_litsediment07 <- distance(raster_litsediment07) # raster of distances between pixels that don't have litsediments and litsediments
gc()


raster_saltmarsh07 <- land_07
values(raster_saltmarsh07)[values(raster_saltmarsh07)!= 21] <- NA # 21 is the code of saltmarshs
values(raster_saltmarsh07)[!is.na(values(raster_saltmarsh07))] <- 0
dist_saltmarsh07 <- distance(raster_saltmarsh07) # raster of distances between pixels that don't have saltmarshs and saltmarshs
gc()


raster_urban07 <- land_07
values(raster_urban07)[values(raster_urban07)!= 22] <- NA # 22 is the code of urbans
values(raster_urban07)[!is.na(values(raster_urban07))] <- 0
dist_urban07 <- distance(raster_urban07) # raster of distances between pixels that don't have urbans and urbans
gc()


raster_suburban07 <- land_07
values(raster_suburban07)[values(raster_suburban07)!= 23] <- NA # 23 is the code of suburbans
values(raster_suburban07)[!is.na(values(raster_suburban07))] <- 0
dist_suburban07 <- distance(raster_suburban07) # raster of distances between pixels that don't have suburbans and suburbans
gc()


#'#### **Land cover 2015**
#'

# land_15 <- raster("layers_27700/cover/landcover_2015.tif")
# land_15 <- raster("path for land cover from 2015")
# unique(land_15)
raster_mixwood15 <- land_15
values(raster_mixwood15)[values(raster_mixwood15)!= 1] <- NA # 1 is the code of mixwoods
values(raster_mixwood15)[!is.na(values(raster_mixwood15))] <- 0
dist_mixwood15 <- distance(raster_mixwood15) # raster of distances between pixels that don't have mixwoods and mixwoods
gc()


raster_conifer15 <- land_15
values(raster_conifer15)[values(raster_conifer15)!= 2] <- NA # 2 is the code of conifers
values(raster_conifer15)[!is.na(values(raster_conifer15))] <- 0
dist_conifer15 <- distance(raster_conifer15) # raster of distances between pixels that don't have conifers and conifers
gc()


raster_arable15 <- land_15
values(raster_arable15)[values(raster_arable15)!= 3] <- NA # 3 is the code of arables
values(raster_arable15)[!is.na(values(raster_arable15))] <- 0
dist_arable15 <- distance(raster_arable15) # raster of distances between pixels that don't have arables and arables
gc()


raster_improvgrass15 <- land_15
values(raster_improvgrass15)[values(raster_improvgrass15)!= 4] <- NA # 4 is the code of improvgrasss
values(raster_improvgrass15)[!is.na(values(raster_improvgrass15))] <- 0
dist_improvgrass15 <- distance(raster_improvgrass15) # raster of distances between pixels that don't have improvgrasss and improvgrasss
gc()


raster_neutralgrass15 <- land_15
values(raster_neutralgrass15)[values(raster_neutralgrass15)!= 5] <- NA # 5 is the code of neutralgrasss
values(raster_neutralgrass15)[!is.na(values(raster_neutralgrass15))] <- 0
dist_neutralgrass15 <- distance(raster_neutralgrass15) # raster of distances between pixels that don't have neutralgrasss and neutralgrasss
gc()


raster_calcgrass15 <- land_15
values(raster_calcgrass15)[values(raster_calcgrass15)!= 6] <- NA # 6 is the code of calcgrasss
values(raster_calcgrass15)[!is.na(values(raster_calcgrass15))] <- 0
dist_calcgrass15 <- distance(raster_calcgrass15) # raster of distances between pixels that don't have calcgrasss and calcgrasss
gc()


raster_acidgrass15 <- land_15
values(raster_acidgrass15)[values(raster_acidgrass15)!= 7] <- NA # 7 is the code of acidgrasss
values(raster_acidgrass15)[!is.na(values(raster_acidgrass15))] <- 0
dist_acidgrass15 <- distance(raster_acidgrass15) # raster of distances between pixels that don't have acidgrasss and acidgrasss
gc()


raster_fen15 <- land_15
values(raster_fen15)[values(raster_fen15)!= 8] <- NA # 8 is the code of fens
values(raster_fen15)[!is.na(values(raster_fen15))] <- 0
dist_fen15 <- distance(raster_fen15) # raster of distances between pixels that don't have fens and fens
gc()


raster_heather15 <- land_15
values(raster_heather15)[values(raster_heather15)!= 9] <- NA # 9 is the code of heathers
values(raster_heather15)[!is.na(values(raster_heather15))] <- 0
dist_heather15 <- distance(raster_heather15) # raster of distances between pixels that don't have heathers and heathers
gc()


raster_heathergrass15 <- land_15
values(raster_heathergrass15)[values(raster_heathergrass15)!= 10] <- NA # 10 is the code of heathergrass
values(raster_heathergrass15)[!is.na(values(raster_heathergrass15))] <- 0
dist_heathergrass15 <- distance(raster_heathergrass15) # raster of distances between pixels that don't have heathers and heathers
gc()


raster_bog15 <- land_15
values(raster_bog15)[values(raster_bog15)!= 11] <- NA # 11 is the code of bogs
values(raster_bog15)[!is.na(values(raster_bog15))] <- 0
dist_bog15 <- distance(raster_bog15) # raster of distances between pixels that don't have bogs and bogs
gc()


raster_inlandrock15 <- land_15
values(raster_inlandrock15)[values(raster_inlandrock15)!= 12] <- NA # 12 is the code of inlandrocks
values(raster_inlandrock15)[!is.na(values(raster_inlandrock15))] <- 0
dist_inlandrock15 <- distance(raster_inlandrock15) # raster of distances between pixels that don't have inlandrocks and inlandrocks
gc()


raster_saltwater15 <- land_15
values(raster_saltwater15)[values(raster_saltwater15)!= 13] <- NA # 13 is the code of saltwaters
values(raster_saltwater15)[!is.na(values(raster_saltwater15))] <- 0
dist_saltwater15 <- distance(raster_saltwater15) # raster of distances between pixels that don't have saltwaters and saltwaters
gc()


raster_rivers15 <- land_15
values(raster_rivers15)[values(raster_rivers15)!= 14] <- NA # 14 is the code of riverss
values(raster_rivers15)[!is.na(values(raster_rivers15))] <- 0
dist_rivers15 <- distance(raster_rivers15) # raster of distances between pixels that don't have riverss and riverss
gc()


raster_supralitrock15 <- land_15
values(raster_supralitrock15)[values(raster_supralitrock15)!= 15] <- NA # 15 is the code of supralitrocks
values(raster_supralitrock15)[!is.na(values(raster_supralitrock15))] <- 0
dist_supralitrock15 <- distance(raster_supralitrock15) # raster of distances between pixels that don't have supralitrocks and supralitrocks
gc()


raster_supralitsediment15 <- land_15
values(raster_supralitsediment15)[values(raster_supralitsediment15)!= 16] <- NA # 16 is the code of supralitsediments
values(raster_supralitsediment15)[!is.na(values(raster_supralitsediment15))] <- 0
dist_supralitsediment15 <- distance(raster_supralitsediment15) # raster of distances between pixels that don't have supralitsediments and supralitsediments
gc()


raster_litrock15 <- land_15
values(raster_litrock15)[values(raster_litrock15)!= 17] <- NA # 17 is the code of litrocks
values(raster_litrock15)[!is.na(values(raster_litrock15))] <- 0
dist_litrock15 <- distance(raster_litrock15) # raster of distances between pixels that don't have litrocks and litrocks
gc()


raster_litsediment15 <- land_15
values(raster_litsediment15)[values(raster_litsediment15)!= 18] <- NA # 18 is the code of litsediments
values(raster_litsediment15)[!is.na(values(raster_litsediment15))] <- 0
dist_litsediment15 <- distance(raster_litsediment15) # raster of distances between pixels that don't have litsediments and litsediments
gc()


raster_saltmarsh15 <- land_15
values(raster_saltmarsh15)[values(raster_saltmarsh15)!= 19] <- NA # 19 is the code of saltmarshs
values(raster_saltmarsh15)[!is.na(values(raster_saltmarsh15))] <- 0
dist_saltmarsh15 <- distance(raster_saltmarsh15) # raster of distances between pixels that don't have saltmarshs and saltmarshs
gc()


raster_urban15 <- land_15
values(raster_urban15)[values(raster_urban15)!= 20] <- NA # 20 is the code of urbans
values(raster_urban15)[!is.na(values(raster_urban15))] <- 0
dist_urban15 <- distance(raster_urban15) # raster of distances between pixels that don't have urbans and urbans
gc()


raster_suburban15 <- land_15
values(raster_suburban15)[values(raster_suburban15)!= 21] <- NA # 21 is the code of suburbans
values(raster_suburban15)[!is.na(values(raster_suburban15))] <- 0
dist_suburban15 <- distance(raster_suburban15) # raster of distances between pixels that don't have suburbans and suburbans
gc()


#'#### **Land cover 2017**
#'
land_17 <- raster("layers_27700/cover/landcover_2017.tif")
# land_17 <- raster("path for land cover from 2017")
# unique(land_17)
raster_mixwood17 <- land_17
values(raster_mixwood17)[values(raster_mixwood17)!= 1] <- NA # 1 is the code of mixwoods
values(raster_mixwood17)[!is.na(values(raster_mixwood17))] <- 0
dist_mixwood17 <- distance(raster_mixwood17) # raster of distances between pixels that don't have mixwoods and mixwoods
gc()


raster_conifer17 <- land_17
values(raster_conifer17)[values(raster_conifer17)!= 2] <- NA # 2 is the code of conifers
values(raster_conifer17)[!is.na(values(raster_conifer17))] <- 0
dist_conifer17 <- distance(raster_conifer17) # raster of distances between pixels that don't have conifers and conifers
gc()


raster_arable17 <- land_17
values(raster_arable17)[values(raster_arable17)!= 3] <- NA # 3 is the code of arables
values(raster_arable17)[!is.na(values(raster_arable17))] <- 0
dist_arable17 <- distance(raster_arable17) # raster of distances between pixels that don't have arables and arables
gc()


raster_improvgrass17 <- land_17
values(raster_improvgrass17)[values(raster_improvgrass17)!= 4] <- NA # 4 is the code of improvgrasss
values(raster_improvgrass17)[!is.na(values(raster_improvgrass17))] <- 0
dist_improvgrass17 <- distance(raster_improvgrass17) # raster of distances between pixels that don't have improvgrasss and improvgrasss
gc()


raster_neutralgrass17 <- land_17
values(raster_neutralgrass17)[values(raster_neutralgrass17)!= 5] <- NA # 5 is the code of neutralgrasss
values(raster_neutralgrass17)[!is.na(values(raster_neutralgrass17))] <- 0
dist_neutralgrass17 <- distance(raster_neutralgrass17) # raster of distances between pixels that don't have neutralgrasss and neutralgrasss
gc()


raster_calcgrass17 <- land_17
values(raster_calcgrass17)[values(raster_calcgrass17)!= 6] <- NA # 6 is the code of calcgrasss
values(raster_calcgrass17)[!is.na(values(raster_calcgrass17))] <- 0
dist_calcgrass17 <- distance(raster_calcgrass17) # raster of distances between pixels that don't have calcgrasss and calcgrasss
gc()


raster_acidgrass17 <- land_17
values(raster_acidgrass17)[values(raster_acidgrass17)!= 7] <- NA # 7 is the code of acidgrasss
values(raster_acidgrass17)[!is.na(values(raster_acidgrass17))] <- 0
dist_acidgrass17 <- distance(raster_acidgrass17) # raster of distances between pixels that don't have acidgrasss and acidgrasss
gc()


raster_fen17 <- land_17
values(raster_fen17)[values(raster_fen17)!= 8] <- NA # 8 is the code of fens
values(raster_fen17)[!is.na(values(raster_fen17))] <- 0
dist_fen17 <- distance(raster_fen17) # raster of distances between pixels that don't have fens and fens
gc()


raster_heather17 <- land_17
values(raster_heather17)[values(raster_heather17)!= 9] <- NA # 9 is the code of heathers
values(raster_heather17)[!is.na(values(raster_heather17))] <- 0
dist_heather17 <- distance(raster_heather17) # raster of distances between pixels that don't have heathers and heathers
gc()


raster_heathergrass17 <- land_17
values(raster_heathergrass17)[values(raster_heathergrass17)!= 10] <- NA # 10 is the code of heathergrass
values(raster_heathergrass17)[!is.na(values(raster_heathergrass17))] <- 0
dist_heathergrass17 <- distance(raster_heathergrass17) # raster of distances between pixels that don't have heathers and heathers
gc()


raster_bog17 <- land_17
values(raster_bog17)[values(raster_bog17)!= 11] <- NA # 11 is the code of bogs
values(raster_bog17)[!is.na(values(raster_bog17))] <- 0
dist_bog17 <- distance(raster_bog17) # raster of distances between pixels that don't have bogs and bogs
gc()


raster_inlandrock17 <- land_17
values(raster_inlandrock17)[values(raster_inlandrock17)!= 12] <- NA # 12 is the code of inlandrocks
values(raster_inlandrock17)[!is.na(values(raster_inlandrock17))] <- 0
dist_inlandrock17 <- distance(raster_inlandrock17) # raster of distances between pixels that don't have inlandrocks and inlandrocks
gc()


raster_saltwater17 <- land_17
values(raster_saltwater17)[values(raster_saltwater17)!= 13] <- NA # 13 is the code of saltwaters
values(raster_saltwater17)[!is.na(values(raster_saltwater17))] <- 0
dist_saltwater17 <- distance(raster_saltwater17) # raster of distances between pixels that don't have saltwaters and saltwaters
gc()


raster_rivers17 <- land_17
values(raster_rivers17)[values(raster_rivers17)!= 14] <- NA # 14 is the code of riverss
values(raster_rivers17)[!is.na(values(raster_rivers17))] <- 0
dist_rivers17 <- distance(raster_rivers17) # raster of distances between pixels that don't have riverss and riverss
gc()


raster_supralitrock17 <- land_17
values(raster_supralitrock17)[values(raster_supralitrock17)!= 15] <- NA # 15 is the code of supralitrocks
values(raster_supralitrock17)[!is.na(values(raster_supralitrock17))] <- 0
dist_supralitrock17 <- distance(raster_supralitrock17) # raster of distances between pixels that don't have supralitrocks and supralitrocks
writeRaster(dist_supralitrock17,
            filename = file.path("C:/Users/elemi.LAPTOP-VIAG61N1/OneDrive - University of Glasgow/dissertation_watervole/layers_27700/coverdist_17",
                                 "dist_supralitrock17copy"))
rm(raster_supralitrock17)
rm(dist_supralitrock17)
gc()


raster_supralitsediment17 <- land_17
values(raster_supralitsediment17)[values(raster_supralitsediment17)!= 16] <- NA # 16 is the code of supralitsediments
values(raster_supralitsediment17)[!is.na(values(raster_supralitsediment17))] <- 0
dist_supralitsediment17 <- distance(raster_supralitsediment17) # raster of distances between pixels that don't have supralitsediments and supralitsediments
gc()


raster_litrock17 <- land_17
values(raster_litrock17)[values(raster_litrock17)!= 17] <- NA # 17 is the code of litrocks
values(raster_litrock17)[!is.na(values(raster_litrock17))] <- 0
dist_litrock17 <- distance(raster_litrock17) # raster of distances between pixels that don't have litrocks and litrocks
gc()


raster_litsediment17 <- land_17
values(raster_litsediment17)[values(raster_litsediment17)!= 18] <- NA # 18 is the code of litsediments
values(raster_litsediment17)[!is.na(values(raster_litsediment17))] <- 0
dist_litsediment17 <- distance(raster_litsediment17) # raster of distances between pixels that don't have litsediments and litsediments
gc()


raster_saltmarsh17 <- land_17
values(raster_saltmarsh17)[values(raster_saltmarsh17)!= 19] <- NA # 19 is the code of saltmarshs
values(raster_saltmarsh17)[!is.na(values(raster_saltmarsh17))] <- 0
dist_saltmarsh17 <- distance(raster_saltmarsh17) # raster of distances between pixels that don't have saltmarshs and saltmarshs
gc()


raster_urban17 <- land_17
values(raster_urban17)[values(raster_urban17)!= 20] <- NA # 20 is the code of urbans
values(raster_urban17)[!is.na(values(raster_urban17))] <- 0
dist_urban17 <- distance(raster_urban17) # raster of distances between pixels that don't have urbans and urbans
gc()


raster_suburban17 <- land_17
values(raster_suburban17)[values(raster_suburban17)!= 21] <- NA # 21 is the code of suburbans
values(raster_suburban17)[!is.na(values(raster_suburban17))] <- 0
dist_suburban17 <- distance(raster_suburban17) # raster of distances between pixels that don't have suburbans and suburbans
gc()


#'#### **Land cover 2018**
#'

land_18 <- raster("layers_27700/cover/landcover_2018.tif")
# land_18 <- raster("path for land cover from 2018")
# unique(land_18)
raster_mixwood18 <- land_18
values(raster_mixwood18)[values(raster_mixwood18)!= 1] <- NA # 1 is the code of mixwoods
values(raster_mixwood18)[!is.na(values(raster_mixwood18))] <- 0
dist_mixwood18 <- distance(raster_mixwood18) # raster of distances between pixels that don't have mixwoods and mixwoods
gc()


raster_conifer18 <- land_18
values(raster_conifer18)[values(raster_conifer18)!= 2] <- NA # 2 is the code of conifers
values(raster_conifer18)[!is.na(values(raster_conifer18))] <- 0
dist_conifer18 <- distance(raster_conifer18) # raster of distances between pixels that don't have conifers and conifers
gc()


raster_arable18 <- land_18
values(raster_arable18)[values(raster_arable18)!= 3] <- NA # 3 is the code of arables
values(raster_arable18)[!is.na(values(raster_arable18))] <- 0
dist_arable18 <- distance(raster_arable18) # raster of distances between pixels that don't have arables and arables
gc()


raster_improvgrass18 <- land_18
values(raster_improvgrass18)[values(raster_improvgrass18)!= 4] <- NA # 4 is the code of improvgrasss
values(raster_improvgrass18)[!is.na(values(raster_improvgrass18))] <- 0
dist_improvgrass18 <- distance(raster_improvgrass18) # raster of distances between pixels that don't have improvgrasss and improvgrasss
gc()


raster_neutralgrass18 <- land_18
values(raster_neutralgrass18)[values(raster_neutralgrass18)!= 5] <- NA # 5 is the code of neutralgrasss
values(raster_neutralgrass18)[!is.na(values(raster_neutralgrass18))] <- 0
dist_neutralgrass18 <- distance(raster_neutralgrass18) # raster of distances between pixels that don't have neutralgrasss and neutralgrasss
gc()


raster_calcgrass18 <- land_18
values(raster_calcgrass18)[values(raster_calcgrass18)!= 6] <- NA # 6 is the code of calcgrasss
values(raster_calcgrass18)[!is.na(values(raster_calcgrass18))] <- 0
dist_calcgrass18 <- distance(raster_calcgrass18) # raster of distances between pixels that don't have calcgrasss and calcgrasss
gc()


raster_acidgrass18 <- land_18
values(raster_acidgrass18)[values(raster_acidgrass18)!= 7] <- NA # 7 is the code of acidgrasss
values(raster_acidgrass18)[!is.na(values(raster_acidgrass18))] <- 0
dist_acidgrass18 <- distance(raster_acidgrass18) # raster of distances between pixels that don't have acidgrasss and acidgrasss
gc()


raster_fen18 <- land_18
values(raster_fen18)[values(raster_fen18)!= 8] <- NA # 8 is the code of fens
values(raster_fen18)[!is.na(values(raster_fen18))] <- 0
dist_fen18 <- distance(raster_fen18) # raster of distances between pixels that don't have fens and fens
gc()


raster_heather18 <- land_18
values(raster_heather18)[values(raster_heather18)!= 9] <- NA # 9 is the code of heathers
values(raster_heather18)[!is.na(values(raster_heather18))] <- 0
dist_heather18 <- distance(raster_heather18) # raster of distances between pixels that don't have heathers and heathers
gc()


raster_heathergrass18 <- land_18
values(raster_heathergrass18)[values(raster_heathergrass18)!= 10] <- NA # 10 is the code of heathergrass
values(raster_heathergrass18)[!is.na(values(raster_heathergrass18))] <- 0
dist_heathergrass18 <- distance(raster_heathergrass18) # raster of distances between pixels that don't have heathers and heathers
gc()


raster_bog18 <- land_18
values(raster_bog18)[values(raster_bog18)!= 11] <- NA # 11 is the code of bogs
values(raster_bog18)[!is.na(values(raster_bog18))] <- 0
dist_bog18 <- distance(raster_bog18) # raster of distances between pixels that don't have bogs and bogs
gc()


raster_inlandrock18 <- land_18
values(raster_inlandrock18)[values(raster_inlandrock18)!= 12] <- NA # 12 is the code of inlandrocks
values(raster_inlandrock18)[!is.na(values(raster_inlandrock18))] <- 0
dist_inlandrock18 <- distance(raster_inlandrock18) # raster of distances between pixels that don't have inlandrocks and inlandrocks
gc()


raster_saltwater18 <- land_18
values(raster_saltwater18)[values(raster_saltwater18)!= 13] <- NA # 13 is the code of saltwaters
values(raster_saltwater18)[!is.na(values(raster_saltwater18))] <- 0
dist_saltwater18 <- distance(raster_saltwater18) # raster of distances between pixels that don't have saltwaters and saltwaters
gc()


raster_rivers18 <- land_18
values(raster_rivers18)[values(raster_rivers18)!= 14] <- NA # 14 is the code of riverss
values(raster_rivers18)[!is.na(values(raster_rivers18))] <- 0
dist_rivers18 <- distance(raster_rivers18) # raster of distances between pixels that don't have riverss and riverss
gc()


raster_supralitrock18 <- land_18
values(raster_supralitrock18)[values(raster_supralitrock18)!= 15] <- NA # 15 is the code of supralitrocks
values(raster_supralitrock18)[!is.na(values(raster_supralitrock18))] <- 0
dist_supralitrock18 <- distance(raster_supralitrock18) # raster of distances between pixels that don't have supralitrocks and supralitrocks
writeRaster(dist_supralitrock18,
            filename = file.path("C:/Users/elemi.LAPTOP-VIAG61N1/OneDrive - University of Glasgow/dissertation_watervole/layers_27700/coverdist_18",
                                 "dist_supralitrock18copy"))
rm(raster_supralitrock18)
rm(dist_supralitrock18)
gc()


raster_supralitsediment18 <- land_18
values(raster_supralitsediment18)[values(raster_supralitsediment18)!= 16] <- NA # 16 is the code of supralitsediments
values(raster_supralitsediment18)[!is.na(values(raster_supralitsediment18))] <- 0
dist_supralitsediment18 <- distance(raster_supralitsediment18) # raster of distances between pixels that don't have supralitsediments and supralitsediments
gc()


raster_litrock18 <- land_18
values(raster_litrock18)[values(raster_litrock18)!= 17] <- NA # 17 is the code of litrocks
values(raster_litrock18)[!is.na(values(raster_litrock18))] <- 0
dist_litrock18 <- distance(raster_litrock18) # raster of distances between pixels that don't have litrocks and litrocks
gc()


raster_litsediment18 <- land_18
values(raster_litsediment18)[values(raster_litsediment18)!= 18] <- NA # 18 is the code of litsediments
values(raster_litsediment18)[!is.na(values(raster_litsediment18))] <- 0
dist_litsediment18 <- distance(raster_litsediment18) # raster of distances between pixels that don't have litsediments and litsediments
gc()


raster_saltmarsh18 <- land_18
values(raster_saltmarsh18)[values(raster_saltmarsh18)!= 19] <- NA # 19 is the code of saltmarshs
values(raster_saltmarsh18)[!is.na(values(raster_saltmarsh18))] <- 0
dist_saltmarsh18 <- distance(raster_saltmarsh18) # raster of distances between pixels that don't have saltmarshs and saltmarshs
gc()


raster_urban18 <- land_18
values(raster_urban18)[values(raster_urban18)!= 20] <- NA # 20 is the code of urbans
values(raster_urban18)[!is.na(values(raster_urban18))] <- 0
dist_urban18 <- distance(raster_urban18) # raster of distances between pixels that don't have urbans and urbans
gc()


raster_suburban18 <- land_18
values(raster_suburban18)[values(raster_suburban18)!= 21] <- NA # 21 is the code of suburbans
values(raster_suburban18)[!is.na(values(raster_suburban18))] <- 0
dist_suburban18 <- distance(raster_suburban18) # raster of distances between pixels that don't have suburbans and suburbans
gc()


#'#### **Land cover 2019**
#'

land_19 <- raster("layers_27700/cover/landcover_2019.tif")
# land_19 <- raster("path for land cover from 2019")
# unique(land_19)
raster_mixwood19 <- land_19
values(raster_mixwood19)[values(raster_mixwood19)!= 1] <- NA # 1 is the code of mixwoods
values(raster_mixwood19)[!is.na(values(raster_mixwood19))] <- 0
dist_mixwood19 <- distance(raster_mixwood19) # raster of distances between pixels that don't have mixwoods and mixwoods
gc()


raster_conifer19 <- land_19
values(raster_conifer19)[values(raster_conifer19)!= 2] <- NA # 2 is the code of conifers
values(raster_conifer19)[!is.na(values(raster_conifer19))] <- 0
dist_conifer19 <- distance(raster_conifer19) # raster of distances between pixels that don't have conifers and conifers
gc()


raster_arable19 <- land_19
values(raster_arable19)[values(raster_arable19)!= 3] <- NA # 3 is the code of arables
values(raster_arable19)[!is.na(values(raster_arable19))] <- 0
dist_arable19 <- distance(raster_arable19) # raster of distances between pixels that don't have arables and arables
gc()


raster_improvgrass19 <- land_19
values(raster_improvgrass19)[values(raster_improvgrass19)!= 4] <- NA # 4 is the code of improvgrasss
values(raster_improvgrass19)[!is.na(values(raster_improvgrass19))] <- 0
dist_improvgrass19 <- distance(raster_improvgrass19) # raster of distances between pixels that don't have improvgrasss and improvgrasss
gc()


raster_neutralgrass19 <- land_19
values(raster_neutralgrass19)[values(raster_neutralgrass19)!= 5] <- NA # 5 is the code of neutralgrasss
values(raster_neutralgrass19)[!is.na(values(raster_neutralgrass19))] <- 0
dist_neutralgrass19 <- distance(raster_neutralgrass19) # raster of distances between pixels that don't have neutralgrasss and neutralgrasss
gc()


raster_calcgrass19 <- land_19
values(raster_calcgrass19)[values(raster_calcgrass19)!= 6] <- NA # 6 is the code of calcgrasss
values(raster_calcgrass19)[!is.na(values(raster_calcgrass19))] <- 0
dist_calcgrass19 <- distance(raster_calcgrass19) # raster of distances between pixels that don't have calcgrasss and calcgrasss
gc()


raster_acidgrass19 <- land_19
values(raster_acidgrass19)[values(raster_acidgrass19)!= 7] <- NA # 7 is the code of acidgrasss
values(raster_acidgrass19)[!is.na(values(raster_acidgrass19))] <- 0
dist_acidgrass19 <- distance(raster_acidgrass19) # raster of distances between pixels that don't have acidgrasss and acidgrasss
gc()


raster_fen19 <- land_19
values(raster_fen19)[values(raster_fen19)!= 8] <- NA # 8 is the code of fens
values(raster_fen19)[!is.na(values(raster_fen19))] <- 0
dist_fen19 <- distance(raster_fen19) # raster of distances between pixels that don't have fens and fens
gc()


raster_heather19 <- land_19
values(raster_heather19)[values(raster_heather19)!= 9] <- NA # 9 is the code of heathers
values(raster_heather19)[!is.na(values(raster_heather19))] <- 0
dist_heather19 <- distance(raster_heather19) # raster of distances between pixels that don't have heathers and heathers
gc()


raster_heathergrass19 <- land_19
values(raster_heathergrass19)[values(raster_heathergrass19)!= 10] <- NA # 10 is the code of heathergrass
values(raster_heathergrass19)[!is.na(values(raster_heathergrass19))] <- 0
dist_heathergrass19 <- distance(raster_heathergrass19) # raster of distances between pixels that don't have heathers and heathers
gc()


raster_bog19 <- land_19
values(raster_bog19)[values(raster_bog19)!= 11] <- NA # 11 is the code of bogs
values(raster_bog19)[!is.na(values(raster_bog19))] <- 0
dist_bog19 <- distance(raster_bog19) # raster of distances between pixels that don't have bogs and bogs
gc()


raster_inlandrock19 <- land_19
values(raster_inlandrock19)[values(raster_inlandrock19)!= 12] <- NA # 12 is the code of inlandrocks
values(raster_inlandrock19)[!is.na(values(raster_inlandrock19))] <- 0
dist_inlandrock19 <- distance(raster_inlandrock19) # raster of distances between pixels that don't have inlandrocks and inlandrocks
gc()


raster_saltwater19 <- land_19
values(raster_saltwater19)[values(raster_saltwater19)!= 13] <- NA # 13 is the code of saltwaters
values(raster_saltwater19)[!is.na(values(raster_saltwater19))] <- 0
dist_saltwater19 <- distance(raster_saltwater19) # raster of distances between pixels that don't have saltwaters and saltwaters
gc()


raster_rivers19 <- land_19
values(raster_rivers19)[values(raster_rivers19)!= 14] <- NA # 14 is the code of riverss
values(raster_rivers19)[!is.na(values(raster_rivers19))] <- 0
dist_rivers19 <- distance(raster_rivers19) # raster of distances between pixels that don't have riverss and riverss
gc()


raster_supralitrock19 <- land_19
values(raster_supralitrock19)[values(raster_supralitrock19)!= 15] <- NA # 15 is the code of supralitrocks
values(raster_supralitrock19)[!is.na(values(raster_supralitrock19))] <- 0
dist_supralitrock19 <- distance(raster_supralitrock19) # raster of distances between pixels that don't have supralitrocks and supralitrocks
writeRaster(dist_supralitrock19,
            filename = file.path("C:/Users/elemi.LAPTOP-VIAG61N1/OneDrive - University of Glasgow/dissertation_watervole/layers_27700/coverdist_19",
                                 "dist_supralitrock19copy"))
rm(raster_supralitrock19)
rm(dist_supralitrock19)
gc()



raster_supralitsediment19 <- land_19
values(raster_supralitsediment19)[values(raster_supralitsediment19)!= 16] <- NA # 16 is the code of supralitsediments
values(raster_supralitsediment19)[!is.na(values(raster_supralitsediment19))] <- 0
dist_supralitsediment19 <- distance(raster_supralitsediment19) # raster of distances between pixels that don't have supralitsediments and supralitsediments
gc()


raster_litrock19 <- land_19
values(raster_litrock19)[values(raster_litrock19)!= 17] <- NA # 17 is the code of litrocks
values(raster_litrock19)[!is.na(values(raster_litrock19))] <- 0
dist_litrock19 <- distance(raster_litrock19) # raster of distances between pixels that don't have litrocks and litrocks
gc()


raster_litsediment19 <- land_19
values(raster_litsediment19)[values(raster_litsediment19)!= 18] <- NA # 18 is the code of litsediments
values(raster_litsediment19)[!is.na(values(raster_litsediment19))] <- 0
dist_litsediment19 <- distance(raster_litsediment19) # raster of distances between pixels that don't have litsediments and litsediments
gc()


raster_saltmarsh19 <- land_19
values(raster_saltmarsh19)[values(raster_saltmarsh19)!= 19] <- NA # 19 is the code of saltmarshs
values(raster_saltmarsh19)[!is.na(values(raster_saltmarsh19))] <- 0
dist_saltmarsh19 <- distance(raster_saltmarsh19) # raster of distances between pixels that don't have saltmarshs and saltmarshs
gc()


raster_urban19 <- land_19
values(raster_urban19)[values(raster_urban19)!= 20] <- NA # 20 is the code of urbans
values(raster_urban19)[!is.na(values(raster_urban19))] <- 0
dist_urban19 <- distance(raster_urban19) # raster of distances between pixels that don't have urbans and urbans
gc()


raster_suburban19 <- land_19
values(raster_suburban19)[values(raster_suburban19)!= 21] <- NA # 21 is the code of suburbans
values(raster_suburban19)[!is.na(values(raster_suburban19))] <- 0
dist_suburban19 <- distance(raster_suburban19) # raster of distances between pixels that don't have suburbans and suburbans
gc()