#' ---
#' title: "Modelling"
#' author: "Eleonora Milea"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#'


# Loading libraries and objects
library(lme4)
library(pROC)
library(visreg)
library(mapview)
load("mink/Robjects/mink9_waterways.RData")



# Preparing data for modelling
# Getting rid of NAs (points in the sea or with no rainfall records i.e. in small islands)
mink2 <- mink
summary(mink2)
mink2 <- subset(mink2, temperature != "NA")
mink2 <- subset(mink2, slope != "NA")
sum(is.na(mink2@data))
summary(mink2@data)
head(mink2@data)


cor_v <-  cor(mink2@data[,4:length(mink2@data)])
cor.test(mink2@data$temperature, mink2@data$elevation)
cor.test(mink2@data$montane_inlandrock500, mink2@data$elevation)



# complex model
model0 <- glm(Occurrence ~ 
                dist_roads +
                dist_paths +
                dist_specialroads +
                dist_water +
                dist_ramsars +
                dist_reserves +
                dist_parks +
                dist_arable +
                dist_coastalhabitats +
                dist_grassland +
                dist_heather +
                dist_montane_inlandrock +
                dist_urban +
                dist_wetland +
                dist_woodland +
              woodland500 +
              wetland500 +
              urban500 +
              freshwater500 +
              grassland500 +
              arable_horticulture500 +
              heather500 +
              montane_inlandrock500 +
              coastal_habitats500 +
                temperature +
                rainfall +
                slope,
              data = mink2@data, family = binomial)


# Model selection
model_best <- step(model0, trace = 0)
summary(model_best)


# Calculating the explained deviance
n_deviance <- model_best[["null.deviance"]]
r_deviance <- model_best[["deviance"]]
e_deviance <- 100 * (n_deviance - r_deviance) / n_deviance


# ROC
par(pty = "s") # to make the graph square
roc_v <- roc(response = mink2@data$Occurrence,
             predictor = model_best$fitted.values, 
             plot = T, legacy.axes = T) # last argument is to invert x axis
# sensitivity (y-axis) is true positive percentage, specificity (x-axis) is false positive percentage
roc_v


# Save file
save.image("mink/workspace/modelworkspace.RData")
