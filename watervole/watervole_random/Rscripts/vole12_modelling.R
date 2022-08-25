#' ---
#' title: "Modelling"
#' author: "Eleonora Milea"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#'
# Loading libraries and objects
library(pROC) # auc
library(visreg) # plotting effects 
library(mapview)
library(gmodels) # conf intervals
library(car) # vif
library(jtools) # plotting
library(caret) # cross validation
library(ggpubr) # graphs
library(ggplot2)
# load("watervole/watervole_random/Robjects/vole11_patchdist.RData")



#'---
#'
#'#### **Preparing data for modelling**
#'
# Getting rid of NAs (points in the sea or with no rainfall records i.e. in small islands)
vole_random2 <- vole_random
names(vole_random2)
vole_random2@data <- vole_random2@data[, c(1:12, 15, 18:21, 32:34, 44:69)] # delete this before submitting
summary(vole_random2@data)
sum(is.na(vole_random2@data))
vole_random2 <- subset(vole_random2, temperature != "NA")
vole_random2 <- subset(vole_random2, urban1000 > -1)
vole_random2 <- subset(vole_random2, slope != "NA")
summary(vole_random2@data)
sum(is.na(vole_random2@data))
vole_random2 <- subset(vole_random2, mink_suitability != "NA")
sum(is.na(vole_random2@data))
summary(vole_random2@data)


# Take out presence points from 2021 (move this to first script)
vole_random2 <- vole_random2[!(vole_random2$Year == 2021),]
unique(vole_random2$Year)


# Check for correlation 
cor_vole <- cor(vole_random2@data[, 4:length(vole_random2@data)], method = "pearson")
cor.test(vole_random2$temperature, vole_random2$elevation, alternative = "two.sided")
cor.test(vole_random2$montane_inlandrock1000, vole_random2$elevation)
# elevation is correlated with both temperature and inlandrock hence it will be excluded from the model


#'---
#'
#'#### **Modelling**
#'
model_complex <- glm(Occurrence ~
                     dist_roads +
                     dist_paths +
                     dist_specialroads +
                     dist_water +
                     dist_ramsars +
                     dist_reserves +
                     dist_parks +
                     dist_arable +
                     dist_coastalhabitats +
                     dist_acidgrassland +
                     dist_improvgrassland +
                     dist_neutralgrassland +
                     dist_calcgrassland +
                     dist_heather +
                     dist_heathergrassland +
                     dist_montane_inlandrock +
                    dist_patch +
                     dist_urban +
                     dist_suburban +
                     dist_fen_marsh_swamp +
                     dist_bog +
                     dist_coniferwoodland +
                     dist_mixedwoodland +
                     mixed_woodland1000 +
                     conifer_woodland1000 +
                     fen_marsh_swamp1000 +
                       heather_grassland1000 +
                     urban1000 +
                     suburban1000 +
                     freshwater1000 +
                     acid_grassland1000 +
                     neutral_grassland1000 +
                     improv_grassland1000 +
                     calc_grassland1000 +
                     arable_horticulture1000 +
                     heather1000 +
                     montane_inlandrock1000 +
                     coastal_habitats1000 +
                     bog1000 +
                     mink_suitability +
                     temperature +
                     rainfall +
                     slope,
                   data = vole_random2@data, family = binomial)


# Model selection
model_best <- step(model_complex, trace = 0)
summary(model_best)


# Calculating the explained deviance
n_deviance <- model_best[["null.deviance"]]
r_deviance <- model_best[["deviance"]]
e_deviance <- 100 * (n_deviance - r_deviance) / n_deviance



# ROC
par(pty = "s") # to make the graph square
auc_val <- roc(response = vole_random2$Occurrence,
                predictor = model_best$fitted.values, 
                plot = T, legacy.axes = T) # last argument is to invert x axis
# sensitivity (y-axis) is true positive percentage, specificity (x-axis) is false positive percentage
auc_val


# Calculating vif 
vif(model_best)  
 

# Cross-validation 5-fold
ctrl <- trainControl(method = "cv", number = 5)
model_train <- train(Occurrence ~
                       dist_roads +
                       dist_paths +
                       dist_water +
                       dist_ramsars +
                       dist_reserves +
                       dist_parks +
                       dist_arable +
                       dist_coastalhabitats +
                       dist_acidgrassland +
                       dist_improvgrassland +
                       dist_neutralgrassland +
                       dist_calcgrassland +
                       dist_heathergrassland +
                       dist_montane_inlandrock +
                       dist_urban +
                       dist_bog +
                       dist_coniferwoodland +
                       dist_mixedwoodland +
                       mixed_woodland1000 +
                       fen_marsh_swamp1000 +
                       suburban1000 +
                       freshwater1000 +
                       acid_grassland1000 +
                       neutral_grassland1000 +
                       improv_grassland1000 +
                       arable_horticulture1000 +
                       heather1000 +
                       bog1000 +
                       mink_suitability +
                       rainfall +
                       temperature +
                       slope,
                     data = vole_random2@data, method = "glm",
                     trControl = ctrl, family = "binomial")
model_train


# Calculate odd ratios
odd_ratios <- round(exp(coef(model_best)),2)
odd_ratios


#'---
#'
#'#### **Plotting effects**
#'
#'
ggarrange(
  effect_plot(model_best, pred = dist_water, interval = TRUE,
              x.label = "Distance from surface water (m)") +
    # y.label = "Probability of water vole occurrence") +
    scale_x_continuous(breaks = seq(0,3000, 500)) +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_line(),
          axis.title.x = element_text(size = 13,face = "bold", colour = "black"),
       axis.title.y = element_text(colour = "white"),
          axis.text.y = element_text(size = 12, colour = "black"),
          axis.text.x = element_text(size = 12, colour = "black")),

effect_plot(model_best, pred = freshwater1000, interval = TRUE,
            x.label = "Proportion of surface water") +
            # y.label = "Probability of water vole occurrence") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_line(),
        axis.title.x = element_text(size = 13,face = "bold", colour = "black"),
     axis.title.y = element_text(colour = "white"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x = element_text(size = 12, colour = "black")),

effect_plot(model_best, pred = suburban1000, interval = TRUE,
            x.label = "Proportion of suburban areas") +
            # y.label = "Probability of water vole occurrence") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_line(),
        axis.title.x = element_text(size = 13,face = "bold", colour = "black"),
     axis.title.y = element_text(colour = "white"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x = element_text(size = 12, colour = "black")),

effect_plot(model_best, pred = fen_marsh_swamp1000, interval = TRUE,
            x.label = "Proportion of fen, marsh or swamp areas",
            y.label = "Probability of water vole occurrence") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_line(),
        axis.title = element_text(size = 13,face = "bold", colour = "black"),
        # axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x = element_text(size = 12, colour = "black")),

effect_plot(model_best, pred = improv_grassland1000, interval = TRUE,
            x.label = "Proportion of improved grassland") +
  # # y.label = "Probability of water vole occurrence") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_line(),
        axis.title.x = element_text(size = 13,face = "bold", colour = "black"),
        axis.title.y = element_text(colour = "white"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x = element_text(size = 12, colour = "black")),

effect_plot(model_best, pred = dist_calcgrassland, interval = TRUE,
            x.label = "Distance from calcareous grassland (m)") +
            # y.label = "Probability of water vole occurrence") +
  xlim(c(0, 180000)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_line(),
        axis.title.x = element_text(size = 13,face = "bold", colour = "black"),
     axis.title.y = element_text(colour = "white"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x = element_text(size = 12, colour = "black")),

effect_plot(model_best, pred = dist_urban, interval = TRUE,
            x.label = "Distance from urban areas (m)") +
            # y.label = "Probability of water vole occurrence") +
  xlim(c(0, 125000)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_line(),
        axis.title.x = element_text(size = 13,face = "bold", colour = "black"),
     axis.title.y = element_text(colour = "white"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x = element_text(size = 12, colour = "black")),

effect_plot(model_best, pred = dist_bog, interval = TRUE,
            x.label = "Distance from bog areas (m)") +
            # y.label = "Probability of water vole occurrence") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_line(),
        axis.title.x = element_text(size = 13,face = "bold", colour = "black"),
        axis.title.y = element_text(colour = "white"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x = element_text(size = 12, colour = "black")),

effect_plot(model_best, pred = slope, interval = TRUE,
            x.label = "Slope (°)") +
            # y.label = "Probability of water vole occurrence") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_line(),
        axis.title.x = element_text(size = 13,face = "bold", colour = "black"),
     axis.title.y = element_text(colour = "white"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x = element_text(size = 12, colour = "black")))


#'---
#'
#'#### **Tabulating model output**
#'
# Creating matrix by defining dimensions
tb <- matrix(ncol = 6, nrow = length(coef(model_best)))

# Creating object with labels
labels <- c("Intercept", "dist_roads", "dist_paths",
            "dist_water", "dist_ramsars",
            "dist_reserves", "dist_parks", "dist_arable",
            "dist_coastalhabitats", "dist_acidgrassland", "dist_improvgrassland",
            "dist_neutralgrassland", "dist_calcgrassland", "dist_heathergrassland",
            "dist_montane_inlandrock", "dist_urban", "dist_bog", "dist_coniferwoodland",
            "dist_mixedwoodland", "prop_mixedwoodland", "prop_fen_marsh_swamp", "prop_suburban",
            "prop_freshwater", "prop_acidgrassland", "prop_neutralgrassland",
            "prop_improvgrassland", "prop_arable", "prop_heather",
            "prop_bog", "mink_suitability", "temperature", "rainfall", "slope")

# Placing labels into matrix
tb[,1] <- labels

# Changing col names
colnames(tb) <- c("Variable", "β", "LCI", "UCI", "SE", "p")

# Calculating CI and SE (it also returns estimate and p-values)
conf_intervals <- gmodels::ci(model_best)
summary(conf_intervals)
tb[, 2:length(colnames(tb))] <- conf_intervals[,]
summary(tb) 
head(tb)
write.csv(tb, file = "table")


# Save file
# save.image("watervole/watervole_random/workspace/modelworkspace.RData")