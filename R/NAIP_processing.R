library(mlr)
library(dplyr)
library(magrittr)
library(rgdal)
library(raster)
library(rgeos)

## Functions
# This function overrides the convenience behavior of sample when x has a length of 1
sampleWithoutSurprises <- function(x) {
      if (length(x) <= 1) {
              return(x)
          } else {
                  return(sample(x,1))
              }
  }

# find the mode of a vector, if there is more than one mode, randomly select one of them
my_random_mode <- function(x,na.rm=F) sampleWithoutSurprises(mfv(x))


extract_bind_df_addclass <- function(x) {
  w <- raster::extract(image,x)
  w <- do.call("rbind",w)
  w <- data.frame(w)
  w$Class <- names(x)
  return(w)
}

UFIA_pal <- c(grass = "#FFFF99", impervious = "#F0027F", soil = "#FDC086", tree = "#7FC97F", water = "#386CB0")

# Read in NAIP images

naip <- stack("../NAIP/madison.tif")
ndvi <- stack("../NAIP/madison_NDVI.tif")
savi <- stack("../NAIP/madison_SAVI.tif")
r1 <- stack("../NAIP/madison_wRatio1.tif")
r2 <- stack("../NAIP/madison_wRatio2.tif")
r3 <- stack("../NAIP/madison_wRatio3.tif")



NAIP <- stack(naip, ndvi, savi, r1, r2, r3)
image <- NAIP

### Read in the plot centers shape file
plot_centers <- readOGR(dsn = "../FieldData/PlotCenterShpFile", layer = "plotCenter")
plot_centers <- spTransform(plot_centers,CRSobj = CRS("+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))


## Create polygon shapefile around plot centers...What should the radius be?

rad <- 200
# Make Diamond polygons around each plot
dia <- list()
for (i in seq_along(plot_centers)) {
      dia[i] <- gBuffer(plot_centers[i,], width = rad,quadsegs = 1)
  }
#dia <-do.call(bind,dia)  Don't run this, but if want to combine into one spatialpolygon object do.

## create a directory for the cropped rasters

image_directory <- "NAIP"
dir.create(paste0("../",image_directory,"/RastersAroundPlots"))


## For every plot, crop the master image, and save the small raster in the directory

for (i in seq_along(dia)){
    r <- crop(image,dia[[i]])
    writeRaster(r, file = paste0("../",image_directory,"/RastersAroundPlots/Plot",i,".tif"),overwrite = T)
}

## merge all of these into one raster
rasters_to_merge <- list.files(path = "../NAIP/RastersAroundPlots", full.names = T)
rasters_to_merge <- rasters_to_merge[grep(pattern = "Plot[0-9]*.tif", x = rasters_to_merge)]
rasters_to_merge

r_list <- lapply(rasters_to_merge, FUN = stack)
plots_101 <- do.call(merge, args = r_list)
writeRaster(plots_101, filename = "../NAIP/RastersAroundPlots/merged101plots.tif")
plot(plots101)


# Read in ROI shapefiles for training classifier
l_water <- readOGR(dsn = "../PAN_SPOT/ROIs/lei", layer = "pan_spot_lei_water", encoding = "ESRI Shapefile")
l_grass <- readOGR(dsn = "../PAN_SPOT/ROIs/lei", layer = "pan_spot_lei_grass", encoding = "ESRI Shapefile")
l_tree <- readOGR(dsn = "../PAN_SPOT/ROIs/lei", layer = "pan_spot_lei_tree", encoding = "ESRI Shapefile")
l_soil <- readOGR(dsn = "../PAN_SPOT/ROIs/lei", layer = "pan_spot_lei_soil", encoding = "ESRI Shapefile")
l_impervious <- readOGR(dsn = "../PAN_SPOT/ROIs/lei", layer = "pan_spot_lei_impervious", encoding = "ESRI Shapefile")
names(l_water) <- "water"
names(l_grass) <- "grass"
names(l_tree) <- "tree"
names(l_soil) <- "soil"
names(l_impervious) <- "impervious"

list_classes <- list(l_water, l_grass, l_tree, l_soil, l_impervious)

beginCluster()
b <- lapply(list_classes, function(x) extract_bind_df_addclass(x))
endCluster()


classified_px <- do.call("rbind", b)

classified_px$Class %<>% as.factor()

lei_df <- classified_px
lei_df <- na.omit(lei_df)
write.table(lei_df, paste0("../","NAIP","/ExtractedTrainingDataFrames/lei_roi_train_df.txt"))



# Make Classification Models

d <- read.table(file = "../NAIP/ExtractedTrainingDataFrames/lei_roi_train_df.txt")
head(d)
levels(d$Class)
## task
tsk <- makeClassifTask(id = "naip", data = d, target = "Class")
tsk

## Quick Tune

## learners
rf.lrn <- makeLearner(cl = "classif.randomForest", predict.type = "prob", par.vals = list(ntree = 500, mtry = 4, nodesize = 3))
rf.lrn

svm.lrn <- makeLearner(cl = "classif.ksvm", predict.type = "response",
                       par.vals = list(kernel = "rbfdot", sigma = .1, C = 105))
svm.lrn

knn.lrn <- makeLearner(cl = "classif.knn", par.vals = list(k = 3))

## train

rf.mod <- train(rf.lrn, tsk)
svm.mod <- train(svm.lrn, tsk)
knn.mod <- train(knn.lrn, tsk)

rf.mod$learner.model
rf.mod
svm.mod
knn.mod

## Predict

r10 <- stack("../NAIP/RastersAroundPlots/Plot10.tif")
r <- stack("../NAIP/RastersAroundPlots/merged101plots.tif")

names(r10) <-attributes(svm.mod$learner.model@terms)$term.labels
names(r) <-attributes(svm.mod$learner.model@terms)$term.labels


a <- max(r10)

# for ggplot and knn
r10df <- as.data.frame(r10, xy = T)
rdf <- as.data.frame(r, xy = T)

## Predictions on Rasters
rf_pred_plt10 <- raster::predict(r10, rf.mod$learner.model)
rf_pred_allplts <- raster::predict(r, rf.mod$learner.model)

svm_pred_plt10 <- raster::predict(r10, svm.mod$learner.model)
svm_pred_allplts <- raster::predict(r, svm.mod$learner.model)

## Predictions on Dataframes
## predictions at plot10
rf.pred <- predict(rf.mod, newdata = r10df)
svm.pred <- predict(svm.mod, newdata = r10df)
knn.pred <- predict(knn.mod, newdata = r10df)

knn_pred_plt10 <- raster(r10)
knn_pred_plt10[] <- knn.pred

## predictions at all plots
rf.pred <- predict(rf.mod, newdata = rdf)
svm.pred <- predict(svm.mod, newdata = rdf)
knn.pred <- predict(knn.mod, newdata = rdf)








## Save these predicted rasters

path <- "../NAIP/RastersAroundPlots/ClassifiedRasters/"

writeRaster(rf_pred_plt10, filename = paste0(path, "rf_Plot10.tif"), overwrite = T)
writeRaster(svm_pred_plt10, filename = paste0(path, "svm_Plot10.tif"), overwrite = T)
writeRaster(knn_pred_plt10, filename = paste0(path, "knn_Plot10.tif"), overwrite = T)

writeRaster(rf_pred_allplts, filename = paste0(path, "rf_allPlots.tif"), overwrite = T)
writeRaster(svm_pred_allplts, filename = paste0(path, "svm_allPlots.tif"), overwrite = T)
writeRaster(knn_pred_allplts, filename = paste0(path, "knn_allPlots.tif"), overwrite = T)





# Creating RF and SVM probability raster layers
r <- stack("../PAN_SPOT/RastersAroundPlots/merged101plots.tif")
plotRGB(r, 4,3,2, stretch = "lin")
r <- stack("../PAN_SPOT/RastersAroundPlots/Plot1.tif")
r <- r[[-18]]
names(r) <- attributes(mod$learner.model@terms)$term.labels
rdf <- as.data.frame(r)
a <- predict(list_best_mods[[1]], newdata = rdf)
r_prob.tree <- raster(r)
r_prob.tree[] <- a$data$prob.tree
r_prob.grass <- raster(r)
r_prob.grass[] <- a$data$prob.grass
plot(r_prob.grass)




# Creating Plots
j <- 10
mod <- list_best_mods[[5]]
r <- stack(paste0("../",image_directory,"/RastersAroundPlots/Plot",j,".tif"))
r <- r[[-18]]
names(r) <- attributes(mod$learner.model@terms)$term.labels
xy <- as.data.frame(r, xy = T)
Class <- predict(mod$learner.model, xy)
df <- cbind(xy, Class)
str(C)
str(df)
plt <- ggplot(data = r_df, aes(x = x, y = y, fill = Class)) + geom_raster() + ggtitle(title) + scale_fill_manual(values = UFIA_pal) + coord_equal()




