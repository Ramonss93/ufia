# Using rasterEngine to efficiency apply predictions to rasters
library("doParallel")
library("e1071")
library("randomForest")
library("class")
library("magrittr")
library("snow")
library("raster")
library("rgdal")
library("mlr")
library("kernlab")
library("irace")

##################################################################################################
#                            Functions
##################################################################################################

add_UFIA_classes <- function(x) {
  x %<>% ratify()
  rat <- levels(x)[[1]]
  rat$landcover <- c('grass', 'impervious', 'soil', 'tree', 'water')
  levels(x) <- rat
  return(x)
}

UFIA_pal <- c(grass = "#FFFF99", impervious = "#F0027F", soil = "#FDC086", tree = "#7FC97F", water = "#386CB0")



##################################################################################################
#                            Load Image
##################################################################################################

image_name <- "geomatica_SPOT_panshp_wRatios"
image_directory <- "PAN_SPOT"

if (exists(commandArgs())) {
    args <- commandArgs(trailingOnly = T)
    image_directory <- args[1]
    image_name <- args[2]
}

image_path <- paste0("../",image_directory,"/",image_name,".tif")
image <- brick(image_path)

plotRGB(image,4,3,2,stretch = "lin")



##################################################################################################
##################################################################################################



##################################################################################################
#                      Make the Dataframe for classification
##################################################################################################

#  There are two ways to do this.
################################
################################


# 1) Load an ASCII file that was exported from ENVI that contains the band information
#      for every classified pixel.  Need to make sure that this was done on the image with
#      additional features added, if that's desired

          # source("ConvertENVI_ROI_ASCII_toDF.R")
          # InputASCIIPath <-
          # OutputPath <-
          # classified_roi_df <- ConvertEnviROI_toDataFrame(InputASCIIPath)
                                        #

          # write.csv(classified_roi_df, OutputPath)



################################
################################
# 2) Load the shapefiles for each class,
#       These would have been likely generated in ENVI and exported.
#       They should have been drawn on top of the image we are classifying.

# These shapefiles came from ENVI ROI,
# they were drawn on top of the image we are classifying


## Ted's ROIs for training data
t_water <- readOGR(dsn = "../PAN_SPOT/ROIs", layer = "pan_spot_subset_water", encoding = "ESRI Shapefile")
t_grass <- readOGR(dsn = "../PAN_SPOT/ROIs", layer = "pan_spot_subset_grass", encoding = "ESRI Shapefile")
t_tree <- readOGR(dsn = "../PAN_SPOT/ROIs", layer = "pan_spot_subset_tree", encoding = "ESRI Shapefile")
t_soil <- readOGR(dsn = "../PAN_SPOT/ROIs", layer = "pan_spot_subset_soil", encoding = "ESRI Shapefile")
t_impervious <- readOGR(dsn = "../PAN_SPOT/ROIs", layer = "pan_spot_subset_impervious", encoding = "ESRI Shapefile")
names(t_water) <- "water"
names(t_grass) <- "grass"
names(t_tree) <- "tree"
names(t_soil) <- "soil"
names(t_impervious) <- "impervious"

list_classes <- list(t_water, t_grass, t_tree, t_soil, t_impervious)

extract_bind_df_addclass <- function(x) {
  w <- raster::extract(image,x)
  w <- do.call("rbind",w)
  w <- data.frame(w)
  w$Class <- names(x)
  return(w)
}

beginCluster()
b <- lapply(list_classes, function(x) extract_bind_df_addclass(x))
endCluster()

classified_px <- do.call("rbind", b)

classified_px$Class %<>% as.factor()

ted_df <- classified_px

## Lei's ROIs for training data

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

##################################################################################################
##################################################################################################



##################################################################################################
#                           Trying to use package MLR
##################################################################################################
## Create Task, Tune Learners, Create learners, Get Model and Predict
lei_classif.task <- makeClassifTask(id = "lei_panSpot", data = lei_df, target = "Class")
lei_classif.task

ctrl <- makeTuneControlIrace(maxExperiments = 300L)
rdesc <- makeResampleDesc("CV",iters = 3L)

rf.ps <- makeParamSet(
    makeIntegerParam("nodesize", lower = 1L, upper = 20L),
    makeIntegerParam("ntree", lower = 1L, upper = 3L,
                      trafo = function(x) 10^x),
    makeIntegerLearnerParam("mtry", lower = 1L, upper = 10L))

rf.res <- tuneParams("classif.randomForest", lei_classif.task, rdesc, par.set = rf.ps, control = ctrl)



rf.lrn <- makeLearner("classif.randomForest",predict.type="prob", fix.factors.prediction = T)
rf.lrn

svm.lrn <- makeLearner("classif.ksvm", predict.type = "prob", fix.factors.prediction = T)
svm.lrn

knn.lrn <- makeLearner("classif.knn")

knn.mod <- train(knn.lrn, classif.task)
a <- getLearnerModel(knn.mod)

rf.mod <- train(rf.lrn, classif.task)
rf.mod
getLearnerModel(rf.mod)

svm.mod <- train(svm.lrn,classif.task)
svm.mod
getLearnerModel(svm.mod)

good.mod <- getLearnerModel(svm.mod)

i <- predict(small_image,good.mod)
plot(i)


plot


                                        # create multiplexed learner
lrn <- makeModelMultiplexer(list(
    makeLearner("classif.randomForest"),
    makeLearner("classif.ksvm")
    ))

                                        # wrap in tuning
inner <- makeResampleDesc("CV", iters = 3L)
ctrl <- makeTuneControlIrace(maxExperiments = 300)
tune.ps <- makeModelMultiplexerParamSet(lrn,
                                        makeIntegerParam("nodesize", lower = 1L, upper = 20L),
                                        makeIntegerParam("ntree", lower = 1L, upper = 3L,
                                                         trafo = function(x) 10^x),
                                        makeIntegerLearnerParam("mtry", lower = 1L, upper = 10L),
                                        makeDiscreteParam("kernel", values = c("vanilladot", "rbfdot")),
                                        makeNumericParam("sigma", lower = -10, upper = 10,
                                                         trafo = function(x) 2^x,
                                                         requires = quote(kernel == "rbfdot"))
                                        )

lrn <- makeTuneWrapper(lrn, inner, mmce, tune.ps, ctrl)
lrn
task <- makeClassifTask(data = lei_df, target = "Class")
task
outer <- makeResampleDesc("CV")

res <- resample(lrn, task, outer, models = TRUE)
res$models[[1]]
res

## This is a way that failed because it was too big for irace
##                                         # create multiplexed learner
## lrn <- makeModelMultiplexer(list(
##     makeLearner("classif.randomForest"),
##     makeLearner("classif.ksvm"),
##     makeLearner("classif.knn")
##     ))

##                                         # wrap in tuning
## inner <- makeResampleDesc("CV", iters = 3L)
## ctrl <- makeTuneControlIrace(maxExperiments = 500)
## tune.ps <- makeModelMultiplexerParamSet(lrn,
##                                         makeIntegerParam("nodesize", lower = 1L, upper = 20L),
##                                         makeIntegerParam("ntree", lower = 1L, upper = 3L,
##                                                          trafo = function(x) 10^x),
##                                         makeIntegerLearnerParam("mtry", lower = 1L, upper = 10L),
##                                         makeDiscreteParam("kernel", values = c("vanilladot", "polydot", "rbfdot")),
##                                         makeNumericParam("sigma", lower = -10, upper = 10,
##                                                          trafo = function(x) 2^x,
##                                                          requires = quote(kernel == "rbfdot")),
##                                         makeIntegerParam("degree", lower = 2L, upper = 5L,
##                                                          requires = quote(kernel == "polydot")),
##                                         makeIntegerParam("k", lower = 1, upper = 100)
##                                         )

## lrn <- makeTuneWrapper(lrn, inner, mmce, tune.ps, ctrl)
## lrn
## task <- makeClassifTask(data = lei_df, target = "Class")
## task
## outer <- makeResampleDesc("CV")

## res <- resample(lrn, task, outer, models = TRUE)
## res$models[[1]]
## res



##################################################################################################
#                            Create Predictive Models   This is the old way.  I should try with package mlr
##################################################################################################



df <- classified_px

# Subsetting classified pixels
sub <- sample(1:nrow(df),size =1000, replace = F)
df <- df[sub,]

# df_test <- classified_px[-sub,]
# df_test <- df_test[sample(1:nrow(df_test), size = 1000),]

# Generating Formula
pred_cols <- names(image)
class_formula <- paste0("Class ~ ",paste(pred_cols, collapse=" + "))
class_formula


###################################################
#                            Random Forest
###################################################

mtry_range <- seq(1, length(pred_cols)/2, 4)
nodesize_range <- c(5,10,30,50,100)
ntree_range <- seq(100,600,50)

rf_out <- tune(randomForest,
               as.formula(class_formula),
               data = df,
               ranges = list(mtry = mtry_range,
                             nodesize = nodesize_range,
                             ntree = ntree_range))
summary(rf_out)
rf_best_mod <- rf_out$best.model
rf_best_mod

save(rf_best_mod, file =  "classificationModels/rf_best_mod")

###################################################
#                            SVM
###################################################
svm_out <- tune(svm,
                as.formula(class_formula),
                data = df,
                ranges = list(kernel = c("radial","linear"),
                              cost = c(10,90,100,300),
                              gamma = c(0.001,0.01,0.1,1,10,100)))


svm_out <- tune(svm,
                as.formula(class_formula),
                data = df,
                ranges = list(kernel = c("radial","linear"),
                              cost = c(0.1,1,5,10),
                              gamma = c(0.001,0.01,0.1)))

summary(svm_out)
svm_best_mod <- svm_out$best.model
svm_best_mod

###################################################
#                            KNN
###################################################

train <- as.matrix(df[,pred_cols])
cl <- df[,"Class"]
k_range <- seq(1,50,5)

knn_out <- tune.knn(x=train,
                    y = cl,
                    k = k_range,
                    tunecontrol = tune.control(sampling = "boot"))
summary(knn_out)
plot(knn_out)
best_k <- knn_out$best.parameters
best_k

knn_calc <- function(x) knn(train, x, cl, k = best_k)






lei_pred <- predict(svm_best_mod, lei_df)


table(lei_pred,lei_df$Class)
??accuracy
