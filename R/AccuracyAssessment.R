# Image based accuracy assessment

library(rgdal)
library(plyr)
library(dplyr)
library(mlr)
                                        #Read in Robi's Shapefile

accuracy_points <- readOGR(dsn = "../accuracy_points", layer = "accuracy_cover_2500")
accuracy_points

plot(accuracy_points)

head(accuracy_points@data)
summary(accuracy_points@data$cover)

                                        # Read in the Image

img_wRatios <- stack("../PAN_SPOT/geomatica_SPOT_panshp_wRatios.tif")

tex <- stack("../PAN_SPOT/geomatica_SPOT_panshp_wRatios_wTexture.tif")

img <- stack(img_wRatios, tex)


                                        # Remove last texture layer
img <- img[[-18]]


                                        # Read in the classification models

best_models <- readRDS("../PAN_SPOT/bestModels.Rdata")
best_models


lei.rf.mod <- best_models[[1]]
lei.svm.mod <- best_models[[2]]
lei.knn.mod <- best_models[[3]]

ted.rf.mod <- best_models[[4]]
ted.svm.mod  <- best_models[[5]]
ted.knn.mod <- best_models[[6]]


                                        # Extract Pixels values at location of Accuracy Assessment points

accuracy_df <- extract(img, accuracy_points)
str(accuracy_df)

a_df <- as.data.frame(accuracy_df)
str(a_df)
summary(a_df)


a_df$Class <- accuracy_points@data$cover

a_df$Class <- mapvalues(a_df$Class,
                        from = c("grass", "imperv", "other", "shadow", "soil", "tree", "water", "wetland"),
                        to = c("grass", "impervious", "other", "shadow", "soil", "tree", "water", "wetland"))

str(a_df)
a_df %<>% na.omit()
str(a_df)
accuracy.tsk <- makeClassifTask(id = "Accuracy", data = a_df, target = "Class")
str(accuracy.tsk)
                                        # Predict Class

#lei.rf Accuracy
lei.rf.pred <- predict(lei.rf.mod, newdata = a_df)
lei.rf.acc <- sum(as.character(lei.rf.pred$data$truth) == as.character(lei.rf.pred$data$response))/accuracy.tsk$task.desc$size
lei.rf.acc

lei.rf.pred <- predict(lei.rf.mod, accuracy.tsk)
lei.rf.acc <- sum(as.character(lei.rf.pred$data$truth) == as.character(lei.rf.pred$data$response))/accuracy.tsk$task.desc$size
lei.rf.acc

#lei.svm prob accuracy
lei.svm.pred <- predict(lei.svm.mod, newdata = a_df, type = "response")
lei.svm.prob.acc <- sum(as.character(lei.svm.pred$data$truth) == as.character(lei.svm.pred$data$response))/accuracy.tsk$task.desc$size
lei.svm.prob.acc

#lei.svm response accuracy
lei.svm.pred2 <- predict(lei.svm.mod$learner.model, a_df)
lei.svm.resp.acc <- sum(as.character(a_df$Class) == as.character(lei.svm.pred2))/accuracy.tsk$task.desc$size
lei.svm.resp.acc


#lei.knn accuracy
lei.knn.pred <- predict(lei.knn.mod, accuracy.tsk)











head(lei.svm.pred2)


lei.knn.pred


ted.rf.pred <- predict(
ted.svm.pred <- predict(
ted.knn.pred <- predict(


